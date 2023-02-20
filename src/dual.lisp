(in-package :cl-forward-diff)

(deftype dual () '(sb-ext:simd-pack double-float))
(deftype ext-number () '(or dual real))

(declaim (inline make-dual))
(sera:-> make-dual
         (double-float &optional double-float)
         (values dual &optional))
(defun make-dual (realpart &optional (imagpart 0d0))
  (simd:make-f64.2 realpart imagpart))

(declaim (inline dual-realpart))
(sera:-> dual-realpart (dual)
         (values double-float &optional))
(defun dual-realpart (x)
  (nth-value 0 (simd:f64.2-values x)))

(declaim (inline dual-imagpart))
(sera:-> dual-imagpart (dual)
         (values double-float &optional))
(defun dual-imagpart (x)
  (nth-value 1 (simd:f64.2-values x)))

(defmacro one-arg-decompose ((re im) x-form &body body)
  `(multiple-value-bind (,re ,im)
       (simd:f64.2-values ,x-form)
     ,@body))

(defmacro two-args-decompose ((x-re x-im y-re y-im)
                              (x-form y-form) &body body)
  `(one-arg-decompose (,x-re ,x-im) ,x-form
     (one-arg-decompose (,y-re ,y-im) ,y-form
       ,@body)))

(declaim (inline promote))
(defun promote (x)
  (if (typep x '(sb-ext:simd-pack double-float)) x
      (simd:make-f64.2 (float x 0d0) 0d0)))

;;;; Helper macros
(defmacro declare-inline-math (name args-type return-type)
  `(progn
     (declaim (inline ,name))
     (sera:-> ,name
              ,args-type
              (values ,return-type &optional))))

(defmacro declare-inline-2 (name return-type)
  `(progn
     (declaim (inline ,name))
     (sera:-> ,name (ext-number ext-number)
              (values ,return-type &optional))))

;;;; Arithmetic functions

;; +
(declare-inline-2 two-arg-+ dual)
(defun two-arg-+ (x y)
  (simd:f64.2+
   (promote x)
   (promote y)))

(declare-inline-math + (&rest ext-number) dual)
(defun + (&rest numbers)
  (apply #'simd:f64.2+ (mapcar #'promote numbers)))

(define-compiler-macro + (&rest numbers)
  (let ((length (length numbers)))
    (cond
      ((cl:= length 0)
       (promote 0))
      ((cl:= length 1)
       (first numbers))
      ((cl:= length 2)
       `(two-arg-+ ,(first  numbers)
                   ,(second numbers)))
      (t
       (reduce (lambda (acc number)
                 `(two-arg-+ ,number ,acc))
               numbers)))))

;; -
(declare-inline-2 two-arg-- dual)
(defun two-arg-- (x y)
  (simd:f64.2-
   (promote x)
   (promote y)))

(declare-inline-math - (ext-number &rest ext-number) dual)
(defun - (number &rest more-numbers)
  (apply #'simd:f64.2-
         (promote number)
         (mapcar #'promote more-numbers)))

(define-compiler-macro - (number &rest more-numbers)
  (let ((length (length more-numbers)))
    (cond
      ((cl:= length 0)
       `(two-arg-- 0 ,number))
      ((cl:= length 1)
       `(two-arg-- ,number ,(car more-numbers)))
      (t
       `(two-arg-- ,number (+ ,@more-numbers))))))

;; *
(declare-inline-2 two-arg-* dual)
(defun two-arg-* (x y)
  (two-args-decompose (x-re x-im y-re y-im)
      ((promote x) (promote y))
    (simd:make-f64.2
     (cl:* x-re y-re)
     (cl:+
      (cl:* x-re y-im)
      (cl:* x-im y-re)))))

(declare-inline-math * (&rest ext-number) dual)
(defun * (&rest numbers)
  (if (= (length numbers) 0)
      (promote 1)
      (reduce #'two-arg-*
              (mapcar #'promote numbers))))

(define-compiler-macro * (&rest numbers)
  (let ((length (length numbers)))
    (cond
      ((cl:= length 0)
       (promote 1))
      ((cl:= length 1)
       (first numbers))
      ((cl:= length 2)
       (destructuring-bind (first second)
           numbers
         (cond
           ((eql first 2)
            `(two-arg-+ ,second ,second))
           ((eql second 2)
            `(two-arg-+ ,first ,first))
           (t
            `(two-arg-* ,first ,second)))))
      (t
       (reduce (lambda (acc number)
                 `(two-arg-* ,number ,acc))
               numbers)))))

;; /
(declare-inline-2 two-arg-/ dual)
(defun two-arg-/ (x y)
  (two-args-decompose (x-re x-im y-re y-im)
      ((promote x) (promote y))
    (simd:make-f64.2
     (cl:/ x-re y-re)
     (cl:/
      (cl:-
       (cl:* y-re x-im)
       (cl:* x-re y-im))
      (cl:expt y-re 2)))))

(declare-inline-math / (ext-number &rest ext-number) dual)
(defun / (number &rest more-numbers)
  (if (= (length more-numbers) 0)
      (two-arg-/ (promote 1) number)
      (two-arg-/ number
                 (apply #'* more-numbers))))

(define-compiler-macro / (number &rest more-numbers)
  (let ((length (length more-numbers)))
    (cond
      ((cl:= length 0)
       `(two-arg-/ 1 ,number))
      ((cl:= length 1)
       `(two-arg-/ ,number ,(car more-numbers)))
      (t
       `(two-arg-/ ,number (* ,@more-numbers))))))

;; 1+
(macrolet ((define-inc-dec (name op)
             `(progn
                (declare-inline-math ,name (ext-number) dual)
                (defun ,name (x)
                  (,op x 1)))))
  (define-inc-dec 1+ +)
  (define-inc-dec 1- -))

;;;; min/max
(macrolet ((define-min-max (name op)
             (let ((two-arg-name (intern
                                  (concatenate 'string "TWO-ARG-"
                                               (symbol-name name)))))
               `(progn
                  (declare-inline-2 ,two-arg-name dual)
                  (defun ,two-arg-name (x y)
                    (let ((x (promote x))
                          (y (promote y)))
                      (if (,op (dual-realpart x)
                               (dual-realpart y))
                          x y)))

                  (declare-inline-math ,name (ext-number &rest ext-number) dual)
                  (defun ,name (number &rest more-numbers)
                    (if (cl:= (length more-numbers) 0) number
                        (,two-arg-name number (reduce #',two-arg-name more-numbers))))

                  (define-compiler-macro ,name (number &rest more-numbers)
                    (let ((length (length more-numbers)))
                      (cond
                        ((cl:= length 0) number)
                        ((cl:= length 1)
                         (list ',two-arg-name number (car more-numbers)))
                        (t
                         (reduce (lambda (acc number)
                                   (list ',two-arg-name number acc))
                                 more-numbers :initial-value number)))))))))
  (define-min-max min <)
  (define-min-max max >))

;;;; Miscellaneous math functions

;; expt
(declare-inline-math expt (ext-number real) dual)
(defun expt (base power)
  (one-arg-decompose (re im)
      (promote base)
    (simd:make-f64.2
     (cl:* (cl:expt re power))
     (cl:* (cl:expt re (cl:1- power)) power im))))

(define-compiler-macro expt (&whole whole base power)
  (if (eql power 2)
      `(two-arg-* ,base ,base)
      whole))

;; abs
(declare-inline-math abs (ext-number) dual)
(defun abs (number)
  (one-arg-decompose (re im)
      (promote number)
    (simd:make-f64.2
     (cl:* (cl:abs re))
     (cl:* (cl:signum re) im))))

;; signum
(declare-inline-math signum (ext-number) dual)
(defun signum (number)
  (one-arg-decompose (re im)
      (promote number)
    (declare (ignore im))
    (simd:make-f64.2 (cl:signum re) 0d0)))

;; sqrt
(declare-inline-math sqrt (ext-number) dual)
(defun sqrt (number)
  (one-arg-decompose (re im)
      (promote number)
    (declare (type (double-float 0d0) re))
    (simd:make-f64.2
     (cl:sqrt re)
     (cl:* 5d-1 (cl:/ (cl:sqrt re)) im))))

;;;; Exponent and logarithm

;; exp
(declare-inline-math exp (ext-number) dual)
(defun exp (number)
  (one-arg-decompose (re im)
      (promote number)
    (simd:make-f64.2
     (cl:* (cl:exp re))
     (cl:* (cl:exp re) im))))

;; log
(declare-inline-math log (ext-number) dual)
(defun log (number)
  (one-arg-decompose (re im)
      (promote number)
    (declare (type (double-float 0d0) re))
    (simd:make-f64.2
     (cl:log re)
     (cl:/ im re))))

;;;; Trigonometric

;; sin
(declare-inline-math sin (ext-number) dual)
(defun sin (number)
  (one-arg-decompose (re im)
      (promote number)
    (simd:make-f64.2
     (cl:* (cl:sin re))
     (cl:* (cl:cos re) im))))

;; cos
(declare-inline-math cos (ext-number) dual)
(defun cos (number)
  (one-arg-decompose (re im)
      (promote number)
    (simd:make-f64.2
     (cl:* (cl:+ (cl:cos re)))
     (cl:* (cl:- (cl:sin re)) im))))

;; tan
(declare-inline-math tan (ext-number) dual)
(defun tan (number)
  (one-arg-decompose (re im)
      (promote number)
    (simd:make-f64.2
     (cl:tan re)
     (cl:/ im (cl:expt (cl:cos re) 2)))))

;;;; Hyper-trigonometric

;; sinh
(declare-inline-math sinh (ext-number) dual)
(defun sinh (number)
  (one-arg-decompose (re im)
      (promote number)
    (simd:make-f64.2
     (cl:* (cl:sinh re))
     (cl:* (cl:cosh re) im))))

(declare-inline-math cosh (ext-number) dual)
(defun cosh (number)
  (one-arg-decompose (re im)
      (promote number)
    (simd:make-f64.2
     (cl:* (cl:cosh re))
     (cl:* (cl:sinh re) im))))

(declare-inline-math tanh (ext-number) dual)
(defun tanh (number)
  (one-arg-decompose (re im)
      (promote number)
    (simd:make-f64.2
     (cl:tanh re)
     (cl:/ im (cl:expt (cl:cosh re) 2)))))

;; Convenient reader for dual numbers. I hope this will not affect
;; anyone's reader macro.
(defun read-dual (stream subchar arg)
  (declare (ignore arg))
  (let ((list (read stream t nil t)))
    (if (and (listp list)
             (cl:= (length list) 2))
        (simd:make-f64.2
         (first  list)
         (second list))
        (error "Cannot read: #~c~a"
               subchar list))))

(set-dispatch-macro-character #\# #\D #'read-dual)
