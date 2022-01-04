(in-package :cl-forward-diff)

;; Since there are not parametric types in CL, just define real and
;; imaginary parts to be single-float
(declaim (inline make-dual%))
(defstruct (dual (:constructor make-dual%))
  (realpart 0f0 :type single-float)
  (imagpart 0f0 :type single-float))

#+sbcl
(sb-c:defknown make-dual (single-float single-float) dual
    (sb-c:movable sb-c:flushable))

(sera:-> make-dual
         (single-float single-float)
         (values dual &optional))
(defun make-dual (x y)
  (make-dual% :realpart x :imagpart y))

(deftype ext-number () '(or dual real))

(define-polymorphic-function promote-to-dual (x))

(defpolymorph promote-to-dual ((x real)) dual
  (make-dual (float x 0f0) 0f0))

(defpolymorph promote-to-dual ((x dual)) dual
  x)

(defmacro two-arg-dual-decompose ((x-re x-im y-re y-im) (x y) &body body)
  `(let ((,x-re (dual-realpart ,x))
         (,x-im (dual-imagpart ,x))
         (,y-re (dual-realpart ,y))
         (,y-im (dual-imagpart ,y)))
     ,@body))

(defmacro one-arg-dual-decompose ((re im) x &body body)
  `(let ((,re (dual-realpart ,x))
         (,im (dual-imagpart ,x)))
     ,@body))

;; Arithmetic functions for dual numbers
(declaim (inline two-arg-dual-/=
                 two-arg-dual-=
                 two-arg-dual-+
                 two-arg-dual--
                 two-arg-dual-*
                 two-arg-dual-/))

(defun two-arg-dual-/= (x y)
  (two-arg-dual-decompose (x-re x-im y-re y-im)
      (x y)
    (or (cl:/= x-re y-re)
        (cl:/= x-im y-im))))

(defun two-arg-dual-= (x y)
  (two-arg-dual-decompose (x-re x-im y-re y-im)
      (x y)
    (and (cl:= x-re y-re)
         (cl:= x-im y-im))))

(defun two-arg-dual-+ (x y)
  (two-arg-dual-decompose (x-re x-im y-re y-im)
      (x y)
    (make-dual (cl:+ x-re y-re)
               (cl:+ x-im y-im))))
        
(defun two-arg-dual-- (x y)
  (two-arg-dual-decompose (x-re x-im y-re y-im)
      (x y)
    (make-dual (cl:- x-re y-re)
               (cl:- x-im y-im))))

(defun two-arg-dual-* (x y)
  (two-arg-dual-decompose (x-re x-im y-re y-im)
      (x y)
    (make-dual (cl:* x-re y-re)
               (cl:+ (cl:* x-re y-im)
                     (cl:* y-re x-im)))))

(defun two-arg-dual-/ (x y)
  (two-arg-dual-decompose (x-re x-im y-re y-im)
      (x y)
    (make-dual (cl:/ x-re y-re)
               (cl:/ (cl:- (cl:* y-re x-im)
                           (cl:* x-re y-im))
                     (cl:expt y-re 2)))))

;; Arithmetic and equality polymorphs
(define-polymorphic-function /= (number &rest numbers))
(define-polymorphic-function =  (number &rest numbers))
(define-polymorphic-function +  (&rest numbers))
(define-polymorphic-function -  (number &rest numbers))
(define-polymorphic-function *  (&rest numbers))
(define-polymorphic-function /  (number &rest numbers))

;; =
(defpolymorph = ((x ext-number))
    (values (eql t) &optional)
  (declare (ignore x))
  t)

(defpolymorph = ((x ext-number)
                 (y ext-number))
    (values boolean &optional)
  (two-arg-dual-=
   (promote-to-dual x)
   (promote-to-dual y)))

(defpolymorph (= :inline t) ((x ext-number)
                             (y ext-number)
                             &rest numbers)
    (values boolean &optional)
  (if (= x y)
      (apply #'= y (car numbers) (cdr numbers))))

;; /=
(defpolymorph /= ((x ext-number))
    (values (eql t) &optional)
  (declare (ignore x))
  t)

(defpolymorph /= ((x ext-number)
                  (y ext-number))
    (values boolean &optional)
  (two-arg-dual-/=
   (promote-to-dual x)
   (promote-to-dual y)))

(defpolymorph (/= :inline t) ((x ext-number)
                              (y ext-number)
                              &rest numbers)
    (values boolean &optional)
  (if (/= x y)
      (apply #'/= y (car numbers) (cdr numbers))))

;; +
(defpolymorph + () (eql 0) 0)

(defpolymorph + ((x ext-number))
    (values ext-number &optional)
  x)

(defpolymorph + ((x ext-number)
                 (y ext-number))
    (values dual &optional)
  (two-arg-dual-+
   (promote-to-dual x)
   (promote-to-dual y)))

(defpolymorph (+ :inline t) ((x ext-number)
                             (y ext-number)
                             &rest numbers)
    (values dual &optional)
  (reduce (lambda (acc x)
            (two-arg-dual-+ acc (promote-to-dual x)))
          numbers
          :initial-value (+ x y)))

;; -
(defpolymorph (- :inline t) ((x ext-number))
    (values ext-number &optional)
  (- (+) x))

(defpolymorph - ((x ext-number)
                 (y ext-number))
    (values dual &optional)
  (two-arg-dual--
   (promote-to-dual x)
   (promote-to-dual y)))

(defpolymorph (- :inline t) ((x ext-number)
                             (y ext-number)
                             &rest numbers)
    (values dual &optional)
  (reduce (lambda (acc x)
            (two-arg-dual-- acc (promote-to-dual x)))
          numbers
          :initial-value (- x y)))

;; * (maybe add some special cases like (* x 2) = (+ x x))
(defpolymorph * () (eql 1) 1)

(defpolymorph * ((x ext-number))
    (values ext-number &optional)
  x)

(defpolymorph * ((x ext-number)
                 (y ext-number))
    (values dual &optional)
  (two-arg-dual-*
   (promote-to-dual x)
   (promote-to-dual y)))

(defpolymorph (* :inline t) ((x ext-number)
                             (y ext-number)
                             &rest numbers)
    (values dual &optional)
  (reduce (lambda (acc x)
            (two-arg-dual-* acc (promote-to-dual x)))
          numbers
          :initial-value (* x y)))

;; /
(defpolymorph (/ :inline t) ((x ext-number))
    (values ext-number &optional)
  (/ (*) x))

(defpolymorph / ((x ext-number)
                 (y ext-number))
    (values dual &optional)
  (two-arg-dual-/
   (promote-to-dual x)
   (promote-to-dual y)))

(defpolymorph (/ :inline t) ((x ext-number)
                             (y ext-number)
                             &rest numbers)
    (values dual &optional)
  (reduce (lambda (acc x)
            (two-arg-dual-/ acc (promote-to-dual x)))
          numbers
          :initial-value (/ x y)))

;; Miscellaneous math functions
(define-polymorphic-function expt (base power))

(defpolymorph expt ((base  ext-number)
                    (power real))
    dual
  (one-arg-dual-decompose (re im)
      (promote-to-dual base)
    (make-dual (cl:* (cl:expt re power))
               (cl:* (cl:expt re (cl:1- power)) power im))))

(defpolymorph expt ((base  ext-number)
                    (power (eql 2)))
    dual
  (declare (ignore power))
  (* base base))

;; Shorter body, but with redundant promotions: (- x 1)
(defpolymorph 1- ((x ext-number))
    dual
  (one-arg-dual-decompose (re im)
      (promote-to-dual x)
    (make-dual (cl:1- re) im)))

;; Shorter body, but with redundant promotions: (+ x 1)
(defpolymorph 1+ ((x ext-number))
    dual
  (one-arg-dual-decompose (re im)
      (promote-to-dual x)
    (make-dual (cl:1+ re) im)))

(defpolymorph abs ((x ext-number))
    dual
  (one-arg-dual-decompose (re im)
      (promote-to-dual x)
    (make-dual (cl:* (cl:abs re))
               (cl:* (cl:signum re) im))))

(defpolymorph signum ((x ext-number))
    dual
  (one-arg-dual-decompose (re im)
      (promote-to-dual x)
    (declare (ignore im))
    (make-dual (cl:signum re) 0f0)))

(defpolymorph sqrt ((x ext-number))
    dual
  (one-arg-dual-decompose (re im)
      (promote-to-dual x)
    (declare (type (single-float 0f0) re))
    (make-dual (cl:sqrt re)
               (cl:* 5f-1 (cl:/ (cl:sqrt re)) im))))

;; Exponent and logarithm
(defpolymorph exp ((x ext-number))
    dual
  (one-arg-dual-decompose (re im)
      (promote-to-dual x)
    (make-dual (cl:* (cl:exp re))
               (cl:* (cl:exp re) im))))

(defpolymorph log ((x ext-number))
    dual
  (one-arg-dual-decompose (re im)
      (promote-to-dual x)
    (declare (type (single-float 0f0) re))
    (make-dual (cl:log re)
               (cl:/ im re))))

;; Trigonometric
(defpolymorph sin ((x ext-number))
    dual
  (one-arg-dual-decompose (re im)
      (promote-to-dual x)
    (make-dual (cl:* (cl:sin re))
               (cl:* (cl:cos re) im))))

(defpolymorph cos ((x ext-number))
    dual
  (one-arg-dual-decompose (re im)
      (promote-to-dual x)
    (make-dual (cl:* (cl:+ (cl:cos re)))
               (cl:* (cl:- (cl:sin re)) im))))

(defpolymorph tan ((x ext-number))
    dual
  (one-arg-dual-decompose (re im)
      (promote-to-dual x)
    (make-dual (cl:tan re)
               (cl:/ im (cl:expt (cl:cos re) 2)))))

;; Hyper-trigonometric
(defpolymorph sinh ((x ext-number))
    dual
  (one-arg-dual-decompose (re im)
      (promote-to-dual x)
    (make-dual (cl:* (cl:sinh re))
               (cl:* (cl:cosh re) im))))

(defpolymorph cosh ((x ext-number))
    dual
  (one-arg-dual-decompose (re im)
      (promote-to-dual x)
    (make-dual (cl:* (cl:cosh re))
               (cl:* (cl:sinh re) im))))

(defpolymorph tanh ((x ext-number))
    dual
  (one-arg-dual-decompose (re im)
      (promote-to-dual x)
    (make-dual (cl:tanh re)
               (cl:/ im (cl:expt (cl:cosh re) 2)))))

;; Convenient printer/reader for dual numbers. I hope this will not
;; affect anyone's reader macro.
(defmethod print-object ((dual dual) stream)
  (format stream "#D(~f ~f)"
          (dual-realpart dual)
          (dual-imagpart dual)))

(defun read-dual (stream subchar arg)
  (declare (ignore arg))
  (let ((list (read stream)))
    (when (cl:/= (length list) 2)
      (error "Cannot read: #~c~a"
             subchar list))
    `(make-dual ,(first  list) ,(second list))))

(set-dispatch-macro-character #\# #\D #'read-dual)
