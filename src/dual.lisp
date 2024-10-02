(in-package :cl-forward-diff)

(deftype dual () '(sb-ext:simd-pack double-float))
(deftype ext-number () '(or dual real))
(deftype ext-irrat  () '(or dual float))

(define-condition complex-result (error)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c))
     (format s "The result of operation is known to be complex"))))

;; Working with dual type
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

(defmacro decompose-dual ((re im) form &body body)
  `(multiple-value-bind (,re ,im)
       (simd:f64.2-values ,form)
     ,@body))

(defmacro decompose-args ((&rest args) &body body)
  (car
   (reduce
    (lambda (arg acc)
      (destructuring-bind (re im form) arg
        `((decompose-dual (,re ,im) ,form
            ,@acc))))
    args
    :initial-value body
    :from-end t)))

(declaim (inline promote-to-dual))
(sera:-> promote-to-dual (ext-number) (values dual &optional))
(defun promote-to-dual (x)
  "If x is real, make a dual number (x, 0), otherwise return x"
  (etypecase x
    (real (make-dual (float x 0d0)))
    (dual x)))

(declaim (inline fill-dual-vector))
(sera:-> fill-dual-vector (real) (values dual &optional))
(defun fill-dual-vector (x)
  "Make a dual number (x, x)"
  (let ((x (float x 0d0)))
    (make-dual x x)))


;; Helper macros

(defmacro define-one-arg-fn (name dual-fn real-fn)
  "Define an unary function with the name NAME"
  `(progn
     (defun ,name (x)
       (declare (sb-int:explicit-check))
       (etypecase x
         (dual (,dual-fn x))
         (real (,real-fn x))))

     (sb-c:deftransform ,name ((x) (dual) cl:*)
       '(,dual-fn x))

     (sb-c:deftransform ,name ((x) (real) cl:*)
       '(,real-fn x))))

(defmacro define-two-arg-fn (name dual-fn real-fn)
  "Define a binary function with the name NAME"
  `(progn
     (defun ,name (x y)
       (declare (sb-int:explicit-check))
       (etypecase x
         (dual
          (etypecase y
            (dual (,dual-fn x y))
            (real (,dual-fn x (promote-to-dual y)))))
         (real
          (etypecase y
            (dual (,dual-fn (promote-to-dual x) y))
            (real (,real-fn x y))))))

     (sb-c:defoptimizer (,name sb-c:derive-type) ((x y))
       (let ((dual (sb-kernel:specifier-type 'dual))
             (real (sb-kernel:specifier-type 'real))
             (x-type (sb-c::lvar-type x))
             (y-type (sb-c::lvar-type y)))
         (cond
           ((and (sb-kernel:csubtypep x-type real)
                 (sb-kernel:csubtypep y-type real))
            (sb-kernel:numeric-contagion x y))
           ((or (sb-kernel:csubtypep x-type dual)
                (sb-kernel:csubtypep y-type dual))
            dual))))

     (sb-c:deftransform ,name ((x y) (dual dual) cl:*)
       '(,dual-fn x y))

     (sb-c:deftransform ,name ((x y) (real real) cl:*)
       '(,real-fn x y))))

(defmacro define-arith-0 (name two-arg-fn identity)
  "Define arithmetic functions with signature (&REST NUMBERS)."
  `(progn
     (declaim (inline ,name))
     (sera:-> ,name (&rest ext-number) (values ext-number &optional))
     (defun ,name (&rest numbers)
       (reduce #',two-arg-fn numbers :initial-value ,identity))
     (define-compiler-macro ,name (&rest numbers)
       (cond
         ((null numbers) ,identity)
         ((null (cdr numbers))
          (car numbers))
         (t
          (reduce (lambda (acc number)
                    (list ',two-arg-fn acc number))
                  numbers))))))

(defmacro %define-arith-1 (name two-arg-fn identity)
  "Define arithmetic functions with signature (NUMBER &REST MORE-NUMBERS)."
  `(progn
     (declaim (inline ,name))
     (sera:-> ,name (ext-number &rest ext-number) (values ext-number &optional))
     (defun ,name (number &rest more-numbers)
       (if more-numbers
           (reduce #',two-arg-fn more-numbers :initial-value number)
           (,two-arg-fn ,identity number)))
     (define-compiler-macro ,name (number &rest more-numbers)
       (if (null more-numbers)
           (list ',two-arg-fn ,identity number)
           (reduce
            (lambda (acc x) (list ',two-arg-fn acc x))
            more-numbers
            :initial-value number)))))

(defmacro define-min-max (name op cl-fn)
  "Define MIN or MAX. This is a special case."
  (let ((dual-dual-fn (intern (format nil "DUAL-DUAL-~a" name)))
        (two-arg-fn (intern (format nil "TWO-ARG-~a" name))))
    `(progn
       (declaim (inline ,dual-dual-fn))
       (sera:-> ,dual-dual-fn (dual dual)
                (values dual &optional))
       (defun ,dual-dual-fn (x y)
         (if (,op (dual-realpart x)
                  (dual-realpart y))
             x y))

       (define-two-arg-fn ,two-arg-fn ,dual-dual-fn ,cl-fn)

       (declaim (inline ,name))
       (defun ,name  (number &rest more-numbers)
         (reduce #',two-arg-fn more-numbers :initial-value number))
       (define-compiler-macro ,name (number &rest more-numbers)
         (if (null more-numbers) number
             (reduce (lambda (acc number)
                       (list ',two-arg-fn acc number))
                     more-numbers
                     :initial-value number))))))

;; Type derivation helper for irrational functions.
;; Like the one in src/compiler/float-tran.lisp, but simplier
(defun irrat-derive-type (arg &optional low high)
  (flet ((make-float-type (&optional format)
           (sb-kernel:make-numeric-type
                   :class  'float
                   :format format
                   :low    low
                   :high   high)))
    (let ((dual (sb-kernel:specifier-type 'dual))
          (real (sb-kernel:specifier-type 'real))
          (type (sb-c::lvar-type arg)))
      (when (sb-kernel:csubtypep type (sb-kernel:type-union dual real))
        ;; REAL-PART = ARG-TYPE \ DUAL, i.e. what we know about the
        ;; real part of the union.
        (let ((real-part (sb-kernel:type-intersection type real)))
          (if (sb-kernel:numeric-type-p real-part)
              ;; REAL-PART can be expressed by NUMERIC-TYPE, e.g. it
              ;; is a subtype of INTEGER or DOUBLE-FLOAT.
              (sb-kernel:type-union
               dual (make-float-type
                     (case (sb-kernel:numeric-type-class real-part)
                       ((integer rational) 'single-float)
                       (t (sb-kernel:numeric-type-format real-part)))))
              ;; REAL-PART is a subtype of REAL which cannot be
              ;; expressed by the means of NUMERIC-TYPE.
              (sb-kernel:type-union
               dual (make-float-type))))))))

;; Type derivation helper for rational functions.
(defun rational-derive-type (arg &optional low high)
  (let ((dual (sb-kernel:specifier-type 'dual))
        (real (sb-kernel:specifier-type 'real))
        (type (sb-c::lvar-type arg)))
    (when (sb-kernel:csubtypep type (sb-kernel:type-union dual real))
      ;; REAL-PART = ARG-TYPE \ DUAL, i.e. what we know about the
      ;; real part of the union.
      (let ((real-part (sb-kernel:type-intersection type real)))
        (if (sb-kernel:numeric-type-p real-part)
            ;; REAL-PART can be expressed by NUMERIC-TYPE, e.g. it
            ;; is a subtype of INTEGER or DOUBLE-FLOAT.
            (sb-kernel:type-union
             dual (sb-kernel:make-numeric-type
                   :class  (sb-kernel:numeric-type-class  real-part)
                   :format (sb-kernel:numeric-type-format real-part)
                   :low    low
                   :high   high))
            ;; REAL-PART is a subtype of REAL which cannot be
            ;; expressed by the means of NUMERIC-TYPE.
            type)))))

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
