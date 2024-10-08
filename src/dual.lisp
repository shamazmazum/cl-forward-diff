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

(defmacro define-arith-1 (name two-arg-fn &key invert identity)
  "Define arithmetic functions with signature (NUMBER &REST MORE-NUMBERS)."
  (assert
   (or (and invert (not identity))
       (and identity (not invert))))
  (let ((invert-form-1
         (if invert
             `(,invert number)
             `(,two-arg-fn ,identity number)))
        (invert-form-2
         (if invert
             `(list ',invert number)
             `(list ',two-arg-fn ,identity number))))
    `(progn
       (declaim (inline ,name))
       (sera:-> ,name (ext-number &rest ext-number) (values ext-number &optional))
       (defun ,name (number &rest more-numbers)
         (if more-numbers
             (reduce #',two-arg-fn more-numbers :initial-value number)
             ,invert-form-1))
       (define-compiler-macro ,name (number &rest more-numbers)
         (if (null more-numbers)
             ,invert-form-2
             (reduce
              (lambda (acc x) (list ',two-arg-fn acc x))
              more-numbers
              :initial-value number))))))

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

;; Type derivation for binary arithmetic functions
(defun arith-derive-type (x y &key (rational nil))
  (let* ((dual (sb-kernel:specifier-type 'dual))
         (real (sb-kernel:specifier-type 'real))
         (ext  (sb-kernel:type-union dual real))
         (x-type (sb-c::lvar-type x))
         (y-type (sb-c::lvar-type y)))
    (when (and (sb-kernel:csubtypep x-type ext)
               (sb-kernel:csubtypep y-type ext))
      (if (or (sb-kernel:type= x-type dual)
              (sb-kernel:type= y-type dual))
          ;; If one of arguments is surely of type DUAL, then result is DUAL.
          dual
          ;; Nothing is known for sure except the args are
          ;; EXT-NUMBERs. The result must be of type DUAL ∪ CONTAGION(X
          ;; ∩ REAL, Y ∩ REAL). NUMERIC-CONTAGION is not the best thing
          ;; to use here, but it's more of less suitable.
          (let ((x-real (sb-kernel:type-intersection x-type real))
                (y-real (sb-kernel:type-intersection y-type real)))
            (sb-kernel:type-union
             (sb-kernel:numeric-contagion x-real y-real :rational rational)
             dual))))))

;; Type derivation for EXPT
(defun expt-derive-type (base power)
  (let* ((dual (sb-kernel:specifier-type 'dual))
         (real (sb-kernel:specifier-type 'real))
         (ext  (sb-kernel:type-union dual real))
         (base-type  (sb-c::lvar-type base))
         (power-type (sb-c::lvar-type power)))
    (when (and (sb-kernel:csubtypep base-type  ext)
               (sb-kernel:csubtypep power-type real))
      (if (sb-kernel:type= base-type dual)
          ;; If the base is DUAL, then result is DUAL.
          dual
          ;; The base is DUAL ∪ REAL
          (let ((base-real (sb-kernel:type-intersection base-type real)))
            (sb-kernel:type-union
             (sb-kernel:numeric-contagion base-real power-type :rational nil)
             dual))))))

;; DEFGENERIC and compile-time type checking

;; FIXME: Is there SBCL counterpart for it?
(defun types-intersect-p (t1 t2)
  (not (eq (sb-kernel:type-intersection t1 t2)
           sb-kernel:*empty-type*)))

(defun ret-type-wide-enough-p (ftype)
  (let* ((dual (sb-kernel:specifier-type 'dual))
         (real (sb-kernel:specifier-type 'real))
         (ext  (sb-kernel:type-union dual real))
         (rtype (sb-kernel:fun-type-returns ftype)))
    (if (sb-kernel:values-type-p rtype)
        ;; Check that return type is always
        ;; 1) some union of DUAL and a non-null subtype of REAL.
        ;; 2) does not intersect (OR DUAL REAL) at all.
        (every (lambda (type)
                 (or
                  (and (types-intersect-p type dual)
                       (types-intersect-p type real))
                  (not (types-intersect-p type ext))))
               (sb-kernel:values-type-types rtype))
        ;; FIXME: If not a VALUES-TYPE, what else? NIL? Just return t if not sure
        t)))

(defun report-narrow-type (name ftype)
  (warn 'sb-int:type-warning
        :format-control
        #.(concatenate
           'string
           "Function ~a is defined with DEFGENERIC but is too specialized.~%"
           "If a function defined with DEFGENERIC returns a value which type intersects~%"
           "with DUAL, it must be greater than DUAL.~%~%"
           "Derived type: ~a~%~%"
           "Possible fixes: 1) Check the function type. 2) Define it with DEFUN.")
        :format-arguments (list name ftype)))

(defmacro defgeneric (name lambda-list &body body)
  "Define a differentiable function. This macro is identical to DEFUN
with exception of stronger type checking, refusing to compile some
valid Common Lisp.

Namely, if you write the following code:

(serapeum:-> foo ((or dual fixnum)) (values (or dual single-float) &optional))
(defun foo (x)
  (1+ x))

it seems to be perfectly valid in the Common Lisp sense, but still
somewhat nonsensical. E.g. if you pass FIXNUM value to that function,
you will get a FIXNUM, not a SINGLE-FLOAT. This macro checks the
derived result type and if it intersects with (OR REAL DUAL), it
ensures that it is strictly greater than both REAL and DUAL, so union
types can be safely used in type declarations. In the above case it
signals a compile-time waring of type SB-INT:TYPE-WARNING:

~~~
Function AAA is defined with DEFGENERIC but is too specialized.
If a function defined with DEFGENERIC returns a value which type intersects
with DUAL, it must be greater than DUAL.

Derived type: #<FUN-TYPE (FUNCTION
                          ((OR (SIMD-PACK DOUBLE-FLOAT) FIXNUM))
                          (VALUES (SIMD-PACK DOUBLE-FLOAT)
                                  &OPTIONAL))>

Possible fixes: 1) Check the function type. 2) Define it with DEFUN.
~~~

Changing the function type to

(serapeum:-> ((or dual fixnum)) (values (or dual fixnum) &optional))

fixes the error."
  (let ((ftype (gensym)))
    `(progn
       (defun ,name ,lambda-list ,@body)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (let ((,ftype (sb-kernel:specifier-type
                        (sb-impl::%fun-ftype
                         ;; FIXME: I have tp compile this functions twice, as DEFUN
                         ;; already expands to NAMED-LAMBDA, but I need type checking to
                         ;; work at compile time, not load time. Fortunately, it seems to
                         ;; produce no side-effects with possible exception of signalling.
                         (sb-int:named-lambda ,name ,lambda-list
                           (block ,name ,@body))))))
           (unless (ret-type-wide-enough-p ,ftype)
             (report-narrow-type ',name ,ftype)))))))

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

;; Helper for shadowing math functions in defpackage
(defun shadowing-import-math ()
  '(:shadowing-import-from
    #:cl-forward-diff
    #:+ #:- #:* #:/ #:1+ #:1-
    #:abs #:signum #:expt #:sqrt
    #:sin #:cos #:tan
    #:sinh #:cosh #:tanh
    #:exp #:log
    #:min #:max #:defgeneric))
