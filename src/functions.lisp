(in-package :cl-forward-diff)

;; Arithmetic
;; +
(define-two-arg-fn two-arg-+ simd:f64.2+ cl:+)
(define-arith-0 + two-arg-+ 0)

;; Special cases (DUAL + REAL and REAL + DUAL)
;; FIXME: Are they really special?
(sb-c:deftransform two-arg-+ ((x y) (dual real) cl:*)
  '(simd:f64.2+ x (promote-to-dual y)))

(sb-c:deftransform two-arg-+ ((x y) (real dual) cl:*)
  '(simd:f64.2+ (promote-to-dual x) y))

;; *
(declaim (inline dual-dual-*))
(sera:-> dual-dual-* (dual dual)
         (values dual &optional))
(defun dual-dual-* (x y)
  (decompose-args ((x-re x-im x)
                   (y-re y-im y))
    (make-dual
     (cl:* x-re y-re)
     (cl:+
      (cl:* x-re y-im)
      (cl:* x-im y-re)))))

(define-two-arg-fn two-arg-* dual-dual-* cl:*)
(define-arith-0 * two-arg-* 1)

(sb-c:deftransform two-arg-*
    ((x y) (ext-number (sb-int:constant-arg (member 2 2.0 2d0))) cl:*)
  '(+ x x))

;; Special cases (DUAL * REAL and REAL * DUAL).
;; We can save some computations
(sb-c:deftransform two-arg-* ((x y) (dual real) cl:*)
  '(simd:f64.2* x (fill-dual-vector y)))

(sb-c:deftransform two-arg-* ((x y) (real dual) cl:*)
  '(simd:f64.2* (fill-dual-vector x) y))

;; -
(define-two-arg-fn two-arg-- simd:f64.2- cl:-)
(define-arith-1 - two-arg-- 0)

(sb-c:deftransform two-arg-- ((x y) (dual real) cl:*)
  '(simd:f64.2- x (promote-to-dual y)))

(sb-c:deftransform two-arg-- ((x y) (real dual) cl:*)
  '(simd:f64.2- (promote-to-dual x) y))

;; /
(declaim (inline dual-dual-/))
(sera:-> dual-dual-/ (dual dual)
         (values dual &optional))
(defun dual-dual-/ (x y)
  (decompose-args ((x-re x-im x)
                   (y-re y-im y))
    (make-dual
     (cl:/ x-re y-re)
     (cl:/
      (cl:-
       (cl:* y-re x-im)
       (cl:* x-re y-im))
      (cl:expt y-re 2)))))

(define-two-arg-fn two-arg-/ dual-dual-/ cl:/)
(define-arith-1 / two-arg-/ 1)

;; The single special case: DUAL / REAL
;; We can save some computations, as with *
(sb-c:deftransform two-arg-/ ((x y) (dual real) cl:*)
  '(simd:f64.2/ x (fill-dual-vector y)))

;; 1+ / 1-
;; I guess I just inline these
(declaim (inline 1-))
(defun 1- (x) (two-arg-- x 1))

(declaim (inline 1+))
(defun 1+ (x) (two-arg-+ x 1))

;; Miscellaneous math functions
;; MIN / MAX (They are so special, so they have a dedicated macro for them).
(define-min-max max > cl:max)
(define-min-max min < cl:min)

;; abs
(declaim (inline dual-abs))
(sera:-> dual-abs (dual)
         (values dual &optional))
(defun dual-abs (x)
  (decompose-dual (re im) x
    (make-dual
     (cl:* (cl:abs re))
     (cl:* (cl:signum re) im))))

(define-one-arg-fn abs dual-abs cl:abs)

;; signum
(declaim (inline dual-signum))
(sera:-> dual-signum (dual)
         (values dual &optional))
(defun dual-signum (x)
  (decompose-dual (re im) x
    (declare (ignore im))
    (make-dual (cl:signum re))))

(define-one-arg-fn signum dual-signum cl:signum)

;; Power / Exponentiation
;; expt
(declaim (inline real-expt))
(sera:-> real-expt (real real)
         (values real &optional))
(defun real-expt (base power)
  "EXPT which signals an error instead of returning COMPLEX result"
  (if (and (< base 0) (not (integerp power)))
      (error 'complex-result)
      (cl:expt base power)))

(declaim (inline dual-expt))
(sera:-> dual-expt (dual real)
         (values dual &optional))
(defun dual-expt (base power)
  (decompose-dual (re im) base
    (make-dual
     (cl:* (real-expt re power))
     (cl:* (real-expt re (cl:1- power)) power im))))

(defun expt (base power)
  (declare (sb-int:explicit-check))
  (assert (typep power 'real))
  (etypecase base
    (dual (dual-expt base power))
    (real (real-expt base power))))

(sb-c:deftransform expt ((base power) (dual real) cl:*)
  '(dual-expt base power))

(sb-c:deftransform expt ((base power) (real real) cl:*)
  '(real-expt base power))

;; sqrt
(declaim (inline dual-sqrt))
(sera:-> dual-sqrt (dual)
         (values dual &optional))
(defun dual-sqrt (x)
  (decompose-dual (re im) x
    (when (< re 0)
      (error 'complex-result))
    (let ((sqrt (cl:sqrt re)))
      (make-dual
       sqrt (cl:* (cl:/ sqrt) 1/2 im)))))

(define-one-arg-fn sqrt dual-sqrt cl:sqrt)

;; exp
(declaim (inline dual-exp))
(sera:-> dual-exp (dual)
         (values dual &optional))
(defun dual-exp (x)
  (decompose-dual (re im) x
    (let ((exp (cl:exp re)))
      (make-dual exp (cl:* exp im)))))

(define-one-arg-fn exp dual-exp cl:exp)

;; log
(declaim (inline dual-log))
(sera:-> dual-log (dual)
         (values dual &optional))
(defun dual-log (x)
  (decompose-dual (re im) x
    (when (< re 0)
      (error 'complex-result))
    (make-dual
     (cl:log re)
     (cl:/ im re))))

(define-one-arg-fn log dual-log cl:log)

;; Trigonometric
;; sin
(declaim (inline dual-sin))
(sera:-> dual-sin (dual)
         (values dual &optional))
(defun dual-sin (x)
  (decompose-dual (re im) x
    (make-dual
     (cl:* (cl:sin re))
     (cl:* (cl:cos re) im))))

(define-one-arg-fn sin dual-sin cl:sin)

;; cos
(declaim (inline dual-cos))
(sera:-> dual-cos (dual)
         (values dual &optional))
(defun dual-cos (x)
  (decompose-dual (re im) x
    (make-dual
     (cl:* (cl:+ (cl:cos re)))
     (cl:* (cl:- (cl:sin re)) im))))

(define-one-arg-fn cos dual-cos cl:cos)

;; tan
(declaim (inline dual-tan))
(sera:-> dual-tan (dual)
         (values dual &optional))
(defun dual-tan (x)
  (decompose-dual (re im) x
    (make-dual
     (cl:tan re)
     (cl:/ im (cl:expt (cl:cos re) 2)))))

(define-one-arg-fn tan dual-tan cl:tan)

;; Hyper-trigonometric
;; sinh
(declaim (inline dual-sinh))
(sera:-> dual-sinh (dual)
         (values dual &optional))
(defun dual-sinh (x)
  (decompose-dual (re im) x
    (make-dual
     (cl:* (cl:sinh re))
     (cl:* (cl:cosh re) im))))

(define-one-arg-fn sinh dual-sinh cl:sinh)

;; cosh
(declaim (inline dual-cosh))
(sera:-> dual-cosh (dual)
         (values dual &optional))
(defun dual-cosh (x)
  (decompose-dual (re im) x
    (make-dual
     (cl:* (cl:cosh re))
     (cl:* (cl:sinh re) im))))

(define-one-arg-fn cosh dual-cosh cl:cosh)

;; tanh
(declaim (inline dual-tanh))
(sera:-> dual-tanh (dual)
         (values dual &optional))
(defun dual-tanh (x)
  (decompose-dual (re im) x
    (make-dual
     (cl:tanh re)
     (cl:/ im (cl:expt (cl:cosh re) 2)))))

(define-one-arg-fn tanh dual-tanh cl:tanh)
