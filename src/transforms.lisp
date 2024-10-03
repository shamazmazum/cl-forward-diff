(in-package :cl-forward-diff)

;; Arithmetic
;; +

;; Special cases (DUAL + REAL and REAL + DUAL)
;; FIXME: Are they really special?
(sb-c:deftransform two-arg-+ ((x y) (dual real) cl:*)
  '(simd:f64.2+ x (promote-to-dual y)))

(sb-c:deftransform two-arg-+ ((x y) (real dual) cl:*)
  '(simd:f64.2+ (promote-to-dual x) y))

;; *
;; Special cases (DUAL * REAL and REAL * DUAL).
;; We can save some computations
(sb-c:deftransform two-arg-* ((x y) (dual real) cl:*)
  '(simd:f64.2* x (fill-dual-vector y)))

(sb-c:deftransform two-arg-* ((x y) (real dual) cl:*)
  '(simd:f64.2* (fill-dual-vector x) y))

;; Real types are optimized by SBCL.
;; Thx to all gods, that DUAL is the most contagious.
;; NB: These optimizations cannot be done for EXT-NUMBER, because we
;; don't know if Y is more contagious than X.
(sb-c:deftransform two-arg-*
    ((x y) (dual (sb-int:constant-arg (member 2 2.0 2d0))) cl:*)
  '(+ x x))

;; -
;; FIXME: Same question as for +
(sb-c:deftransform two-arg-- ((x y) (dual real) cl:*)
  '(simd:f64.2- x (promote-to-dual y)))

(sb-c:deftransform two-arg-- ((x y) (real dual) cl:*)
  '(simd:f64.2- (promote-to-dual x) y))

;; /
;; We can save some computations, as with * for DUAL / REAL
(sb-c:deftransform two-arg-/ ((x y) (dual real) cl:*)
  '(simd:f64.2/ x (fill-dual-vector y)))

;; Transform to a full DUAL-DUAL operation to avoid calling of
;; TWO-ARG-/.
(sb-c:deftransform two-arg-/ ((x y) (real dual) cl:*)
  '(dual-dual-/ (make-dual (float x 0d0)) y))

;; +/-/*// with identity
;; Again, only for DUAL type, as SBCL makes the same for REAL
(macrolet ((def (op identity)
             `(sb-c:deftransform ,op
                  ((x y) (dual (sb-int:constant-arg real)) cl:*
                   ;; Beware the SNaN and negative zeros
                   :policy (zerop sb-c::float-accuracy))
                (if (= (sb-c:lvar-value y) ,identity)
                'x (sb-c::give-up-ir1-transform)))))
  (def two-arg-+ 0)
  (def two-arg-- 0)
  (def two-arg-* 1)
  (def two-arg-/ 1))

;; Multiplication by zero
(sb-c:deftransform two-arg-*
    ((x y) (dual (sb-int:constant-arg (member 0 0.0 0d0))) cl:*
                 ;; Beware the SNaN
                 :policy (zerop sb-c::float-accuracy))
  '(make-dual 0d0 0d0))

;; expt
(sb-c:deftransform expt ((base power) (dual real) cl:*)
  '(dual-expt base power))

(sb-c:deftransform expt ((base power) (real real) cl:*)
  '(real-expt base power))

(sb-c:deftransform expt
    ((base power) (t (sb-int:constant-arg real)) cl:*)
  (let ((val (sb-c:lvar-value power)))
    (cond
      ;; I skip (= power 0) because I don't know the type of 1 I
      ;; should return.
      ((= val +1) 'base)
      ((= val -1) '(/ base))
      ((= val +2) '(* base base))
      ((= val -2) '(/ (* base base)))
      ((= val +3) '(* base base base))
      ((= val -3) '(/ (* base base base)))
      (t (sb-c::give-up-ir1-transform)))))
