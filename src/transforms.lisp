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
(sb-c:deftransform two-arg-*
    ((x y) (dual (sb-int:constant-arg (member 2 2.0 2d0))) cl:*)
  '(+ x x))

;; Special cases (DUAL * REAL and REAL * DUAL).
;; We can save some computations
(sb-c:deftransform two-arg-* ((x y) (dual real) cl:*)
  '(simd:f64.2* x (fill-dual-vector y)))

(sb-c:deftransform two-arg-* ((x y) (real dual) cl:*)
  '(simd:f64.2* (fill-dual-vector x) y))

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

;; To avoid calling of TWO-ARG-/
(sb-c:deftransform two-arg-/ ((x y) (real dual) cl:*)
  '(* x (expt y -1)))

;; expt
(sb-c:deftransform expt ((base power) (dual real) cl:*)
  '(dual-expt base power))

(sb-c:deftransform expt ((base power) (real real) cl:*)
  '(real-expt base power))
