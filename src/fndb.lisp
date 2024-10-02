(in-package :cl-forward-diff)

;; COMMUTATIVE is only used for (* x 2) = (+ x x) optimization
(sb-c:defknown (two-arg-+ two-arg-*)
    (ext-number ext-number) ext-number
    (sb-c:movable sb-c:foldable sb-c:flushable sb-c::commutative))

(sb-c:defknown two-arg--
    (ext-number ext-number) ext-number
    (sb-c:movable sb-c:foldable sb-c:flushable))

;; Can signal DIVIDE-BY-ZERO
(sb-c:defknown two-arg-/
    (ext-number ext-number) ext-number
    (sb-c:movable sb-c:foldable sb-c:unsafely-flushable))

;; These are perfectly safe (rational)
(sb-c:defknown (abs signum)
    (ext-number) ext-number
    (sb-c:movable sb-c:foldable sb-c:flushable))

;; These are perfectly safe (irrational)
(sb-c:defknown (exp tan sinh)
    (ext-number) ext-irrat
    (sb-c:movable sb-c:foldable sb-c:flushable))

;; Negation is pure and rational (in the sense that the result is
;; rational if the argument is rational).
(sb-c:defknown (negate)
    (ext-number) ext-number
    (sb-c:movable sb-c:foldable sb-c:flushable))

;; Some functions with restricted domain
(sb-c:defknown (sin cos tanh)
    (ext-number) (or dual (float -1.0 1.0))
    (sb-c:movable sb-c:foldable sb-c:flushable))

(sb-c:defknown cosh
    (ext-number) (or dual (float 1.0))
    (sb-c:movable sb-c:foldable sb-c:flushable))

;; Can signal an error if arguments have unsuitable values
(sb-c:defknown (sqrt log) ((or dual (real 0))) ext-irrat
    (sb-c:movable sb-c:foldable sb-c:unsafely-flushable))

;; Perfectly safe, binary
(sb-c:defknown (two-arg-min two-arg-max)
    (ext-number ext-number) ext-number
    (sb-c:movable sb-c:foldable sb-c:flushable))

;; Can signal an error
(sb-c:defknown expt
    ((or dual (real 0)) real) ext-number
    (sb-c:movable sb-c:foldable sb-c:unsafely-flushable))
