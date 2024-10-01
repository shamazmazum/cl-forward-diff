(in-package :cl-forward-diff)

;; COMMUTATIVE is only used for (* x 2) = (+ x x) optimization
(sb-c:defknown (two-arg-+ two-arg-*)
    (ext-number ext-number) ext-number
    (sb-c:movable sb-c:foldable sb-c:flushable sb-c::commutative))

(sb-c:defknown (two-arg--)
    (ext-number ext-number) ext-number
    (sb-c:movable sb-c:foldable sb-c:flushable))

;; Can signal DIVIDE-BY-ZERO
(sb-c:defknown (two-arg-/)
    (ext-number ext-number) ext-number
    (sb-c:movable sb-c:foldable sb-c:unsafely-flushable))

;; These are perfectly safe
(sb-c:defknown (abs signum exp sin cos tan sinh cosh tanh)
    (ext-number) ext-number
    (sb-c:movable sb-c:foldable sb-c:flushable))

;; Can signal an error if arguments have unsuitable values
(sb-c:defknown (sqrt log) (ext-number) ext-number
    (sb-c:movable sb-c:foldable sb-c:unsafely-flushable))

;; Perfectly safe, binary
(sb-c:defknown (two-arg-min two-arg-max)
    (ext-number ext-number) ext-number
    (sb-c:movable sb-c:foldable sb-c:flushable))

;; Can signal an error
(sb-c:defknown (expt)
    (ext-number real) ext-number
    (sb-c:movable sb-c:foldable sb-c:unsafely-flushable))
