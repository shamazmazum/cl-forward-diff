(defpackage cl-forward-diff
  (:local-nicknames (:sera :serapeum)
                    (:simd :sb-simd-avx))
  (:use #:cl)
  (:shadow
   #:+ #:- #:* #:/ #:1+ #:1-
   #:abs #:signum #:expt #:sqrt
   #:sin #:cos #:tan
   #:sinh #:cosh #:tanh
   #:exp #:log #:min #:max)
  (:export
   ;; Dual numbers
   #:ext-number #:dual
   #:make-dual #:dual-realpart #:dual-imagpart
   ;; Function types
   #:differentiable-multivariate #:differentiable-univariate
   ;; Arithmetic functions
   #:+ #:- #:* #:/ #:1+ #:1-
   ;; Miscellaneous math functions
   #:expt #:sqrt #:abs #:signum
   ;; Trigonometric functions
   #:sin #:cos #:tan
   ;; Hyper trigonometric functions
   #:sinh #:cosh #:tanh
   ;; Exponent and logarithm
   #:exp #:log
   ;; Minimum and maximum
   #:min #:max
   ;; Differentiation helpers
   #:ad-univariate #:ad-multivariate
   ;; Helpers
   #:shadowing-import-math))
