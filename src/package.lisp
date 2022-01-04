(defpackage cl-forward-diff
  (:local-nicknames (:sera :serapeum))
  (:use #:cl #:polymorphic-functions)
  (:shadow
   #:/= #:= #:+ #:- #:* #:/ #:1+ #:1-
   #:abs #:signum #:expt #:sqrt
   #:sin #:cos #:tan
   #:sinh #:cosh #:tanh
   #:exp #:log)
  (:export
   ;; Dual numbers
   #:ext-number #:dual
   #:make-dual #:dual-realpart #:dual-imagpart
   ;; Equality
   #:= #:/=
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
   ;; Differentiation helpers
   #:ad-univariate #:ad-multivariate
   ;; Helpers
   #:shadowing-import-math))
