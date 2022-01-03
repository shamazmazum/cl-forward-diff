(defpackage cl-forward-diff
  (:local-nicknames (:sera :serapeum))
  (:use #:cl #:polymorphic-functions)
  (:shadow
   #:= #:+ #:- #:* #:/ #:1+ #:1-
   #:abs #:signum #:expt #:sqrt
   #:sin #:cos #:tan
   #:exp #:log)
  (:export
   ;; Dual numbers
   #:ext-number #:dual
   #:make-dual #:dual-realpart #:dual-imagpart
   ;; Arithmetic functions
   #:= #:+ #:- #:* #:/ #:1+ #:1-
   ;; Miscellaneous math functions
   #:expt #:sqrt #:abs #:signum
   ;; Trigonometry
   #:sin #:cos #:tan
   ;; Exponent and logarithm
   #:exp #:log
   ;; Differentiation helpers
   #:differentiable-univariate   #:ad-univariate
   #:differentiable-multivariate #:ad-multivariate))
