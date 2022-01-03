(defpackage cl-forward-diff-tests
  (:use #:cl #:fiveam #:alexandria)
  (:import-from
   #:cl-forward-diff
   #:dual #:dual-realpart
   #:ad-univariate #:ad-multivariate)
  (:shadowing-import-from
   #:cl-forward-diff
   #:+ #:- #:* #:/ #:expt
   #:1+ #:1-
   #:sin #:cos #:tan
   #:exp #:log)
  (:export #:run-tests))
