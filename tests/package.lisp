(defpackage cl-forward-diff-tests
  (:use #:cl #:fiveam #:alexandria)
  (:import-from
   #:cl-forward-diff
   #:dual #:dual-realpart
   #:ad-univariate #:ad-multivariate #:to-doubles)
  #.(cl-forward-diff:shadowing-import-math)
  (:export #:run-tests))
