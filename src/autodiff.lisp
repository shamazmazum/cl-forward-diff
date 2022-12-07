(in-package :cl-forward-diff)

;; List of single-floats, I cannot express it in CL
(deftype differentiable-multivariate ()
  '(sera:-> (list) (values dual &optional)))

(sera:-> ad-multivariate
         (differentiable-multivariate list)
         (values list &optional))
(defun ad-multivariate (function xs)
  (declare (optimize (speed 3))
           (type differentiable-multivariate function)
           (type list xs))
  (loop with length = (length xs)
        for i below length collect
       (dual-imagpart
        (funcall function
                 (loop for j from 0 by 1 for x in xs collect
                      (make-dual (float x 0f0)
                                 (if (cl:= i j)
                                     1f0 0f0)))))))

(deftype differentiable-univariate ()
  '(sera:-> (dual) (values dual &optional)))

(sera:-> ad-univariate
         (differentiable-univariate real)
         (values single-float &optional))
(defun ad-univariate (function x)
  (declare (optimize (speed 3))
           (type differentiable-univariate function))
  (first (ad-multivariate
          (lambda (x)
            (funcall function (first x)))
          (list x))))

;; Helper for shadowing math functions in defpackage
(defun shadowing-import-math ()
  '(:shadowing-import-from
    #:cl-forward-diff
    #:= #:/=
    #:+ #:- #:* #:/ #:1+ #:1-
    #:abs #:signum #:expt #:sqrt
    #:sin #:cos #:tan
    #:sinh #:cosh #:tanh
    #:exp #:log
    #:min #:max))
