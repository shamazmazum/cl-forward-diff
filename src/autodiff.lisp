(in-package :cl-forward-diff)

(deftype %vector (elt-type) `(simple-array ,elt-type (cl:*)))

(deftype differentiable-multivariate ()
  '(sera:-> ((%vector dual)) (values dual &optional)))

(declaim (inline map-with-indices!))
(defun map-with-indices! (destination function vector)
  (loop for x across vector
        for i fixnum from 0 by 1 do
        (setf (aref destination i)
              (funcall function i x)))
  destination)

(sera:-> to-doubles (sequence)
         (values (%vector double-float) &optional))
(declaim (inline to-doubles))
(defun to-doubles (sequence)
  "Convert a sequence of reals to simple vector of double floats."
  (map '(vector double-float)
       (lambda (x) (float x 0d0))
       sequence))

(sera:-> ad-multivariate
         (differentiable-multivariate (%vector double-float))
         (values (%vector double-float) &optional))
(defun ad-multivariate (function xs)
  "Calculate a gradient of a multivariate function at the point
XS. The function takes a simple array of DUALs as its only argument
and returns a DUAL. XS is a simple array of doubles."
  (declare (optimize (speed 3)))
  (loop with grad = (make-array (length xs) :element-type 'double-float)
        with args = (make-array (length xs) :element-type 'dual :initial-element #d(0d0 0d0))
        for i below (length xs) do
        (setf (aref grad i)
              (dual-imagpart
               (funcall function
                        (map-with-indices!
                         args
                         (lambda (j x)
                           (make-dual x (if (cl:= i j) 1d0 0d0)))
                         xs))))
        finally (return grad)))

(deftype differentiable-univariate ()
  '(sera:-> (dual) (values dual &optional)))

(sera:-> ad-univariate
         (differentiable-univariate real)
         (values double-float &optional))
(defun ad-univariate (function x)
  "Calculate a derivative of a univariate function at the point X."
  (declare (optimize (speed 3))
           (type differentiable-univariate function))
  (dual-imagpart (funcall function (make-dual (float x 0d0) 1d0))))

;; Helper for shadowing math functions in defpackage
(defun shadowing-import-math ()
  '(:shadowing-import-from
    #:cl-forward-diff
    #:+ #:- #:* #:/ #:1+ #:1-
    #:abs #:signum #:expt #:sqrt
    #:sin #:cos #:tan
    #:sinh #:cosh #:tanh
    #:exp #:log
    #:min #:max))
