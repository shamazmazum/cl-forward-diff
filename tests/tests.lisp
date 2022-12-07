(in-package :cl-forward-diff-tests)

(def-suite diff :description "Check differentiation")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (explain! (run suite)))
                 '(diff))))

(defun almost= (x y)
  (< (cl:abs (cl:- x y)) 1f-3))

(in-suite diff)

(serapeum:-> some-calculations
             (dual) (values dual &optional))
(defun some-calculations (x)
  (declare (optimize (speed 3))
           (type dual x))
  (1+ (cos (* 3 x))))

(defun differentiable-function (variant x)
  (declare (optimize (speed 3))
           (type dual x))
  (ecase variant
    (:poly      (+ (* 3 x) (* 5 (expt x 2))))
    (:trig-poly (+ (* 3 (sin x)) (* 5 (expt (sin x) 2))))
    (:mul       (* (log x) x))
    (:div       (/ x (sin x)))
    (:compose   (* (some-calculations x) (some-calculations (1- x))))
    (:hyper     (+ (* (sinh x) (cosh (* 3 x))) (tanh x)))
    (:min       (min x (expt x 2)))
    (:max       (max x (expt x 2)))))

(defun piecewise (x)
  (declare (optimize (speed 3))
           (type dual x))
  (if (evenp (floor (dual-realpart x)))
      x (- x)))

(test equality
  (is-true  (= #d(3.0 4.0) #d(3.0 4.0)))
  (is-false (= #d(3.0 4.0) #d(3.0 5.0)))
  (is-true  (= #d(3.0 4.0) #d(3.0 4.0) #d(3.0 4.0)))
  (is-false (= #d(3.0 4.0) #d(3.0 4.0) #d(3.0 4.1)))

  (is-true  (/= #d(3.0 4.0) #d(3.0 4.4)))
  (is-false (/= #d(3.0 4.0) #d(3.0 4.0)))
  (is-true  (/= #d(3.3 4.0) #d(3.0 4.0) #d(3.0 4.1)))
  (is-false (/= #d(3.0 4.0) #d(3.0 4.0) #d(3.0 4.1))))

(test diff-univariate
  (is (almost= (ad-univariate (curry #'differentiable-function :poly) 2)
               23))
  (is (almost= (ad-univariate (curry #'differentiable-function :trig-poly)
                              (cl:/ pi 3))
               5.830127))
  (is (almost= (ad-univariate (curry #'differentiable-function :mul)
                              (cl:exp 4))
               5))
  (is (almost= (ad-univariate (curry #'differentiable-function :div)
                              (cl:/ pi 2))
               1))
  (is (almost= (ad-univariate (curry #'differentiable-function :compose)
                              (cl:/ pi 6))
               -0.4533825))
  (is (almost= (ad-univariate (curry #'differentiable-function :hyper) 1.5)
               393.5443169))
  (is (almost= (ad-univariate (curry #'differentiable-function :min) 0.2) 0.4))
  (is (almost= (ad-univariate (curry #'differentiable-function :min) 3.0) 1.0))
  (is (almost= (ad-univariate (curry #'differentiable-function :max) 0.2) 1.0))
  (is (almost= (ad-univariate (curry #'differentiable-function :max) 3.0) 6.0))
  (is (almost= (ad-univariate #'piecewise 4.5)  1))
  (is (almost= (ad-univariate #'piecewise 5.5) -1)))

(defun multivar (list)
  (declare (optimize (speed 3)))
  (destructuring-bind (x y)
      list
    (declare (type dual x y))
    (/ (log x) (cos y))))

(test diff-multivariate
  (is-true (every #'almost=
                  (ad-multivariate #'multivar '(2 3))
                  '(-0.50505435 0.09980452))))
