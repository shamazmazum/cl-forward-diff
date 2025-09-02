(in-package :cl-forward-diff-tests)

(def-suite diff  :description "Check differentiation")
(def-suite eval  :description "Evaluates into the same")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(diff eval))))

(defun ≈ (x y &optional (diff 1d-6))
  (< (cl:abs (cl:- x y)) diff))

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
    (:compose   (* (some-calculations x) (some-calculations (1- x)) (some-calculations (1+ x))))
    (:hyper     (+ (* (sinh x) (cosh (* 3 x))) (tanh x)))
    (:min       (min x (expt x 2)))
    (:max       (max x (expt x 2)))))

(defun piecewise (x)
  (declare (optimize (speed 3))
           (type dual x))
  (if (evenp (floor (dual-realpart x)))
      x (- x)))

(defun expt-calc-1 (x)
  (declare (optimize (speed 3))
           (type dual x))
  (expt (abs (1- x)) 2.5d0))

(defun expt-calc-2 (x)
  (declare (optimize (speed 3))
           (type dual x))
  (expt (sin x) 2.5d0))

(defun expt-calc-3 (x)
  (declare (optimize (speed 3))
           (type dual x))
  (expt x 3))

(test diff-univariate
  (is (≈ (ad-univariate (curry #'differentiable-function :poly) 2)
         23))
  (is (≈ (ad-univariate (curry #'differentiable-function :trig-poly)
                        (cl:/ pi 3))
         5.830127d0))
  (is (≈ (ad-univariate (curry #'differentiable-function :mul)
                        (cl:exp 4))
         5))
  (is (≈ (ad-univariate (curry #'differentiable-function :div)
                        (cl:/ pi 2))
         1))
  (is (≈ (ad-univariate (curry #'differentiable-function :compose)
                        (cl:/ pi 6))
         2.9996995d0))
  (is (≈ (ad-univariate (curry #'differentiable-function :hyper) 1.5)
         393.5443168880577d0))
  (is (≈ (ad-univariate (curry #'differentiable-function :min) 0.2) 0.4))
  (is (≈ (ad-univariate (curry #'differentiable-function :min) 3.0) 1.0))
  (is (≈ (ad-univariate (curry #'differentiable-function :max) 0.2) 1.0))
  (is (≈ (ad-univariate (curry #'differentiable-function :max) 3.0) 6.0))
  (is (≈ (ad-univariate #'piecewise 4.5)  1))
  (is (≈ (ad-univariate #'piecewise 5.5) -1))
  (is (≈ (ad-univariate #'expt-calc-1 3.0) 7.071068))
  (is (≈ (ad-univariate #'expt-calc-1 -1.0) -7.071068))
  (is (≈ (ad-univariate #'expt-calc-2 0.4) 0.55956817))
  (is (≈ (ad-univariate #'expt-calc-3 3) 27)))

(defun multivar (xs)
  (declare (optimize (speed 3))
           (type (simple-array dual (cl:*)) xs))
  (/ (log (aref xs 0))
     (cos (aref xs 1))))

(test diff-multivariate
  (is-true (every #'≈
                  (ad-multivariate #'multivar (to-doubles '(2 3)))
                  '(-0.50505435d0 0.09980452d0))))

(in-suite eval)

;; Some very long function
(declaim (inline foobar))
(defun foobar (x y z)
  (*
   (expt
    (+ (+ x) (- y) (+ x y) (/ z) (/ z y) (/ z y x)
       (* x) (* x y) (* x y z))
    z)
  (+ (sin x) (cos y) (tan z)
     (sinh x) (cosh y) (tanh z))
  (/ (log y) x)
  (signum (* (abs x) y))))

(serapeum:-> foobar-1 (dual dual single-float)
             (values dual &optional))
(defun foobar-1 (x y z)
  (declare (optimize (speed 3)))
  (foobar x y z))

(serapeum:-> foobar-2 (single-float single-float single-float)
             (values single-float &optional))
(defun foobar-2 (x y z)
  (declare (optimize (speed 3)))
  (foobar x y z))

(test specialization
  (let ((v1 (foobar 3.0 4.0 2.0))
        (v2 (foobar 3d0 4d0 2d0))
        (v3 (foobar 3d0 4d0 2d0))
        (v4 (foobar-1 #d(3d0 1d0)
                      #d(4d0 2d0)
                      2.0))
        (v5 (foobar-2 3.0 4.0 2.0)))
    (is-true (≈ v1 v2 1d-2))
    (is-true (≈ v1 v3 1d-2))
    (is-true (≈ v1 (dual-realpart v4) 1d-2))
    (is-true (≈ v1 v5 1d-2))))
