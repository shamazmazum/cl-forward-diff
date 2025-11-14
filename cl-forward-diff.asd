(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors
    (require 'sb-simd)
    (push :sb-simd *features*)))

(defsystem :cl-forward-diff
  :name :cl-forward-diff
  :version "0.6"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Automatic differentiation system (forward mode)"
  :licence "2-clause BSD"
  :serial t
  :pathname "src"
  :components ((:file "ultralisp-hack" :if-feature (:not :sb-simd))
               (:file "package")
               (:file "dual")
               (:file "fndb")
               (:file "functions")
               (:file "transforms")
               (:file "autodiff"))
  :depends-on (:serapeum)
  :in-order-to ((test-op (load-op "cl-forward-diff/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :cl-forward-diff-tests '#:run-tests)))


(defsystem :cl-forward-diff/tests
  :name :cl-forward-diff/tests
  :version "0.6"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:fiveam :alexandria :cl-forward-diff))
