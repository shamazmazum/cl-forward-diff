(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-simd))

(defsystem :cl-forward-diff
  :name :cl-forward-diff
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Automatic differentiation system (forward mode)"
  :licence "2-clause BSD"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "dual")
               (:file "autodiff"))
  :depends-on (:serapeum)
  :in-order-to ((test-op (load-op "cl-forward-diff/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :cl-forward-diff-tests '#:run-tests)))


(defsystem :cl-forward-diff/tests
  :name :cl-forward-diff/tests
  :version "0.2"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :pathname "tests"
  :components ((:file "package")
               (:file "tests" :depends-on ("package")))
  :depends-on (:fiveam :alexandria :cl-forward-diff))
