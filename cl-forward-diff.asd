(defsystem :cl-forward-diff
  :name :cl-forward-diff
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Automatic differentiation system (forward mode)"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "dual")
               (:file "autodiff"))
  :depends-on ((:feature :single-float-tran :sbcl-single-float-tran)
               :polymorphic-functions
               :serapeum)
  :in-order-to ((test-op (load-op "cl-forward-diff/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :cl-forward-diff-tests  '#:run-tests)))


(defsystem :cl-forward-diff/tests
  :name :cl-forward-diff/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :components ((:file "tests/package")
               (:file "tests/tests" :depends-on ("tests/package")))
  :depends-on (:fiveam :alexandria :cl-forward-diff))
