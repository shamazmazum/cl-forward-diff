(defun do-all()
  (handler-case
      (asdf:load-system :cl-forward-diff/tests)
    (error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "cl-forward-diff-tests:run-tests")
       0 1)))

(do-all)
