(defun do-all()
  (ql:quickload :cl-forward-diff/tests :verbose t)
  (uiop:quit
   (if (uiop:call-function "cl-forward-diff-tests:run-tests")
       0 1)))

(do-all)
