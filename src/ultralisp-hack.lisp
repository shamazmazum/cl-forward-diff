(defpackage sb-simd-avx
  (:use #:cl))
(in-package :sb-simd-avx)

(macrolet ((def-stub (name nargs has-rest)
             (let ((args (loop repeat nargs collect (gensym "ARG-")))
                   (rest (gensym "REST-")))
               `(progn
                  (defun ,name ,(if has-rest (append args (list '&rest rest)) args)
                    (declare ,@(if (not (zerop nargs)) (list `(ignore ,@args)))
                             ,@(if has-rest (list `(ignore ,rest))))
                    (error "SBCL > 2.2.6 is required for this system"))
                  (export ',name)))))
  (def-stub make-f64.2   2 nil)
  (def-stub f64.2-values 1 nil)
  (def-stub f64.2+       0 t)
  (def-stub f64.2-       1 t))
