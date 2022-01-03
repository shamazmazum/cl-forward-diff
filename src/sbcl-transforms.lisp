;; Stuff for optimizing math functions for single-float type.
;; Should be integrated in SBCL itself, but it's hard to do it right.
(defpackage sbcl-transforms
  (:use :cl))
(in-package :sbcl-transforms)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolicate (&rest args)
    (intern
     (apply
      #'concatenate
      'string
      (mapcar
       (lambda (x)
         (typecase x
           (string (string-upcase x))
           (symbol (symbol-name x))
           (t (error "Not a symbol or string"))))
       args)))))

;; Alien mathematical functions (look at src/code/irrat.lisp in SBCL sources)
(macrolet
    ((def-alien (name)
       (let ((func-name (symbolicate "%" name "f"))
             (alien-name (format nil "~af" name))
             (arg (gensym)))
         `(progn
            (declaim (inline ,func-name))
            (defun ,func-name (,arg)
              (sb-ext:truly-the
               single-float
               (sb-alien:alien-funcall
                (sb-alien:extern-alien
                 ,alien-name
                 (function single-float (single-float)))
                ,arg)))))))
  (def-alien "exp")
  (def-alien "sin")
  (def-alien "cos")
  (def-alien "tan")
  (def-alien "log"))

(defun print-condition-and-continue (c)
  (princ c)
  (terpri)
  (continue))

;; Tell the compiler these functions are pure
;; (look at src/compiler/generic/vm-fndb.lisp in SBCL source code).

;; Additional hack is needed for these functions to be really flushable:
;; https://sourceforge.net/p/sbcl/mailman/message/37134684/
(handler-bind
    ;; KLUDGE: Provide clean foreced recompilation
    ((simple-error #'print-condition-and-continue))
  (sb-c:defknown (%exp %sin %cos %tan %log)
      (single-float) single-float
      (sb-c:movable sb-c:flushable sb-c:foldable)))

;; Define IR1 transformations from EXP to %EXP and so on.
;; (look at src/compiler/float-tran.lisp in SBCL source code).
(macrolet
    ((def-trans (name ret-type)
       (let ((trans-name (symbolicate "%" name "f"))
             (arg (gensym)))
         `(sb-c:deftransform ,name ((,arg) (single-float) ,ret-type)
            '(,trans-name ,arg)))))
  (def-trans exp *)
  (def-trans sin *)
  (def-trans cos *)
  (def-trans tan *)
  (def-trans log float))
