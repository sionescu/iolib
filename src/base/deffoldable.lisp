;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Declaring forms as foldable(pure)
;;;

(in-package :iolib.base)

#+sbcl
(progn
  (defun defknown-redefinition-error-p (e)
    (declare (optimize speed))
    (and (typep e 'simple-error)
         (search "overwriting old FUN-INFO"
                 (the string (simple-condition-format-control e)))))

  (deftype defknown-redefinition-error ()
    '(satisfies defknown-redefinition-error-p))

  (defmacro %deffoldable (func argument-types return-type)
    `(handler-bind ((defknown-redefinition-error #'continue))
       (sb-c:defknown ,func ,argument-types ,return-type (sb-c:foldable)))))

#-(or sbcl)
(defmacro %deffoldable (&rest args)
  (declare (ignore args)))

(defun constantp (form &optional environment)
  (cl:constantp (cond ((symbolp form)
                       (macroexpand form environment))
                      (t form))
                environment))

(defmacro deffoldable (func &optional
                       (argument-types (list t))
                       (return-type t))
  (alexandria:with-gensyms (form env args)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%deffoldable ,func ,argument-types ,return-type)
       (define-compiler-macro ,func (&whole ,form &rest ,args
                                            &environment ,env)
         (declare (ignore ,args))
         (if (constantp ,form ,env)
             (eval ,form)
             ,form)))))
