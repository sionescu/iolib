;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- RETURN* wrappers.
;;;

(in-package :iolib.utils)

(defun wrap-body-for-return-star (body)
  (multiple-value-bind (body declarations docstring)
      (parse-body body :documentation t)
    (with-gensyms (return-star-block)
      `(,docstring ,@declarations
        (block ,return-star-block
          (macrolet
              ((return* (value)
                 `(return-from ,',return-star-block ,value)))
            ,@body))))))

(defmacro defun* (name args &body body)
  `(,(ensure-symbol :defun)
     ,name ,args ,@(wrap-body-for-return-star body)))

(defmacro defmethod* (name args &body body)
  `(,(ensure-symbol :defmethod)
     ,name ,args ,@(wrap-body-for-return-star body)))

(defmacro lambda* (name args &body body)
  `(,(ensure-symbol :lambda)
     ,name ,args ,@(wrap-body-for-return-star body)))

(defmacro defmacro* (name args &body body)
  `(,(ensure-symbol :defmacro)
     ,name ,args ,@(wrap-body-for-return-star body)))

(defmacro define-compiler-macro* (name args &body body)
  `(,(ensure-symbol :define-compiler-macro)
     ,name ,args ,@(wrap-body-for-return-star body)))
