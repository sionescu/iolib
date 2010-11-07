;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- ASDF component classes
;;;

(defpackage :iolib.asdf
  (:use :common-lisp))
(in-package :iolib.asdf)

(defclass :iolib-muffled-source-file (asdf:cl-source-file) ())

(macrolet ((with-muffled-output (&body body)
             `(let ((*load-print* nil)
                    (*load-verbose* nil)
                    (*compile-print* nil)
                    (*compile-verbose* nil)
                    #+cmu (ext:*gc-verbose* nil))
                ,@body)))
  (defmethod asdf:perform :around ((o asdf:compile-op)
                                   (c :iolib-muffled-source-file))
    (with-muffled-output
      (call-next-method)))

  (defmethod asdf:perform :around ((o asdf:load-source-op)
                                   (c :iolib-muffled-source-file))
    (with-muffled-output
      (call-next-method))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; SCL 1.3.9.1 doesn't allow superclasses to be keywords
  ;; Fix suggested by DTC
  (defun clos::legal-class-name-p (x) (and x (symbolp x))))

(defclass :iolib-source-file (:iolib-muffled-source-file) ())
