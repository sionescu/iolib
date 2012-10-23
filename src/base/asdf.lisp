;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- ASDF component classes
;;;

(defpackage :iolib.asdf
  (:use :common-lisp)
  #+sb-package-locks
  (:lock t))
(in-package :iolib.asdf)

(defclass :iolib-muffled-source-file (asdf:cl-source-file) ())

(macrolet ((with-muffled-output (&body body)
             `(handler-bind
                  (#+sbcl (sb-int:package-at-variance #'muffle-warning))
                ,@body)))
  (defmethod asdf:perform :around ((o asdf:compile-op)
                                   (c :iolib-muffled-source-file))
    (with-muffled-output
      (with-standard-io-syntax
        (let (;; Compilation fails because of CFFI types that
              ;; can't be printed readably, so bind to NIL
              (*print-readably* nil)
              (*readtable* (copy-readtable)))
          (call-next-method)))))

  (defmethod asdf:perform :around ((o asdf:load-op)
                                   (c :iolib-muffled-source-file))
    (with-muffled-output
      (with-standard-io-syntax
        (let (;; See above
              (*print-readably* nil)
              (*readtable* (copy-readtable)))
          (call-next-method)))))

  (defmethod asdf:perform :around ((o asdf:load-source-op)
                                   (c :iolib-muffled-source-file))
    (with-muffled-output
      (with-standard-io-syntax
        (let (;;See above
              (*print-readably* nil)
              (*readtable* (copy-readtable)))
          (call-next-method))))))

#+scl
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; SCL 1.3.9.1 doesn't allow superclasses to be keywords
  ;; Fix suggested by DTC
  (defun clos::legal-class-name-p (x) (and x (symbolp x))))

(defclass :iolib-source-file (:iolib-muffled-source-file) ())
