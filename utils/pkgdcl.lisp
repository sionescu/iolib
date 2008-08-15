;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :common-lisp-user)

(defpackage :iolib.utils
  (:use #:common-lisp :alexandria)
  (:shadow #:defun #:defmethod #:lambda
           #:defmacro #:define-compiler-macro)
  (:export #:return*))
