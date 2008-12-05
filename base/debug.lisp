;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Debug helpers.
;;;

(in-package :iolib.base)

(defparameter *debug* t)

(defmacro debug-only (&body body)
  (if *debug*
      `(progn
         ,@body)
      (values)))

(defmacro debug-only* (&body body)
  `(if *debug*
       (progn
         ,@body)
       (values)))

(defmacro production-only (&body body)
  (if *debug*
      (values)
      `(progn
         ,@body)))

(defmacro production-only* (&body body)
  `(unless *debug*
     ,@body))
