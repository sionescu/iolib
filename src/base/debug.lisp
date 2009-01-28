;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Debug helpers.
;;;

(in-package :iolib.base)

(defparameter *safety-checks* t
  "Enables safety checks throught the IOLib codebase.
ACHTUNG!!! Don't disable this unless you're very confident about the quality of the code in IOLib.")

(defmacro debug-only (&body body)
  (when *safety-checks*
    `(progn ,@body)))

(defmacro debug-only* (&body body)
  `(when *safety-checks*
     (progn ,@body)))

(defmacro production-only (&body body)
  (unless *safety-checks*
    `(progn ,@body)))

(defmacro production-only* (&body body)
  `(unless *safety-checks*
     ,@body))
