;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Types.
;;;

(in-package :iolib.base)

(deftype function-designator ()
  '(or symbol function))
