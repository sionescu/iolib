;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Various time-related functions.
;;;

(in-package :io.multiplex)

;;;; Timeouts

(defun abs-timeout (timeout)
  (+ (osicat:get-monotonic-time) (normalize-timeout timeout)))

(defun min-timeout (&rest timeouts)
  (collect-min (choose-if #'identity (scan timeouts))))
