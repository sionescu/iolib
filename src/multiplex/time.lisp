;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Various time-related functions.
;;;

(in-package :iolib.multiplex)

;;;; Timeouts

(defun abs-timeout (timeout)
  (+ (isys:%sys-get-monotonic-time) (normalize-timeout timeout)))

(defun min-timeout (&rest timeouts)
  (let* ((good-timeout-start (member-if-not #'null timeouts))
         (min (car good-timeout-start)))
    (loop :for timeout :in (cdr good-timeout-start)
          :if timeout :do
          (setf min (if min (min min timeout) timeout))
       :finally (return min))))
