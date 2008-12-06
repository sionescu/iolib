;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Gray stream conditions.
;;;

(in-package :io.streams)

(define-condition hangup (stream-error) ()
  (:report (lambda (c s)
             (format s "Stream ~S hang up."
                     (stream-error-stream c))))
  (:documentation "Condition signaled when the underlying device of a stream
is closed by the remote end while writing to it."))
