;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Error conditions.
;;;

(in-package :io.zeta-streams)

(define-condition posix-file-error (file-error)
  ((action :initarg :action :reader action-of)
   (code :initarg :code :reader code-of)
   (identifier :initarg :identifier :reader identifier-of))
  (:report (lambda (condition stream)
             (format stream "Error while ~A ~S: ~A"
                     (action-of condition)
                     (file-error-pathname condition)
                     (%sys-strerror (code-of condition))))))

(defun posix-file-error (posix-error filename action)
  (error 'posix-file-error
         :code (code-of posix-error)
         :identifier (identifier-of posix-error)
         :pathname filename :action action))

(define-condition hangup (stream-error) ()
  (:report (lambda (c s)
             (format s "Stream ~S hang up."
                     (stream-error-stream c))))
  (:documentation "Condition signaled when the underlying device of a stream
is closed by the remote end while writing to it."))
