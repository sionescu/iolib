;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Error conditions.
;;;

(in-package :io.zeta-streams)

(define-condition posix-file-error (file-error)
  ((action :initarg :action :reader posix-file-error-action)
   (code :initarg :code :reader posix-file-error-code)
   (identifier :initarg :identifier :reader posix-file-error-identifier))
  (:report (lambda (condition stream)
             (format stream "Error while ~A ~S: ~A"
                     (posix-file-error-action condition)
                     (file-error-pathname condition)
                     (%sys-strerror (posix-file-error-code condition))))))

(defun posix-file-error (posix-error filename action)
  (error 'posix-file-error
         :code (posix-file-error-code posix-error)
         :identifier (posix-file-error-identifier posix-error)
         :pathname filename :action action))

(define-condition hangup (stream-error) ()
  (:report (lambda (c s)
             (format s "Stream ~S hang up."
                     (stream-error-stream c))))
  (:documentation "Condition signaled when the underlying device of a stream
is closed by the remote end while writing to it."))
