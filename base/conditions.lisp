;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Error conditions.
;;;

(in-package :iolib.base)

;;;-------------------------------------------------------------------------
;;; Bugs
;;;-------------------------------------------------------------------------

(define-condition bug (error)
  ((message :reader message :initarg :message))
  (:report (lambda (condition stream)
             (format stream "~A~%. This seems to be a bug in IOlib. ~
                             Please report on iolib-devel@common-lisp.net."
                     (message condition)))))

(defun bug (control &rest args)
  (error 'bug :message (format nil "~?" control args)))
