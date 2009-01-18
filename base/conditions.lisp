;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Error conditions.
;;;

(in-package :iolib.base)

(define-condition subtype-error (error)
  ((datum :initarg :type :reader subtype-error-datum)
   (expected-supertype :initarg :expected-supertype
                       :reader subtype-error-expected-supertype))
  (:report
   (lambda (condition stream)
     (format stream "~S is not a recognizable subtype of ~S"
             (subtype-error-datum condition)
             (subtype-error-expected-supertype condition)))))


;;;-------------------------------------------------------------------------
;;; Bugs
;;;-------------------------------------------------------------------------

(define-condition iolib-bug (error)
  ((message :initarg :message :reader iolib-bug-message))
  (:report
   (lambda (condition stream)
     (format stream "~A.~%This seems to be a bug in IOlib. ~
                     Please report it to iolib-devel@common-lisp.net"
             (iolib-bug-message condition)))))

(defun bug (control &rest args)
  (error 'iolib-bug :message (format nil "~?" control args)))
