;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Error conditions.
;;;

(in-package :iolib.base)

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
