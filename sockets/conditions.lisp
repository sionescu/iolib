;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (C) 2006 by Stelian Ionescu                                 ;
;                                                                         ;
;   This program is free software; you can redistribute it and/or modify  ;
;   it under the terms of the GNU General Public License as published by  ;
;   the Free Software Foundation; either version 2 of the License, or     ;
;   (at your option) any later version.                                   ;
;                                                                         ;
;   This program is distributed in the hope that it will be useful,       ;
;   but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;   GNU General Public License for more details.                          ;
;                                                                         ;
;   You should have received a copy of the GNU General Public License     ;
;   along with this program; if not, write to the                         ;
;   Free Software Foundation, Inc.,                                       ;
;   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (optimize (speed 0) (safety 3) (space 0) (debug 2)))

(in-package #:net.sockets)


(define-condition condition-with-message-mixin (condition)
  ((message :initarg :message :initform "" :reader condition-message))
  (:documentation "Simple mixin class that adds a slot named \"message\" which may 
be used as a more thorough explanation of the errors's cause."))

(defun print-message-if-not-null (condition stream &optional (eof-place :before))
  (declare (type stream stream))
  (let ((msg (condition-message condition)))
    (when msg
      (when (eql eof-place :before)
        (format stream "~%"))
      (format stream "~a" msg)
      (when (eql eof-place :after)
        (format stream "~%")))))


(define-condition possible-bug (error condition-with-message-mixin)
  ((data :initarg :data :reader bug-data))
  (:report (lambda (condition stream)
             (declare (type stream stream))
             (print-message-if-not-null condition stream :after)
             (format stream "Bug data: ~a"
                     (bug-data condition))))
  (:documentation "Raised when a situation that is probably a bug occurs."))


(define-condition invalid-argument (error condition-with-message-mixin)
  ((argument :initarg :argument :reader invalid-argument))
  (:report (lambda (condition stream)
             (declare (type stream stream))
             (format stream "Invalid argument: ~a"
                     (invalid-argument condition))
             (print-message-if-not-null condition stream)))
  (:documentation "Signaled when one or more of a function's arguments are considered
not valid(wrong type or good type but not within a certain range of
values, et caetera)."))



(define-condition system-error (error)
  ((code       :initarg :code       :initform 0        :reader system-error-code)
   (identifier :initarg :identifier :initform :unknown :reader system-error-identifier))
  (:documentation "Condition raised when a system error occurs."))

(define-condition network-error (system-error condition-with-message-mixin)
  ()
  (:documentation "Condition raised when a network error occurs."))

(define-condition resolver-error (network-error)
  ((data :initarg :data :reader resolver-data)
   (data-presentation :initarg :data-presentation :initform nil
                      :reader resolver-data-presentation))
  (:documentation "Signaled when an error occurs while trying to resolve an data."))

(defmacro define-resolver-error (name code identifier message documentation)
  `(define-condition ,name (resolver-error)
     ((code :initform ,code)
      (identifier :initform ,identifier))
     (:report (lambda (condition stream)
                (declare (type stream stream))
                (format stream ,message (or (resolver-data-presentation condition)
                                            (resolver-data condition)))
                (print-message-if-not-null condition stream)))
     (:documentation ,documentation)))

(define-resolver-error resolver-again-error sb-posix::eai-again :resolver-again
  "Temporary failure occurred while resolving: ~a"
  "Condition signaled when a temporary failure occurred.")

(define-resolver-error resolver-fail-error sb-posix::eai-fail :resolver-fail
  "Non recoverable error occurred while resolving: ~a"
  "Condition signaled when a non-recoverable error occurred.")

(define-resolver-error resolver-no-name-error sb-posix::eai-noname :resolver-no-name
  "Host or service not found: ~a"
  "Condition signaled when a host or service was not found.")

(define-resolver-error resolver-no-service-error sb-posix::eai-service :resolver-no-service
  "Service not found for specific socket type: ~a"
  "Condition signaled when a service was not found for the socket type requested.")

(define-condition unknown-interface (system-error condition-with-message-mixin)
  ((name  :initarg :name  :initform nil :reader interface-name)
   (index :initarg :index :initform nil :reader interface-index))
  (:report (lambda (condition stream)
             (if (interface-name condition)
                 (format stream "Unknown interface: ~a"
                         (interface-name condition))
                 (format stream "Unknown interface index: ~a"
                         (interface-index condition)))
             (print-message-if-not-null condition stream)))
  (:documentation "Condition raised when a network interface is not found."))

(define-condition unknown-protocol (system-error)
  ((name :initarg :name :initform nil :reader protocol-name))
  (:report (lambda (condition stream)
             (format stream "Unknown protocol: ~s"
                     (protocol-name condition))))
  (:documentation "Condition raised when a network protocol is not found."))

(define-condition invalid-address ()
  ((address  :initarg :address  :initform nil :reader address)
   (addrtype :initarg :type     :initform nil :reader address-type))
  (:report (lambda (condition stream)
             (format stream "Invalid ~a address: ~a" (address-type condition) (address condition))))
  (:documentation "Condition raised when an address designator is invalid."))
