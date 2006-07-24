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

;; (declaim (optimize (speed 2) (safety 2) (space 1) (debug 2)))
(declaim (optimize (speed 0) (safety 2) (space 0) (debug 2)))

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
        (fresh-line stream))
      (format stream "~a" msg)
      (when (eql eof-place :after)
        (fresh-line stream)))))


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
