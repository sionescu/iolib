;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; protocols.lisp --- Protocol lookup.
;;;
;;; Copyright (C) 2006-2007, Stelian Ionescu  <sionescu@common-lisp.net>
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by
;;; the Free Software Foundation, as clarified by the
;;; preamble found here:
;;;     http://opensource.franz.com/preamble.html
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General
;;; Public License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;; Boston, MA 02110-1301, USA

(in-package :net.sockets)

(defclass protocol ()
  ((name :initarg :name :reader protocol-name
         :documentation "The protocol's primary name.")
   (aliases :initarg :aliases :reader protocol-aliases
            :documentation "A list of aliases for this protocol.")
   (number :initarg :number :reader protocol-number
           :documentation "The protocol number."))
  (:documentation "Class representing a protocol."))

(defun make-protocol (name number &optional aliases)
  "Constructor for PROTOCOL objects."
  (make-instance 'protocol :name name :number number :aliases aliases))

(defmethod print-object ((protocol protocol) stream)
  (print-unreadable-object (protocol stream :type t :identity nil)
    (with-slots (name aliases protonum) protocol
      (format stream "Name: ~S Protocol number: ~A Aliases: ~{~S~^, ~}"
              name protonum aliases))))

(defun lookup-protocol-number (proto))

(defun lookup-protocol-name (proto))

(define-condition unknown-protocol ()
  ((name :initarg :name :initform nil :reader protocol-name))
  (:report (lambda (condition stream)
             (format stream "Unknown protocol: ~S" (protocol-name condition))))
  (:documentation "Condition raised when a network protocol is not found."))

(defun lookup-protocol (protocol)
  "Lookup a protocol by name or number.  Signals an
UNKNOWN-PROTOCOL error if no protocol is found."
  (let* ((parsed-number (parse-number-or-nil protocol))
         (proto (if parsed-number
                    (lookup-protocol-number parsed-number)
                    (lookup-protocol-name protocol))))
    (or proto (error 'unknown-protocol :name protocol))))
