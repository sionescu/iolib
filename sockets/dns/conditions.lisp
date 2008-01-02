;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; conditions.lisp --- Resolver conditions.
;;;
;;; Copyright (C) 2006-2008, Stelian Ionescu  <sionescu@common-lisp.net>
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

(define-condition resolver-error ()
  ((data :initarg :data :reader resolver-error-data))
  (:documentation
   "Signaled when an error occurs while trying to resolve an address."))

(defmacro define-resolver-error (name format-string &optional documentation)
  `(define-condition ,name (resolver-error) ()
     (:report (lambda (condition stream)
                (format stream ,format-string (resolver-error-data condition))))
     (:documentation ,documentation)))

(define-resolver-error resolver-again-error
  "Temporary failure occurred while resolving: ~S"
  "Condition signaled when a temporary failure occurred.")

(define-resolver-error resolver-fail-error
  "Non recoverable error occurred while resolving: ~S"
  "Condition signaled when a non-recoverable error occurred.")

(define-resolver-error resolver-no-name-error
  "Host or service not found: ~S"
  "Condition signaled when a host or service was not found.")

(define-resolver-error resolver-unknown-error
  "Unknown error while resolving: ~S"
  "Condition signaled when an unknown error is signaled while resolving
an address.")
