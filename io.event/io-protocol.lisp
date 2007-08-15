;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; io-protocol.lisp --- Manage an I/O protocol.
;;;
;;; Copyright (C) 2007, Stelian Ionescu  <sionescu@common-lisp.net>
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

(in-package :io.event)

;;;; Base Protocol Class

(defclass io-protocol ()
  ((transport :initarg :transport :accessor transport-of))
  (:documentation ""))

(defgeneric on-protocol-start (protocol)
  (:documentation ""))

(defgeneric on-protocol-stop (protocol)
  (:documentation ""))

;;;; Stream Protocol

(defclass stream-protocol (io-protocol)
  ()
  (:documentation ""))

(defgeneric on-connection-made (protocol)
  (:documentation "")
  ;; default empty method
  (:method ((sp stream-protocol))
    (values)))

(defgeneric on-connection-lost (protocol reason)
  (:documentation "")
  ;; default empty method
  (:method ((sp stream-protocol) reason)
    (declare (ignore reason))
    (values)))

(defgeneric on-connection-end (protocol)
  (:documentation "")
  ;; default empty method
  (:method ((sp stream-protocol))
    (values)))

(defgeneric on-data-received (protocol data)
  (:documentation "")
  ;; default empty method
  (:method ((sp stream-protocol) data)
    (declare (ignore data))
    (values)))

;;;; Datagram Protocol

(defclass datagram-protocol (io-protocol)
  ()
  (:documentation ""))

(defgeneric on-datagram-received (protocol datagram address)
  (:documentation ""))

;;;; Debug Mixins

;;; seemed like a good idea at first but so far it's less useful than
;;; plain TRACE.

(defclass protocol-debug-mixin ()
  ()
  (:documentation ""))

(defmethod on-connection-made :before ((p protocol-debug-mixin))
  (format *debug-io* "~&Connection made: ~S~%" p))

(defmethod on-connection-end :after ((p protocol-debug-mixin))
  (format *debug-io* "~&Connection end: ~S~%" p))

(defmethod on-data-received :before ((p protocol-debug-mixin) data)
  (format *debug-io* "~&Data received: ~A bytes~%" (length data)))

(defun debug-protocols ()
  (trace on-data-received
         on-transport-writable
         on-transport-readable
         on-transport-error
         on-connection-made
         on-connection-end))