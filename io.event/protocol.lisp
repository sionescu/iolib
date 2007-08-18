;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; protocol.lisp --- Protocol classes and generic functions.
;;;
;;; Copyright (C) 2007, Stelian Ionescu  <sionescu@common-lisp.net>
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
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

(defclass protocol ()
  ((transport :initarg :transport :accessor transport-of))
  (:documentation ""))

;;; What would the semantics for these GFs be?  Namely, how would
;;; ON-PROTOCOL-START differ from INITIALIZE-INSTANCE?

;; (defgeneric on-protocol-start (protocol))
;; (defgeneric on-protocol-stop (protocol))

;;;; Stream Protocol

(defclass stream-protocol (protocol)
  ()
  (:documentation ""))

;;; These generic functions all take a TRANSPORT argument because a
;;; PROTOCOL may need to deal with multiple TRANSPORTS (per
;;; connection).  FTP would be an example of such a PROTOCOL.

(defgeneric on-connection-made (protocol transport)
  (:documentation "")
  ;; default empty method
  (:method ((sp stream-protocol) transport)
    (declare (ignore transport))
    (values)))

(defgeneric on-connection-lost (protocol transport reason)
  (:documentation "")
  ;; default empty method
  (:method ((sp stream-protocol) transport reason)
    (declare (ignore transport reason))
    (values)))

(defgeneric on-connection-end (protocol transport)
  (:documentation "")
  ;; default empty method
  (:method ((sp stream-protocol) transport)
    (declare (ignore transport))
    (values)))

(defgeneric on-data-received (protocol transport data)
  (:documentation "")
  ;; default empty method, though perhaps we should make it issue a
  ;; warning, since it's probably a good bet that we want to do
  ;; something with the data we receive.
  (:method ((sp stream-protocol) transport data)
    (declare (ignore transport data))
    (values)))

;;;; Datagram Protocol

(defclass datagram-protocol (protocol)
  ()
  (:documentation ""))

(defgeneric on-datagram-received (protocol transport datagram address port)
  (:documentation "")
  ;; default empty method.  Again, same issue as with ON-DATA-RECEIVED.
  (:method ((dp datagram-protocol) transport datagram address port)
    (declare (ignore transport datagram address port))
    (values)))

;;;; Debug Mixins

;;; This seemed like a good idea at first but so far it's less useful
;;; than plain TRACE.

(defclass protocol-debug-mixin ()
  ()
  (:documentation ""))

(defmethod on-connection-made :before ((p protocol-debug-mixin) protocol)
  (declare (ignore protocol))
  (format *debug-io* "~&Connection made: ~S~%" p))

(defmethod on-connection-end :after ((p protocol-debug-mixin) protocol)
  (declare (ignore protocol))
  (format *debug-io* "~&Connection end: ~S~%" p))

(defmethod on-data-received :before ((p protocol-debug-mixin) protocol data)
  (declare (ignore protocol))
  (format *debug-io* "~&Data received: ~A bytes~%" (length data)))

#-(and)
(defun debug-protocols ()
  (trace on-data-received
         on-transport-writable
         on-transport-readable
         on-transport-error
         on-connection-made
         on-connection-end))
