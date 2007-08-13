;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; io-protocol.lisp - Manage an I/O protocol.
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

(defclass io-protocol ()
  ((transport :initarg :transport
              :accessor transport-of)))

(defclass stream-protocol () ())

(defgeneric on-protocol-start (protocol))

(defgeneric on-protocol-stop (protocol))

(defgeneric on-connection-made (protocol))

(defgeneric on-connection-lost (protocol reason))

(defgeneric on-connection-end (protocol))

(defgeneric on-message-received (protocol message))

(defclass datagram-protocol () ())

(defgeneric on-datagram-received (protocol datagram address))
