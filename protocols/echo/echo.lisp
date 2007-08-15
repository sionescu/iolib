;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; echo.lisp --- Server and client implementations of the ECHO protocol.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:cl-user)

(defpackage #:net.echo
  (:use #:cl #:io.event #:net.sockets))

(in-package #:net.echo)

;;;; Echo Server

(defclass echo-server (tcp-server)
  ()
  (:default-initargs :protocol 'echo-server-stream-protocol)
  (:documentation "Uses the TCP protocol by default."))

(defclass echo-server-stream-protocol (stream-protocol)
  ()
  (:documentation "Echo protocol implementation. (TCP)"))

(defmethod on-data-received ((con echo-server-stream-protocol) data)
  (write-sequence data (transport-of con))
  (finish-output (transport-of con)))







;;;; ....

(defclass echo-server-datagram-protocol (datagram-protocol)
  ()
  (:documentation "Echo protocol implementation. (UDP)"))

(defmethod)

;;;; Echo Client

(defclass echo-client (client)
  ()
  (:default-initargs :protocol 'echo-client-stream-protocol)
  (:documentation "Uses the TCP protocol by default."))

(defclass echo-client-stream-protocol (stream-protocol)
  ()
  (:documentation ""))

(defmethod )

(defclass echo-client-datagram-protocol (datagram-protocol)
  ()
  (:documentation ""))

(defmethod)
