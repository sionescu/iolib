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
  (:use #:cl #:io.event)
  (:export #:echo-server))

(in-package #:net.echo)

;;;; Echo Server

(defclass echo-server (tcp-server udp-server)
  ()
  (:default-initargs :protocol 'echo-server-protocol
                     :default-local-port 7)
  (:documentation "ECHO server."))

(defclass echo-server-protocol (stream-protocol datagram-protocol)
  ()
  (:documentation "Server implementation of the ECHO protocol. (TCP and UDP)"))

(defmethod on-data-received ((con echo-server-protocol) transport data)
  (write-data data transport))

(defmethod on-datagram-received ((con echo-server-protocol)
                                 transport datagram address port)
  (write-datagram datagram address port transport))

;;;; Echo Client

(defclass echo-client (tcp-client)
  ()
  (:default-initargs :protocol 'echo-client-protocol
                     :default-remote-port 7)
  (:documentation "ECHO client."))

(defvar *default-echo-client* (make-instance 'echo-client))

(defclass echo-client-protocol (stream-protocol deferred)
  ((request :initarg :request :accessor request-of))
  (:documentation "Client implementation of the ECHO protocol. (TCP and UDP)"))

(defmethod on-connection-made ((con echo-client-protocol) transport)
  (write-data (request-of con) transport))

(defmethod on-data-received ((con echo-client-protocol) transport data)
  (close-transport transport)
  (funcall (result-callback-of con) data))

(defmethod on-connection-lost ((con echo-client) transport reason)
  (funcall (result-callback-of con) reason))

(defun get-echo (data host &optional (port 7) (client *default-echo-client*))
  (with-deferred-result ()
    (let ((protocol (make-instance 'echo-client-protocol :request data)))
      (add-connection client protocol :remote-host host :remote-port port)
      protocol)))
