;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; base-sockets.lisp --- Base socket classes.
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

;;;; Sockets

(defclass socket (dual-channel-single-fd-mixin)
  ((family   :initarg :family   :accessor socket-family)
   (protocol :initarg :protocol :accessor socket-protocol)
   (bound    :initform nil      :reader   socket-bound-p :type boolean)))

(defgeneric socket-fd (socket))
(defgeneric (setf socket-fd) (fd socket))
(defgeneric socket-type (socket))
(defgeneric socket-open-p (socket))
(defgeneric local-name (socket))
(defgeneric local-address (socket))
(defgeneric local-port (socket))
(defgeneric remote-name (socket))
(defgeneric remote-address (socket))
(defgeneric remote-port (socket))
(defgeneric socket-option (socket option-name))

(defclass stream-socket (socket) ()
  (:default-initargs :type :stream))

(defclass datagram-socket (socket) ()
  (:default-initargs :type :datagram))

(defgeneric disconnect (socket))

(define-symbol-macro +default-inet-family+
    (if *ipv6* :ipv6 :ipv4))

(defclass internet-socket (socket) ()
  (:default-initargs :family +default-inet-family+))

(defclass local-socket (socket) ()
  (:default-initargs :family :local))

(defun socket-read-fn (fd buffer nbytes)
  (%recvfrom fd buffer nbytes 0 (null-pointer) (null-pointer)))

(defun socket-write-fn (fd buffer nbytes)
  (%sendto fd buffer nbytes 0 (null-pointer) 0))

(defclass active-socket (socket dual-channel-gray-stream) ()
  (:default-initargs :read-fn 'socket-read-fn
                     :write-fn 'socket-write-fn))

(defgeneric connect (socket address &key &allow-other-keys))

(defgeneric socket-connected-p (socket))

(defgeneric shutdown (socket &key read write))

(defgeneric receive-from (socket &rest args &key &allow-other-keys))

(defgeneric send-to (socket buffer &rest args &key &allow-other-keys))

(defclass passive-socket (socket)
  ((listening :initform nil :reader socket-listening-p :type boolean)
   (external-format :initarg :external-format :reader external-format-of)
   (active-class :initarg :active-class :reader active-class
                 :type symbol :allocation :class))
  (:default-initargs :external-format :default))

(defgeneric bind-address (socket address &key &allow-other-keys))

(defgeneric socket-listen (socket &key &allow-other-keys))

(defgeneric accept-connection (passive-socket &key &allow-other-keys))

(defclass socket-stream-internet-active
    (active-socket stream-socket internet-socket)
  ())

(defclass socket-stream-internet-passive
    (passive-socket stream-socket internet-socket)
  ()
  (:default-initargs :active-class 'socket-stream-internet-active))

(defclass socket-stream-local-active (active-socket stream-socket local-socket)
  ())

(defclass socket-stream-local-passive
    (passive-socket stream-socket local-socket)
  ()
  (:default-initargs :active-class 'socket-stream-local-active))

(defclass socket-datagram-local-active
    (active-socket datagram-socket local-socket)
  ())

(defclass socket-datagram-internet-active
    (active-socket datagram-socket internet-socket)
  ())
