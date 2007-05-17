;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;   Copyright (C) 2006, 2007 Stelian Ionescu
;;
;;   This code is free software; you can redistribute it and/or
;;   modify it under the terms of the version 2.1 of
;;   the GNU Lesser General Public License as published by
;;   the Free Software Foundation, as clarified by the
;;   preamble found here:
;;       http://opensource.franz.com/preamble.html
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU Lesser General
;;   Public License along with this library; if not, write to the
;;   Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;   Boston, MA 02110-1301, USA

(in-package :net.sockets)

;;;;;;;;;;;;;;;
;;;         ;;;
;;; Sockets ;;;
;;;         ;;;
;;;;;;;;;;;;;;;

(defclass socket (dual-channel-single-fd-mixin)
  ((family   :initarg :family   :accessor socket-family)
   (protocol :initarg :protocol :accessor socket-protocol)
   (bound    :initform nil      :reader   socket-bound-p :type boolean)))

(defgeneric socket-fd (socket))
(defgeneric (setf socket-fd) (fd socket))

(defgeneric socket-type (socket))

(defgeneric socket-open-p (socket))

(defgeneric local-name (socket))

(defgeneric socket-address (socket))

(defgeneric socket-port (socket))

(defgeneric remote-name (socket))

(defgeneric get-socket-option (socket option-name))

(defgeneric set-socket-option (socket option-name &key &allow-other-keys))

(defclass stream-socket (socket) ()
  (:default-initargs :type :stream))

(defclass datagram-socket (socket) ()
  (:default-initargs :type :datagram))

(defgeneric unconnect (socket))

(defclass internet-socket (socket) ()
  (:default-initargs :family (if *ipv6* :ipv6 :ipv4)))

(defclass local-socket (socket) ()
  (:default-initargs :family :local))

(defclass active-socket (socket dual-channel-gray-stream) ())

(defgeneric connect (socket address &key &allow-other-keys))

(defgeneric socket-connected-p (socket))

(defgeneric shutdown (socket direction))

(defgeneric socket-send (buffer socket &key &allow-other-keys))

(defgeneric socket-receive (buffer socket &key &allow-other-keys))

(defclass passive-socket (socket)
  ((listening :initform nil :reader socket-listening-p :type boolean)
   (external-format :initarg :external-format :reader external-format-of
                    :type external-format)
   (active-class :initarg :active-class :reader active-class
                 :type symbol :allocation :class))
  (:default-initargs :external-format :default))

(defgeneric bind-address (socket address &key &allow-other-keys))

(defgeneric socket-listen (socket &key backlog &allow-other-keys))

(defgeneric accept-connection (passive-socket &key wait &allow-other-keys))

(defclass socket-stream-internet-active (active-socket stream-socket internet-socket) ())

(defclass socket-stream-internet-passive (passive-socket stream-socket internet-socket) ()
  (:default-initargs :active-class 'socket-stream-internet-active))

(defclass socket-stream-local-active (active-socket stream-socket local-socket) ())

(defclass socket-stream-local-passive (passive-socket stream-socket local-socket) ()
  (:default-initargs :active-class 'socket-stream-local-active))

(defclass socket-datagram-local-active (active-socket datagram-socket local-socket) ())

(defclass socket-datagram-internet-active (active-socket datagram-socket internet-socket) ())
