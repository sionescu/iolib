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

(declaim (optimize (speed 2) (safety 2) (space 1) (debug 2)))

(in-package #:net.sockets)

;;;;;;;;;;;;;;;
;;;         ;;;
;;; Sockets ;;;
;;;         ;;;
;;;;;;;;;;;;;;;

(defclass socket ()
  ((fd       :reader socket-fd)
   (address  :initarg :address  :reader socket-address :type netaddr)
   (family   :initarg :family   :reader socket-family)
   (protocol :initarg :protocol :reader socket-protocol)))

(defgeneric socket-non-blocking-mode (socket))
(defgeneric (setf socket-non-blocking-mode) (value socket))

(defgeneric socket-close (socket)
  (:method-combination progn :most-specific-last))

(defgeneric socket-open-p (socket))

(defgeneric local-name (socket))

(defgeneric remote-name (socket))

(defgeneric get-socket-option (socket option-name &key))

(defgeneric set-socket-option (socket option-name &key))

(defclass stream-socket (socket)
  ((lisp-stream :reader socket-lisp-stream))
  (:default-initargs :type :stream))

(defgeneric socket-send (buffer socket &key
                         dont-route dont-wait no-signal
                         out-of-band &allow-other-keys))

(defgeneric socket-receive (buffer socket &key
                            out-of-band peek wait-all
                            dont-wait &allow-other-keys))

(defclass datagram-socket (socket) ()
  (:default-initargs :type :datagram))

(defgeneric socket-unconnect (socket))

(defclass internet-socket (socket)
  ((port :reader port :type '(unsigned-byte 16)))
  (:default-initargs :family (if *ipv6* :ipv6 :ipv4)))

(defclass unix-socket (socket) ()
  (:default-initargs :family :unix))

(defclass active-socket (socket) ())

(defgeneric socket-connect (socket address &key &allow-other-keys))

(defgeneric socket-shutdown (socket direction))

(defclass passive-socket (socket) ())

(defgeneric socket-bind-address (socket address &key))

(defgeneric socket-listen (socket &key backlog))

(defgeneric socket-accept-connection (passive-socket &key wait))

(defclass socket-stream-internet-active (active-socket stream-socket internet-socket) ())

(defclass socket-stream-internet-passive (passive-socket stream-socket internet-socket) ())

(defclass socket-datagram-internet-active (active-socket datagram-socket internet-socket) ())

(defclass socket-datagram-internet-passive (passive-socket datagram-socket internet-socket) ())

(defclass socket-stream-unix-active (active-socket stream-socket unix-socket) ())

(defclass socket-stream-unix-passive (passive-socket stream-socket unix-socket) ())

(defclass socket-datagram-unix-active (active-socket datagram-socket unix-socket) ())

(defclass socket-datagram-unix-passive (passive-socket datagram-socket unix-socket) ())
