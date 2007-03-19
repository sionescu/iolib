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

;;;;;;;;;;;;;;;;;;;;;;
;;;                ;;;
;;; Socket buffers ;;;
;;;                ;;;
;;;;;;;;;;;;;;;;;;;;;;

(deftype stream-buffer ()
  'et:foreign-pointer)

(deftype buffer-index ()
  '(unsigned-byte 24))

(defstruct (iobuf
             (:constructor %make-iobuf ()))
  (data (cffi:null-pointer) :type stream-buffer)
  (size 0 :type buffer-index)
  (start 0 :type buffer-index)
  (end 0 :type buffer-index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                             ;;;
;;; Bivalent socket Gray stream ;;;
;;;                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype stream-position ()
  '(unsigned-byte 64))

(defclass dual-channel-gray-stream (fundamental-binary-input-stream
                                    fundamental-binary-output-stream
                                    fundamental-character-input-stream
                                    fundamental-character-output-stream)
  ((external-format :initform (find-external-format :default))
   ;; Input buffer.
   (input-buffer :initform nil :type (or iobuf null))
   ;; Output buffer.
   (output-buffer :initform nil :type (or iobuf null))
   ;; Input stream position
   (istream-pos :initform 0 :type stream-position)
   ;; Output stream position
   (ostream-pos :initform 0 :type stream-position)))


;;;;;;;;;;;;;;;
;;;         ;;;
;;; Sockets ;;;
;;;         ;;;
;;;;;;;;;;;;;;;

(defclass socket ()
  ((fd       :initform nil      :reader socket-fd)
   (family   :initarg :family   :reader socket-family)
   (protocol :initarg :protocol :reader socket-protocol)))

(defgeneric socket-type (socket))

(defgeneric socket-non-blocking (socket))
(defgeneric (setf socket-non-blocking) (value socket))

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
  ((bound     :initform nil :reader socket-bound-p     :type boolean)
   (listening :initform nil :reader socket-listening-p :type boolean)
   (active-class :initarg :active-class :reader active-class
                 :type symbol :allocation :class)))

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
