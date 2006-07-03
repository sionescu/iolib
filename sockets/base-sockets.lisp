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

(declaim (optimize (speed 0) (safety 3) (space 0) (debug 2)))
;; (declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

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

(defgeneric socket-close (socket))

(defgeneric socket-open-p (socket))

(defclass stream-socket (socket) ())

(defgeneric socket-write-char (char socket))

(defgeneric socket-read-char (socket &key eof-error-p eof-value))

(defgeneric socket-write-byte (byte socket))

(defgeneric socket-read-byte (socket &key eof-error-p eof-value))

(defgeneric socket-write-sequence (sequence socket &key start end))

(defgeneric socket-read-sequence (sequence socket &key start end))

(defgeneric socket-send (buffer socket &key
                         dont-route dont-wait no-signal out-of-band &allow-other-keys))

(defgeneric socket-receive (buffer socket &key
                            out-of-band peek wait-all dont-wait &allow-other-keys))

(defclass datagram-socket (socket) ())

(defgeneric write-datagram (socket &key address &allow-other-keys))

(defgeneric read-datagram (socket &key address &allow-other-keys))

(defclass internet-socket (socket)
  ((port :reader port :type '(unsigned-byte 16)))
  (:default-initargs :family (if *ipv6* :ipv6 :ipv4)))

(defgeneric local-name (socket))

(defgeneric local-host (socket))

(defgeneric local-port (socket))

(defclass unix-socket (socket) ()
  (:default-initargs :family :unix))

(defclass active-socket (socket) ())

(defgeneric socket-connect (socket address &key &allow-other-keys))

(defgeneric remote-name (socket))

(defgeneric remote-host (socket))

(defgeneric remote-port (socket))

(defgeneric shutdown (socket &key direction))

(defclass passive-socket (socket) ())

(defgeneric socket-bind-address (socket address &key reuse-address interface))

(defgeneric socket-listen (socket &key backlog))

(defgeneric socket-accept-connection (passive-socket &key active-socket wait))

(defclass internet-active-socket (internet-socket active-socket) ())

(defclass internet-passive-socket (internet-socket passive-socket) ())

(defclass unix-active-socket (unix-socket active-socket) ())

(defclass unix-passive-socket (unix-socket passive-socket) ())



(defun translate-make-socket-keywords-to-constants (family type protocol)
  (let ((sf (ecase family
              (:ipv4 sb-posix::af-inet)
              (:ipv6 sb-posix::af-inet6)
              (:unix sb-posix::af-unix)))
        (st (ecase type
              (:stream   sb-posix::sock-stream)
              (:datagram sb-posix::sock-dgram)))
        (sp (cond
              ((integerp protocol) protocol)
              ((eql protocol :default) 0)
              ((keywordp protocol)
               (protocol-number
                (get-protocol-by-name (string-downcase
                                       (symbol-name protocol))))))))
    (values sf st sp)))

(defun set-finalizer-on-socket (socket fd)
  (sb-ext:finalize socket #'(lambda () (sb-posix:close fd))))

(defun create-socket-lisp-stream (socket)
  (setf (slot-value socket 'lisp-stream)
        (sb-sys:make-fd-stream (socket-fd socket)
                               :name (format nil "Socket stream, fd: ~a" (socket-fd socket))
                               :input t :output t :buffering :none :dual-channel-p t
                               :element-type :default :auto-close nil)))

(defmethod socket-open-p ((socket socket))
  (handler-case
      (progn
        (sb-posix:fcntl (socket-fd socket) sb-posix::f-getfl)
        t)
    (sb-posix:syscall-error (err)
      (declare (ignore err))
      nil)))

(defmethod socket-close ((socket socket))
  (sb-posix:close (socket-fd socket))
  (sb-ext:cancel-finalization socket))

(defmethod shared-initialize :after ((socket socket) slot-names &key family type (protocol :default))
  (when (and (slot-boundp socket 'fd)
             (socket-open-p socket))
    (socket-close socket))
  (with-slots (fd (fam family) (proto protocol)) socket
    (multiple-value-bind (sf st pr)
        (translate-make-socket-keywords-to-constants family type protocol)
      (setf fd (sb-posix::socket sf st pr))
      (setf fam family)
      (setf proto pr)
      (set-finalizer-on-socket socket fd))))

(defmethod socket-non-blocking-mode ((socket socket))
  (with-slots (fd) socket
    (let ((fflags (sb-posix:fcntl fd sb-posix::f-getfl)))
      (not (zerop (logand fflags sb-posix:o-nonblock))))))

(defmethod (setf socket-non-blocking-mode) (value (socket socket))
  (declare (type boolean value))
  (with-slots (fd) socket
    (let ((fflags (sb-posix:fcntl fd sb-posix::f-getfl)))
      (sb-posix:fcntl fd sb-posix::f-setfl
                      (logior fflags
                              (if value sb-posix:o-nonblock 0))))))

(defmethod socket-connect ((socket internet-active-socket)
                           (address ipv4addr) &key port)
  (with-alien ((sin sb-posix::sockaddr-in))
    (sb-sys:with-pinned-objects (sin)
      (make-sockaddr-in (addr sin) (name address) port)
      (sb-posix::connect (socket-fd socket)
                         (addr sin)
                         sb-posix::size-of-sockaddr-in)
      (setf (slot-value socket 'address) (copy-netaddr address))
      (setf (slot-value socket 'port) port))))

(defmethod socket-connect ((socket internet-active-socket)
                           (address ipv6addr) &key port)
  (with-alien ((sin6 sb-posix::sockaddr-in6))
    (sb-sys:with-pinned-objects (sin6)
      (make-sockaddr-in6 (addr sin6) (name address) port)
      (sb-posix::connect (socket-fd socket)
                         (addr sin6)
                         sb-posix::size-of-sockaddr-in6)
      (setf (slot-value socket 'address) (copy-netaddr address))
      (setf (slot-value socket 'port) port))))

(defmethod socket-connect ((socket unix-active-socket)
                           (address unixaddr) &key)
  (with-alien ((sun sb-posix::sockaddr-un))
    (sb-sys:with-pinned-objects (sun)
      (make-sockaddr-un (addr sun) (name address))
      (sb-posix::connect (socket-fd socket)
                         (addr sun)
                         sb-posix::size-of-sockaddr-un)
      (setf (slot-value socket 'address) (copy-netaddr address)))))
