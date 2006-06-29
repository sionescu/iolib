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
  ((fd      :reader socket-fd)
   (address :reader socket-address :type netaddr)))

(defgeneric socket-non-blocking-mode (socket))
(defgeneric (setf socket-non-blocking-mode) (value socket))

(defgeneric socket-close (socket))

(defgeneric socket-open-p (socket))

(defclass stream-socket (socket)
  ())

(defgeneric socket-write-char (char socket))

(defgeneric socket-read-char (socket))

(defgeneric socket-write-byte (byte socket))

(defgeneric socket-read-byte (socket))

(defgeneric socket-write-sequence (sequence socket))

(defgeneric socket-read-sequence (sequence socket))

(defgeneric socket-send (buffer socket &key
                         remote-address
                         end-of-record out-of-band
                         &allow-other-keys))

(defgeneric socket-receive (buffer socket &key
                            out-of-band peek wait-all
                            &allow-other-keys))

(defclass datagram-socket (socket)
  ())

(defgeneric write-datagram (socket &key address))

(defgeneric read-datagram (socket &key address))

(defclass internet-socket (socket)
  ((port :initarg :port :reader port :type 'ub16)))

(defgeneric local-name (socket))

(defgeneric local-host (socket))

(defgeneric local-port (socket))

(defclass unix-socket (socket)
  ())

(defclass active-socket (socket)
  ())

(defgeneric socket-connect (socket address port &key))

(defgeneric remote-name (socket))

(defgeneric remote-host (socket))

(defgeneric remote-port (socket))

(defgeneric shutdown (socket &key direction))

(defclass passive-socket (socket)
  ())

(defgeneric socket-bind-address (socket address &key reuse-address interface))

(defgeneric socket-listen (socket &key backlog))

(defgeneric socket-accept-connection (passive-socket &key active-socket wait))




(defun translate-make-socket-keywords-to-constants (family type)
  (let ((sf (ecase family
              (:ipv4 et::af-inet)
              (:ipv6 et::af-inet6)
              (:unix et::af-unix)))
        (st (ecase type
              (:stream   et::sock-stream)
              (:datagram et::sock-dgram))))
    (values sf st)))

(defmethod initialize-instance :after ((socket socket) &key family type (protocol 0))
  (with-slots (fd) socket
    (multiple-value-bind (sf st)
        (translate-make-socket-keywords-to-constants family type)
      (setf fd (et::socket sf st protocol)))))

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

(defmethod socket-close ((socket socket))
  (sb-posix:close (socket-fd socket)))

(defmethod socket-open-p ((socket socket))
  (handler-case
      (progn
        (sb-posix:fcntl (socket-fd socket) sb-posix::f-getfl)
        t)
    (sb-posix:syscall-error (err)
      (declare (ignore err))
      nil)))
