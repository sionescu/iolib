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

(defclass socket-stream-unix-active (active-socket stream-socket unix-socket) ())

(defclass socket-stream-unix-passive (passive-socket stream-socket unix-socket) ())

(defclass socket-datagram-unix-active (active-socket datagram-socket unix-socket) ())

(defclass socket-datagram-unix-passive (passive-socket datagram-socket unix-socket) ())

(defmethod socket-connect ((socket unix-socket)
                           (address unixaddr) &key)
  (with-pinned-aliens ((sun sb-posix::sockaddr-un))
    (make-sockaddr-un (addr sun) (name address))
    (sb-posix::connect (socket-fd socket)
                       (addr sun)
                       sb-posix::size-of-sockaddr-un)
    (setf (slot-value socket 'address) (copy-netaddr address))))

(defmethod socket-bind :before ((socket unix-socket)
                                (address unixaddr) &key)
  (when (typep socket 'active-socket)
    (error "You can't bind an active Unix socket.")))

(defmethod socket-bind ((socket unix-socket)
                        (address unixaddr) &key)
  (with-pinned-aliens ((sun sb-posix::sockaddr-un))
    (make-sockaddr-un (addr sun) (name address))
    (sb-posix::bind (socket-fd socket)
                    (addr sun)
                    sb-posix::size-of-sockaddr-un)
    (setf (slot-value socket 'address) (copy-netaddr address))))
