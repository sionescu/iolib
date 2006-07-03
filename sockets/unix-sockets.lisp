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

(defmethod socket-connect ((socket unix-socket)
                           (address unixaddr) &key)
  (with-alien ((sun sb-posix::sockaddr-un))
    (sb-sys:with-pinned-objects (sun)
      (make-sockaddr-un (addr sun) (name address))
      (sb-posix::connect (socket-fd socket)
                         (addr sun)
                         sb-posix::size-of-sockaddr-un)
      (setf (slot-value socket 'address) (copy-netaddr address)))))

(defclass socket-stream-unix-active (stream-socket unix-socket active-socket) ())

(defclass socket-stream-unix-passive (stream-socket unix-socket passive-socket) ())

(defclass socket-datagram-unix-active (datagram-socket unix-socket active-socket) ())

(defclass socket-datagram-unix-passive (datagram-socket unix-socket passive-socket) ())
