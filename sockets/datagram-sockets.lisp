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

(defclass socket-datagram-internet-active (active-socket datagram-socket internet-socket) ())

(defclass socket-datagram-internet-passive (passive-socket datagram-socket internet-socket) ())

(defmethod socket-unconnect ((socket datagram-socket))
  (with-pinned-aliens ((sin sb-posix::sockaddr-in))
    (sb-posix::memset (addr sin) 0 sb-posix::size-of-sockaddr-in)
    (setf (slot sin 'sb-posix::addr) sb-posix::af-unspec)
    (sb-posix::connect (socket-fd socket)
                       (addr sin)
                       sb-posix::size-of-sockaddr-in)
    (slot-makunbound socket 'address)
    (when (typep socket 'internet-socket)
      (slot-makunbound socket 'port))))
