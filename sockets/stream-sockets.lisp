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

(defclass socket-stream-internet-active (active-socket stream-socket internet-socket) ())

(defclass socket-stream-internet-passive (passive-socket stream-socket internet-socket) ())

(defmethod socket-write-char (char (socket stream-socket))
  (write-char char (socket-lisp-stream socket)))

(defmethod socket-read-char ((socket stream-socket)
                             &key eof-error-p eof-value)
  (read-char (socket-lisp-stream socket) eof-error-p eof-value))

(defmethod socket-write-byte (byte (socket stream-socket))
  (write-byte byte (socket-lisp-stream socket)))

(defmethod socket-read-byte ((socket stream-socket)
                             &key eof-error-p eof-value)
  (read-byte (socket-lisp-stream socket) eof-error-p eof-value))

(defmethod socket-write-sequence ((sequence simple-array)
                                  (socket stream-socket)
                                  &key (start 0) end)
  (write-sequence sequence (socket-lisp-stream socket) :start start :end end))

(defmethod socket-read-sequence ((sequence simple-array)
                                 (socket stream-socket)
                                 &key (start 0) end)
  (read-sequence sequence (socket-lisp-stream socket) :start start :end end))


(defmethod socket-send ((buffer simple-array)
                        (socket socket-stream-internet-active) &key
                        dont-route dont-wait (no-signal *no-sigpipe*)
                        out-of-band #+linux more &allow-other-keys)
  )

(defmethod socket-receive ((buffer simple-array)
                           (socket socket-stream-internet-active) &key
                           out-of-band peek wait-all dont-wait &allow-other-keys)
  )
