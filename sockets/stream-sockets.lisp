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

(defclass socket-stream-internet-active (stream-socket internet-socket active-socket) ()
  (:default-initargs :type :stream))

(defmethod socket-write-char (char (socket socket-stream-internet-active))
  (write-char char (socket-lisp-stream socket)))

(defmethod socket-read-char ((socket socket-stream-internet-active)
                             &key eof-error-p eof-value)
  (read-char (socket-lisp-stream socket) eof-error-p eof-value))

(defmethod socket-write-byte (byte (socket socket-stream-internet-active))
  (write-byte byte (socket-lisp-stream socket)))

(defmethod socket-read-byte ((socket socket-stream-internet-active)
                             &key eof-error-p eof-value)
  (read-byte (socket-lisp-stream socket) eof-error-p eof-value))

(defmethod socket-write-sequence ((sequence simple-array)
                                  (socket socket-stream-internet-active)
                                  &key (start 0) end)
  (write-sequence sequence (socket-lisp-stream socket) :start start :end end))

(defmethod socket-read-sequence ((sequence simple-array)
                                 (socket socket-stream-internet-active)
                                 &key (start 0) end)
  (read-sequence sequence (socket-lisp-stream socket) :start start :end end))

(defmethod socket-send ((buffer simple-array)
                        (socket socket-stream-internet-active) &key
                        dont-route dont-wait no-signal out-of-band
                        #+linux more &allow-other-keys)
  )

(defmethod socket-receive ((buffer simple-array)
                           (socket socket-stream-internet-active) &key
                           out-of-band peek wait-all dont-wait &allow-other-keys)
  )

(defclass socket-stream-internet-passive (stream-socket internet-passive-socket) ())
