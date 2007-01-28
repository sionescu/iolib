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

(in-package :net.sockets)

(defvar *ipv6* t
  "Specifies the default behaviour with respect to IPv6:
- nil   : Only IPv4 addresses are used.
- :ipv6 : Only IPv6 addresses are used.
- t     : If both IPv4 and IPv6 addresses are found they are returned in the best order possible(see RFC 3484).
Default value is T.")

(define-constant +max-backlog-size+ et:somaxconn
  "Maximum length of the pending connections queue(hard limit).")

(defvar *default-backlog-size* 5
  "Default length of the pending connections queue(soft limit).")

(defvar *no-sigpipe* t
  "When T an EOF received on a stream socket won't raise a SIGPIPE. Default value it T.")
