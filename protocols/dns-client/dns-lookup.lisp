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

;; (declaim (optimize (speed 2) (safety 2) (space 1) (debug 2)))
(declaim (optimize (speed 0) (safety 2) (space 0) (debug 2)))

(in-package :net.sockets)

(defvar *hosts-file* "/etc/hosts")

(defgeneric dns-lookup-host (host &key ipv6))

(defmethod dns-lookup-host ((host string) &key (ipv6 *ipv6*))
  (or (search-etc-hosts-name *hosts-file* host ipv6)
      (dns-query host :type :a)))

(defun dns-lookup-host-ip (vector ipv6)
  (or (search-etc-hosts-ip *hosts-file* vector ipv6)
      (dns-query vector :type :ptr)))

(defmethod dns-lookup-host ((host ipv4addr) &key (ipv6 *ipv6*))
  (dns-lookup-host-ip (name host) ipv6))

(defmethod dns-lookup-host ((host ipv6addr) &key (ipv6 *ipv6*))
  (dns-lookup-host-ip (name host) ipv6))

(defmethod dns-lookup-host (host &key (ipv6 *ipv6*))
  (etypecase host
    (simple-array (dns-lookup-host-ip host ipv6))))
