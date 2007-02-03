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
