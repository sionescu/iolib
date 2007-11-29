;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; lookup.lisp --- High-level name lookup.
;;;
;;; Copyright (C) 2006-2007, Stelian Ionescu  <sionescu@common-lisp.net>
;;; Copyright (C) 2006-2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by
;;; the Free Software Foundation, as clarified by the
;;; preamble found here:
;;;     http://opensource.franz.com/preamble.html
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General
;;; Public License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;; Boston, MA 02110-1301, USA

(in-package :net.sockets)

;;;; High-level Interface

;;; TODO: caching

(defun remove-trailing-dot (string)
  (assert (> (length string) 1))
  (assert (char= #\. (char string (1- (length string)))))
  (subseq string 0 (1- (length string))))

(defun check-reply-for-errors (reply)
  (flet ((qname-of (query)
           (remove-trailing-dot
            (dns-record-name (aref (dns-message-question reply) 0)))))
    (cond ((dns-flag-p reply :name-error)
           (error 'resolver-no-name-error :data (qname-of reply)))
          ((dns-flag-p reply :server-failure)
           (error 'resolver-fail-error :data (qname-of reply))))))

(defun dns-lookup-host-by-address (address ipv6)
  (let ((reply (dns-query address :type :ptr)))
    (check-reply-for-errors reply)
    (let ((hostname (remove-trailing-dot
                     (dns-rr-data (aref (dns-message-answer reply) 0)))))
      (assert (eq :ptr (dns-record-type (aref (dns-message-answer reply) 0))))
      (values (list address)
              (list (cons hostname address))))))

(defun lookup-host-by-address (address ipv6)
  (multiple-value-bind (addresses aliases)
      (search-host-by-address address)
    (cond (addresses (values addresses aliases))
          (t (dns-lookup-host-by-address address ipv6)))))

(defun dns-lookup-host-by-name (host ipv6)
  )

(defun lookup-host-by-name (host ipv6)
  )

(defun lookup-host (host &key (ipv6 *ipv6*))
  "Looks up a host by name or address.  IPV6 determines the IPv6
behaviour, defaults to *IPV6*."
  (check-type ipv6 (member nil :ipv6 t) "valid IPv6 configuration")
  (let ((address (ignore-some-conditions (parse-error)
                   (ensure-address host))))
    (update-monitor *resolv.conf-monitor*)
    (update-monitor *hosts-monitor*)
    (cond (address
           (lookup-host-by-address address ipv6))
          (t
           (check-type host string)
           (lookup-host-by-name host ipv6)))))
