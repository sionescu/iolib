;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; lookup.lisp --- High-level name lookup.
;;;
;;; Copyright (C) 2006-2007, Stelian Ionescu  <sionescu@common-lisp.net>
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

(defgeneric dns-lookup-host (host &key ipv6))

;;; FIXME: add caching
(defmethod dns-lookup-host :around (host &key &allow-other-keys)
  (declare (ignore host))
  (flet ((%setup-dns-params ()
           #-windows (search-etc-resolv-conf *resolv-file*)
           #+windows (ensure-address (get-first-dns-server))))
    (multiple-value-bind (*dns-nameservers* *dns-domain* *dns-search-domain*)
        (%setup-dns-params)
      (call-next-method))))

(defmethod dns-lookup-host ((host string) &key (ipv6 *ipv6*))
  (declare (ignorable ipv6))
  (or #-windows (search-etc-hosts-name *hosts-file* host ipv6)
      (dns-query host :type :a)))

(defun dns-lookup-host-ip (vector ipv6)
  (declare (ignorable ipv6))
  (or #-windows (search-etc-hosts-ip *hosts-file* vector ipv6)
      (dns-query vector :type :ptr)))

(defmethod dns-lookup-host ((host inet-address) &key (ipv6 *ipv6*))
  (dns-lookup-host-ip (address-name host) ipv6))

(defmethod dns-lookup-host ((host vector) &key (ipv6 *ipv6*))
  (dns-lookup-host (ensure-address host) :ipv6 ipv6))

;;;; High-level Interface

;;; TODO: caching, etc.  Also, verify that this isn't completely
;;; wrong.  It's very likely that it isn't complete because my
;;; knowledge of DNS is almost nil.  --luis

(defun lookup-host (host &key (ipv6 *ipv6*))
  "Looks up a host by name or address.  IPV6 determines the IPv6
behaviour, defaults to *IPV6*."
  (check-type ipv6 (member nil :ipv6 t) "valid IPv6 configuration")
  (let ((reply (dns-lookup-host host :ipv6 ipv6)))
    (when (typep reply 'host)
      (return-from lookup-host reply))
    ;; or check the :NAME-ERROR flag?
    (cond ((member :name-error (decoded-flags reply))
           (error 'resolver-no-name-error :data nil :message nil))
          ((member :server-failure (decoded-flags reply))
           (error 'resolver-fail-error :data nil :message nil)))
    (flet ((rtd (string)
             ;; remove trailing dot
             (assert (> (length string) 1))
             (assert (char= #\. (char string (1- (length string)))))
             (subseq string 0 (1- (length string)))))
      (loop :with aliases := nil :and truename := nil :and addresses := nil
            :for record :across (dns-message-answer reply) :do
            (case (dns-record-type record)
              (:cname (push (rtd (dns-record-name record)) aliases))
              (:a (setq truename (rtd (dns-record-name record)))
                  (push (ensure-address (dns-rr-data record)) addresses))
              (:ptr (setq truename (rtd (dns-rr-data record)))
                    ;; is this right?
                    (push (ensure-address host) addresses)))
            :finally (return (make-host truename addresses aliases))))))
