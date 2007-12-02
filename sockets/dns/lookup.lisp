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

(define-constant +max-ipv4-value+ (1- (expt 2 32))
  :documentation "Integer denoting 255.255.255.255")

;;;; High-level Interface

;;; TODO: caching

(defun remove-trailing-dot (string)
  (assert (> (length string) 1))
  (assert (char= #\. (char string (1- (length string)))))
  (subseq string 0 (1- (length string))))

(defun reply-error-condition (reply query-type)
  (cond ((dns-flag-p reply :name-error) 'resolver-no-name-error)
        ((dns-flag-p reply :server-failure) 'resolver-fail-error)
        ((loop :for rr :across (dns-message-answer reply)
               :never (eq query-type (dns-record-type rr)))
         'resolver-no-name-error)))

(defun check-reply-for-errors (reply host query-type)
  (let ((condition (reply-error-condition reply query-type)))
    (and condition (error condition :data host))))

(defun dns-lookup-host-by-address (address ipv6)
  (let ((reply (dns-query address :type :ptr)))
    (check-reply-for-errors reply address)
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

(defun process-one-reply (reply query-type)
  (let (addresses aliases)
    (loop :for rr :across (dns-message-answer reply) :do
       (switch ((dns-record-type rr) :test #'eq)
         (:cname 'ok)
         (query-type (let ((address (ensure-address (dns-rr-data rr)))
                           (name (remove-trailing-dot (dns-record-name rr))))
                       (push address addresses)
                       (push (cons name address) aliases)))
         (t (warn "Invalid RR type: ~S" (dns-record-type rr)))))
    (values (nreverse addresses)
            (nreverse aliases))))

(defun dns-lookup-host-in-one-domain (host query-type)
  (let ((reply (dns-query host :type query-type)))
    (check-reply-for-errors reply host)
    (process-one-query reply query-type)))

(defun merge-a-and-aaaa-replies (4-reply 6-reply)
  (multiple-value-bind (4-addresses 4-aliases)
      (process-one-reply 4-reply :a)
    (multiple-value-bind (6-addresses 6-aliases)
        (process-one-reply 6-reply :aaaa)
      (values (nconc 4-addresses 6-addresses)
              (nconc 4-aliases 6-aliases)))))

(defun dns-lookup-host-in-a-and-aaaa (host)
  (let* ((4-reply (dns-query host :type :a))
         (4-err (reply-error-condition 4-reply :a))
         (6-reply (dns-query host :type :aaaa))
         (6-err (reply-error-condition 6-reply :aaaa)))
    (cond
      ((and 4-err 6-err)
       (error (if (member 'resolver-fail-error (list 4-err 6-err)
                          :test #'eq)
                  'resolver-fail-error
                  'resolver-no-name-error)
              :data host))
      (4-err (process-one-reply 6-reply :aaaa))
      (6-err (process-one-reply 4-reply :a))
      (t (merge-a-and-aaaa-replies 4-reply 6-reply)))))

(defun dns-lookup-host-by-name (host ipv6)
  (case ipv6
    ((nil)   (dns-lookup-host-in-one-domain host :a))
    ((:ipv6) (dns-lookup-host-in-one-domain host :aaaa))
    ((t)     (dns-lookup-host-in-a-and-aaaa host))))

(defun lookup-host-by-name (host ipv6)
  (multiple-value-bind (addresses aliases)
      (search-host-by-name host ipv6)
    (cond (addresses (values addresses aliases))
          (t (dns-lookup-host-by-name host ipv6)))))

;; TODO: * implement address selection as per RFC 3484
;;       * add caching
;;       * profile the whole thing
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
