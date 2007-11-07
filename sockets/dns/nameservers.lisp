;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; nameservers.lisp --- Nameservers management.
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

(defvar *resolv-file-timestamp* nil)
(defvar *resolv-file* "/etc/resolv.conf")

(defvar *dns-nameservers* nil
  "List of the DNS nameservers to use.")

(defvar *dns-domain* nil
  "The current machine's domain.")

(defvar *dns-search-domain* nil
  "A domain name to be appended to the name to be searched when
the latter does not contain dots.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Only parses NAMESERVER, DOMAIN and SEARCH directives, for now.
  (defun search-etc-resolv-conf (&optional (file *resolv-file*))
    (flet ((split-tokens (line) (split-sequence-if #'space-char-p line
                                                   :remove-empty-subseqs t)))
      (let (nameservers domain search-domain)
        (iterate ((tokens (#msplit-tokens (scan-file file #'read-line))))
          (switch ((first tokens) :test #'string-equal)
            ("nameserver" (ignore-some-conditions (parse-error)
                            (push (ensure-address (second tokens))
                                  nameservers)))
            ("domain" (setf domain (second tokens)))
            ("search" (setf search-domain (second tokens))))
          (return-from search-etc-resolv-conf
            (values (nreverse nameservers) domain search-domain))))))

  (defun resolv-conf-oldp ()
    (let ((ts (file-write-date *resolv-file*)))
      (values (or (not *resolv-file-timestamp*)
                  (/= *resolv-file-timestamp* ts))
              ts)))

  (defun update-dns-parameters ()
    (multiple-value-bind (oldp ts) (resolv-conf-oldp)
      (when oldp
        (multiple-value-bind (ns domain search) (search-etc-resolv-conf)
          (setf *dns-nameservers* (or ns +ipv4-loopback+)
                ;; everything after the first dot
                *dns-domain* (cdr (split-sequence #\. domain :count 2))
                *dns-search-domain* search
                *resolv-file-timestamp* ts)
          (values *dns-nameservers* *dns-domain* *dns-search-domain*))))))

(update-dns-parameters)
