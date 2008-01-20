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

(defvar *resolv.conf-file* "/etc/resolv.conf")

(defvar *dns-nameservers* nil
  "List of the DNS nameservers to use.")

(defvar *dns-domain* nil
  "The current machine's domain.")

(defvar *dns-search-domain* nil
  "A domain name to be appended to the name to be searched when
the latter does not contain dots.")

(defvar *resolvconf-lock* (bt:make-lock "/etc/resolv.conf lock"))

;;; Only parses NAMESERVER, DOMAIN and SEARCH directives, for now.
(defun parse-/etc/resolv.conf (file)
  (let (nameservers domain search-domain)
    (flet ((parse-one-line (tokens)
             (when (< (length tokens) 2) (error 'parse-error))
             (destructuring-bind (option value &rest more-values) tokens
               (switch (option :test #'string-equal)
                 ("nameserver" (ignore-parse-errors
                                 (push (ensure-address value)
                                       nameservers)))
                 ("domain" (setf domain value))
                 ("search" (setf search-domain (cons value more-values)))))))
      (iterate ((tokens (serialize-etc-file file)))
        (ignore-errors (parse-one-line tokens)))
      (values (nreverse nameservers) domain search-domain))))

(defun update-dns-parameters (file)
  (multiple-value-bind (ns domain search)
      (parse-/etc/resolv.conf file)
    (setf *dns-nameservers* (or ns +ipv4-loopback+)
          ;; everything after the first dot
          *dns-domain* (cdr (split-sequence #\. domain :count 2))
          *dns-search-domain* search)))

(defvar *resolv.conf-monitor*
  (make-instance 'file-monitor
                 :file *resolv.conf-file*
                 :update-fn 'update-dns-parameters
                 :lock *resolvconf-lock*))
