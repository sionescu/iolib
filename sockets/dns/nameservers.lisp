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

(defvar *resolv-file* "/etc/resolv.conf")

(defvar *dns-nameservers* nil
  "List of the DNS nameservers to use.")

(defvar *dns-domain* nil
  "The current machine's domain.")

(defvar *dns-search-domain* nil
  "A domain name to be appended to the name to be searched when
the latter does not contain dots.")

;;; Only parses NAMESERVER, DOMAIN and SEARCH directives, for now.
(defun search-etc-resolv-conf (file)
  (with-open-file (s file :direction :input)
    (let (nameservers domain search-domain)
      (loop :for line := (read-line s nil nil) :while line :do
            (let ((tokens (split-sequence #\Space line)))
              ;; case sensitive?
              (switch ((first tokens) :test #'string-equal)
                ("nameserver" (ignore-some-conditions (parse-error)
                                (push (ensure-address (second tokens))
                                      nameservers)))
                ("domain" (setq domain (second tokens)))
                ("search" (setq search-domain (second tokens))))))
      (values (nreverse nameservers) domain search-domain))))
