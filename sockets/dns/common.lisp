;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; common.lisp --- DNS client constants.
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

(defconstant +opcode-standard+ 0)

(define-constant +query-type-map+
    '((:a     .   1)
      (:ns    .   2)
      (:cname .   5)
      (:soa   .   6)
      (:wks   .  11)
      (:ptr   .  12)
      (:hinfo .  13)
      (:mx    .  15)
      (:txt   .  16)
      (:aaaa  .  28)
      (:any   . 255))
  :test #'equal)

(defun query-type-number (id)
  (cdr (assoc id +query-type-map+)))

(defun query-type-id (number)
  (car (rassoc number +query-type-map+)))

(defun dns-record-type-p (id)
  (query-type-number id))

(define-constant +query-class-map
    '((:in  .   1)
      (:any . 255))
  :test #'equal)

(defun query-class-number (id)
  (cdr (assoc id +query-class-map)))

(defun query-class-id (number)
  (car (rassoc number +query-class-map)))

(define-constant +rcode-map+
    '((:no-error        . 0)
      (:format-error    . 1)
      (:server-failure  . 2)
      (:name-error      . 3)
      (:not-implemented . 4)
      (:refused         . 5))
  :test #'equal)

(defun rcode-number (id)
  (cdr (assoc id +rcode-map+)))

(defun rcode-id (number)
  (car (rassoc number +rcode-map+)))

(defconstant +dns-max-datagram-size+ 4096)
