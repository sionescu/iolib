;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; gnutls.lisp --- Bindings to GNUTLS 2.2.0(and compatible).
;;;
;;; Copyright (C) 2008, Stelian Ionescu  <sionescu@common-lisp.net>
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

(in-package :net.tls)

(defmacro define-gnutls-function (name return-type &body args)
  (multiple-value-bind (lisp-name c-name options)
      (cffi::parse-name-and-options name)
    `(defcfun (,c-name ,lisp-name ,@options)
         (errno-wrapper ,return-type
                        :error-generator signal-gnutls-error)
       ,@args)))

(defmacro define-gnutls-function* (name return-type &body args)
  (multiple-value-bind (lisp-name c-name options)
      (cffi::parse-name-and-options name)
    `(defcfun (,c-name ,lisp-name ,@options)
         (errno-wrapper ,return-type)
       ,@args)))

(define-gnutls-function* (%check-version "gnutls_check_version") :string
  (required-version :string))
