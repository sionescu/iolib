;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; io-multiplex.asd --- ASDF system definition.
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

(in-package :common-lisp-user)

;;; Need trivial-features to correctly handle the reader conditionals
;;; in the system definition form.
(eval-when (:load-toplevel :execute)
  (asdf:oos 'asdf:load-op :trivial-features))

(defpackage #:io.multiplex-system
  (:use #:common-lisp))

(in-package #:io.multiplex-system)

(asdf:defsystem :io.multiplex
  :description "I/O multiplexing library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "LLGPL-2.1"
  :depends-on (:osicat :alexandria)
  :pathname (merge-pathnames (make-pathname :directory '(:relative "io-multiplex"))
                             *load-truename*)
  :serial t
  :components
  ((:file "pkgdcl")
   (:file "time")
   (:file "queue")
   (:file "utils")
   (:file "common")
   #-windows (:file "select")
   #+windows (:file "wait")
   #+linux (:file "epoll")
   #+bsd (:file "kqueue")
   (:file "detect")))
