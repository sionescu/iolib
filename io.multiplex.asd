;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; io-multiplex.asd --- ASDF system definition.
;;;
;;; Copyright (C) 2006-2008, Stelian Ionescu  <sionescu@common-lisp.net>
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
(eval-when (:load-toplevel)
  (asdf:oos 'asdf:load-op :trivial-features))

(asdf:defsystem :io.multiplex
  :description "I/O multiplexing library."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "LLGPL-2.1"
  :depends-on (:osicat :alexandria :bordeaux-threads :series)
  :pathname (merge-pathnames #p"io.multiplex/" *load-truename*)
  :components
  ((:file "pkgdcl")
   (:file "time" :depends-on ("pkgdcl"))
   (:file "utils" :depends-on ("pkgdcl" "time"))
   (:file "timers" :depends-on ("pkgdcl" "time"))
   (:file "queue" :depends-on ("pkgdcl"))
   (:file "scheduler" :depends-on ("pkgdcl" "timers" "queue"))
   (:file "fd-entry" :depends-on ("pkgdcl" "timers"))
   (:file "multiplexer" :depends-on ("pkgdcl" "utils" "fd-entry"))
   (:file "event-loop" :depends-on ("pkgdcl" "time" "timers" "queue"
                                    "scheduler" "fd-entry" "multiplexer"))
   (:file "fd-wait" :depends-on ("pkgdcl" "utils"))
   (:file "select" :depends-on ("pkgdcl" "utils" "fd-entry" "multiplexer"))
   #+linux (:file "epoll" :depends-on ("pkgdcl" "utils" "fd-entry" "multiplexer"))
   #+bsd (:file "kqueue" :depends-on ("pkgdcl" "utils" "fd-entry" "multiplexer"))
   (:file "detect" :depends-on ("pkgdcl" "multiplexer" "select"
                                #+linux "epoll" #+bsd "kqueue"))))
