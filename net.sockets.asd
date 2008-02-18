;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; net.sockets.asd --- ASDF system definition.
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(asdf:defsystem :net.sockets
  :description "Socket library."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "LLGPL-2.1"
  :depends-on (:osicat :babel :bordeaux-threads :series
               :io.streams :alexandria :split-sequence)
  :pathname (merge-pathnames #p"net.sockets/" *load-truename*)
  :components
  ((:file "pkgdcl")
   (cffi-grovel:grovel-file "grovel" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl" "grovel"))
   (:file "bsd" :depends-on ("pkgdcl" "grovel" "conditions"))
   (:file "common" :depends-on ("pkgdcl" "grovel" "bsd"))
   (:file "config" :depends-on ("pkgdcl" "grovel"))

   (:file "iface" :depends-on ("pkgdcl" "grovel" "bsd" "common"))

   (:file "address" :depends-on ("pkgdcl" "common"))
   (:file "address-predicates" :depends-on ("pkgdcl" "common" "address"))
   (:file "address-arithmetic" :depends-on ("pkgdcl" "common" "address" "address-predicates"))

   (:file "base-sockets" :depends-on ("pkgdcl" "bsd" "config"))
   (:file "socket-options"
          :depends-on ("pkgdcl" "grovel" "conditions" "bsd" "common" "config" "base-sockets"))

   (:file "etc-files" :pathname #p"namedb/etc-files"
          :depends-on ("pkgdcl"))
   (:file "file-monitor" :pathname #p"namedb/file-monitor"
          :depends-on ("pkgdcl"))
   (:file "protocols" :pathname #p"namedb/protocols"
          :depends-on ("pkgdcl" "common" "etc-files" "file-monitor"))
   (:file "services" :pathname #p"namedb/services"
          :depends-on ("pkgdcl" "common" "etc-files" "file-monitor"))
   (:file "hosts" :pathname #p"namedb/hosts"
          :depends-on ("pkgdcl" "address" "address-predicates" "etc-files"
                       "file-monitor"))

   (:file "socket-methods"
          :depends-on ("pkgdcl" "grovel" "conditions" "bsd" "common" "config"
                       "address" "address-predicates" "base-sockets" "socket-options"
                       "protocols" "services"))
   (:file "make-socket"
          :depends-on ("pkgdcl" "common" "config" "address" "address-predicates"
                       "socket-options" "services" "socket-methods"))

   (:file "dns-common" :pathname #p"dns/common"
          :depends-on ("pkgdcl" "common"))
   (:file "nameservers" :pathname #p"dns/nameservers"
          :depends-on ("pkgdcl" "address" "etc-files" "file-monitor"))
   (:file "dynamic-buffer" :pathname #p"dns/dynamic-buffer"
          :depends-on ("pkgdcl"))
   (:file "message" :pathname #p"dns/message"
          :depends-on ("pkgdcl" "common" "dns-common" "dynamic-buffer"))
   (:file "query" :pathname #p"dns/query"
          :depends-on ("pkgdcl" "conditions" "address" "address-predicates"
                       "socket-options" "socket-methods" "make-socket" "dns-common"
                       "nameservers" "dynamic-buffer" "message"))
   (:file "dns-conditions" :pathname #p"dns/conditions"
          :depends-on ("pkgdcl"))
   (:file "lookup" :pathname #p"dns/lookup"
          :depends-on ("pkgdcl" "address" "address-predicates" "file-monitor" "hosts"
                       "nameservers" "message" "query" "dns-conditions"))))
