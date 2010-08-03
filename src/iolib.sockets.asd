;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :iolib.base)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(in-package :iolib.asdf)

(defsystem :iolib.sockets
  :description "Socket library."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version #.(with-open-file (f (merge-pathnames "../version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :licence "MIT"
  :depends-on (:iolib.base :iolib.syscalls :iolib.streams
               :babel :cffi :cffi-grovel :bordeaux-threads)
  :default-component-class iolib-source-file
  :pathname #-asdf2 (merge-pathnames "sockets/" *load-truename*)
            #+asdf2 "sockets/"
  :components
  ((:file "pkgdcl")
   (cffi-grovel:grovel-file "grovel" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl" "grovel"))
   (:file "bsd" :depends-on ("pkgdcl" "grovel" "conditions"))
   (:file "common" :depends-on ("pkgdcl" "grovel" "bsd"))
   (:file "config" :depends-on ("pkgdcl" "grovel" "bsd"))

   (:file "iface" :depends-on ("pkgdcl" "grovel" "bsd" "common"))

   (:file "address" :depends-on ("pkgdcl" "common"))
   (:file "address-predicates" :depends-on ("pkgdcl" "common" "address"))
   (:file "address-arithmetic" :depends-on ("pkgdcl" "common" "address" "address-predicates"))

   (:file "base-sockets" :depends-on ("pkgdcl" "bsd" "config"))
   (:file "socket-options"
     :depends-on ("pkgdcl" "grovel" "conditions" "bsd" "common" "config" "base-sockets"))

   ;; Local file configuration (/etc/hosts etc...)
   (:file "etc-files" :pathname "namedb/etc-files"
     :depends-on ("pkgdcl"))
   (:file "file-monitor" :pathname "namedb/file-monitor"
     :depends-on ("pkgdcl"))
   (:file "protocols" :pathname "namedb/protocols"
     :depends-on ("pkgdcl" "common" "etc-files" "file-monitor"))
   (:file "services" :pathname "namedb/services"
     :depends-on ("pkgdcl" "common" "etc-files" "file-monitor"))
   (:file "hosts" :pathname "namedb/hosts"
     :depends-on ("pkgdcl" "address" "address-predicates" "etc-files" "file-monitor"))

   (:file "socket-methods"
     :depends-on ("pkgdcl" "grovel" "conditions" "bsd" "common" "config"
                  "address" "address-predicates" "base-sockets" "socket-options"
                  "protocols" "services"))
   (:file "make-socket"
     :depends-on ("pkgdcl" "grovel" "common" "config" "address" "address-predicates"
                  "socket-options" "services" "socket-methods"))

   ;; DNS client
   (:file "dns-common" :pathname "dns/common"
     :depends-on ("pkgdcl" "common"))
   (:file "nameservers" :pathname "dns/nameservers"
     :depends-on ("pkgdcl" "address" "etc-files" "file-monitor"))
   (:file "dynamic-buffer" :pathname "dns/dynamic-buffer"
     :depends-on ("pkgdcl"))
   (:file "message":pathname "dns/message"
     :depends-on ("pkgdcl" "common" "dns-common" "dynamic-buffer"))
   (:file "query" :pathname "dns/query"
     :depends-on ("pkgdcl" "conditions" "address" "address-predicates"
                  "socket-options" "socket-methods" "make-socket" "dns-common"
                  "nameservers" "dynamic-buffer" "message"))
   (:file "dns-conditions" :pathname "dns/conditions"
     :depends-on ("pkgdcl"))
   (:file "lookup" :pathname "dns/lookup"
     :depends-on ("pkgdcl" "address" "address-predicates" "file-monitor" "hosts"
                  "nameservers" "message" "query" "dns-conditions"))))
