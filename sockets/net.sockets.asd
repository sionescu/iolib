;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :cffi))

(defpackage #:net.sockets-system
  (:use #:common-lisp #:asdf))

#+cffi-features:no-finalizers
(error "NET.SOCKETS needs an implementation that has support for finalizers.")

(in-package #:net.sockets-system)

(defsystem :net.sockets
  :description "Socket library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "GPL-2.1"
  :depends-on (:iolib-alien-ng
               :iolib-utils-symbols
               :iolib-utils-misc
               :io.multiplex
               :flexi-streams
               :split-sequence)
  :default-component-class muffled-source-file
  :components
  ((:file "defpackage")
   (:file "common" :depends-on ("defpackage"))
   (:file "conditions" :depends-on ("defpackage" "common"))
   (:file "config" :depends-on ("defpackage" "common"))
   (:file "iface" :depends-on ("defpackage" "common" "conditions"))
   (:file "address" :depends-on ("defpackage" "common" "conditions"))
   (:file "resolv" :depends-on ("defpackage" "common"
                                "config" "conditions" "address"))
   (:file "base-sockets" :depends-on ("defpackage" "config" "resolv"))
   (:file "socket-options" :depends-on ("defpackage" "common" "base-sockets"))
   (:file "socket-methods"
          :depends-on ("defpackage" "config" "common" "address"
                       "base-sockets" "socket-options"))
   (:file "make-socket"
          :depends-on ("defpackage" "config" "socket-methods"))))
