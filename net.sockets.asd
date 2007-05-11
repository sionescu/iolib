;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :asdf-additions))

(defpackage #:net.sockets-system
  (:use #:common-lisp #:asdf #:asdf-additions))

(in-package #:net.sockets-system)

(defsystem :net.sockets
  :description "Socket library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "LLGPL-2.1"
  :depends-on (:iolib-posix
               :iolib-utils-symbols
               :iolib-utils-misc
               :io.encodings
               :io.streams
               :split-sequence)
  :default-component-class muffled-source-file
  :pathname (merge-pathnames (make-pathname :directory '(:relative "sockets"))
                             *load-truename*)
  :components
  ((:file "pkgdcl")
   (:file "common" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl" "common"))
   (:file "config" :depends-on ("pkgdcl" "common"))
   (:file "iface" :depends-on ("pkgdcl" "common" "conditions"))
   (:file "address" :depends-on ("pkgdcl" "common" "conditions"))
   (:file "resolv" :depends-on ("pkgdcl" "common"
                                "config" "conditions" "address"))
   (:file "base-sockets" :depends-on ("pkgdcl" "config" "resolv"))
   (:file "socket-options" :depends-on ("pkgdcl" "common" "base-sockets"))
   (:file "socket-methods"
          :depends-on ("pkgdcl" "config" "common" "address"
                       "base-sockets" "socket-options"))
   (:file "make-socket"
          :depends-on ("pkgdcl" "config" "socket-methods"))))
