;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package #:cl-user)

(defpackage #:net.sockets-system
  (:use #:common-lisp #:asdf))

(in-package #:net.sockets-system)

(defclass iolib-source-file (cl-source-file) ())
  
(defmethod perform :around ((o compile-op) (s iolib-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let ((*compile-print* nil))
      (call-next-method))))

(defsystem net.sockets
  :description "Socket library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "GPL-2.1"
  :depends-on (:iolib-alien-ng
               :io.multiplex)
  :default-component-class iolib-source-file
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
