;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :asdf-additions))

(defpackage #:io.multiplex-system
  (:use #:common-lisp #:asdf))

#+cffi-features:no-finalizers
(error "IO.MULTIPLEX needs an implementation that has support for finalizers.")

(in-package #:io.multiplex-system)

(defsystem :io.multiplex
  :description "I/O multiplexing library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "GPL-2.1"
  :depends-on (:iolib-alien-ng)
  :default-component-class muffled-source-file
  :components
  ((:file "defpackage")
   (:file "common" :depends-on ("defpackage"))
   (:file "select" :depends-on ("defpackage" "common"))
   #+linux (:file "epoll" :depends-on ("defpackage" "common"))
   (:file "detect" :depends-on ("defpackage" "common" "select"
                                #+linux "epoll"))))
