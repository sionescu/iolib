;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package #:cl-user)

(defpackage #:io.multiplex-system
  (:use #:common-lisp #:asdf))

(in-package #:io.multiplex-system)

(defsystem io.multiplex
  :description "I/O multiplexing library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "GPL-2.1"
  :depends-on (#:iolib-alien-ng)
  :components
  ((:file "defpackage")
   (:file "common" :depends-on ("defpackage"))
   (:file "select" :depends-on ("defpackage" "common"))
   (:file "event-loop" :depends-on ("defpackage" "common" "select"))))
