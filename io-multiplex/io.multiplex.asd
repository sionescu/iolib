;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :asdf-additions))

(defpackage #:io.multiplex-system
  (:use #:common-lisp #:asdf))

(in-package #:io.multiplex-system)

(defsystem :io.multiplex
  :description "I/O multiplexing library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "GPL-2.1"
  :depends-on (:iolib-posix)
  :default-component-class muffled-source-file
  :components
  ((:file "defpackage")
   (:file "time" :depends-on ("defpackage"))
   (:file "queue" :depends-on ("defpackage"))
   (:file "utils" :depends-on ("defpackage"))
   (:file "common" :depends-on ("defpackage" "time" "utils" "queue"))
   (:file "select" :depends-on ("defpackage" "common" "utils" "queue"))
   #+linux (:file "epoll" :depends-on ("defpackage" "common" "utils" "queue"))
   #+freebsd (:file "kqueue" :depends-on ("defpackage" "common" "utils" "queue"))
   (:file "detect" :depends-on ("defpackage" "common"
                                "select"
                                #+linux "epoll"
                                #+freebsd "kqueue"))))
