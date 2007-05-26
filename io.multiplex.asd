;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :asdf-additions))

(defpackage #:io.multiplex-system
  (:use #:common-lisp #:asdf-additions))

(in-package #:io.multiplex-system)

(asdf:defsystem :io.multiplex
  :description "I/O multiplexing library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "LLGPL-2.1"
  :depends-on (:iolib-posix
               :iolib-utils-misc)
  :default-component-class muffled-source-file
  :pathname (merge-pathnames (make-pathname :directory '(:relative "io-multiplex"))
                             *load-truename*)
  :components
  ((:file "pkgdcl")
   (:file "time" :depends-on ("pkgdcl"))
   (:file "queue" :depends-on ("pkgdcl"))
   (:file "utils" :depends-on ("pkgdcl" "time"))
   (:file "common" :depends-on ("pkgdcl" "time" "utils" "queue"))
   (:file "select" :depends-on ("pkgdcl" "common" "utils" "queue"))
   #+linux (:file "epoll" :depends-on ("pkgdcl" "common" "utils" "queue"))
   #+freebsd (:file "kqueue" :depends-on ("pkgdcl" "common" "utils" "queue"))
   (:file "detect" :depends-on ("pkgdcl" "common"
                                "select"
                                #+linux "epoll"
                                #+freebsd "kqueue"))))
