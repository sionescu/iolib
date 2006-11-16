;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package #:cl-user)

(defpackage #:io.multiplex-system
  (:use #:common-lisp #:asdf))

(in-package #:io.multiplex-system)

(defclass iolib-source-file (cl-source-file) ())
  
(defmethod perform :around ((o compile-op) (s iolib-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let ((*compile-print* nil))
      (call-next-method))))

(defsystem :io.multiplex
  :description "I/O multiplexing library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "GPL-2.1"
  :depends-on (#:iolib-alien-ng)
  :default-component-class iolib-source-file
  :components
  ((:file "defpackage")
   (:file "common" :depends-on ("defpackage"))
   (:file "select" :depends-on ("defpackage" "common"))
   #+linux (:file "epoll" :depends-on ("defpackage" "common"))
   (:file "detect" :depends-on ("defpackage" "common" "select"
                                #+linux "epoll"))))
