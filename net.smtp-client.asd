;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :asdf-additions))

(defpackage #:net.smtp-client.system
  (:use #:common-lisp #:asdf-additions))

(in-package #:net.smtp-client.system)

(asdf:defsystem :net.smtp-client
  :description "SMTP client library."
  :author "Jan Idzikowski"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "LLGPL-2.1"
  :depends-on (:iolib-posix
               :iolib-utils-symbols
               :net.sockets
               :cl-base64)
  :default-component-class muffled-source-file
  :pathname (merge-pathnames (make-pathname :directory '(:relative "protocols" "smtp"))
                             *load-truename*)
  :serial t
  :components
  ((:file "pkgdcl")
   (:file "client-net")
   (:file "client-commands")
   (:file "client-authentication")
   (:file "attachments")
   (:file "smtp")))
