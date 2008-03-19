;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :common-lisp-user)

(asdf:defsystem :net.smtp-client
  :description "SMTP client library."
  :author "Jan Idzikowski"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "LLGPL-2.1"
  :depends-on (:alexandria :osicat :net.sockets :cl-base64)
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
