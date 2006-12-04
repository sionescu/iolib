;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage #:net.dns-client.system
  (:use #:common-lisp #:asdf))

(in-package #:net.dns-client.system)

(defsystem :net.dns-client
  :description "DNS client library."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "GPL-2.1"
  :depends-on (:net.sockets
               :flexi-streams)
  :components
  ((:file "export")
   (:file "dns-constants")
   (:file "dynamic-buffer" :depends-on ("dns-constants"))
   (:file "dns-query" :depends-on ("dns-constants" "dynamic-buffer"))
   (:file "dns-response" :depends-on ("dns-constants" "dns-query"))
   (:file "dns-do-query" :depends-on ("dns-constants" "dns-query" "dns-response"))
   (:file "etc-files" :depends-on ("dynamic-buffer"))
   (:file "dns-lookup" :depends-on ("dns-do-query" "etc-files"))))
