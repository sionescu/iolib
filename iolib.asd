;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(asdf:defsystem :iolib
  :description "I/O library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :version "0.5.4"
  :licence "LLGPL-2.1"
  :depends-on (:net.sockets
               ;; comment out until it's brought back up to date
               ;; :net.dns-client
               ))
