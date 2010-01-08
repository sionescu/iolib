;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(defsystem :iolib.trivial-sockets
  :description "Trivial-Sockets compatibility layer."
  :author "Dan Barlow <dan@telent.net>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.sockets)
  :pathname (merge-pathnames #p"sockets/" *load-truename*)
  :components
  ((:file "trivial-sockets")))
