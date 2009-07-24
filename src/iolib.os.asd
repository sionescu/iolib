;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :common-lisp-user)

(asdf:defsystem :iolib.os
  :description "OS interface."
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.syscalls :iolib.pathnames)
  :pathname (merge-pathnames "os/" *load-truename*)
  :serial t
  :components
  ((:file "pkgdcl")
   (:file "os" :depends-on ("pkgdcl")
     :pathname #+unix "os-unix")))
