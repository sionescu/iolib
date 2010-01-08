;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(defsystem :iolib.os
  :description "OS interface."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.syscalls :iolib.pathnames)
  :pathname (merge-pathnames "os/" *load-truename*)
  :serial t
  :components
  ((:file "pkgdcl")
   (:file "os" :pathname #+unix "os-unix"
     :depends-on ("pkgdcl"))))
