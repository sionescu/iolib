;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(defsystem :iolib.pathnames
  :description "New pathnames."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.syscalls)
  :pathname (merge-pathnames #p"pathnames/" *load-truename*)
  :serial t
  :components
  ((:file "pkgdcl")
   (:file "file-path")
   (:file "file-path-os" :pathname #+unix "file-path-unix")))
