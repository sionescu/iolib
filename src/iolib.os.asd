;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (oos 'load-op :iolib.base))

(defsystem :iolib.os
  :description "OS interface."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.syscalls :iolib.pathnames)
  :default-component-class iolib.base:cl-source-file
  :pathname #-asdf2 (merge-pathnames "os/" *load-truename*)
            #+asdf2 "os/"
  :components
  ((:file "pkgdcl")
   (:file "os" :pathname #+unix "os-unix"
     :depends-on ("pkgdcl")))
  :serial t)
