;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (oos 'load-op :iolib.base))

(defsystem :iolib.os
  :description "OS interface."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.syscalls :iolib.pathnames)
  :pathname (merge-pathnames "os/" *load-truename*)
  :serial t
  :default-component-class iolib.base:cl-source-file
  :components
  ((:file "pkgdcl")
   (:file "os" :pathname #+unix "os-unix"
     :depends-on ("pkgdcl"))))
