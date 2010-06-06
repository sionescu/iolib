;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (oos 'load-op :iolib.base))

(defsystem :iolib.pathnames
  :description "New pathnames."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.syscalls)
  :default-component-class iolib.base:cl-source-file
  :pathname #-asdf2 (merge-pathnames "pathnames/" *load-truename*)
            #+asdf2 "pathnames/"
  :components
  ((:file "pkgdcl")
   (:file "file-path")
   (:file "file-path-os" :pathname #+unix "file-path-unix"))
  :serial t)
