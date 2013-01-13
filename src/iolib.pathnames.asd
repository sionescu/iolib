;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.pathnames
  :description "New pathnames."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :version (:read-file-form "../version.lisp-expr")
  :licence "MIT"
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:iolib.base :iolib.syscalls)
  :default-component-class :iolib-source-file
  :pathname "pathnames/"
  :components
  ((:file "pkgdcl")
   (:file "file-path")
   (:file "file-path-os" :pathname #+unix "file-path-unix"))
  :serial t)
