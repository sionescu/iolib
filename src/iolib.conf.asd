;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.conf
  :description "Compile-time configuration for IOLib."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :version (:read-file-form "../version.lisp-expr")
  :licence "MIT"
  :defsystem-depends-on (:iolib.asdf)
  :around-compile "iolib.asdf:compile-wrapper"
  :pathname "conf/"
  :components
  ((:file "pkgdcl")
   (:file "requires" :depends-on ("pkgdcl"))))
