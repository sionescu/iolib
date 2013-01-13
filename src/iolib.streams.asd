;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.streams
  :description "Gray streams."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version (:read-file-form "../version.lisp-expr")
  :licence "MIT"
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:iolib.base :iolib.multiplex :cffi)
  :default-component-class :iolib-source-file
  :pathname "streams/gray/"
  :components
  ((:file "pkgdcl")
   (:file "classes" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl"))
   (:file "buffer" :depends-on ("pkgdcl" "classes"))
   (:file "fd-mixin" :depends-on ("pkgdcl" "classes"))
   (:file "io-helpers"
     :depends-on ("pkgdcl" "classes" "conditions" "buffer" "fd-mixin"))
   (:file "gray-stream-methods"
     :depends-on ("pkgdcl" "classes" "conditions" "buffer" "fd-mixin"
                  "io-helpers"))))
