;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (oos 'load-op :iolib.base))

(defsystem :iolib.streams
  :description "Gray streams."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.multiplex :cffi :trivial-garbage)
  :pathname #-asdf2 (merge-pathnames "streams/gray/" *load-truename*)
            #+asdf2 "streams/gray/"
  :default-component-class iolib.base:cl-source-file
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
