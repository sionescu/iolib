;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(defsystem :iolib.streams
  :description "Gray streams."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.multiplex :cffi :trivial-garbage)
  :pathname (merge-pathnames #p"streams/gray/" *load-truename*)
  :components
  ((:file "pkgdcl")
   (:file "classes" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl"))
   (:file "buffer" :depends-on ("pkgdcl" "classes"))
   (:file "fd-mixin" :depends-on ("pkgdcl" "classes"))
   (:file "gray-stream-methods"
     :depends-on ("pkgdcl" "classes" "conditions" "buffer" "fd-mixin"))))
