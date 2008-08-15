;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :common-lisp-user)

(asdf:defsystem :io.streams
  :description "Gray streams."
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "MIT"
  :depends-on (:iolib.base :cffi :osicat :io.multiplex
               :trivial-gray-streams :trivial-garbage)
  :pathname (merge-pathnames #p"io.streams/gray/" *load-truename*)
  :components
  ((:file "pkgdcl")
   (:file "classes" :depends-on ("pkgdcl"))
   (:file "buffer" :depends-on ("pkgdcl" "classes"))
   (:file "fd-mixin" :depends-on ("pkgdcl" "classes"))
   (:file "gray-stream-methods" :depends-on ("pkgdcl" "classes" "buffer" "fd-mixin"))))
