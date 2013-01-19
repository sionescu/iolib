;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.asdf
  :description "A few ASDF component classes."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version (:read-file-form "../version.lisp-expr")
  :licence "MIT"
  :depends-on (:alexandria)
  :pathname "base/"
  :encoding :utf-8
  :components
  ((:file "asdf")))
