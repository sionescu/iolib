;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :iolib.grovel
  :description "The CFFI Groveller"
  :author "Dan Knapp <dankna@accela.net>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:iolib.asdf :iolib.base :iolib.conf
               :alexandria :split-sequence #+allegro (:require "osi") :cffi :uiop)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "src/grovel/"
  :components
  ((:file "package")
   (:static-file "grovel-common.h")
   (:file "grovel")
   (:file "asdf"))
  :serial t)
