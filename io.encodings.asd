;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage #:io.encodings.system
  (:use #:common-lisp #:asdf))

(in-package #:io.encodings.system)

(defsystem :io.encodings
  :description "Charset encoding/decoding library."
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "LLGPL"
  :depends-on (:iolib-utils-symbols
               :iolib-utils-misc
               :cffi
               :iolib-posix)
  :pathname (merge-pathnames (make-pathname :directory '(:relative "io.encodings"))
                             *load-truename*)
  :components ((:file "defpackage")
               (:file "common" :depends-on ("defpackage"))
               (:file "iso-8859-tables" :depends-on ("defpackage"))
               (:file "external-format"
                      :depends-on ("defpackage" "common" "iso-8859-tables"))))
