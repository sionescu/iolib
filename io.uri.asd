;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :io.uri
  :description "URI library."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "MIT"
  :depends-on (:iolib.base :babel :cl-ppcre)
  :pathname (merge-pathnames #p"io.uri/" *load-truename*)
  :components
  ((:file "pkgdcl")
   (:file "uri" :depends-on ("pkgdcl"))))
