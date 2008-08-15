;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.base
  :description "Base IOlib package, used instead of CL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :version "0.5.4"
  :licence "MIT"
  :depends-on (:alexandria)
  :pathname (merge-pathnames (make-pathname :directory '(:relative "base"))
                             *load-truename*)
  :components ((:file "pkgdcl")
               (:file "return-star" :depends-on ("pkgdcl"))))
