;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :common-lisp-user)

(asdf:defsystem :iolib.pathnames
  :description "New pathnames."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.syscalls :cl-ppcre)
  :pathname (merge-pathnames #p"pathnames/" *load-truename*)
  :serial t
  :components
  ((:file "pkgdcl")
   (:file "file-path")
   (:file "file-path-os"
     :pathname #+unix "file-path-unix")))
