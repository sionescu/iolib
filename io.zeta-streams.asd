;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- ASDF system definition.
;;;

(in-package :common-lisp-user)

(asdf:defsystem :io.zeta-streams
  :description "Zeta streams."
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "MIT"
  :depends-on (:cffi :osicat :io.multiplex :alexandria)
  :pathname (merge-pathnames #p"io.streams/zeta/" *load-truename*)
  :components
  ((:file "pkgdcl")
   (:file "classes" :depends-on ("pkgdcl"))
   (:file "buffer" :depends-on ("pkgdcl" "classes"))))
