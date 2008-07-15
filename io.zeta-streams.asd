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
   (:file "conditions" :depends-on ("pkgdcl" "classes"))
   (:file "common" :depends-on ("pkgdcl" "classes" "conditions"))
   (:file "internal" :depends-on ("pkgdcl" "classes" "conditions" "common"))

   ;; Devices
   (:file "file" :depends-on ("pkgdcl" "classes" "conditions" "common" "internal"))

   ;; Buffers
   (:file "buffer" :depends-on ("pkgdcl" "classes" "conditions" "common"))))
