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
   (:file "types" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl" "types"))
   (:file "device" :depends-on ("pkgdcl" "types" "conditions"))

   ;; Devices
   (:file "file" :depends-on ("pkgdcl" "types" "conditions" "device"))

   ;; Buffers
   (:file "iobuf" :depends-on ("pkgdcl" "types" "conditions" "device"))
   (:file "buffer" :depends-on ("pkgdcl" "types" "conditions" "device" "iobuf"))))
