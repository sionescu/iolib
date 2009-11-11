;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- ASDF system definition.
;;;

(asdf:defsystem :iolib.zeta-streams
  :description "Zeta streams."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.syscalls :iolib.pathnames :cffi :bordeaux-threads)
  :pathname (merge-pathnames #p"streams/zeta/" *load-truename*)
  :components
  ((:file "pkgdcl")
   (:file "types" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl"))

   ;; Platform-specific files
   (:file "ffi-functions"
     :pathname #+unix "ffi-functions-unix"
     :depends-on ("pkgdcl" "conditions"))

   ;; Device interface definition
   (:file "device" :depends-on ("pkgdcl" "types"))

   ;; Low-level buffers
   (:file "iobuf" :depends-on ("pkgdcl" "types"))

   ;; Streams
   (:file "stream" :depends-on ("pkgdcl" "types" "conditions" "device" "iobuf"))

   ;; Devices
   (:file "file"
     :pathname #+unix "file-unix"
     :depends-on ("pkgdcl" "types" "conditions" "ffi-functions" "device" "stream"))))
