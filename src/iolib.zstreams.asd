;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :iolib.base))

(in-package :iolib.asdf)

(defsystem :iolib.zstreams
  :description "Zeta streams."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.syscalls :iolib.pathnames :cffi :bordeaux-threads)
  :default-component-class iolib-source-file
  :pathname #-asdf2 (merge-pathnames "streams/zeta/" *load-truename*)
            #+asdf2 "streams/zeta/"
  :components
  ((:file "pkgdcl")
   (:file "types" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl"))

   ;; Platform-specific files
   (:file "ffi-functions" :pathname #+unix "ffi-functions-unix"
     :depends-on ("pkgdcl" "conditions"))

   ;; Device interface definition
   (:file "device" :depends-on ("pkgdcl" "types"))

   ;; Low-level buffers
   (:file "iobuf" :depends-on ("pkgdcl" "types"))

   ;; Streams
   (:file "stream" :depends-on ("pkgdcl" "types" "conditions" "device" "iobuf"))

   ;; Devices
   (:file "file" :pathname #+unix "file-unix"
     :depends-on ("pkgdcl" "types" "conditions" "ffi-functions" "device" "stream"))))
