;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (oos 'load-op :cffi-grovel)
  (oos 'load-op :iolib.base))

(defsystem :iolib.os
  :description "OS interface."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :depends-on (:cffi-grovel :iolib.base :iolib.syscalls
               :iolib.streams :iolib.pathnames)
  :default-component-class iolib.base:cl-source-file
  :pathname #-asdf2 (merge-pathnames "os/" *load-truename*)
            #+asdf2 "os/"
  :components
  ((:file "pkgdcl")
   (:file "os" :pathname #+unix "os-unix")
   (cffi-grovel:grovel-file "ffi-types" :pathname #+unix "ffi-types-unix")
   (:file "ffi-functions" :pathname #+unix "ffi-functions-unix")
   (:file "create-process" :pathname #+unix "create-process-unix"))
  :serial t)
