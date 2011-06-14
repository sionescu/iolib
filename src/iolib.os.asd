;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.os
  :description "OS interface."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version #.(with-open-file (f (merge-pathnames "../version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :licence "MIT"
  :defsystem-depends-on (:iolib.asdf :iolib.conf :iolib-grovel)
  :depends-on (:iolib.base :iolib-grovel :iolib.syscalls
               :iolib.streams :iolib.pathnames)
  :default-component-class :iolib-source-file
  :pathname "os/"
  :components
  ((:file "pkgdcl")
   (:file "os" :pathname #+unix "os-unix")
   (:iolib-grovel-file "ffi-types" :pathname #+unix "ffi-types-unix")
   (:file "ffi-functions" :pathname #+unix "ffi-functions-unix")
   (:file "create-process" :pathname #+unix "create-process-unix"))
  :serial t)
