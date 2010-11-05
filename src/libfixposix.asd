;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :libfixposix
  :description "Raw OS interface through LibFixPOSIX."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version #.(with-open-file (f (merge-pathnames "../version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :licence "Boost-1.0"
  :defsystem-depends-on (:iolib.asdf :iolib-grovel)
  :depends-on (:iolib.base :cffi :iolib-grovel)
  :default-component-class :iolib-source-file
  :pathname "libfixposix/"
  :components
  ((:file "pkgdcl")
   (:file "constants" :depends-on ("pkgdcl"))
   (:iolib-grovel-file "ffi-types" :depends-on ("pkgdcl"))
   (:file "ffi-functions" :depends-on ("pkgdcl" "ffi-types"))))
