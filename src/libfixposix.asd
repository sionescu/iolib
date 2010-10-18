;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :iolib-grovel)
  (asdf:oos 'asdf:load-op :iolib.base))

(in-package :iolib.asdf)

(defsystem :libfixposix
  :description "Raw OS interface through LibFixPOSIX."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version #.(with-open-file (f (merge-pathnames "../version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :licence "Boost-1.0"
  :depends-on (:cffi :iolib-grovel :iolib.base)
  :default-component-class iolib-source-file
  :pathname #-asdf2 (merge-pathnames "libfixposix/" *load-truename*)
            #+asdf2 "libfixposix/"
  :components
  ((:file "pkgdcl")
   (:file "constants" :depends-on ("pkgdcl"))
   (iolib-grovel:grovel-file "ffi-types" :depends-on ("pkgdcl"))
   (:file "ffi-functions" :depends-on ("pkgdcl" "ffi-types"))))
