;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.pathnames
  :description "New pathnames."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :version #.(with-open-file (f (merge-pathnames "../version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :licence "MIT"
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:iolib.base :iolib.syscalls)
  :default-component-class :iolib-source-file
  :pathname "pathnames/"
  :components
  ((:file "pkgdcl")
   (:file "file-path")
   (:file "file-path-os" :pathname #+unix "file-path-unix"))
  :serial t)
