;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.conf
  :description "Compile-time configuration for IOLib."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :version #.(with-open-file (f (merge-pathnames "../version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :licence "MIT"
  :defsystem-depends-on (:iolib.asdf)
  :default-component-class :iolib-source-file
  :pathname "conf/"
  :components
  ((:file "pkgdcl")
   (:file "requires" :depends-on ("pkgdcl"))))
