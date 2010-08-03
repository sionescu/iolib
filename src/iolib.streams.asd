;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :iolib.base))

(in-package :iolib.asdf)

(defsystem :iolib.streams
  :description "Gray streams."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version #.(with-open-file (f (merge-pathnames "../version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :licence "MIT"
  :depends-on (:iolib.base :iolib.multiplex :cffi :trivial-garbage)
  :default-component-class iolib-source-file
  :pathname #-asdf2 (merge-pathnames "streams/gray/" *load-truename*)
            #+asdf2 "streams/gray/"
  :components
  ((:file "pkgdcl")
   (:file "classes" :depends-on ("pkgdcl"))
   (:file "conditions" :depends-on ("pkgdcl"))
   (:file "buffer" :depends-on ("pkgdcl" "classes"))
   (:file "fd-mixin" :depends-on ("pkgdcl" "classes"))
   (:file "io-helpers"
     :depends-on ("pkgdcl" "classes" "conditions" "buffer" "fd-mixin"))
   (:file "gray-stream-methods"
     :depends-on ("pkgdcl" "classes" "conditions" "buffer" "fd-mixin"
                  "io-helpers"))))
