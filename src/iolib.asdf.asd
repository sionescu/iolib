;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.asdf
  :description "A few ASDF component classes."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version #.(with-open-file (f (merge-pathnames "../version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :licence "MIT"
  :depends-on (:alexandria)
  :pathname "base/"
  :components
  ((:file "asdf")))
