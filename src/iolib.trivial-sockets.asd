;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.trivial-sockets
  :description "Trivial-Sockets compatibility layer."
  :author "Dan Barlow <dan@telent.net>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version #.(with-open-file (f (merge-pathnames "../version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :licence "MIT"
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:iolib.base :iolib.sockets)
  :default-component-class :iolib-source-file
  :pathname "sockets/"
  :components
  ((:file "trivial-sockets")))
