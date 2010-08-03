;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :iolib.base))

(in-package :iolib.asdf)

(defsystem :iolib
  :description "I/O library."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version #.(with-open-file (f (merge-pathnames "../version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :licence "MIT"
  :depends-on (:iolib.base :iolib.multiplex :iolib.streams :iolib.sockets)
  :default-component-class iolib-source-file
  :pathname #-asdf2 (merge-pathnames "iolib/" *load-truename*)
            #+asdf2 "iolib/"
  :components ((:file "pkgdcl")))

(defmethod perform ((o test-op) (c (eql (find-system :iolib))))
  (oos 'test-op :iolib-tests))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :iolib))))
  nil)
