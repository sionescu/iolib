;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (oos 'load-op :iolib.base))

(defsystem :iolib
  :description "I/O library."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version "0.6.0-dev"
  :licence "MIT"
  :depends-on (:iolib.base :iolib.multiplex :iolib.streams :iolib.sockets)
  :default-component-class iolib.base:cl-source-file
  :pathname #-asdf2 (merge-pathnames "iolib/" *load-truename*)
            #+asdf2 "iolib/"
  :components ((:file "pkgdcl")))

(defmethod perform ((o test-op) (c (eql (find-system :iolib))))
  (oos 'test-op :iolib-tests))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :iolib))))
  nil)
