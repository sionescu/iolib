;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib
  :description "I/O library."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version (:read-file-form "../version.lisp-expr")
  :licence "MIT"
  :defsystem-depends-on (:iolib.asdf :iolib.conf)
  :depends-on (:iolib.base :iolib.multiplex :iolib.streams :iolib.sockets)
  :default-component-class :iolib-source-file
  :pathname "iolib/"
  :components ((:file "pkgdcl")))

(defmethod perform ((o test-op) (c (eql (find-system :iolib))))
  (oos 'test-op :iolib-tests))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :iolib))))
  nil)
