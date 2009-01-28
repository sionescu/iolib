;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(in-package :common-lisp-user)

(asdf:defsystem :iolib
  :description "I/O library."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :version "0.6.0"
  :licence "MIT"
  :depends-on (:iolib.base :io.multiplex :io.streams :net.sockets)
  :pathname (merge-pathnames (make-pathname :directory '(:relative "iolib"))
                             *load-truename*)
  :components ((:file "pkgdcl")))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :iolib))))
  (asdf:operate 'asdf:test-op :iolib-tests))

(defmethod asdf:operation-done-p ((o asdf:test-op) (c (eql (asdf:find-system :iolib))))
  nil)
