;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.conf
  :version (:read-file-form "../version.lisp-expr")
  :depends-on (:iolib/conf))
