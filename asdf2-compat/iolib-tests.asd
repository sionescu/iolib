;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib-tests
  :version (:read-file-form "../version.lisp-expr")
  :depends-on (:iolib/tests))
