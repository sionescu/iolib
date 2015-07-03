;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib-grovel
  :version (:read-file-form "../version.lisp-expr")
  :depends-on (:iolib/grovel))
