;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.syscalls
  :version (:read-file-form "../version.lisp-expr")
  :depends-on (:iolib/syscalls))
