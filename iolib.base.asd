;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.base
  :description "Base IOlib package, used instead of CL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :version "0.5.4"
  :licence "MIT"
  :depends-on (:alexandria)
  :pathname (merge-pathnames (make-pathname :directory '(:relative "base"))
                             *load-truename*)
  :components ((:file "pkgdcl")
               (:file "return-star" :depends-on ("pkgdcl"))
               (:file "types" :depends-on ("pkgdcl"))
               (:file "conditions" :depends-on ("pkgdcl" "return-star"))
               (:file "defobsolete" :depends-on ("pkgdcl" "return-star"))
               (:file "reader" :depends-on ("pkgdcl" "return-star"))
               (:file "sequence" :depends-on ("pkgdcl" "return-star"))
               (:file "matching" :depends-on ("pkgdcl" "return-star"))
               (:file "time" :depends-on ("pkgdcl" "return-star"))
               (:file "split-sequence" :depends-on ("pkgdcl" "return-star" "sequence"))
               #+scl (:file "scl-gray-streams")
               (:file "gray-stream-mixin"
                      :depends-on ("pkgdcl" "return-star" #+scl "scl-gray-streams"))))
