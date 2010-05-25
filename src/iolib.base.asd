;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-

(defsystem :iolib.base
  :description "Base IOlib package, used instead of CL."
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :depends-on (:alexandria)
  :pathname #-asdf2 (merge-pathnames "base/" *load-truename*)
            #+asdf2 "base/"
  :components
  ((:file "pkgdcl")
   (:file "return-star" :depends-on ("pkgdcl"))
   (:file "types" :depends-on ("pkgdcl" "return-star"))
   (:file "definitions" :depends-on ("pkgdcl" "return-star"))
   (:file "debug" :depends-on ("pkgdcl" "return-star"))
   (:file "conditions" :depends-on ("pkgdcl" "return-star"))
   (:file "defobsolete" :depends-on ("pkgdcl" "return-star"))
   (:file "reader" :depends-on ("pkgdcl" "return-star" "definitions" "conditions"))
   (:file "sequence" :depends-on ("pkgdcl" "return-star"))
   (:file "matching" :depends-on ("pkgdcl" "return-star"))
   (:file "time" :depends-on ("pkgdcl" "return-star"))
   (:file "split-sequence" :depends-on ("pkgdcl" "return-star" "sequence"))
   #+scl (:file "scl-gray-streams")
   (:file "gray-stream-mixin"
     :depends-on ("pkgdcl" "return-star" #+scl "scl-gray-streams"))
   (:file "asdf" :depends-on ("pkgdcl" "return-star"))))
