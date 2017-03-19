;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem :iolib.tests
  :description "IOLib test suite."
  :author "Luis Oliveira <loliveira@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :version (:read-file-form "version.sexp")
  :defsystem-depends-on (:iolib.base)
  :depends-on (:fiveam :iolib :iolib/pathnames)
  :around-compile "iolib/asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "tests/"
  :components
  ((:file "pkgdcl")
   (:file "defsuites" :depends-on ("pkgdcl"))
   (:file "base" :depends-on ("pkgdcl" "defsuites"))
   (:file "file-paths-os" :depends-on ("pkgdcl" "defsuites")
     :pathname #+unix "file-paths-unix")
   (:file "events" :depends-on ("pkgdcl" "defsuites"))
   (:file "streams" :depends-on ("pkgdcl" "defsuites"))
   (:file "sockets" :depends-on ("pkgdcl" "defsuites")))
  :perform (test-op (o c) (symbol-call :5am :run! :iolib)))
