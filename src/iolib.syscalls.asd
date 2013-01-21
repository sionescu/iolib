;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :iolib.syscalls
  :description "Syscalls and foreign types."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version (:read-file-form "../version.lisp-expr")
  :licence "MIT"
  :defsystem-depends-on (:iolib.asdf :iolib.conf :iolib-grovel)
  :depends-on (:trivial-features :cffi :iolib.base :iolib-grovel)
  :around-compile "iolib.asdf:compile-wrapper"
  :encoding :utf-8
  :pathname "syscalls/"
  :components
  ((:file "pkgdcl")
   #+unix
   (:file "syscall-path-strings" :pathname "unix-syscall-path-strings")
   ;; Platform-specific files
   (:iolib-grovel-file "ffi-types" :pathname #+unix "ffi-types-unix")
   (:file "conditions")
   (:file "os-conditions" :pathname #+unix "os-conditions-unix")
   (:file "designators")
   (:file "early")
   (:file "ffi-functions" :pathname #+unix "ffi-functions-unix"))
  :serial t)
