;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :iolib.base)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(in-package :iolib.asdf)

(defsystem :iolib.syscalls
  :description "Syscalls and foreign types."
  :maintainer "Stelian Ionescu <sionescu@cddr.org>"
  :version #.(with-open-file (f (merge-pathnames "../version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :licence "MIT"
  :depends-on (:trivial-features :cffi :cffi-grovel :iolib.base)
  :default-component-class iolib-source-file
  :pathname #-asdf2 (merge-pathnames "syscalls/" *load-truename*)
            #+asdf2 "syscalls/"
  :components
  ((:file "pkgdcl")
   #+unix
   (:file "syscall-path-strings" :pathname "unix-syscall-path-strings")
   ;; Platform-specific files
   (cffi-grovel:grovel-file "ffi-types" :pathname #+unix "ffi-types-unix")
   (:file "conditions")
   (:file "os-conditions" :pathname #+unix "os-conditions-unix")
   (:file "designators")
   (:file "early")
   (cffi-grovel:wrapper-file "ffi-wrappers" :pathname #+unix "ffi-wrappers-unix"
     :soname "libiolib-syscalls")
   (:file "ffi-functions" :pathname #+unix "ffi-functions-unix"))
  :serial t)
