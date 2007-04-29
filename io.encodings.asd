;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :asdf-additions))

(defpackage #:io.encodings.system
  (:use #:common-lisp #:asdf))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (<= char-code-limit 256)
    (pushnew :ucs-chars *features*)))

(in-package #:io.encodings.system)

(defsystem :io.encodings
  :description "Charset encoding/decoding library."
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "LLGPL"
  :depends-on (:iolib-utils-symbols
               :iolib-utils-misc
               :cffi
               :iolib-posix)
  :default-component-class muffled-source-file
  :pathname (merge-pathnames (make-pathname :directory '(:relative "io.encodings"))
                             *load-truename*)
  :components ((:file "defpackage")
               (:file "common" :depends-on ("defpackage"))
               (:file "iso-8859-tables" :depends-on ("defpackage"))
               (:file "external-format"
                      :depends-on ("defpackage" "common" "iso-8859-tables"))))
