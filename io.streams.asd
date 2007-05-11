;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :asdf-additions)
  #+cmu (require :gray-streams)
  #+allegro (require :streamc))

(defpackage #:io.streams.system
  (:use #:common-lisp #:asdf #:asdf-additions))

(in-package #:io.streams.system)

(defsystem :io.streams
  :description "Gray streams."
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "LLGPL-2.1"
  :depends-on (:iolib-posix
               :iolib-utils-misc
               :io.multiplex
               :io.encodings)
  :default-component-class muffled-source-file
  :pathname (merge-pathnames (make-pathname :directory '(:relative "io.streams"))
                             *load-truename*)
  :serial t
  :components
  ((:file "pkgdcl")
   (:file "classes")
   (:file "buffer")
   (:file "fd-mixin")
   (:file "gray-stream-methods")))
