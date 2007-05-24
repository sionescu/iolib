;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :asdf-additions))

(defpackage #:iolib-utils-misc.system
  (:use #:common-lisp #:asdf-additions))

(in-package #:iolib-utils-misc.system)

(asdf:defsystem :iolib-utils-misc
  :description "Miscellaneous utils."
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "LLGPL-2.1"
  :depends-on (:iolib-utils-package
               :iolib-utils-symbols)
  :default-component-class muffled-source-file
  :pathname (merge-pathnames (make-pathname :directory '(:relative "utils"))
                             *load-truename*)
  :components
  ((:file "misc")))
