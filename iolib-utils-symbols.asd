;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op :asdf-additions))

(defpackage #:iolib-utils-symbols.system
  (:use #:common-lisp #:asdf #:asdf-additions))

(in-package #:iolib-utils-symbols.system)

(defsystem :iolib-utils-symbols
  :description "Symbol manipulation library."
  :author "Kevin M. Rosenberg"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :licence "LLGPL-2.1"
  :depends-on (:iolib-utils-package)
  :default-component-class muffled-source-file
  :pathname (merge-pathnames (make-pathname :directory '(:relative "utils"))
                             *load-truename*)
  :components
  ((:file "symbols")))
