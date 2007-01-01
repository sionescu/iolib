;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage #:iolib-utils.system
  (:use #:common-lisp #:asdf))

(in-package #:iolib-utils.system)

(defsystem :iolib-utils
  :description "Metapackage that depends on all IOLIB-UTILS-* packages."
  :depends-on (:iolib-utils-symbols
               :iolib-utils-misc))
