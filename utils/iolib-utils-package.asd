;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage #:iolib-utils-package.system
  (:use #:common-lisp #:asdf))

(in-package #:iolib-utils-package.system)

(defsystem :iolib-utils-package
  :components ((:file "defpackage")))
