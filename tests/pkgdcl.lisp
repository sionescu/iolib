;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; pkgdcl.lisp --- Package definition.
;;;

(in-package #:cl-user)

(defpackage #:iolib-tests
  (:use :5am :iolib.base :iolib :iolib.pathnames)
  (:export #:*echo-address* #:*echo-port*))
