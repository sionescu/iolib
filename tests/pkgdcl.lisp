;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; pkgdcl.lisp --- Package definition.
;;;

(in-package #:cl-user)

(defpackage #:iolib-tests
  (:use :iolib.base :iolib :5am :trivial-gray-streams)
  (:export #:*echo-address* #:*echo-port*))
