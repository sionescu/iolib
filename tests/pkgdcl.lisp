;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; pkgdcl.lisp --- Package definition.
;;;

(in-package #:cl-user)

(defpackage #:iolib-tests
  (:use #:common-lisp :5am :net.sockets :io.streams
        :trivial-gray-streams :alexandria :io.multiplex)
  (:export #:*echo-address* #:*echo-port*))
