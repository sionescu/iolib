;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

(in-package :common-lisp-user)

(defpackage #:iolib-system
  (:use #:common-lisp))

(in-package #:iolib-system)

(asdf:defsystem :iolib
  :description "I/O library for SBCL."
  :author "Stelian Ionescu <sionescu@common-lisp.net>"
  :maintainer "Stelian Ionescu <sionescu@common-lisp.net>"
  :version "0.5.3"
  :licence "LLGPL-2.1"
  :depends-on (:iolib-posix
               :net.sockets
               :net.dns-client))
