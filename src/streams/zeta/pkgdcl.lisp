;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :common-lisp-user)

(defpackage :iolib.zeta-streams
  (:use :iolib.base :iolib.syscalls :iolib.pathnames :cffi)
  (:import-from :iolib.pathnames #:file-path-namestring/ustring)
  (:export
   ))
