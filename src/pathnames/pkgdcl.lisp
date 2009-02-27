;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :common-lisp-user)

(defpackage :iolib.pathnames
  (:nicknames #:ipath)
  (:use :iolib.base)
  (:export
   ;; Classes
   #:file-path
   #:unix-path

   ;; Accessors
   #:file-path-host
   #:file-path-device
   #:file-path-directory
   #:file-path-name
   #:file-path-directory-delimiter
   #:file-path-alternative-delimiter
   #:file-path-execution-path-delimiter
   #:file-path-namestring

   ;; Constructors
   #:make-file-path
   #:parse-file-path
   #:file-path

   ;; Operations
   #:merge-file-paths
   #:enough-file-path
   #:concatenate-paths

   ;; Predicates
   #:file-path-p
   #:file-path-absolute-p
   #:file-path-relative-p))
