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
   #+unix    #:unix-path
   #+windows #:unc-path

   ;; Accessors
   #:file-path-host
   #:file-path-device
   #:file-path-directory
   #:file-path-file
   #:file-path-type
   #:file-path-namestring

   ;; Constructors
   #:file-path
   #:make-file-path
   #:parse-file-path

   ;; Named reader
   #:p

   ;; Operations
   #:merge-file-paths
   #:enough-file-path
   #:expand-user-directory

   ;; Predicates
   #:file-path-p
   #:file-path-absolute-p
   #:file-path-relative-p

   ;; Conditions
   #:invalid-file-path

   ;; Constants
   #:+directory-delimiter+
   #:+alternative-delimiter+
   #:+execution-path-delimiter+

   ;; Specials
   #:*default-file-path-defaults*
   #:*default-execution-path*))
