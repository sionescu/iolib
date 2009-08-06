;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :common-lisp-user)

(defpackage :iolib.os
  (:use :iolib.base :iolib.syscalls :iolib.pathnames :cffi)
  (:import-from :iolib.pathnames #:file-path-namestring/ustring)
  (:export

   ;; Evironment
   #:environment
   #:environment-variable
   #:makunbound-environment-variable

   ;; Directories
   #:current-directory
   #:with-current-directory
   ;; #:delete-directory
   ;; #:delete-directory-and-files
   ;; #:directory-exists-p
   ;; #:list-directory
   ;; #:mapdir
   ;; #:walk-directory
   ;; #:with-directory-iterator

   ;; Files
   #:resolve-file-path
   #:file-exists-p
   #:good-symlink-exists-p
   #:regular-file-exists-p
   #:file-kind

   ;; Symlinks
   #:read-link
   #:make-link

   ;; Permissions
   #:file-permissions

   ;; Temporary files
   ;; #:open-temporary-file
   ;; #:with-temporary-file

   ;; Password entries
   #:user-info

   ;; Time
   #:get-monotonic-time

   ;; Specials
   #:*temporary-directory*
   ))
