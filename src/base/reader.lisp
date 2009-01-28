;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Reader utils
;;;

(in-package :iolib.base)

;; Reader macros

(defgeneric enable-reader-macro* (name))

(defgeneric disable-reader-macro* (name))

(defmacro enable-reader-macro (name)
  `(eval-when (:compile-toplevel)
     (enable-reader-macro* ,name)))

(defmacro disable-reader-macro (name)
  `(eval-when (:compile-toplevel)
     (disable-reader-macro* ,name)))

(defun save-old-readtable (symbol readtable)
  (setf (getf (symbol-plist symbol) 'old-readtable) readtable))

(defun get-old-readtable (symbol)
  (getf (symbol-plist symbol) 'old-readtable))

(defmethod enable-reader-macro* :before ((name symbol))
  (save-old-readtable name *readtable*)
  (setf *readtable* (copy-readtable)))

(defmethod disable-reader-macro* ((name symbol))
  (assert (readtablep (get-old-readtable name)))
  (setf *readtable* (get-old-readtable name))
  (save-old-readtable name nil))

(defmacro define-syntax (name &body body)
  `(defmethod enable-reader-macro* ((name (eql ',name)))
     ,@body))
