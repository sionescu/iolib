;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- ASDF component classes
;;;

(defpackage :iolib.asdf
  (:nicknames :iolib/asdf)
  (:use :common-lisp)
  (:export #:compile-wrapper)
  #+sb-package-locks
  (:lock t))
(in-package :iolib.asdf)

(defun compile-wrapper (continuation)
  (let (;; Compilation fails because of CFFI types that
        ;; can't be printed readably, so bind to NIL
        (*print-readably* nil)
        (*readtable* (copy-readtable))
        (asdf/lisp-build:*uninteresting-compiler-conditions*
          '(#+sbcl sb-int:package-at-variance))
        (asdf/lisp-build:*uninteresting-loader-conditions*
          '(#+sbcl sb-int:package-at-variance)))
    (funcall continuation)))
