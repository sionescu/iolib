;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- ASDF component classes
;;;

(in-package :iolib.base)

(defclass cl-source-file (asdf:cl-source-file) ())

(defmethod asdf:perform :around ((o asdf:compile-op) (c cl-source-file))
  (let (#+sbcl
        (sb-impl::*default-external-format* :utf-8))
    (call-next-method)))

(defmethod asdf:perform :around ((o asdf:load-source-op) (c cl-source-file))
  (let (#+sbcl
        (sb-impl::*default-external-format* :utf-8))
    (call-next-method)))
