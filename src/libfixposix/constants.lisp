;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Misc. constants
;;;

(in-package :libfixposix)

;;;-------------------------------------------------------------------------
;;; Sizes of Standard Types
;;;-------------------------------------------------------------------------

(defconstant size-of-char (foreign-type-size :char))
(defconstant size-of-int (foreign-type-size :int))
(defconstant size-of-long (foreign-type-size :long))
(defconstant size-of-long-long (foreign-type-size :long-long))
(defconstant size-of-pointer (foreign-type-size :pointer))
(defconstant size-of-short (foreign-type-size :short))
