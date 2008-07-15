;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Various types.
;;;

(in-package :io.zeta-streams)

;;;-----------------------------------------------------------------------------
;;; Types
;;;-----------------------------------------------------------------------------

(deftype ub8  () '(unsigned-byte 8))
(deftype ub16 () '(unsigned-byte 16))
(deftype ub32 () '(unsigned-byte 32))
(deftype sb8  () '(signed-byte 8))
(deftype sb16 () '(signed-byte 16))
(deftype sb32 () '(signed-byte 32))

(deftype ub8-sarray (&optional (size '*))
  `(simple-array ub8 (,size)))

(deftype ub8-vector () '(vector ub8))

(deftype ub16-sarray (&optional (size '*))
  `(simple-array ub16 (,size)))
