;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Streams.
;;;

(in-package :io.zeta-streams)

;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(defclass zeta-stream ()
  ((external-format :reader external-format-of)))

(defclass single-channel-zeta-stream (single-channel-buffer zeta-stream)
  ())

(defclass dual-channel-zeta-stream (dual-channel-buffer zeta-stream)
  ())


;;;-------------------------------------------------------------------------
;;; Generic Functions
;;;-------------------------------------------------------------------------



;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

