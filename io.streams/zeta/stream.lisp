;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Streams.
;;;

(in-package :io.zeta-streams)

;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(defclass zeta-stream () ())

(defclass single-channel-zeta-stream (single-channel-buffer)
  ())

(defclass dual-channel-zeta-stream (dual-channel-buffer)
  ())


;;;-------------------------------------------------------------------------
;;; Generic Functions
;;;-------------------------------------------------------------------------



;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

