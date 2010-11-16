;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Load all non-ASDF deps - usually implementation-specific REQUIREs
;;;

(in-package :iolib.conf)

(defun load-gray-streams ()
  #+allegro
  (unless (fboundp 'stream:stream-write-string)
    (require "streamc"))
  #+cmu
  (require :gray-streams)
  #+ecl
  (when (fboundp 'gray::redefine-cl-functions)
    (gray::redefine-cl-functions)))

(defun load-os-package ()
  #+allegro
  (require "osi"))
