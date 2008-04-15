;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; etc-files.lisp --- Common parsing routines for /etc namedb files.
;;;

(in-package :net.sockets)

(defun space-char-p (char)
  (declare (type character char))
  (or (char= char #\Space)
      (char= char #\Tab)))

(defun split-etc-tokens (line)
  (declare (type string line))
  (let ((comment-start (position #\# line)))
    (split-sequence-if #'space-char-p line
                       :remove-empty-subseqs t
                       :start 0 :end comment-start)))

(defmacro serialize-etc-file (file)
  `(#msplit-etc-tokens (scan-file ,file #'read-line)))
