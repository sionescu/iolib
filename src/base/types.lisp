;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Types.
;;;

(in-package :iolib.base)

(deftype function-designator ()
  '(or symbol function))

(defun symbol-with-name-of-length-one (thing)
  (if (and (symbolp thing)
           (= 1 (length (symbol-name thing))))
      (char (symbol-name thing) 0)
      nil))

(deftype character-designator ()
  '(or character (string 1) (satisfies symbol-with-name-of-length-one)))
