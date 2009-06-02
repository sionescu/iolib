;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Types.
;;;

(in-package :iolib.base)

(deftype function-designator ()
  '(or symbol function))

(defun string-designator-of-length-one (thing)
  (let ((string
         (typecase thing
           (string thing)
           (symbol (symbol-name thing)))))
    (if (and string (= 1 (length string)))
        (char string 0)
        nil)))

(deftype character-designator ()
  '(or character (satisfies string-designator-of-length-one)))
