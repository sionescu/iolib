;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- rods test suite.
;;;

(in-package :iolib-tests)

(in-suite :iolib.base.rods)


(test make-rod.1
  (is-true (typep (make-rod 1) '(rod 1))))

(test make-rod.2
  (is-true (= 0 (aref (make-rod 1) 0))))

(test make-rod.3
  (is-true (= 40 (aref (make-rod 1 :initial-element 40) 0))))

(test make-rod.error.1
  (signals type-error
    (make-rod -1 :initial-element 40)))

(test make-rod.error.2
  (signals type-error
    (make-rod 1 :initial-element -1)))

(test make-rod.error.3
  (signals type-error
    (make-rod 1 :initial-element rune-code-limit)))

(test make-rod.error.3
  (signals type-error
    (make-rod 1 :initial-element #\a)))
