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


(test string-rod.1
  (is-true (typep (string-rod "a") '(rod 1))))

(test string-rod.2
  (is (equalp (string-rod "a")
              #(97))))

(test string-rod.3
  (is (equalp (string-rod #\a)
              #(97))))

(test string-rod.4
  (is (equalp (string-rod 'a)
              #(65))))


(test rod.1
  (is (equalp #(97) (rod (make-rod 1 :initial-element 97)))))

(test rod.2
  (is (equalp #(97) (rod 97))))

(test rod.3
  (is (equalp #(97) (rod #(97)))))

(test rod.4
  (is (equalp #(97) (rod "a"))))

(test rod.5
  (is (equalp #(97) (rod #\a))))

(test rod.6
  (is (equalp #(65) (rod 'a))))

(test rod.7
  (is-true
   (let ((rod (make-rod 1 :initial-element 100)))
     (eq rod (rod rod :new nil)))))

(test rod.8
  (is-false
   (let ((rod (make-rod 1 :initial-element 100)))
     (eq rod (rod rod :new t)))))

(test rod.error.1
  (signals type-error
    (rod (make-hash-table))))
