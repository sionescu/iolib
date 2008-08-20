;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- iolib.base test suite.
;;;

(in-package :iolib-tests)

(in-suite* :iolib.base :in :iolib)

;;;; SPLIT-SEQUENCE

(test split-sequence.1
  (is (equalp (split-sequence #\; "a;;b;c")
              (values '("a" "" "b" "c") 6))))

(test split-sequence.2
  (is (equalp (split-sequence #\; "a;;b;c" :from-end t)
              (values '("a" "" "b" "c") 0))))

(test split-sequence.3
  (is (equalp (split-sequence #\; "a;;b;c" :from-end t :count 1)
              (values '("c") 4))))

(test split-sequence.4
  (is (equalp (split-sequence #\; "a;;b;c" :remove-empty-subseqs t)
              (values '("a" "b" "c") 6))))

(test split-sequence.5
  (is (equalp (split-sequence-if (lambda (x) (member x '(#\a #\b))) "abracadabra")
              (values '("" "" "r" "c" "d" "" "r" "") 11))))

(test split-sequence.6
  (is (equalp (split-sequence-if-not (lambda (x) (member x '(#\a #\b))) "abracadabra")
              (values '("ab" "a" "a" "ab" "a") 11))))

(test split-sequence.7
  (is (equalp (split-sequence #\; ";oo;bar;ba;" :start 1 :end 9)
              (values '("oo" "bar" "b") 9))))
