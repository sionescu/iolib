;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- iolib.base test suite.
;;;

(in-package :iolib-tests)

(in-suite :iolib.base)

;;;; SPLIT-SEQUENCE

(test (split-sequence.1 :compile-at :definition-time)
  (is (equalp (split-sequence #\; "a;;b;c")
              (values '("a" "" "b" "c") 6))))

(test (split-sequence.2 :compile-at :definition-time)
  (is (equalp (split-sequence #\; "a;;b;c" :from-end t)
              (values '("a" "" "b" "c") 0))))

(test (split-sequence.3 :compile-at :definition-time)
  (is (equalp (split-sequence #\; "a;;b;c" :from-end t :count 1)
              (values '("c") 4))))

(test (split-sequence.4 :compile-at :definition-time)
  (is (equalp (split-sequence #\; "a;;b;c" :remove-empty-subseqs t)
              (values '("a" "b" "c") 6))))

(test (split-sequence.5 :compile-at :definition-time)
  (is (equalp (split-sequence-if (lambda (x) (member x '(#\a #\b))) "abracadabra")
              (values '("" "" "r" "c" "d" "" "r" "") 11))))

(test (split-sequence.6 :compile-at :definition-time)
  (is (equalp (split-sequence-if-not (lambda (x) (member x '(#\a #\b))) "abracadabra")
              (values '("ab" "a" "a" "ab" "a") 11))))

(test (split-sequence.7 :compile-at :definition-time)
  (is (equalp (split-sequence #\; ";oo;bar;ba;" :start 1 :end 9)
              (values '("oo" "bar" "b") 9))))
