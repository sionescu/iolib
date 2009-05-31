;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- rods test suite.
;;;

(in-package :iolib-tests)

(in-suite :iolib.base.rods)


(test make-rod.1
  (is-true (typep (make-rod 1) '(rod 1))))

(test make-rod.2
  (is-true (eql 0 (aref (make-rod 1) 0))))

(test make-rod.3
  (is-true (eql 40 (aref (make-rod 1 :initial-element 40) 0))))

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
  (is (equalp #(97) (string-rod "a"))))

(test string-rod.3
  (is (equalp #(97) (string-rod #\a))))

(test string-rod.4
  (is (equalp #(65) (string-rod 'a))))


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


(test rodp.1
  (is-true (rodp (make-rod 1))))

(test rodp.error.1
  (is-false (rodp "string")))


(test rod=.1
  (is-true (rod= "" "")))

(test rod=.2
  (is-true (rod= "a" "a")))

(test rod=.3
  (is-true (rod= "bac" "acb" :start1 1 :end2 2)))

(test rod=.4
  (is-false (rod= "a" "b")))

(test rod=.5
  (is-false (rod= "bac" "")))

(test rod=.6
  (is-false (rod= "bac" "bac" :end1 0)))


(test rod-equal.1
  (is-true (rod-equal "" "")))

(test rod-equal.2
  (is-true (rod-equal "a" "a")))

(test rod-equal.3
  (is-true (rod-equal "a" "A")))

(test rod-equal.4
  (is-true (rod-equal "bac" "acb" :start1 1 :end2 2)))

(test rod-equal.5
  (is-true (rod-equal "bac" "ACB" :start1 1 :end2 2)))

(test rod-equal.6
  (is-false (rod-equal "a" "b")))

(test rod-equal.7
  (is-false (rod-equal "bac" "")))

(test rod-equal.8
  (is-false (rod-equal "bac" "bac" :end1 0)))


(test rod/=.1
  (is-false (rod/= "" "")))

(test rod/=.2
  (is (eql 0 (rod/= "" "a"))))

(test rod/=.3
  (is (eql 0 (rod/= "a" "b"))))

(test rod/=.4
  (is-false (rod/= "bac" "acb" :start1 1 :end2 2)))

(test rod/=.5
  (is (eql 1 (rod/= "abc" "acb"))))


(test rod-not-equal.1
  (is-false (rod-not-equal "" "")))

(test rod-not-equal.2
  (is (eql 0 (rod-not-equal "" "a"))))

(test rod-not-equal.3
  (is (eql 0 (rod-not-equal "a" "b"))))

(test rod-not-equal.4
  (is-false (rod-not-equal "a" "A")))

(test rod-not-equal.5
  (is-false (rod-not-equal "bac" "acb" :start1 1 :end2 2)))

(test rod-not-equal.6
  (is-false (rod-not-equal "bac" "ACB" :start1 1 :end2 2)))

(test rod-not-equal.7
  (is (eql 1 (rod-not-equal "abc" "acb"))))

(test rod-not-equal.8
  (is (eql 1 (rod-not-equal "abc" "ACB"))))


(test rod<.1
  (is-false (rod< "" "")))

(test rod<.2
  (is (eql 0 (rod< "" "a"))))

(test rod<.3
  (is-false (rod< "a" "")))

(test rod<.4
  (is (eql 0 (rod< "a" "b"))))

(test rod<.5
  (is-false (rod< "b" "a")))

(test rod<.6
  (is-false (rod< "bac" "acb" :start1 1 :end2 2)))

(test rod<.7
  (is-false (rod< "acb" "bac" :start1 1 :end2 2)))

(test rod<.8
  (is (eql 1 (rod< "abc" "acb"))))

(test rod<.9
  (is-false (rod< "acb" "abc")))

(test rod<.10
  (is-false (rod< "a" "a")))

(test rod<.11
  (is-false (rod< "a" "A")))

(test rod<.12
  (is (eql 0 (rod< "A" "a"))))


(test rod-lessp.1
  (is-false (rod-lessp "" "")))

(test rod-lessp.2
  (is (eql 0 (rod-lessp "" "a"))))

(test rod-lessp.3
  (is-false (rod-lessp "a" "")))

(test rod-lessp.4
  (is (eql 0 (rod-lessp "a" "b"))))

(test rod-lessp.5
  (is-false (rod-lessp "b" "a")))

(test rod-lessp.6
  (is-false (rod-lessp "bac" "acb" :start1 1 :end2 2)))

(test rod-lessp.7
  (is-false (rod-lessp "acb" "bac" :start1 1 :end2 2)))

(test rod-lessp.8
  (is (eql 1 (rod-lessp "abc" "acb"))))

(test rod-lessp.9
  (is-false (rod-lessp "acb" "abc")))

(test rod-lessp.10
  (is-false (rod-lessp "a" "a")))

(test rod-lessp.11
  (is-false (rod-lessp "a" "A")))

(test rod-lessp.12
  (is-false (rod-lessp "A" "a")))


(test rod>.1
  (is-false (rod> "" "")))

(test rod>.2
  (is-false (rod> "" "a")))

(test rod>.3
  (is (eql 0 (rod> "a" ""))))

(test rod>.4
  (is-false (rod> "a" "b")))

(test rod>.5
  (is (eql 0 (rod> "b" "a"))))

(test rod>.6
  (is-false (rod> "bac" "acb" :start1 1 :end2 2)))

(test rod>.7
  (is (eql 1 (rod> "acb" "bac" :start1 1 :end2 2))))

(test rod>.8
  (is-false (rod> "abc" "acb")))

(test rod>.9
  (is (eql 1 (rod> "acb" "abc"))))

(test rod>.10
  (is-false (rod> "a" "a")))

(test rod>.11
  (is (eql 0 (rod> "a" "A"))))

(test rod>.12
  (is-false (rod> "A" "a")))


(test rod-greaterp.1
  (is-false (rod-greaterp "" "")))

(test rod-greaterp.2
  (is-false (rod-greaterp "" "a")))

(test rod-greaterp.3
  (is (eql 0 (rod-greaterp "a" ""))))

(test rod-greaterp.4
  (is-false (rod-greaterp "a" "b")))

(test rod-greaterp.5
  (is (eql 0 (rod-greaterp "b" "a"))))

(test rod-greaterp.6
  (is-false (rod-greaterp "bac" "acb" :start1 1 :end2 2)))

(test rod-greaterp.7
  (is (eql 1 (rod-greaterp "acb" "bac" :start1 1 :end2 2))))

(test rod-greaterp.8
  (is-false (rod-greaterp "abc" "acb")))

(test rod-greaterp.9
  (is (eql 1 (rod-greaterp "acb" "abc"))))

(test rod-greaterp.10
  (is-false (rod-greaterp "a" "a")))

(test rod-greaterp.11
  (is-false (rod-greaterp "a" "A")))

(test rod-greaterp.12
  (is-false (rod-greaterp "A" "a")))


(test rod<=.1
  (is (eql 0 (rod<= "" ""))))

(test rod<=.2
  (is (eql 0 (rod<= "" "a"))))

(test rod<=.3
  (is-false (rod<= "a" "")))

(test rod<=.4
  (is (eql 0 (rod<= "a" "b"))))

(test rod<=.5
  (is-false (rod<= "b" "a")))

(test rod<=.6
  (is (eql 3 (rod<= "bac" "acb" :start1 1 :end2 2))))

(test rod<=.7
  (is-false (rod<= "acb" "bac" :start1 1 :end2 2)))

(test rod<=.8
  (is (eql 1 (rod<= "abc" "acb"))))

(test rod<=.9
  (is-false (rod<= "acb" "abc")))

(test rod<=.10
  (is (eql 1 (rod<= "a" "a"))))

(test rod<=.11
  (is-false (rod<= "a" "A")))

(test rod<=.12
  (is (eql 0 (rod<= "A" "a"))))


(test rod-not-greaterp.1
  (is (eql 0 (rod-not-greaterp "" ""))))

(test rod-not-greaterp.2
  (is (eql 0 (rod-not-greaterp "" "a"))))

(test rod-not-greaterp.3
  (is-false (rod-not-greaterp "a" "")))

(test rod-not-greaterp.4
  (is (eql 0 (rod-not-greaterp "a" "b"))))

(test rod-not-greaterp.5
  (is-false (rod-not-greaterp "b" "a")))

(test rod-not-greaterp.6
  (is (eql 3 (rod-not-greaterp "bac" "acb" :start1 1 :end2 2))))

(test rod-not-greaterp.7
  (is-false (rod-not-greaterp "acb" "bac" :start1 1 :end2 2)))

(test rod-not-greaterp.8
  (is (eql 1 (rod-not-greaterp "abc" "acb"))))

(test rod-not-greaterp.9
  (is-false (rod-not-greaterp "acb" "abc")))

(test rod-not-greaterp.10
  (is (eql 1 (rod-not-greaterp "a" "a"))))

(test rod-not-greaterp.11
  (is (eql 1 (rod-not-greaterp "a" "A"))))

(test rod-not-greaterp.12
  (is (eql 1 (rod-not-greaterp "A" "a"))))


(test rod>=.1
  (is (eql 0 (rod>= "" ""))))

(test rod>=.2
  (is-false (rod>= "" "a")))

(test rod>=.3
  (is (eql 0 (rod>= "a" ""))))

(test rod>=.4
  (is-false (rod>= "a" "b")))

(test rod>=.5
  (is (eql 0 (rod>= "b" "a"))))

(test rod>=.6
  (is (eql 3 (rod>= "bac" "acb" :start1 1 :end2 2))))

(test rod>=.7
  (is (eql 1 (rod>= "acb" "bac" :start1 1 :end2 2))))

(test rod>=.8
  (is-false (rod>= "abc" "acb")))

(test rod>=.9
  (is (eql 1 (rod>= "acb" "abc"))))

(test rod>=.10
  (is (eql 1 (rod>= "a" "a"))))

(test rod>=.11
  (is (eql 0 (rod>= "a" "A"))))

(test rod>=.12
  (is-false (rod>= "A" "a")))


(test rod-not-lessp.1
  (is (eql 0 (rod-not-lessp "" ""))))

(test rod-not-lessp.2
  (is-false (rod-not-lessp "" "a")))

(test rod-not-lessp.3
  (is (eql 0 (rod-not-lessp "a" ""))))

(test rod-not-lessp.4
  (is-false (rod-not-lessp "a" "b")))

(test rod-not-lessp.5
  (is (eql 0 (rod-not-lessp "b" "a"))))

(test rod-not-lessp.6
  (is (eql 3 (rod-not-lessp "bac" "acb" :start1 1 :end2 2))))

(test rod-not-lessp.7
  (is (eql 1 (rod-not-lessp "acb" "bac" :start1 1 :end2 2))))

(test rod-not-lessp.8
  (is-false (rod-not-lessp "abc" "acb")))

(test rod-not-lessp.9
  (is (eql 1 (rod-not-lessp "acb" "abc"))))

(test rod-not-lessp.10
  (is (eql 1 (rod-not-lessp "a" "a"))))

(test rod-not-lessp.11
  (is (eql 1 (rod-not-lessp "a" "A"))))

(test rod-not-lessp.12
  (is (eql 1 (rod-not-lessp "A" "a"))))


(test rod-upcase.1
  (is (rod= "AHA" (rod-upcase "aha"))))

(test rod-upcase.2
  (is (rod= "" (rod-upcase ""))))

(test rod-upcase.3
  (is-false (let ((rod (rod "AHA")))
              (eql rod (rod-upcase rod)))))

(test rod-upcase.error.1
  (signals type-error
    (rod-upcase 5)))


(test nrod-upcase.1
  (is (rod= "AHA" (nrod-upcase (rod "aha")))))

(test nrod-upcase.2
  (is (rod= "" (nrod-upcase (rod "")))))

(test nrod-upcase.3
  (is-true (let ((rod (rod "AHA")))
             (eql rod (nrod-upcase rod)))))

(test nrod-upcase.error.1
  (signals type-error
    (nrod-upcase 5)))


(test rod-downcase.1
  (is (rod= "aha" (rod-downcase "AHA"))))

(test rod-downcase.2
  (is (rod= "" (rod-downcase ""))))

(test rod-downcase.3
  (is-false (let ((rod (rod "aha")))
              (eql rod (rod-downcase rod)))))

(test rod-downcase.error.1
  (signals type-error
    (rod-downcase 5)))


(test nrod-downcase.1
  (is (rod= "aha" (nrod-downcase (rod "AHA")))))

(test nrod-downcase.2
  (is (rod= "" (nrod-downcase (rod "")))))

(test nrod-downcase.3
  (is-true (let ((rod (rod "aha")))
             (eql rod (nrod-downcase rod)))))

(test nrod-downcase.error.1
  (signals type-error
    (nrod-downcase 5)))


(test rod-capitalize.1
  (is (rod= "Hak Mak" (rod-capitalize "hak mak"))))

(test rod-capitalize.2
  (is (rod= "" (rod-capitalize ""))))

(test rod-capitalize.3
  (is-false (let ((rod (rod "Hak Mak")))
              (eql rod (rod-capitalize rod)))))

(test rod-capitalize.error.1
  (signals type-error
    (rod-capitalize 5)))


(test nrod-capitalize.1
  (is (rod= "Hak Mak" (nrod-capitalize (rod "hak mak")))))

(test nrod-capitalize.2
  (is (rod= "" (nrod-capitalize (rod "")))))

(test nrod-capitalize.3
  (is-true (let ((rod (rod "Hak Mak")))
             (eql rod (nrod-capitalize rod)))))

(test nrod-capitalize.error.1
  (signals type-error
    (nrod-capitalize 5)))


(test rod-trim.1
  (is (rod= "aha" (rod-trim "kekahakek" "ke"))))

(test rod-trim.2
  (is (rod= "aha" (rod-trim "kekahakek" '(#\k #\e)))))

(test rod-trim.3
  (is-false (let ((rod (rod "aha")))
              (eql rod (rod-trim rod "z")))))

(test rod-trim.error.1
  (signals type-error
    (rod-trim "kekahakek" 5)))

(test rod-trim.error.2
  (signals type-error
    (rod-trim (make-hash-table) '(#\k #\e))))


(test rod-left-trim.1
  (is (rod= "ahakek" (rod-left-trim "kekahakek" "ke"))))

(test rod-left-trim.2
  (is (rod= "ahakek" (rod-left-trim "kekahakek" '(#\k #\e)))))

(test rod-left-trim.3
  (is-false (let ((rod (rod "aha")))
              (eql rod (rod-left-trim rod "z")))))

(test rod-left-trim.error.1
  (signals type-error
    (rod-left-trim "kekahakek" 5)))

(test rod-left-trim.error.2
  (signals type-error
    (rod-left-trim (make-hash-table) '(#\k #\e))))


(test rod-right-trim.1
  (is (rod= "kekaha" (rod-right-trim "kekahakek" "ke"))))

(test rod-right-trim.2
  (is (rod= "kekaha" (rod-right-trim "kekahakek" '(#\k #\e)))))

(test rod-right-trim.3
  (is-false (let ((rod (rod "aha")))
              (eql rod (rod-right-trim rod "z")))))

(test rod-right-trim.error.1
  (signals type-error
    (rod-right-trim "kekahakek" 5)))

(test rod-right-trim.error.2
  (signals type-error
    (rod-right-trim (make-hash-table) '(#\k #\e))))
