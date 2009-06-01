;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- ustrings test suite.
;;;

(in-package :iolib-tests)

(in-suite :iolib.base.ustrings)


(test make-ustring.1
  (is-true (typep (make-ustring 1) '(ustring 1))))

(test make-ustring.2
  (is-true (eql 0 (aref (make-ustring 1) 0))))

(test make-ustring.3
  (is-true (eql 40 (aref (make-ustring 1 :initial-element 40) 0))))

(test make-ustring.error.1
  (signals type-error
    (make-ustring -1 :initial-element 40)))

(test make-ustring.error.2
  (signals type-error
    (make-ustring 1 :initial-element -1)))

(test make-ustring.error.3
  (signals type-error
    (make-ustring 1 :initial-element uchar-code-limit)))

(test make-ustring.error.3
  (signals type-error
    (make-ustring 1 :initial-element #\a)))


(test string-to-ustring.1
  (is-true (typep (string-to-ustring "a") '(ustring 1))))

(test string-to-ustring.2
  (is (equalp #(97) (string-to-ustring "a"))))

(test string-to-ustring.3
  (is (equalp #(97) (string-to-ustring #\a))))

(test string-to-ustring.4
  (is (equalp #(65) (string-to-ustring 'a))))


(test ustring.1
  (is (equalp #(97) (ustring (make-ustring 1 :initial-element 97)))))

(test ustring.2
  (is (equalp #(97) (ustring 97))))

(test ustring.3
  (is (equalp #(97) (ustring #(97)))))

(test ustring.4
  (is (equalp #(97) (ustring "a"))))

(test ustring.5
  (is (equalp #(97) (ustring #\a))))

(test ustring.6
  (is (equalp #(65) (ustring 'a))))

(test ustring.7
  (is-true
   (let ((ustring (make-ustring 1 :initial-element 100)))
     (eq ustring (ustring ustring :new nil)))))

(test ustring.8
  (is-false
   (let ((ustring (make-ustring 1 :initial-element 100)))
     (eq ustring (ustring ustring :new t)))))

(test ustring.error.1
  (signals type-error
    (ustring (make-hash-table))))


(test ustringp.1
  (is-true (ustringp (make-ustring 1))))

(test ustringp.error.1
  (is-false (ustringp "string")))


(test ustring=.1
  (is-true (ustring= "" "")))

(test ustring=.2
  (is-true (ustring= "a" "a")))

(test ustring=.3
  (is-true (ustring= "bac" "acb" :start1 1 :end2 2)))

(test ustring=.4
  (is-false (ustring= "a" "b")))

(test ustring=.5
  (is-false (ustring= "bac" "")))

(test ustring=.6
  (is-false (ustring= "bac" "bac" :end1 0)))


(test ustring-equal.1
  (is-true (ustring-equal "" "")))

(test ustring-equal.2
  (is-true (ustring-equal "a" "a")))

(test ustring-equal.3
  (is-true (ustring-equal "a" "A")))

(test ustring-equal.4
  (is-true (ustring-equal "bac" "acb" :start1 1 :end2 2)))

(test ustring-equal.5
  (is-true (ustring-equal "bac" "ACB" :start1 1 :end2 2)))

(test ustring-equal.6
  (is-false (ustring-equal "a" "b")))

(test ustring-equal.7
  (is-false (ustring-equal "bac" "")))

(test ustring-equal.8
  (is-false (ustring-equal "bac" "bac" :end1 0)))


(test ustring/=.1
  (is-false (ustring/= "" "")))

(test ustring/=.2
  (is (eql 0 (ustring/= "" "a"))))

(test ustring/=.3
  (is (eql 0 (ustring/= "a" "b"))))

(test ustring/=.4
  (is-false (ustring/= "bac" "acb" :start1 1 :end2 2)))

(test ustring/=.5
  (is (eql 1 (ustring/= "abc" "acb"))))


(test ustring-not-equal.1
  (is-false (ustring-not-equal "" "")))

(test ustring-not-equal.2
  (is (eql 0 (ustring-not-equal "" "a"))))

(test ustring-not-equal.3
  (is (eql 0 (ustring-not-equal "a" "b"))))

(test ustring-not-equal.4
  (is-false (ustring-not-equal "a" "A")))

(test ustring-not-equal.5
  (is-false (ustring-not-equal "bac" "acb" :start1 1 :end2 2)))

(test ustring-not-equal.6
  (is-false (ustring-not-equal "bac" "ACB" :start1 1 :end2 2)))

(test ustring-not-equal.7
  (is (eql 1 (ustring-not-equal "abc" "acb"))))

(test ustring-not-equal.8
  (is (eql 1 (ustring-not-equal "abc" "ACB"))))


(test ustring<.1
  (is-false (ustring< "" "")))

(test ustring<.2
  (is (eql 0 (ustring< "" "a"))))

(test ustring<.3
  (is-false (ustring< "a" "")))

(test ustring<.4
  (is (eql 0 (ustring< "a" "b"))))

(test ustring<.5
  (is-false (ustring< "b" "a")))

(test ustring<.6
  (is-false (ustring< "bac" "acb" :start1 1 :end2 2)))

(test ustring<.7
  (is-false (ustring< "acb" "bac" :start1 1 :end2 2)))

(test ustring<.8
  (is (eql 1 (ustring< "abc" "acb"))))

(test ustring<.9
  (is-false (ustring< "acb" "abc")))

(test ustring<.10
  (is-false (ustring< "a" "a")))

(test ustring<.11
  (is-false (ustring< "a" "A")))

(test ustring<.12
  (is (eql 0 (ustring< "A" "a"))))


(test ustring-lessp.1
  (is-false (ustring-lessp "" "")))

(test ustring-lessp.2
  (is (eql 0 (ustring-lessp "" "a"))))

(test ustring-lessp.3
  (is-false (ustring-lessp "a" "")))

(test ustring-lessp.4
  (is (eql 0 (ustring-lessp "a" "b"))))

(test ustring-lessp.5
  (is-false (ustring-lessp "b" "a")))

(test ustring-lessp.6
  (is-false (ustring-lessp "bac" "acb" :start1 1 :end2 2)))

(test ustring-lessp.7
  (is-false (ustring-lessp "acb" "bac" :start1 1 :end2 2)))

(test ustring-lessp.8
  (is (eql 1 (ustring-lessp "abc" "acb"))))

(test ustring-lessp.9
  (is-false (ustring-lessp "acb" "abc")))

(test ustring-lessp.10
  (is-false (ustring-lessp "a" "a")))

(test ustring-lessp.11
  (is-false (ustring-lessp "a" "A")))

(test ustring-lessp.12
  (is-false (ustring-lessp "A" "a")))


(test ustring>.1
  (is-false (ustring> "" "")))

(test ustring>.2
  (is-false (ustring> "" "a")))

(test ustring>.3
  (is (eql 0 (ustring> "a" ""))))

(test ustring>.4
  (is-false (ustring> "a" "b")))

(test ustring>.5
  (is (eql 0 (ustring> "b" "a"))))

(test ustring>.6
  (is-false (ustring> "bac" "acb" :start1 1 :end2 2)))

(test ustring>.7
  (is (eql 1 (ustring> "acb" "bac" :start1 1 :end2 2))))

(test ustring>.8
  (is-false (ustring> "abc" "acb")))

(test ustring>.9
  (is (eql 1 (ustring> "acb" "abc"))))

(test ustring>.10
  (is-false (ustring> "a" "a")))

(test ustring>.11
  (is (eql 0 (ustring> "a" "A"))))

(test ustring>.12
  (is-false (ustring> "A" "a")))


(test ustring-greaterp.1
  (is-false (ustring-greaterp "" "")))

(test ustring-greaterp.2
  (is-false (ustring-greaterp "" "a")))

(test ustring-greaterp.3
  (is (eql 0 (ustring-greaterp "a" ""))))

(test ustring-greaterp.4
  (is-false (ustring-greaterp "a" "b")))

(test ustring-greaterp.5
  (is (eql 0 (ustring-greaterp "b" "a"))))

(test ustring-greaterp.6
  (is-false (ustring-greaterp "bac" "acb" :start1 1 :end2 2)))

(test ustring-greaterp.7
  (is (eql 1 (ustring-greaterp "acb" "bac" :start1 1 :end2 2))))

(test ustring-greaterp.8
  (is-false (ustring-greaterp "abc" "acb")))

(test ustring-greaterp.9
  (is (eql 1 (ustring-greaterp "acb" "abc"))))

(test ustring-greaterp.10
  (is-false (ustring-greaterp "a" "a")))

(test ustring-greaterp.11
  (is-false (ustring-greaterp "a" "A")))

(test ustring-greaterp.12
  (is-false (ustring-greaterp "A" "a")))


(test ustring<=.1
  (is (eql 0 (ustring<= "" ""))))

(test ustring<=.2
  (is (eql 0 (ustring<= "" "a"))))

(test ustring<=.3
  (is-false (ustring<= "a" "")))

(test ustring<=.4
  (is (eql 0 (ustring<= "a" "b"))))

(test ustring<=.5
  (is-false (ustring<= "b" "a")))

(test ustring<=.6
  (is (eql 3 (ustring<= "bac" "acb" :start1 1 :end2 2))))

(test ustring<=.7
  (is-false (ustring<= "acb" "bac" :start1 1 :end2 2)))

(test ustring<=.8
  (is (eql 1 (ustring<= "abc" "acb"))))

(test ustring<=.9
  (is-false (ustring<= "acb" "abc")))

(test ustring<=.10
  (is (eql 1 (ustring<= "a" "a"))))

(test ustring<=.11
  (is-false (ustring<= "a" "A")))

(test ustring<=.12
  (is (eql 0 (ustring<= "A" "a"))))


(test ustring-not-greaterp.1
  (is (eql 0 (ustring-not-greaterp "" ""))))

(test ustring-not-greaterp.2
  (is (eql 0 (ustring-not-greaterp "" "a"))))

(test ustring-not-greaterp.3
  (is-false (ustring-not-greaterp "a" "")))

(test ustring-not-greaterp.4
  (is (eql 0 (ustring-not-greaterp "a" "b"))))

(test ustring-not-greaterp.5
  (is-false (ustring-not-greaterp "b" "a")))

(test ustring-not-greaterp.6
  (is (eql 3 (ustring-not-greaterp "bac" "acb" :start1 1 :end2 2))))

(test ustring-not-greaterp.7
  (is-false (ustring-not-greaterp "acb" "bac" :start1 1 :end2 2)))

(test ustring-not-greaterp.8
  (is (eql 1 (ustring-not-greaterp "abc" "acb"))))

(test ustring-not-greaterp.9
  (is-false (ustring-not-greaterp "acb" "abc")))

(test ustring-not-greaterp.10
  (is (eql 1 (ustring-not-greaterp "a" "a"))))

(test ustring-not-greaterp.11
  (is (eql 1 (ustring-not-greaterp "a" "A"))))

(test ustring-not-greaterp.12
  (is (eql 1 (ustring-not-greaterp "A" "a"))))


(test ustring>=.1
  (is (eql 0 (ustring>= "" ""))))

(test ustring>=.2
  (is-false (ustring>= "" "a")))

(test ustring>=.3
  (is (eql 0 (ustring>= "a" ""))))

(test ustring>=.4
  (is-false (ustring>= "a" "b")))

(test ustring>=.5
  (is (eql 0 (ustring>= "b" "a"))))

(test ustring>=.6
  (is (eql 3 (ustring>= "bac" "acb" :start1 1 :end2 2))))

(test ustring>=.7
  (is (eql 1 (ustring>= "acb" "bac" :start1 1 :end2 2))))

(test ustring>=.8
  (is-false (ustring>= "abc" "acb")))

(test ustring>=.9
  (is (eql 1 (ustring>= "acb" "abc"))))

(test ustring>=.10
  (is (eql 1 (ustring>= "a" "a"))))

(test ustring>=.11
  (is (eql 0 (ustring>= "a" "A"))))

(test ustring>=.12
  (is-false (ustring>= "A" "a")))


(test ustring-not-lessp.1
  (is (eql 0 (ustring-not-lessp "" ""))))

(test ustring-not-lessp.2
  (is-false (ustring-not-lessp "" "a")))

(test ustring-not-lessp.3
  (is (eql 0 (ustring-not-lessp "a" ""))))

(test ustring-not-lessp.4
  (is-false (ustring-not-lessp "a" "b")))

(test ustring-not-lessp.5
  (is (eql 0 (ustring-not-lessp "b" "a"))))

(test ustring-not-lessp.6
  (is (eql 3 (ustring-not-lessp "bac" "acb" :start1 1 :end2 2))))

(test ustring-not-lessp.7
  (is (eql 1 (ustring-not-lessp "acb" "bac" :start1 1 :end2 2))))

(test ustring-not-lessp.8
  (is-false (ustring-not-lessp "abc" "acb")))

(test ustring-not-lessp.9
  (is (eql 1 (ustring-not-lessp "acb" "abc"))))

(test ustring-not-lessp.10
  (is (eql 1 (ustring-not-lessp "a" "a"))))

(test ustring-not-lessp.11
  (is (eql 1 (ustring-not-lessp "a" "A"))))

(test ustring-not-lessp.12
  (is (eql 1 (ustring-not-lessp "A" "a"))))


(test ustring-upcase.1
  (is (ustring= "AHA" (ustring-upcase "aha"))))

(test ustring-upcase.2
  (is (ustring= "" (ustring-upcase ""))))

(test ustring-upcase.3
  (is-false (let ((ustring (ustring "AHA")))
              (eql ustring (ustring-upcase ustring)))))

(test ustring-upcase.error.1
  (signals type-error
    (ustring-upcase 5)))


(test nustring-upcase.1
  (is (ustring= "AHA" (nustring-upcase (ustring "aha")))))

(test nustring-upcase.2
  (is (ustring= "" (nustring-upcase (ustring "")))))

(test nustring-upcase.3
  (is-true (let ((ustring (ustring "AHA")))
             (eql ustring (nustring-upcase ustring)))))

(test nustring-upcase.error.1
  (signals type-error
    (nustring-upcase 5)))


(test ustring-downcase.1
  (is (ustring= "aha" (ustring-downcase "AHA"))))

(test ustring-downcase.2
  (is (ustring= "" (ustring-downcase ""))))

(test ustring-downcase.3
  (is-false (let ((ustring (ustring "aha")))
              (eql ustring (ustring-downcase ustring)))))

(test ustring-downcase.error.1
  (signals type-error
    (ustring-downcase 5)))


(test nustring-downcase.1
  (is (ustring= "aha" (nustring-downcase (ustring "AHA")))))

(test nustring-downcase.2
  (is (ustring= "" (nustring-downcase (ustring "")))))

(test nustring-downcase.3
  (is-true (let ((ustring (ustring "aha")))
             (eql ustring (nustring-downcase ustring)))))

(test nustring-downcase.error.1
  (signals type-error
    (nustring-downcase 5)))


(test ustring-capitalize.1
  (is (ustring= "Hak Mak" (ustring-capitalize "hak mak"))))

(test ustring-capitalize.2
  (is (ustring= "" (ustring-capitalize ""))))

(test ustring-capitalize.3
  (is-false (let ((ustring (ustring "Hak Mak")))
              (eql ustring (ustring-capitalize ustring)))))

(test ustring-capitalize.error.1
  (signals type-error
    (ustring-capitalize 5)))


(test nustring-capitalize.1
  (is (ustring= "Hak Mak" (nustring-capitalize (ustring "hak mak")))))

(test nustring-capitalize.2
  (is (ustring= "" (nustring-capitalize (ustring "")))))

(test nustring-capitalize.3
  (is-true (let ((ustring (ustring "Hak Mak")))
             (eql ustring (nustring-capitalize ustring)))))

(test nustring-capitalize.error.1
  (signals type-error
    (nustring-capitalize 5)))


(test ustring-trim.1
  (is (ustring= "aha" (ustring-trim "kekahakek" "ke"))))

(test ustring-trim.2
  (is (ustring= "aha" (ustring-trim "kekahakek" '(#\k #\e)))))

(test ustring-trim.3
  (is (ustring= "" (ustring-left-trim "aha" "ah"))))

(test ustring-trim.4
  (is-false (let ((ustring (ustring "aha")))
              (eql ustring (ustring-trim ustring "z")))))

(test ustring-trim.error.1
  (signals type-error
    (ustring-trim "kekahakek" 5)))

(test ustring-trim.error.2
  (signals type-error
    (ustring-trim (make-hash-table) '(#\k #\e))))


(test ustring-left-trim.1
  (is (ustring= "ahakek" (ustring-left-trim "kekahakek" "ke"))))

(test ustring-left-trim.2
  (is (ustring= "ahakek" (ustring-left-trim "kekahakek" '(#\k #\e)))))

(test ustring-left-trim.3
  (is (ustring= "" (ustring-left-trim "aha" "ah"))))

(test ustring-left-trim.4
  (is-false (let ((ustring (ustring "aha")))
              (eql ustring (ustring-left-trim ustring "z")))))

(test ustring-left-trim.error.1
  (signals type-error
    (ustring-left-trim "kekahakek" 5)))

(test ustring-left-trim.error.2
  (signals type-error
    (ustring-left-trim (make-hash-table) '(#\k #\e))))


(test ustring-right-trim.1
  (is (ustring= "kekaha" (ustring-right-trim "kekahakek" "ke"))))

(test ustring-right-trim.2
  (is (ustring= "kekaha" (ustring-right-trim "kekahakek" '(#\k #\e)))))

(test ustring-right-trim.3
  (is (ustring= "" (ustring-left-trim "aha" "ah"))))

(test ustring-right-trim.4
  (is-false (let ((ustring (ustring "aha")))
              (eql ustring (ustring-right-trim ustring "z")))))

(test ustring-right-trim.error.1
  (signals type-error
    (ustring-right-trim "kekahakek" 5)))

(test ustring-right-trim.error.2
  (signals type-error
    (ustring-right-trim (make-hash-table) '(#\k #\e))))
