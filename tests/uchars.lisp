;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- uchars test suite.
;;;

(in-package :iolib-tests)

(in-suite :iolib.base.uchars)


(test code-uchar.1
  (is (eql #x1234 (code-uchar #x1234))))

(test code-uchar.error.1
  (signals type-error
    (code-uchar uchar-code-limit)))


(test uchar-code.1
  (is (eql #x1234 (uchar-code #x1234))))

(test uchar-code.error.1
  (signals type-error
    (uchar-code uchar-code-limit)))


(test char-to-uchar.1
  (is (eql 49 (char-to-uchar #\1))))


(test uchar-to-char.1
  (is (char= #\1 (uchar-to-char 49))))

(test uchar-to-char.error.1
  (signals type-error
    (uchar-to-char uchar-code-limit)))


(test name-uchar.1
  (is (eql (char-to-uchar #\space) (name-uchar "Space"))))

(test name-uchar.2
  (is (eql #xD800 (name-uchar "Non-Unicode uchar #xD800"))))

(test name-uchar.error.1
  (is-false (name-uchar "This is not a uchar name")))


(test uchar-name.1
  (is (string-equal "Space" (uchar-name (char-to-uchar #\space)))))

(test uchar-name.2
  (is (string-equal "Non-Unicode uchar #xD800"
                    (uchar-name #xD800))))


(test digit-uchar.1
  (is (eql (+ #x30 9) (digit-uchar 9))))

(test digit-uchar.2
  (is (loop :for i :below 16 :always (digit-uchar i 16))))

(test digit-uchar.3
  (is-true
   (loop :for i :from 0 :to 255
         :always (eql (digit-uchar i)
                      (if-let (char (digit-char i))
                        (char-to-uchar char))))))

(test digit-uchar.error.1
  (is-false (digit-uchar 16 16)))

(test digit-uchar.error.2
  (signals type-error
    (digit-uchar "string")))


(test uchar.1
  (is (eql 9 (uchar 9))))

(test uchar.2
  (is (eql 9 (uchar (make-array 1 :element-type 'uchar :initial-element 9)))))

(test uchar.3
  (is (eql 65 (uchar #\A))))

(test uchar.4
  (is (eql 65 (uchar "A"))))

(test uchar.5
  (is (eql 65 (uchar 'a))))

(test uchar.error.1
  (signals type-error
    (uchar uchar-code-limit)))

(test uchar.error.2
  (signals type-error
    (uchar -1)))

(test uchar.error.3
  (signals type-error
    (uchar (make-array 2 :element-type 'uchar :initial-element 9))))

(test uchar.error.4
  (signals type-error
    (uchar "FOO")))

(test uchar.error.5
  (signals type-error
    (uchar 'nil)))


(test ucharp.1
  (is-true (ucharp 0)))

(test ucharp.2
  (is-true (ucharp (1- uchar-code-limit))))

(test ucharp.3
  (is-false (ucharp -1)))

(test ucharp.4
  (is-false (ucharp uchar-code-limit)))

(test ucharp.5
  (is-false (ucharp #\a)))

(test ucharp.6
  (is-false (ucharp "string")))


(test unicode-uchar-p.1
  (is-true (unicode-uchar-p #xD7FF)))

(test unicode-uchar-p.2
  (is-true (unicode-uchar-p #xDE00)))

(test unicode-uchar-p.1
  (is-false (unicode-uchar-p #xD800)))

(test unicode-uchar-p.2
  (is-false (unicode-uchar-p #xDFFF)))


(test uchar=.1
  (is (eql t (uchar= #x40))))

(test uchar=.2
  (is (eql t (uchar= #x40 #x40))))

(test uchar=.3
  (is (eql t (uchar= #x40 #x40 #x40))))

(test uchar=.4
  (is (eql nil (uchar= #x40 #x41))))


(test uchar/=.1
  (is (eql t (uchar/= #x40))))

(test uchar/=.2
  (is (eql t (uchar/= #x40 #x41))))

(test uchar/=.3
  (is (eql t (uchar/= #x40 #x41 #x42))))

(test uchar/=.4
  (is (eql nil (uchar/= #x40 #x40))))

(test uchar/=.5
  (is (eql nil (uchar/= #x40 #x41 #x40))))


(test uchar<.1
  (is (eql t (uchar< #x40))))

(test uchar<.2
  (is (eql t (uchar< #x40 #x41))))

(test uchar<.3
  (is (eql t (uchar< #x40 #x41 #x42))))

(test uchar<.4
  (is (eql nil (uchar< #x40 #x40))))

(test uchar<.5
  (is (eql nil (uchar< #x40 #x41 #x40))))


(test uchar>.1
  (is (eql t (uchar> #x40))))

(test uchar>.2
  (is (eql t (uchar> #x41 #x40))))

(test uchar>.3
  (is (eql t (uchar> #x42 #x41 #x40))))

(test uchar>.4
  (is (eql nil (uchar> #x40 #x40))))

(test uchar>.5
  (is (eql nil (uchar> #x41 #x40 #x40))))


(test uchar<=.1
  (is (eql t (uchar<= #x40))))

(test uchar<=.2
  (is (eql t (uchar<= #x40 #x41))))

(test uchar<=.3
  (is (eql t (uchar<= #x40 #x41 #x42))))

(test uchar<=.4
  (is (eql t (uchar<= #x40 #x40))))

(test uchar<=.5
  (is (eql nil (uchar<= #x40 #x41 #x40))))


(test uchar>=.1
  (is (eql t (uchar>= #x40))))

(test uchar>=.2
  (is (eql t (uchar>= #x41 #x40))))

(test uchar>=.3
  (is (eql t (uchar>= #x42 #x41 #x40))))

(test uchar>=.4
  (is (eql t (uchar>= #x40 #x40))))

(test uchar>=.5
  (is (eql nil (uchar>= #x40 #x41 #x40))))


(test uchar-equal.1
  (is (eql t (uchar-equal #x40))))

(test uchar-equal.2
  (is (eql t (uchar-equal #x40 #x40))))

(test uchar-equal.3
  (is (eql t (uchar-equal #x40 #x40 #x40))))

(test uchar-equal.4
  (is (eql t (uchar-equal #x41 #x61))))

(test uchar-equal.5
  (is (eql t (uchar-equal #x41 #x61 #x41))))

(test uchar-equal.6
  (is (eql nil (uchar-equal #x40 #x41))))


(test uchar-not-equal.1
  (is (eql t (uchar-not-equal #x40))))

(test uchar-not-equal.2
  (is (eql t (uchar-not-equal #x40 #x41))))

(test uchar-not-equal.3
  (is (eql t (uchar-not-equal #x40 #x41 #x42))))

(test uchar-not-equal.4
  (is (eql nil (uchar-not-equal #x40 #x40))))

(test uchar-not-equal.5
  (is (eql nil (uchar-not-equal #x40 #x41 #x40))))

(test uchar-not-equal.6
  (is (eql nil (uchar-not-equal #x41 #x61))))

(test uchar-not-equal.7
  (is (eql nil (uchar-not-equal #x41 #x61 #x41))))


(test uchar-lessp.1
  (is (eql t (uchar-lessp #x40))))

(test uchar-lessp.2
  (is (eql t (uchar-lessp #x40 #x41))))

(test uchar-lessp.3
  (is (eql t (uchar-lessp #x40 #x41 #x42))))

(test uchar-lessp.4
  (is (eql nil (uchar-lessp #x40 #x40))))

(test uchar-lessp.5
  (is (eql nil (uchar-lessp #x40 #x41 #x40))))

(test uchar-lessp.6
  (is (eql nil (uchar-lessp #x41 #x61))))

(test uchar-lessp.7
  (is (eql nil (uchar-lessp #x41 #x61 #x62))))


(test uchar-greaterp.1
  (is (eql t (uchar-greaterp #x40))))

(test uchar-greaterp.2
  (is (eql t (uchar-greaterp #x41 #x40))))

(test uchar-greaterp.3
  (is (eql t (uchar-greaterp #x42 #x41 #x40))))

(test uchar-greaterp.4
  (is (eql nil (uchar-greaterp #x40 #x40))))

(test uchar-greaterp.5
  (is (eql nil (uchar-greaterp #x41 #x40 #x40))))

(test uchar-greaterp.6
  (is (eql nil (uchar-greaterp #x61 #x41))))

(test uchar-greaterp.7
  (is (eql nil (uchar-greaterp #x62 #x61 #x41))))


(test uchar-not-greaterp.1
  (is (eql t (uchar-not-greaterp #x40))))

(test uchar-not-greaterp.2
  (is (eql t (uchar-not-greaterp #x40 #x41))))

(test uchar-not-greaterp.3
  (is (eql t (uchar-not-greaterp #x40 #x41 #x42))))

(test uchar-not-greaterp.4
  (is (eql t (uchar-not-greaterp #x40 #x40))))

(test uchar-not-greaterp.5
  (is (eql nil (uchar-not-greaterp #x40 #x41 #x40))))

(test uchar-not-greaterp.6
  (is (eql t (uchar-not-greaterp #x41 #x61))))

(test uchar-not-greaterp.7
  (is (eql t (uchar-not-greaterp #x41 #x61 #x62))))


(test uchar-not-lessp.1
  (is (eql t (uchar-not-lessp #x40))))

(test uchar-not-lessp.2
  (is (eql t (uchar-not-lessp #x41 #x40))))

(test uchar-not-lessp.3
  (is (eql t (uchar-not-lessp #x42 #x41 #x40))))

(test uchar-not-lessp.4
  (is (eql t (uchar-not-lessp #x40 #x40))))

(test uchar-not-lessp.5
  (is (eql t (uchar-not-lessp #x61 #x41))))

(test uchar-not-lessp.6
  (is (eql t (uchar-not-lessp #x62 #x61 #x41))))

(test uchar-not-lessp.7
  (is (eql nil (uchar-not-lessp #x40 #x41 #x40))))


(test alpha-uchar-p.1
  (is-true
   (and (loop :for r :from (char-to-uchar #\a) :to (char-to-uchar #\z)
              :always (alpha-uchar-p r))
        (loop :for r :from (char-to-uchar #\A) :to (char-to-uchar #\Z)
              :always (alpha-uchar-p r)))))

(test alpha-uchar-p.2
  (is-false
   (alpha-uchar-p (char-to-uchar #\5))))

(test alpha-uchar-p.3
  (is-true
   (loop :for i :from 0 :to 255
         :always (let ((a-c-p (alpha-uchar-p i)))
                   (if (alpha-char-p (code-char i))
                       (uchar= a-c-p i)
                       (null a-c-p))))))

(test alpha-uchar-p.error.1
  (signals type-error
   (alpha-uchar-p "string")))


(test digit-uchar-p.1
  (is (eql 9 (digit-uchar-p (+ #x30 9)))))

(test digit-uchar-p.2
  (is (loop :for i :below 10 :always (eql i (digit-uchar-p (+ i #x30) 10)))))

(test digit-uchar-p.3
  (is (loop :for i :from 10 :below 36
            :always (eql i (digit-uchar-p (+ i #x57) 36)))))

(test digit-uchar-p.4
  (is-true
   (loop :for i :from 0 :to 255
         :always (eql (digit-uchar-p i)
                      (digit-char-p (code-char i))))))

(test digit-uchar.error.1
  (is-false (digit-uchar-p 16 16)))

(test digit-uchar.error.2
  (signals type-error
    (digit-uchar-p "string")))


(test alphanumeric-uchar-p.1
  (is-true
   (loop :for i :from 0 :to 255
         :always (let ((a-c-p (alphanumeric-uchar-p i)))
                   (if (alphanumericp (code-char i))
                       (uchar= a-c-p i)
                       (null a-c-p))))))

(test alphanumeric-uchar-p.error.1
  (signals type-error
    (alphanumeric-uchar-p "string")))


(test graphic-uchar-p.1
  (is-true
   (loop :for i :from 0 :to 255
         :always (let ((g-c-p (graphic-uchar-p i)))
                   (if (graphic-char-p (code-char i))
                       (uchar= g-c-p i)
                       (null g-c-p))))))

(test graphic-uchar-p.error.1
  (signals type-error
    (graphic-uchar-p "string")))


(test upper-case-uchar-p.1
  (is-true
   (loop :for i :from 0 :to 255
         :always (let ((u-c-p (upper-case-uchar-p i)))
                   (if (upper-case-p (code-char i))
                       (uchar= u-c-p i)
                       (null u-c-p))))))

(test upper-case-uchar-p.error.1
  (signals type-error
    (upper-case-uchar-p "string")))


(test lower-case-uchar-p.1
  (is-true
   (loop :for i :from 0 :to 255
         :always (let ((l-c-p (lower-case-uchar-p i)))
                   (if (lower-case-p (code-char i))
                       (uchar= l-c-p i)
                       (null l-c-p))))))

(test lower-case-uchar-p.error.1
  (signals type-error
    (lower-case-uchar-p "string")))


(test both-case-uchar-p.1
  (is-true
   (loop :for i :from 0 :to 255
         :always (let ((b-c-p (both-case-uchar-p i)))
                   (if (both-case-p (code-char i))
                       (uchar= i b-c-p)
                       (null b-c-p))))))

(test both-case-uchar-p.error.1
  (signals type-error
    (both-case-uchar-p "string")))


(test uchar-upcase.1
  (is-true
   (loop :for i :from 0 :to 255
         :always (eql (uchar-upcase i)
                      (char-to-uchar (char-upcase (code-char i)))))))

(test uchar-upcase.error.1
  (signals type-error
    (uchar-upcase "string")))


(test uchar-downcase.1
  (is-true
   (loop :for i :from 0 :to 255
         :always (eql (uchar-downcase i)
                      (char-to-uchar (char-downcase (code-char i)))))))

(test uchar-downcase.error.1
  (signals type-error
    (uchar-downcase "string")))
