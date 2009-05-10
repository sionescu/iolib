;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- runes test suite.
;;;

(in-package :iolib-tests)

(in-suite :iolib.base.runes)


(test code-rune.1
  (is (= #x1234 (code-rune #x1234))))

(test code-rune.error.1
  (signals type-error
    (code-rune rune-code-limit)))


(test rune-code.1
  (is (= #x1234 (rune-code #x1234))))

(test rune-code.error.1
  (signals type-error
    (rune-code rune-code-limit)))


(test char-rune.1
  (is (= 49 (char-rune #\1))))


(test rune-char.1
  (is (char= #\1 (rune-char 49))))

(test rune-char.error.1
  (signals type-error
    (rune-char rune-code-limit)))


(test name-rune.1
  (is (= (char-rune #\space) (name-rune "Space"))))

(test name-rune.2
  (is (= #xD800 (name-rune "Non-Unicode rune #xD800"))))

(test name-rune.error.1
  (is-false (name-rune "This is not a rune name")))


(test rune-name.1
  (is (string-equal "Space" (rune-name (char-rune #\space)))))

(test rune-name.2
  (is (string-equal "Non-Unicode rune #xD800"
                    (rune-name #xD800))))


(test digit-rune.1
  (is (= (+ #x30 9) (digit-rune 9))))

(test digit-rune.2
  (is (loop :for i :below 16 :always (digit-rune i 16))))

(test digit-rune.3
  (is-true
   (loop :for i :from 0 :to 255
         :always (eql (digit-rune i)
                      (if-let (char (digit-char i))
                        (char-rune char))))))

(test digit-rune.error.1
  (is-false (digit-rune 16 16)))

(test digit-rune.error.2
  (signals type-error
    (digit-rune "string")))


(test rune.1
  (is (= 9 (rune 9))))

(test rune.2
  (is (= 9 (rune (make-array 1 :element-type 'rune :initial-element 9)))))

(test rune.3
  (is (= 65 (rune #\A))))

(test rune.4
  (is (= 65 (rune "A"))))

(test rune.5
  (is (= 65 (rune 'a))))

(test rune.error.1
  (signals type-error
    (rune rune-code-limit)))

(test rune.error.2
  (signals type-error
    (rune -1)))

(test rune.error.3
  (signals type-error
    (rune (make-array 2 :element-type 'rune :initial-element 9))))

(test rune.error.4
  (signals type-error
    (rune "FOO")))

(test rune.error.5
  (signals type-error
    (rune 'nil)))


(test runep.1
  (is-true (runep 0)))

(test runep.2
  (is-true (runep (1- rune-code-limit))))

(test runep.3
  (is-false (runep -1)))

(test runep.4
  (is-false (runep rune-code-limit)))

(test runep.5
  (is-false (runep #\a)))

(test runep.6
  (is-false (runep "string")))


(test unicode-rune-p.1
  (is-true (unicode-rune-p #xD7FF)))

(test unicode-rune-p.2
  (is-true (unicode-rune-p #xDE00)))

(test unicode-rune-p.1
  (is-false (unicode-rune-p #xD800)))

(test unicode-rune-p.2
  (is-false (unicode-rune-p #xDFFF)))


(test rune=.1
  (is (eql t (rune= #x40))))

(test rune=.2
  (is (eql t (rune= #x40 #x40))))

(test rune=.3
  (is (eql t (rune= #x40 #x40 #x40))))

(test rune=.4
  (is (eql nil (rune= #x40 #x41))))


(test rune/=.1
  (is (eql t (rune/= #x40))))

(test rune/=.2
  (is (eql t (rune/= #x40 #x41))))

(test rune/=.3
  (is (eql t (rune/= #x40 #x41 #x42))))

(test rune/=.4
  (is (eql nil (rune/= #x40 #x40))))

(test rune/=.5
  (is (eql nil (rune/= #x40 #x41 #x40))))


(test rune<.1
  (is (eql t (rune< #x40))))

(test rune<.2
  (is (eql t (rune< #x40 #x41))))

(test rune<.3
  (is (eql t (rune< #x40 #x41 #x42))))

(test rune<.4
  (is (eql nil (rune< #x40 #x40))))

(test rune<.5
  (is (eql nil (rune< #x40 #x41 #x40))))


(test rune>.1
  (is (eql t (rune> #x40))))

(test rune>.2
  (is (eql t (rune> #x41 #x40))))

(test rune>.3
  (is (eql t (rune> #x42 #x41 #x40))))

(test rune>.4
  (is (eql nil (rune> #x40 #x40))))

(test rune>.5
  (is (eql nil (rune> #x41 #x40 #x40))))


(test rune<=.1
  (is (eql t (rune<= #x40))))

(test rune<=.2
  (is (eql t (rune<= #x40 #x41))))

(test rune<=.3
  (is (eql t (rune<= #x40 #x41 #x42))))

(test rune<=.4
  (is (eql t (rune<= #x40 #x40))))

(test rune<=.5
  (is (eql nil (rune<= #x40 #x41 #x40))))


(test rune>=.1
  (is (eql t (rune>= #x40))))

(test rune>=.2
  (is (eql t (rune>= #x41 #x40))))

(test rune>=.3
  (is (eql t (rune>= #x42 #x41 #x40))))

(test rune>=.4
  (is (eql t (rune>= #x40 #x40))))

(test rune>=.5
  (is (eql nil (rune>= #x40 #x41 #x40))))


(test rune-equal.1
  (is (eql t (rune-equal #x40))))

(test rune-equal.2
  (is (eql t (rune-equal #x40 #x40))))

(test rune-equal.3
  (is (eql t (rune-equal #x40 #x40 #x40))))

(test rune-equal.4
  (is (eql t (rune-equal #x41 #x61))))

(test rune-equal.5
  (is (eql t (rune-equal #x41 #x61 #x41))))

(test rune-equal.6
  (is (eql nil (rune-equal #x40 #x41))))


(test rune-not-equal.1
  (is (eql t (rune-not-equal #x40))))

(test rune-not-equal.2
  (is (eql t (rune-not-equal #x40 #x41))))

(test rune-not-equal.3
  (is (eql t (rune-not-equal #x40 #x41 #x42))))

(test rune-not-equal.4
  (is (eql nil (rune-not-equal #x40 #x40))))

(test rune-not-equal.5
  (is (eql nil (rune-not-equal #x40 #x41 #x40))))

(test rune-not-equal.6
  (is (eql nil (rune-not-equal #x41 #x61))))

(test rune-not-equal.7
  (is (eql nil (rune-not-equal #x41 #x61 #x41))))


(test rune-lessp.1
  (is (eql t (rune-lessp #x40))))

(test rune-lessp.2
  (is (eql t (rune-lessp #x40 #x41))))

(test rune-lessp.3
  (is (eql t (rune-lessp #x40 #x41 #x42))))

(test rune-lessp.4
  (is (eql nil (rune-lessp #x40 #x40))))

(test rune-lessp.5
  (is (eql nil (rune-lessp #x40 #x41 #x40))))

(test rune-lessp.6
  (is (eql nil (rune-lessp #x41 #x61))))

(test rune-lessp.7
  (is (eql nil (rune-lessp #x41 #x61 #x62))))


(test rune-greaterp.1
  (is (eql t (rune-greaterp #x40))))

(test rune-greaterp.2
  (is (eql t (rune-greaterp #x41 #x40))))

(test rune-greaterp.3
  (is (eql t (rune-greaterp #x42 #x41 #x40))))

(test rune-greaterp.4
  (is (eql nil (rune-greaterp #x40 #x40))))

(test rune-greaterp.5
  (is (eql nil (rune-greaterp #x41 #x40 #x40))))

(test rune-greaterp.6
  (is (eql nil (rune-greaterp #x61 #x41))))

(test rune-greaterp.7
  (is (eql nil (rune-greaterp #x62 #x61 #x41))))


(test rune-not-greaterp.1
  (is (eql t (rune-not-greaterp #x40))))

(test rune-not-greaterp.2
  (is (eql t (rune-not-greaterp #x40 #x41))))

(test rune-not-greaterp.3
  (is (eql t (rune-not-greaterp #x40 #x41 #x42))))

(test rune-not-greaterp.4
  (is (eql t (rune-not-greaterp #x40 #x40))))

(test rune-not-greaterp.5
  (is (eql nil (rune-not-greaterp #x40 #x41 #x40))))

(test rune-not-greaterp.6
  (is (eql t (rune-not-greaterp #x41 #x61))))

(test rune-not-greaterp.7
  (is (eql t (rune-not-greaterp #x41 #x61 #x62))))


(test rune-not-lessp.1
  (is (eql t (rune-not-lessp #x40))))

(test rune-not-lessp.2
  (is (eql t (rune-not-lessp #x41 #x40))))

(test rune-not-lessp.3
  (is (eql t (rune-not-lessp #x42 #x41 #x40))))

(test rune-not-lessp.4
  (is (eql t (rune-not-lessp #x40 #x40))))

(test rune-not-lessp.5
  (is (eql t (rune-not-lessp #x61 #x41))))

(test rune-not-lessp.6
  (is (eql t (rune-not-lessp #x62 #x61 #x41))))

(test rune-not-lessp.7
  (is (eql nil (rune-not-lessp #x40 #x41 #x40))))


(test alpha-rune-p.1
  (is-true
   (and (loop :for r :from (char-rune #\a) :to (char-rune #\z)
              :always (alpha-rune-p r))
        (loop :for r :from (char-rune #\A) :to (char-rune #\Z)
              :always (alpha-rune-p r)))))

(test alpha-rune-p.2
  (is-false
   (alpha-rune-p (char-rune #\5))))

(test alpha-rune-p.3
  (is-true
   (loop :for i :from 0 :to 255
         :always (eql (alpha-rune-p i)
                      (alpha-char-p (code-char i))))))

(test alpha-rune-p.error.1
  (signals type-error
   (alpha-rune-p "string")))


(test digit-rune-p.1
  (is (= 9 (digit-rune-p (+ #x30 9)))))

(test digit-rune-p.2
  (is (loop :for i :below 10 :always (= i (digit-rune-p (+ i #x30) 10)))))

(test digit-rune-p.3
  (is (loop :for i :from 10 :below 36
            :always (= i (digit-rune-p (+ i #x57) 36)))))

(test digit-rune-p.4
  (is-true
   (loop :for i :from 0 :to 255
         :always (eql (digit-rune-p i)
                      (digit-char-p (code-char i))))))

(test digit-rune.error.1
  (is-false (digit-rune-p 16 16)))

(test digit-rune.error.2
  (signals type-error
    (digit-rune-p "string")))


(test alphanumeric-rune-p.1
  (is-true
   (loop :for i :from 0 :to 255
         :always (eql (alphanumeric-rune-p i)
                      (alphanumericp (code-char i))))))

(test alphanumeric-rune-p.error.1
  (signals type-error
    (alphanumeric-rune-p "string")))


(test graphic-rune-p.1
  (is-true
   (loop :for i :from 0 :to 255
         :always (eql (graphic-rune-p i)
                      (graphic-char-p (code-char i))))))

(test graphic-rune-p.error.1
  (signals type-error
    (graphic-rune-p "string")))


(test upper-case-rune-p.1
  (is-true
   (loop :for i :from 0 :to 255
         :always (eql (upper-case-rune-p i)
                      (upper-case-p (code-char i))))))

(test upper-case-rune-p.error.1
  (signals type-error
    (upper-case-rune-p "string")))


(test lower-case-rune-p.1
  (is-true
   (loop :for i :from 0 :to 255
         :always (eql (lower-case-rune-p i)
                      (lower-case-p (code-char i))))))

(test lower-case-rune-p.error.1
  (signals type-error
    (lower-case-rune-p "string")))


(test both-case-rune-p.1
  (is-true
   (loop :for i :from 0 :to 255
         :always (eql (both-case-rune-p i)
                      (both-case-p (code-char i))))))

(test both-case-rune-p.error.1
  (signals type-error
    (both-case-rune-p "string")))
