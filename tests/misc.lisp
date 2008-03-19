;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; misc.lisp --- test suite for miscellaneous routines.
;;;
;;; Copyright (C) 2008, Stelian Ionescu  <sionescu@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package :iolib-tests)

(in-suite* :iolib.misc :in :iolib)

(test literal-hash-tables.creation.empty-hash-table-default-test
  (finishes
    (read-from-string "#h{}")))

(test (literal-hash-tables.type.empty-hash-table-default-test
       :depends-on literal-hash-tables.creation.empty-hash-table-default-test)
  (is-true
   (let ((ht (read-from-string "#h{}")))
     (and (zerop (hash-table-count ht))
          (eq (hash-table-test ht) #-clisp 'eql
                                   #+clisp 'ext:fasthash-eql)))))

(test literal-hash-tables.creation.empty-hash-table-test-eq
  (finishes
    (read-from-string "#h:eq{}")))

(test (literal-hash-tables.type.empty-hash-table-test-eq
       :depends-on literal-hash-tables.creation.empty-hash-table-test-eq)
  (is-true
   (let ((ht (read-from-string "#h:eq{}")))
     (and (zerop (hash-table-count ht))
          (eq (hash-table-test ht) #-clisp 'eq
                                   #+clisp 'ext:fasthash-eq)))))

(test literal-hash-tables.creation.one-element-hash-table-default-test
  (finishes
    (read-from-string "#h{1 => 3}")))

(test (literal-hash-tables.type.one-element-hash-table-default-test
       :depends-on literal-hash-tables.creation.one-element-hash-table-default-test)
  (is-true
   (let ((ht (read-from-string "#h{1 => 3}")))
     (and (= 1 (hash-table-count ht))
          (eq (hash-table-test ht) #-clisp 'eql
                                   #+clisp 'ext:fasthash-eql)
          (= 3 (gethash 1 ht))))))

(test literal-hash-tables.creation.one-element-hash-table-test-eq
  (finishes
    (read-from-string "#h:eq{:a => :test}")))

(test (literal-hash-tables.type.one-element-hash-table-test-eq
       :depends-on literal-hash-tables.creation.one-element-hash-table-test-eq)
  (is-true
   (let ((ht (read-from-string "#h:eq{:a => :test}")))
     (and (= 1 (hash-table-count ht))
          (eq (hash-table-test ht) #-clisp 'eq
                                   #+clisp 'ext:fasthash-eq)
          (eq :test (gethash :a ht))))))

(test literal-hash-tables.error.only-key
  (signals reader-error
    (read-from-string "#h{2}")))

(test literal-hash-tables.error.only-key-and-arrow
  (signals reader-error
    (read-from-string "#h{2 =>}")))

(test literal-hash-tables.error.only-separator
  (signals reader-error
    (read-from-string "#h{,}")))

(test literal-hash-tables.error.mismatched-separator
  (signals reader-error
    (read-from-string "#h{2 => 3 ,}")))

(test literal-hash-tables.error.wrong-test
  (signals error
    (read-from-string "#h:nologo{2 => 3}")))
