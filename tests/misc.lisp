;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- test suite for miscellaneous routines.
;;;

(in-package :iolib-tests)

(in-suite* :iolib.misc :in :iolib)

(defmacro with-literal-hash-table-syntax (&body body)
  `(let ((*readtable* (copy-readtable)))
     (unwind-protect
          (progn
            (enable-reader-macro 'literal-hash-table)
            ,@body)
       (disable-reader-macro 'literal-hash-table))))

(test literal-hash-tables.creation.empty-hash-table-default-test
  (finishes
    (with-literal-hash-table-syntax
      (read-from-string "#h()"))))

(test (literal-hash-tables.type.empty-hash-table-default-test
       :depends-on literal-hash-tables.creation.empty-hash-table-default-test)
  (is-true
   (let ((ht (with-literal-hash-table-syntax
               (read-from-string "#h()"))))
     (and (zerop (hash-table-count ht))
          (eq (hash-table-test ht) #-clisp 'eql
                                   #+clisp 'ext:fasthash-eql)))))

(test literal-hash-tables.creation.empty-hash-table-test-eq
  (finishes
    (with-literal-hash-table-syntax
      (read-from-string "#h:eq()"))))

(test (literal-hash-tables.type.empty-hash-table-test-eq
       :depends-on literal-hash-tables.creation.empty-hash-table-test-eq)
  (is-true
   (let ((ht (with-literal-hash-table-syntax
               (read-from-string "#h:eq()"))))
     (and (zerop (hash-table-count ht))
          (eq (hash-table-test ht) #-clisp 'eq
                                   #+clisp 'ext:fasthash-eq)))))

(test literal-hash-tables.creation.one-element-hash-table-default-test
  (finishes
    (with-literal-hash-table-syntax
      (read-from-string "#h((1 . 3))"))))

(test (literal-hash-tables.type.one-element-hash-table-default-test
       :depends-on literal-hash-tables.creation.one-element-hash-table-default-test)
  (is-true
   (let ((ht (with-literal-hash-table-syntax
               (read-from-string "#h((1 . 3))"))))
     (and (= 1 (hash-table-count ht))
          (eq (hash-table-test ht) #-clisp 'eql
                                   #+clisp 'ext:fasthash-eql)
          (= 3 (gethash 1 ht))))))

(test literal-hash-tables.creation.one-element-hash-table-test-eq
  (finishes
    (with-literal-hash-table-syntax
      (read-from-string "#h:eq((:a . :test))"))))

(test (literal-hash-tables.type.one-element-hash-table-test-eq
       :depends-on literal-hash-tables.creation.one-element-hash-table-test-eq)
  (is-true
   (let ((ht (with-literal-hash-table-syntax
               (read-from-string "#h:eq((:a . :test))"))))
     (and (= 1 (hash-table-count ht))
          (eq (hash-table-test ht) #-clisp 'eq
                                   #+clisp 'ext:fasthash-eq)
          (eq :test (gethash :a ht))))))

(test literal-hash-tables.error.wrong-test
  (signals error
    (with-literal-hash-table-syntax
      (read-from-string "#h:nologo((2 . 3))"))))

(test literal-hash-tables.error.wrong-element
  (signals reader-error
    (with-literal-hash-table-syntax
      (read-from-string "#h(2)"))))
