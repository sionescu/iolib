;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; Programmer: Kevin Rosenberg


(in-package #:cl-user)
(defpackage #:net.uri-system
  (:use #:common-lisp #:asdf))
(in-package #:net.uri-system)

(defsystem net.uri
  :version "1.4.0"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "GNU Lesser General Public License"
  :description "Portable Universal Resource Indentifier Library"
  :components
  ((:file "net.uri")))

(defmethod perform ((o test-op) (c (eql (find-system 'net.uri))))
  (oos 'load-op 'net.uri-tests)
  (oos 'test-op 'net.uri-tests))

(defsystem net.uri-tests
  :depends-on (#:net.uri
               #:ptester) 
  :components
  ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system 'net.uri-tests))))
  (or (funcall (intern (symbol-name '#:do-tests)
		       (find-package :net.uri-tests)))
      (error "test-op failed")))

(defmethod operation-done-p ((o test-op) (c (eql (find-system 'net.uri-tests))))
  (values nil))
