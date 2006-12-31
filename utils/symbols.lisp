;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          symbols.lisp
;;;; Purpose:       Returns all defined Common Lisp symbols
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Apr 2000
;;;;
;;;; $Id: symbols.lisp 9652 2004-06-17 20:32:00Z kevin $
;;;;
;;;; This file, part of KMRCL, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; KMRCL users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :iolib-utils)

;;; Symbol functions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (char= #\a (schar (symbol-name '#:a) 0))
    (pushnew :iolib-utils-lowercase-reader *features*))
  (when (not (string= (symbol-name '#:a)
		      (symbol-name '#:A)))
    (pushnew :iolib-utils-case-sensitive *features*)))

(defun string-default-case (str)
  #+(and (not iolib-utils-lowercase-reader)) (string-upcase str)
  #+(and iolib-utils-lowercase-reader) (string-downcase str))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq cl:*features* (delete :iolib-utils-lowercase-reader *features*))
  (setq cl:*features* (delete :iolib-utils-case-sensitive *features*)))

(defun concat-symbol-pkg (pkg &rest args)
  (declare (dynamic-extent args))
  (flet ((stringify (arg)
           (etypecase arg
             (string
              (string-upcase arg))
             (symbol
              (symbol-name arg)))))
    (let ((str (apply #'concatenate 'string (mapcar #'stringify args))))
      (nth-value 0 (intern (string-default-case str)
			   (if pkg pkg *package*))))))

(defun concat-symbol (&rest args)
  (apply #'concat-symbol-pkg nil args))

(defun ensure-keyword (name)
  "Returns keyword for a name"
  (etypecase name
    (keyword name)
    (string (nth-value 0 (intern (string-default-case name) :keyword)))
    (symbol (nth-value 0 (intern (symbol-name name) :keyword)))))

(defun ensure-keyword-upcase (desig)
  (nth-value 0 (intern (string-upcase
			(symbol-name (ensure-keyword desig))) :keyword)))

(defun ensure-keyword-default-case (desig)
  (nth-value 0 (intern (string-default-case
			(symbol-name (ensure-keyword desig))) :keyword)))

(export '(string-default-case concat-symbol-pkg concat-symbol
          ensure-keyword ensure-keyword-upcase ensure-keyword-default-case))
