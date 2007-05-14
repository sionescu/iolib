;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;   Copyright (C) 2006, 2007 Stelian Ionescu
;;
;;   This code is free software; you can redistribute it and/or
;;   modify it under the terms of the version 2.1 of
;;   the GNU Lesser General Public License as published by
;;   the Free Software Foundation, as clarified by the
;;   preamble found here:
;;       http://opensource.franz.com/preamble.html
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU Lesser General
;;   Public License along with this library; if not, write to the
;;   Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;   Boston, MA 02110-1301, USA

(in-package :iolib-utils)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defun %check-bounds (sequence start end)
  (unless end (setf end (length sequence)))
  (when (> start end) (error "~S ~S wrong sequence bounds" start end))
  (values start end))

(defmacro return-if (block form)
  (let (($form$ (gensym "FORM-")))
    `(let ((,$form$ ,form))
       (when ,$form$ (return-from ,block ,$form$)))))

(defun featurep (feature)
  (etypecase feature
    (symbol (consp (member feature *features* :test #'eq)))
    (cons (ecase (first feature)
            ((or :or)   (some #'featurep (rest feature)))
            ((and :and) (every #'featurep (rest feature)))
            ((not :not) (and (third feature) (error "Incorrect feature expression: ~S" feature))
                        (not (featurep (second feature))))))))

(defun xnor (x1 x2)
  (eq (not x1) (not x2)))

(export '(define-constant %check-bounds return-if featurep xnor))
