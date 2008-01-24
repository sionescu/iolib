;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; conditions.lisp --- GNUTLS error conditions.
;;;
;;; Copyright (C) 2008, Stelian Ionescu  <sionescu@common-lisp.net>
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by
;;; the Free Software Foundation, as clarified by the
;;; preamble found here:
;;;     http://opensource.franz.com/preamble.html
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General
;;; Public License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;; Boston, MA 02110-1301, USA

(in-package :net.tls)

(define-condition gnutls-error (error)
  ((code :initarg :code :reader gnutls-error-code))
  (:report (lambda (condition stream)
             (format stream "GNUTLS error: ~A"
                     (%gnutls_strerror (gnutls-error-code condition))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *gnutls-errors* (make-hash-table :test #'eql)))

;;; Used in the ERRNO-WRAPPER foreign type.
(defun signal-gnutls-error (errno)
  (switch (errno :test #'=)
    (#.(foreign-enum-value 'gnutls-error-codes :again)
     (error 'nix:eagain))
    (#.(foreign-enum-value 'gnutls-error-codes :interrupted)
     (error 'nix:eintr))
    (t (error (gethash errno *gnutls-errors*)))))

(macrolet ((define-gnutls-error (keyword)
             (let ((code (foreign-enum-value 'gnutls-error-codes keyword))
                   (csym (format-symbol t "~A-~A-~A" '#:gnutls keyword '#:error)))
               `(progn
                  (setf (gethash ,code *gnutls-errors*) ',csym)
                  (define-condition ,csym (gnutls-error) ()
                    (:default-initargs :code ,code)))))
           (define-gnutls-errors (keywords)
             `(progn ,@(loop :for kw :in keywords :collect
                          `(define-gnutls-error ,kw)))))
  (define-gnutls-errors #.(foreign-enum-keyword-list 'gnutls-error-codes)))
