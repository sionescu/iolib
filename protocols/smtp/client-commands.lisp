;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;   Copyright (C) 2007 Stelian Ionescu
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

(in-package :net.smtp-client)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *smtp-client-commands* (make-hash-table :test #'eq))
  
  (defun make-smtp-cmd-name (name)
    (concat-symbol 'smtp- name '-cmd)))

(defun read-smtp-return-code (sock expected-code error-msg)
  (multiple-value-bind (code msg)
      (read-from-smtp sock)
    (when (/= code expected-code)
      (error "~A: ~A" error-msg msg))))

(defmacro define-smtp-command (name (sock &rest args) &body body)
  (let ((cmd-name (make-smtp-cmd-name name)))
    `(progn
       (defun ,cmd-name (,sock ,@args)
         ,@body)
       (setf (gethash ,name *smtp-client-commands*)
             ',cmd-name))))

(defmacro invoke-smtp-command (name sock &rest args)
  (let ((cmd-sym (gethash name *smtp-client-commands*)))
    (if cmd-sym
        `(,cmd-sym ,sock ,@args)
        (error "Unknown SMTP command: ~A" name))))

;;;
;;; SMTP Commands
;;;

(define-smtp-command :mail-from (sock from)
  (format-socket sock "MAIL FROM: <~A>" from)
  (read-smtp-return-code sock 250 "in MAIL FROM command"))

(define-smtp-command :rcpt-to (sock addresses)
  (dolist (to addresses)
    (format-socket sock "RCPT TO: <~A>" to)
    (read-smtp-return-code sock 250 "in RCPT TO command")))

(define-smtp-command :data (sock)
  (format-socket sock "DATA")
  (read-smtp-return-code sock 354 "in DATA command"))

(define-smtp-command :quit (sock)
  (format-socket sock "QUIT")
  (read-smtp-return-code sock 221 "in QUIT command"))

(define-smtp-command :ehlo (sock host-name)
  (format-socket sock "EHLO ~A" host-name)
  (read-smtp-return-code sock 250 "in EHLO command"))

(define-smtp-command :helo (sock host-name)
  (format-socket sock "HELO ~A" host-name)
  (read-smtp-return-code sock 250 "in HELO command"))
