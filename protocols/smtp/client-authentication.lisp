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
  (defvar *smtp-authenticators* (make-hash-table :test #'eq)))

(defun invoke-authentication (name args)
  (check-type args cons)
  (let ((auth-fun (gethash name *smtp-authenticators*)))
    (if auth-fun
        (funcall auth-fun args)
        (error "Unknown authentication method: ~A" name))))

(defmacro defauthentication (name (socket args) &body body)
  `(setf (gethash ,name *smtp-authenticators*)
         #'(lambda (,socket ,args)
             ,@body)))

(defauthentication :plain (sock args)
  (format-socket sock "AUTH PLAIN ~A" 
                 (string-to-base64-string
                  (format nil "~A~C~A~C~A" (first args)
                          #\Null (first args) #\Null
                          (second args))))
  (read-smtp-return-code sock 235 "Plain authentication failed"))

(defauthentication :login (sock args)
  (write-to-smtp sock "AUTH LOGIN")
  (read-smtp-return-code sock 334 "Login authentication start failed")
  (write-to-smtp sock (string-to-base64-string (first args)))
  (read-smtp-return-code sock 334 "Login authentication username send failed")
  (write-to-smtp sock (string-to-base64-string (second args)))
  (read-smtp-return-code sock 235 "Login authentication password send failed"))
