;;; -*- mode: Lisp -*-
	
;;; This file is part of CL-SMTP, the Lisp SMTP Client

;;; Copyright (C) 2004/2005 Jan Idzikowski

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser General Public License
;;; (http://opensource.franz.com/preamble.html), known as the LLGPL.

;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; Lisp Lesser GNU General Public License for more details.

;;; File: cl-smtp.lisp
;;; Description: main smtp client logic

(in-package :net.smtp-client)

(defvar *x-mailer*
  (format nil "(~A ~A)" 
          (lisp-implementation-type)
          (lisp-implementation-version)))

;;;
;;; Protocol handling
;;;

(defun check-arg (arg name)
  (cond
    ((or (stringp arg)
         (pathnamep arg))
     (list arg))
    ((listp arg)
     arg)
    (t
     (error "the \"~A\" argument is not a string or cons" name))))

(defun mask-dot (str)
  "replace \r\n.\r\n with \r\n..\r\n"
  (let ((dotstr (format nil "~C.~C" #\NewLine #\NewLine))
        (maskdotsr (format nil "~C..~C" #\NewLine #\NewLine))
        (resultstr ""))
    (labels ((mask (tempstr)
               (let ((n (search dotstr tempstr)))
                 (cond
                   (n
                    (setf resultstr (concatenate 'string resultstr 
                                                 (subseq tempstr 0 n)
                                                 maskdotsr))
                    (mask (subseq tempstr (+ n 3))))
                   (t
                    (setf resultstr (concatenate 'string resultstr 
                                                 tempstr)))))))
      (mask str))
    resultstr))

(defun string-to-base64-string (str)
  (string-to-base64-string str))

(defun send-email (host from to subject message 
                   &key (port 25) cc bcc reply-to extra-headers
                   display-name authentication
                   attachments (buffer-size 256))
  (send-smtp host from (check-arg to "to") subject (mask-dot message)
             :port port :cc (check-arg cc "cc") :bcc (check-arg bcc "bcc")
             :reply-to reply-to 
             :extra-headers extra-headers
             :display-name display-name
             :authentication authentication
             :attachments (check-arg attachments "attachments")
             :buffer-size (if (numberp buffer-size) 
                              buffer-size
                              256)))

(defun send-smtp (host from to subject message 
                  &key (port 25) cc bcc reply-to extra-headers
                  display-name authentication attachments buffer-size)
  (with-open-stream (sock (make-smtp-socket host port))
    (open-smtp-connection sock authentication)
    (send-message-envelope sock from to cc bcc)
    (invoke-smtp-command :data sock)
    (send-message-headers sock from to subject cc reply-to extra-headers display-name)
    (send-message-body sock message attachments buffer-size)
    (invoke-smtp-command :quit sock)))

(defun open-smtp-connection (sock authentication)
  (read-smtp-return-code sock 220 "Wrong response from smtp server")
  (cond
    (authentication
     (invoke-smtp-command :ehlo sock (et:get-host-name))
     (invoke-authentication (first authentication) (rest authentication)))
    (t
     (invoke-smtp-command :helo sock (et:get-host-name)))))

(defun send-message-envelope (sock from to cc bcc)
  (invoke-smtp-command :mail-from sock from)
  (invoke-smtp-command :rcpt-to sock to)
  (invoke-smtp-command :rcpt-to sock cc)
  (invoke-smtp-command :rcpt-to sock bcc))

(defun send-message-headers (sock from to subject cc reply-to extra-headers display-name)
  (format-socket sock "Date: ~A" (get-email-date-string))
  (format-socket sock "From: ~@[~A <~]~A~@[>~]" 
                 display-name from display-name)
  (format-socket sock "To: ~{ ~a~^,~}" to)
  (when cc
    (format-socket sock "Cc: ~{ ~A~^,~}" cc))
  (format-socket sock "Subject: ~A" subject)
  (format-socket sock "X-Mailer: cl-smtp ~A"  *x-mailer*)
  (when reply-to
    (format-socket sock "Reply-To: ~A" reply-to))
  (dolist (l extra-headers)
    (format-socket sock "~A: ~{~A~^,~}" (car l) (rest l)))
  (write-to-smtp sock "Mime-Version: 1.0"))

(defun send-message-body (sock message attachments buffer-size)
  (let ((boundary (make-random-boundary)))
    (when attachments
      (generate-multipart-header sock boundary)
      (terpri sock)
      (setf message (wrap-message-with-multipart-dividers 
                     message boundary)))
    (write-to-smtp sock message)
    (when attachments
      (dolist (attachment attachments)
        (send-attachment sock attachment boundary buffer-size))
      (send-attachments-end-marker sock boundary))
    (write-char #\. sock) (terpri sock) (finish-output sock)
    (read-smtp-return-code sock 250 "Message send failed")))

(defun get-email-date-string ()
  (multiple-value-bind (sec min h d m y wd) (get-decoded-time)
    (let* ((month (aref #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (- m 1)))
           (weekday (aref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") wd))
           (timezone (get-timezone-from-integer
                      (- (encode-universal-time sec min h d m y 0)
                         (get-universal-time)))))
      (format nil "~A, ~2,'0d ~A ~d ~2,'0d:~2,'0d:~2,'0d ~D" 
              weekday d month y h min sec timezone))))

(defun get-timezone-from-integer (x)
  (let ((min (/ x 60))
        (hour (/ x 3600)))
    (if (integerp hour)
        (cond
          ((>= hour 0)
           (format nil "+~2,'0d00" hour))
          ((< hour 0)
           (format nil "-~2,'0d00" (* -1 hour))))
        (multiple-value-bind (h m) (truncate min 60)
          (cond
            ((>= hour 0)
             (format nil "+~2,'0d~2,'0d" h (truncate m)))
            ((< hour 0)
             (format nil "-~2,'0d~2,'0d" (* -1 h) (* -1 (truncate m)))))))))
