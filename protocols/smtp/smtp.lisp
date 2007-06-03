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

(defparameter *debug* nil)
(defparameter *x-mailer*
  (format nil "(~A ~A)" 
          (lisp-implementation-type)
          (lisp-implementation-version)))

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
  (cl-base64:string-to-base64-string str))

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

(defun make-smtp-socket (host port)
  (make-socket :address-family :internet :type :stream :connect :active
               :remote-host host :remote-port port
               :external-format '(:iso-8859-1 :line-terminator :dos)))

(defun compute-rcpt-command (sock adresses)
  (dolist (to adresses)
    (write-to-smtp sock (format nil "RCPT TO: <~A>" to))
    (multiple-value-bind (code msgstr)
        (read-from-smtp sock)
      (when (/= code 250)
        (error "in RCPT TO command: ~A" msgstr)))))

(defun write-to-smtp (sock command)
  (write-string command sock)
  (terpri sock)
  (finish-output sock))

(defun read-from-smtp (sock)
  (let* ((line (read-line sock))
         (response-code (parse-integer line :start 0 :junk-allowed t)))
    (if (= (char-code (elt line 3)) (char-code #\-))
        (read-from-smtp sock)
        (values response-code line))))

(defun send-smtp (host from to subject message 
                  &key (port 25) cc bcc reply-to extra-headers
                  display-name authentication attachments buffer-size)
  (let ((boundary (make-random-boundary)))
    (with-open-stream (sock (make-smtp-socket host port))
      (open-smtp-connection sock :authentication authentication)
      (write-to-smtp sock (format nil "MAIL FROM: ~@[~A ~]<~A>" display-name from))
      (multiple-value-bind (code msgstr)
          (read-from-smtp sock)
        (when (/= code 250)
          (error "in MAIL FROM command: ~A" msgstr)))
      (compute-rcpt-command sock to)
      (compute-rcpt-command sock cc)
      (compute-rcpt-command sock bcc)
      (write-to-smtp sock "DATA")
      (multiple-value-bind (code msgstr)
          (read-from-smtp sock)
        (when (/= code 354)
          (error "in DATA command: ~A" msgstr)))
      (write-to-smtp sock (format nil "Date: ~A" (get-email-date-string)))
      (write-to-smtp sock (format nil "From: ~@[~A <~]~A~@[>~]" 
                                  display-name from display-name))
      (write-to-smtp sock (format nil "To: ~{ ~a~^,~}" to))
      (when cc
        (write-to-smtp sock (format nil "Cc: ~{ ~a~^,~}" cc)))
      (write-to-smtp sock (format nil "Subject: ~A" subject))
      (write-to-smtp sock (format nil "X-Mailer: cl-smtp ~A" 
                                  *x-mailer*))
      (when reply-to
        (write-to-smtp sock (format nil "Reply-To: ~A" reply-to)))
      (when (and extra-headers
                 (listp extra-headers))
        (dolist (l extra-headers)
          (write-to-smtp sock 
                         (format nil "~A: ~{~a~^,~}" (car l) (rest l)))))
      (write-to-smtp sock "Mime-Version: 1.0")
      (when attachments
        (generate-multipart-header sock boundary))
      (terpri sock)
      (when attachments 
        (setq message (wrap-message-with-multipart-dividers 
                       message boundary)))
      (write-to-smtp sock message)
      (when attachments
        (dolist (attachment attachments)
          (send-attachment sock attachment boundary buffer-size))
        (send-attachments-end-marker sock boundary))
      (write-char #\. sock)
      (terpri sock)
      (finish-output sock)
      (multiple-value-bind (code msgstr)
          (read-from-smtp sock)
        (when (/= code 250)
          (error "Message send failed: ~A" msgstr)))
      (write-to-smtp sock "QUIT")
      (multiple-value-bind (code msgstr)
          (read-from-smtp sock)
        (when (/= code 221)
          (error "in QUIT command:: ~A" msgstr))))))

(defun open-smtp-connection (sock &key authentication)
  (multiple-value-bind (code msgstr)
      (read-from-smtp sock)
    (when (/= code 220)
      (error "wrong response from smtp server: ~A" msgstr)))
  (cond
    (authentication
     (write-to-smtp sock (format nil "EHLO ~A" (et:get-host-name)))
     (multiple-value-bind (code msgstr)
         (read-from-smtp sock)
       (when (/= code 250)
         (error "wrong response from smtp server: ~A" msgstr)))
     (cond
       ((eq (car authentication) :plain)
        (write-to-smtp sock (format nil "AUTH PLAIN ~A" 
                                    (string-to-base64-string
                                     (format nil "~A~C~A~C~A" (cadr authentication)
                                             #\null (cadr authentication) #\null
                                             (caddr authentication)))))
        (multiple-value-bind (code msgstr)
            (read-from-smtp sock)
          (when (/= code 235)
            (error "plain authentication failed: ~A" msgstr))))
       ((eq (car authentication) :login)
        (write-to-smtp sock "AUTH LOGIN")
        (multiple-value-bind (code msgstr)
            (read-from-smtp sock)
          (when (/= code 334)
            (error "login authentication failed: ~A" msgstr)))
        (write-to-smtp sock (string-to-base64-string (cadr authentication)))
        (multiple-value-bind (code msgstr)
            (read-from-smtp sock)
          (when (/= code 334)
            (error "login authentication send username failed: ~A" msgstr)))
        (write-to-smtp sock (string-to-base64-string (caddr authentication)))
        (multiple-value-bind (code msgstr)
            (read-from-smtp sock)
          (when (/= code 235)
            (error "login authentication send password failed: ~A" msgstr))))
       (t
        (error "authentication ~A is not supported in cl-smtp" 
               (car authentication)))))
    (t
     (write-to-smtp sock (format nil "HELO ~A" (et:get-host-name)))
     (multiple-value-bind (code msgstr)
         (read-from-smtp sock)
       (when (/= code 250)
         (error "wrong response from smtp server: ~A" msgstr))))))

(defun get-email-date-string ()
  (multiple-value-bind (sec min h d m y wd) (get-decoded-time)
    (let* ((month (elt '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (- m 1)))
           (weekday (elt '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") wd))
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
