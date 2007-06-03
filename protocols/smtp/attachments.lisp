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

;;; File: attachments.lisp
;;; Description: encoding and transmitting login to include a mime attachment

;;;
;;; Contributed by Brian Sorg
;;;
;;; Thanks to David Cooper for make-random-boundary
;;;
(in-package :net.smtp-client)

;;; Addition to allow for sending mime attachments along with the smtp message 

;;---- Initialize array of possible boundary characters to make start of attachments
(defparameter *boundary-chars*
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defun make-random-boundary (&optional (length 50) (boundary-chars *boundary-chars*))
  (let ((boundary (make-string length))
        (chars-length (length boundary-chars)))
    (dotimes (i length boundary)
      (setf (aref boundary i) (char *boundary-chars* (random chars-length))))))

(defun generate-multipart-header (sock boundary)
  (write-to-smtp sock 
                 (format nil "Content-type: multipart/mixed;~%~tBoundary=\"~a\"" 
                         boundary)))

(defun wrap-message-with-multipart-dividers (message boundary)
  (concatenate 'string (format nil "--~a~%" boundary)
               (format nil "Content-type: text/plain~%")
               (format nil "Content-Disposition: inline~%")
               (format nil "~%")
               message (format nil "~%")))

(defun send-attachment (sock attachment boundary buffer-size)
  (when (probe-file attachment)
    (let ((name (file-namestring attachment)))
      (send-attachment-header sock boundary name)
      (base64-encode-file attachment sock :buffer-size buffer-size))))

(defun send-attachment-header (sock boundary name)
  (write-to-smtp
   sock 
   (format nil "~%--~a~%Content-type: application/octet-stream;~%~tname=\"~a\"~%Content-Transfer-Encoding: base64~%Content-Disposition: attachment; filename=\"~a\"~%"
           boundary name name)))

(defun send-attachments-end-marker (sock boundary)
  (write-to-smtp sock (format nil "~%--~a--~%" boundary)))
 
(defun base64-encode-file (file-in sock
                           &key 
                           (buffer-size 256) ;; in KB
                           (wrap-at-column 76))
  (let* ((max-buffer-size (* buffer-size 1024))
         (byte-count 0)
         (buffer (make-array max-buffer-size 
                             :element-type '(unsigned-byte 8))))
    (when (probe-file file-in)
      ;;-- open filein ---------
      (with-open-file (strm-in file-in
                               :element-type '(unsigned-byte 8))        
        (loop	  
           (setq byte-count 0)
           ;; read a portion of the file into the buffer 
           (setq byte-count (dotimes (i max-buffer-size max-buffer-size)
                              (let ((bchar (read-byte strm-in nil 'EOF)))
                                (if (eql bchar 'EOF)
                                    (return i)
                                    (setf (aref buffer i) bchar))))) 
           ;; encode the buffer and write out to stream 
           (cl-base64:usb8-array-to-base64-stream 
            (if (< byte-count max-buffer-size)
                (trimmed-buffer byte-count buffer)
                buffer)
            sock :columns wrap-at-column)
           (finish-output sock)
           ;;-- when finished reading exit do loop 
           (when (< byte-count max-buffer-size)
             (return)))))))

(defun trimmed-buffer (byte-count buffer)
  "Creates an array the length of byte-count and copies contents of buffer into it. 
Needed in Lispworks, Lispworks initialized all elements of the buffer array when it was made, allegro doesn't
seem to have this behavior"
  (let ((trimmed-buffer (make-array byte-count :element-type '(unsigned-byte 8))))
    (dotimes (i byte-count trimmed-buffer)
      (setf (aref trimmed-buffer i) (aref buffer i)))))
        
    
                                   
                                   



