;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: NIL -*-
;;;
;;; Copyright (C) 2006-2008, Attila Lendvai  <attila.lendvai@gmail.com>
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

(in-package :net.sockets.cc)

(defun/cc read-char/cc (connection &optional eof-error-p eof-value recursive-p)
  (loop
     (bind ((result (read-char-no-hang connection eof-error-p eof-value recursive-p)))
       (if (or result
               (and eof-value
                    (eq result eof-value)))
           (return-from read-char/cc result)
           (let/cc k
             (values k :read))))))

(defun/cc read-line/cc (connection &optional eof-error-p eof-value recursive-p)
  ;; TODO lame, but works for now
  (bind ((length 80)
         (result (make-array length :element-type 'character :adjustable t :fill-pointer 0)))
    (loop
       (bind ((ch (read-char/cc connection eof-error-p eof-value recursive-p)))
         (if (eq ch eof-value)
             (return (values eof-value t))
             (progn
               (when (char= ch #\Newline)
                 (return (values result nil)))
               (vector-push-extend ch result)))))))

(defun/cc wait-until-fd-ready/cc (file-descriptor event-type)
  (loop
     :until (fd-ready-p file-descriptor event-type)
     :do (let/cc k
           (values k event-type))))

(defun/cc write-char/cc (character connection)
  ;; TODO lame, but works for now
  (write-string/cc (make-string 1 :initial-element character) connection)
  character)

(defun/cc write-string/cc (string connection)
  (bind ((buffer (babel:string-to-octets string :encoding (external-format-of connection)))
         (bytes-to-write (length buffer))
         (start 0))
    (loop
       while (> bytes-to-write 0) do
       (bind ((written-count (%write-ub8-vector-without-blocking
                              connection buffer start bytes-to-write)))
         (if (zerop written-count)
             (let/cc k
               (values k :write))
             (progn
               (decf bytes-to-write written-count)
               (incf start written-count))))))
  string)

(defun %write-ub8-vector-without-blocking (connection buffer start count)
  (handler-case
      (progn
        ;; TODO fix error handling in io.streams::%write-simple-array-ub8
        (io.streams::%write-simple-array-ub8 connection buffer start count)
        count)
    (nix:ewouldblock ()
      0)))

(defun/cc write-line/cc (string connection)
  (write-string/cc string connection)
  ;; TODO handle different line endings through babel
  (write-char/cc #\Newline connection)
  string)
