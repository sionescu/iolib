;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
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
       (if result
           (return-from read-char/cc result)
           (let/cc k
             (values k :read))))))

(defun/cc read-line/cc (connection)
  ;; TODO this is really naiive
  (bind ((length 80)
         (result (make-array length :element-type 'character :adjustable t :fill-pointer 0)))
    (loop
       (bind ((ch (read-char/cc connection nil :eof)))
         (assert ch)
         (if (eq ch :eof)
             (return (values result t))
             (progn
               (when (char= ch #\Newline)
                 (return (values result nil)))
               (vector-push-extend ch result)))))))

(defun/cc wait-until-fd-ready/cc (file-descriptor event-type)
  (loop
     until (fd-ready-p file-descriptor event-type) do
     (let/cc k
       (values k event-type))))
