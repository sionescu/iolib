;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (C) 2006 by Stelian Ionescu                                 ;
;                                                                         ;
;   This program is free software; you can redistribute it and/or modify  ;
;   it under the terms of the GNU General Public License as published by  ;
;   the Free Software Foundation; either version 2 of the License, or     ;
;   (at your option) any later version.                                   ;
;                                                                         ;
;   This program is distributed in the hope that it will be useful,       ;
;   but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;   GNU General Public License for more details.                          ;
;                                                                         ;
;   You should have received a copy of the GNU General Public License     ;
;   along with this program; if not, write to the                         ;
;   Free Software Foundation, Inc.,                                       ;
;   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declaim (optimize (speed 2) (safety 2) (space 1) (debug 2)))
(declaim (optimize (speed 0) (safety 2) (space 0) (debug 2)))

(in-package #:io.multiplex)

(defstruct (handler
             (:constructor make-handler (fd read-func write-func except-func))
             (:copier nil))
  (fd            0 :type et:select-file-descriptor)
  (read-func   nil :type (or function null))
  (write-func  nil :type (or function null))
  (except-func nil :type (or function null)))

(defclass multiplex-interface ()
  ((fd-handlers :initform (make-hash-table :test 'eql))))

(defgeneric set-handlers (multiplex-interface fd &key &allow-other-keys)
  (:method-combination progn :most-specific-last))

(defgeneric remove-handlers (multiplex-interface fd &key &allow-other-keys)
  (:method-combination progn :most-specific-first))

(defgeneric serve-events (multiplex-interface))

(eval-when (:compile-toplevel :load-toplevel :execute)
    (defvar *multiplex-available-interfaces* nil)
    (defvar *multiplex-active-interface* nil))
