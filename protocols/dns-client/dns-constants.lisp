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

(in-package :net.sockets)

(define-constant +opcode-standard+ 0)

(define-constant +query-type-map
  '((:a     .   1)
    (:ns    .   2)
    (:cname .   5)
    (:soa   .   6)
    (:wks   .  11)
    (:ptr   .  12)
    (:hinfo .  13)
    (:mx    .  15)
    (:txt   .  16)
    (:aaaa  .  28)
    (:any   . 255)))

(defun query-type-number (id)
  (cdr (assoc id +query-type-map)))

(defun query-type-id (number)
  (car (rassoc number +query-type-map)))

(defun valid-type-p (id)
  (query-type-number id))

(define-constant +query-class-map
  '((:in  .   1)
    (:any . 255)))

(defun query-class-number (id)
  (cdr (assoc id +query-class-map)))

(defun query-class-id (number)
  (car (rassoc number +query-class-map)))

(define-constant +rcode-map
  '((:no-error        . 0)
    (:format-error    . 1)
    (:server-failure  . 2)
    (:name-error      . 3)
    (:not-implemented . 4)
    (:refused         . 5)))

(defun rcode-number (id)
  (cdr (assoc id +rcode-map)))

(defun rcode-id (number)
  (car (rassoc number +rcode-map)))

(define-constant +dns-datagram-size+ 512)
