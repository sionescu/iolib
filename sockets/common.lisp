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

(declaim (optimize (speed 3) (safety 1) (space 1) (debug 1)))

(in-package #:net.sockets)

(defmacro with-alien-saps ((&rest vars) &body body)
  `(sb-sys:with-pinned-objects ,(mapcar #'second vars)
     (let (,@(mapcar #'(lambda (pair)
                         `(,(first pair) (sb-alien:alien-sap ,(second pair))))
                     vars))
       ,@body)))

(defmacro with-pinned-aliens ((&rest vars) &body body)
  `(sb-alien:with-alien ,vars
     (sb-sys:with-pinned-objects ,(mapcar #'first vars)
       ,@body)))

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(deftype ub8 () `(unsigned-byte 8))
(deftype ub16 () `(unsigned-byte 16))
(deftype ub32 () `(unsigned-byte 32))
(deftype sb8 () `(signed-byte 8))
(deftype sb16 () `(signed-byte 16))
(deftype sb32 () `(signed-byte 32))

(defun string-or-parsed-number (val)
  (let ((tmpval val)
        type)
    (etypecase val
      (ub16
       (values :number val))
      (string
       (multiple-value-bind (parsed pos)
           (parse-integer val :junk-allowed t)
         (if (and parsed
                  (eql pos (length val))) ; the entire string is a number
             (progn
               (setf type :number)
               (setf tmpval parsed))
             (setf type :string))
         (values type tmpval))))))

(defun c->lisp-bool (val)
  (if (zerop val) nil t))

(defun lisp->c-bool (val)
  (if val 1 0))

;;;
;;; Byte-swap functions
;;;

(declaim (inline htons))
(defun htons (short)
#+little-endian
  (let ((newshort 0))
    (declare (type ub16 newshort)
             (type ub16 short))
    (setf (ldb (byte 8 0) newshort) (ldb (byte 8 8) short))
    (setf (ldb (byte 8 8) newshort) (ldb (byte 8 0) short))
    newshort)
#+big-endian short)

(declaim (inline ntohs))
(defun ntohs (short)
  (htons short))

(declaim (inline htonl))
(defun htonl (long)
#+little-endian
  (let ((newlong 0))
    (declare (type ub32 newlong)
             (type ub32 long))
    (setf (ldb (byte 8  0) newlong) (ldb (byte 8 24) long))
    (setf (ldb (byte 8 24) newlong) (ldb (byte 8  0) long))
    (setf (ldb (byte 8  8) newlong) (ldb (byte 8 16) long))
    (setf (ldb (byte 8 16) newlong) (ldb (byte 8  8) long))
    newlong)
#+big-endian long)

(declaim (inline ntohl))
(defun ntohl (long)
  (htonl long))

(defun copy-simple-array-ub16-to-alien-vector (lisp-vec alien-vec)
  (declare (type (simple-array ub16 (8)) lisp-vec)
           (type (alien (array sb-posix::uint16-t 8)) alien-vec))
  (dotimes (i 8)
    (setf (deref alien-vec i)
          (htons (aref lisp-vec i)))))

(defun map-ipv4-to-ipv6 (addr)
  (declare (type (simple-array ub8 (*)) addr))
  (let ((ipv6addr (make-array 8 :element-type 'ub16
                                :initial-element 0)))
    ;; setting the IPv4 marker
    (setf (aref ipv6addr 5) #xFFFF)
    ;; setting the first two bytes
    (setf (aref ipv6addr 6) (+ (ash (aref addr 0) 8)
                               (aref addr 1)))
    ;; setting the last two bytes
    (setf (aref ipv6addr 7) (+ (ash (aref addr 2) 8)
                               (aref addr 3)))

    ipv6addr))

(defun make-sockaddr-in (sin ub8-vector &optional (port 0))
  (declare (type (alien (* sb-posix::sockaddr-in)) sin))
  (sb-posix::memset sin 0 sb-posix::size-of-sockaddr-in)
  (setf (slot sin 'sb-posix::family) sb-posix::af-inet)
  (setf (slot sin 'sb-posix::addr) (htonl (vector-to-ipaddr ub8-vector)))
  (setf (slot sin 'sb-posix::port) (htons port))
  sin)

(defun make-sockaddr-in6 (sin6 ub16-vector &optional (port 0))
  (declare (type (alien (* sb-posix::sockaddr-in6)) sin6))
  (sb-posix::memset sin6 0 sb-posix::size-of-sockaddr-in6)
  (setf (slot sin6 'sb-posix::family) sb-posix::af-inet6)
  (let ((u16-vector (slot (slot (slot sin6 'sb-posix::addr)
                                'sb-posix::in6-u)
                          'sb-posix::addr16)))
    (copy-simple-array-ub16-to-alien-vector ub16-vector u16-vector)
    (setf (slot sin6 'sb-posix::port) (htons port)))
  sin6)

(defun make-sockaddr-un (sun string)
  (declare (type (alien (* sb-posix::sockaddr-un)) sun)
           (type string string))
  (sb-posix::memset sun 0 sb-posix::size-of-sockaddr-un)
  (setf (slot sun 'sb-posix::family) sb-posix::af-unix)
  (let ((buff (sb-ext:string-to-octets string))
        (path (slot sun 'sb-posix::path)))
    (loop
       :for off :below (min (length buff)
                            (1- sb-posix::unix-path-max))
       :do (setf (deref path off) (aref buff off))))
  sun)
