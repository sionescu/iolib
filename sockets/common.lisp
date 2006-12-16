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

;; (declaim (optimize (speed 3) (safety 1) (space 1) (debug 1)))
(declaim (optimize (speed 0) (safety 2) (space 0) (debug 2)))

(in-package :net.sockets)

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

(defun addrerr-value (keyword)
  (foreign-enum-value 'et:addrinfo-errors keyword))

(defun unixerr-value (keyword)
  (foreign-enum-value 'et:errno-values keyword))

(defun xor (x1 x2)
  (not (eql (not x1) (not x2))))

;;;
;;; Byte-swap functions
;;;

(defun htons (short)
#+little-endian
  (let ((newshort 0))
    (declare (type ub16 newshort)
             (type ub16 short))
    (setf (ldb (byte 8 0) newshort) (ldb (byte 8 8) short))
    (setf (ldb (byte 8 8) newshort) (ldb (byte 8 0) short))
    newshort)
#+big-endian short)

(defun ntohs (short)
  (htons short))

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

(defun ntohl (long)
  (htonl long))

(defun copy-simple-array-ub16-to-alien-vector (lisp-vec alien-vec)
  (declare (type (simple-array ub16 (8)) lisp-vec))
  (dotimes (i 8)
    (setf (mem-aref alien-vec :uint16 i)
          (htons (aref lisp-vec i)))))

(defun map-ipv4-vector-to-ipv6 (addr)
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

;; From CLOCC's PORT library
(defun vector-to-ipaddr (vector)
  (coerce vector '(simple-array ub8 (4)))
  (+ (ash (aref vector 0) 24)
     (ash (aref vector 1) 16)
     (ash (aref vector 2) 8)
     (aref vector 3)))

(defun make-sockaddr-in (sin ub8-vector &optional (port 0))
  (et:memset sin 0 #.(foreign-type-size 'et:sockaddr-in))
  (let ((tmp port))
    (with-foreign-slots ((et:family et:address et:port) sin et:sockaddr-in)
      (setf et:family et:af-inet)
      (setf et:address (htonl (vector-to-ipaddr ub8-vector)))
      (setf et:port (htons tmp))))
  sin)

(defun make-sockaddr-in6 (sin6 ub16-vector &optional (port 0))
  (et:memset sin6 0 #.(foreign-type-size 'et:sockaddr-in6))
  (let ((tmp port))
    (with-foreign-slots ((et:family et:address et:port) sin6 et:sockaddr-in6)
      (setf et:family et:af-inet6)
      (copy-simple-array-ub16-to-alien-vector ub16-vector et:address)
      (setf et:port (htons tmp))))
  sin6)

(defun make-sockaddr-un (sun string)
  (check-type string string)
  (et:memset sun 0 (foreign-type-size 'et:sockaddr-un))
  (with-foreign-slots ((et:family et:path) sun et:sockaddr-un)
    (setf et:family et:af-local)
    (with-foreign-string (c-string string)
      (loop
         :for off :below (1- et:unix-path-max)
         :do (setf (mem-aref et:path :uint8 off)
                   (mem-aref c-string :uint8 off)))))
  sun)

(defun make-vector-u8-4-from-in-addr (in-addr)
  (check-type in-addr ub32)
  (let ((vector (make-array 4 :element-type 'ub8)))
    (setf in-addr (ntohl in-addr))
    (setf (aref vector 0) (ldb (byte 8 24) in-addr))
    (setf (aref vector 1) (ldb (byte 8 16) in-addr))
    (setf (aref vector 2) (ldb (byte 8  8) in-addr))
    (setf (aref vector 3) (ldb (byte 8  0) in-addr))
    vector))

(defun make-vector-u16-8-from-in6-addr (in6-addr)
  (let ((newvector (make-array 8 :element-type 'ub16)))
    (dotimes (i 8)
      (setf (aref newvector i)
            (ntohs (mem-aref in6-addr :uint16 i))))
    newvector))

(defun sockaddr-in->netaddr (sin)
  (with-foreign-slots ((et:address et:port) sin et:sockaddr-in)
    (values (make-instance 'ipv4addr
                           :name (make-vector-u8-4-from-in-addr et:address))
            (ntohs et:port))))

(defun sockaddr-in6->netaddr (sin6)
  (with-foreign-slots ((et:address et:port) sin6 et:sockaddr-in6)
    (values (make-instance 'ipv6addr
                           :name (make-vector-u16-8-from-in6-addr et:address))
            (ntohs et:port))))

(defun sockaddr-un->netaddr (sun)
  (with-foreign-slots ((et:path) sun et:sockaddr-un)
    (let ((name (make-string (1- et:unix-path-max)))
          (abstract nil))
      (if (zerop (mem-aref et:path :uint8 0))
          ;; abstract address
          (progn
            (setf abstract t)
            (loop
               :for sindex :from 0 :below (1- et:unix-path-max)
               :for pindex :from 1 :below et:unix-path-max
               :do (setf (schar name sindex)
                         (code-char (mem-aref et:path :uint8 pindex)))))
          ;; address is in the filesystem
          (setf name (foreign-string-to-lisp et:path)))
      (make-instance 'unixaddr
                     :name name
                     :abstract abstract))))

(defun sockaddr-storage->netaddr (ss)
  (with-foreign-slots ((et:family) ss et:sockaddr-storage)
    (ecase et:family
      (#.et:af-inet
       (sockaddr-in->netaddr ss))
      (#.et:af-inet6
       (sockaddr-in6->netaddr ss))
      (#.et:af-local
       (sockaddr-un->netaddr ss)))))

(defun netaddr->sockaddr-storage (ss netaddr &optional (port 0))
  (etypecase netaddr
    (ipv4addr
     (make-sockaddr-in ss (name netaddr) port))
    (ipv6addr
     (make-sockaddr-in6 ss (name netaddr) port))
    (localaddr
     (make-sockaddr-un ss (name netaddr)))))
