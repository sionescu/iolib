;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (C) 2006,2007 by Stelian Ionescu                            ;
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

(in-package :net.sockets)

(deftype ub8 () `(unsigned-byte 8))
(deftype ub16 () `(unsigned-byte 16))
(deftype ub32 () `(unsigned-byte 32))
(deftype sb8 () `(signed-byte 8))
(deftype sb16 () `(signed-byte 16))
(deftype sb32 () `(signed-byte 32))

(defun parse-number-or-nil (value &optional (type :any) (radix 10))
  (check-type value (or string unsigned-byte))
  (let ((parsed
         (if (stringp value)
             (ignore-errors (parse-integer value :radix radix
                                           :junk-allowed nil))
             value)))
    (if parsed
        ;; if it's a number and its type is ok return it
        (and (ecase type
               (:any  t)
               (:ub8  (typep parsed 'ub8))
               (:ub16 (typep parsed 'ub16))
               (:ub32 (typep parsed 'ub32)))
             parsed)
        ;; otherwise nil
        nil)))

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
  (check-type short ub16 "a 16-bit unsigned number")
  #+little-endian
  (logior (ash (logand (the ub16 short) #x00FF) 8)
          (ash (logand (the ub16 short) #xFF00) -8))
  #+big-endian short)

(defun ntohs (short)
  (htons short))

(defun htonl (long)
  (check-type long ub32 "a 32-bit unsigned number")
  #+little-endian
  (logior (ash (logand (the ub32 long) #x000000FF) 24)
          (ash (logand (the ub32 long) #x0000FF00) 8)
          (ash (logand (the ub32 long) #x00FF0000) -8)
          (ash (logand (the ub32 long) #xFF000000) -24))
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
  (et:bzero sin et:size-of-sockaddr-in)
  (let ((tmp port))
    (with-foreign-slots ((et:family et:address et:port) sin et:sockaddr-in)
      (setf et:family et:af-inet)
      (setf et:address (htonl (vector-to-ipaddr ub8-vector)))
      (setf et:port (htons tmp))))
  sin)

(defun make-sockaddr-in6 (sin6 ub16-vector &optional (port 0))
  (et:bzero sin6 et:size-of-sockaddr-in6)
  (let ((tmp port))
    (with-foreign-slots ((et:family et:address et:port) sin6 et:sockaddr-in6)
      (setf et:family et:af-inet6)
      (copy-simple-array-ub16-to-alien-vector ub16-vector et:address)
      (setf et:port (htons tmp))))
  sin6)

(defun make-sockaddr-un (sun string)
  (check-type string string)
  (et:bzero sun et:size-of-sockaddr-un)
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

(defun sockaddr-in->sockaddr (sin)
  (with-foreign-slots ((et:address et:port) sin et:sockaddr-in)
    (values (make-instance 'ipv4addr
                           :name (make-vector-u8-4-from-in-addr et:address))
            (ntohs et:port))))

(defun sockaddr-in6->sockaddr (sin6)
  (with-foreign-slots ((et:address et:port) sin6 et:sockaddr-in6)
    (values (make-instance 'ipv6addr
                           :name (make-vector-u16-8-from-in6-addr et:address))
            (ntohs et:port))))

(defun sockaddr-un->sockaddr (sun)
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
      (make-instance 'localaddr
                     :name name
                     :abstract abstract))))

(defun sockaddr-storage->sockaddr (ss)
  (with-foreign-slots ((et:family) ss et:sockaddr-storage)
    (ecase et:family
      (#.et:af-inet
       (sockaddr-in->sockaddr ss))
      (#.et:af-inet6
       (sockaddr-in6->sockaddr ss))
      (#.et:af-local
       (sockaddr-un->sockaddr ss)))))

(defun sockaddr->sockaddr-storage (ss sockaddr &optional (port 0))
  (etypecase sockaddr
    (ipv4addr
     (make-sockaddr-in ss (name sockaddr) port))
    (ipv6addr
     (make-sockaddr-in6 ss (name sockaddr) port))
    (localaddr
     (make-sockaddr-un ss (name sockaddr)))))
