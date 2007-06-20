;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;   Copyright (C) 2006, 2007 Stelian Ionescu
;;
;;   This code is free software; you can redistribute it and/or
;;   modify it under the terms of the version 2.1 of
;;   the GNU Lesser General Public License as published by
;;   the Free Software Foundation, as clarified by the
;;   preamble found here:
;;       http://opensource.franz.com/preamble.html
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU Lesser General
;;   Public License along with this library; if not, write to the
;;   Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;   Boston, MA 02110-1301, USA

(in-package :net.sockets)

;;;;;;;;;;;;;
;;;       ;;;
;;; Types ;;;
;;;       ;;;
;;;;;;;;;;;;;

(deftype ipv4-array ()
  '(ub8-sarray 4))
(deftype ipv6-array ()
  '(ub16-sarray 8))

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

;;;
;;; Conversion between address formats
;;;

(defun copy-simple-array-ub16-to-alien-vector (lisp-vec alien-vec)
  (declare (type ipv6-array lisp-vec))
  (dotimes (i 8)
    (setf (mem-aref alien-vec :uint16 i)
          (htons (aref lisp-vec i)))))

(defun map-ipv4-vector-to-ipv6 (addr)
  (declare (type ipv4-array addr))
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
  "Convert a vector to a 32-bit unsigned integer."
  (coercef vector 'ipv4-array)
  (+ (ash (aref vector 0) 24)
     (ash (aref vector 1) 16)
     (ash (aref vector 2) 8)
     (aref vector 3)))

(defun ipaddr-to-vector (ipaddr)
  "Convert a 32-bit unsigned integer to a vector."
  (check-type ipaddr ub32)
  (let ((vector (make-array 4 :element-type 'ub8)))
    (setf (aref vector 0) (ldb (byte 8 24) ipaddr)
          (aref vector 1) (ldb (byte 8 16) ipaddr)
          (aref vector 2) (ldb (byte 8  8) ipaddr)
          (aref vector 3) (ldb (byte 8  0) ipaddr))
    vector))

(defun in6-addr-to-ipv6-array (in6-addr)
  (let ((vector (make-array 8 :element-type 'ub16)))
    (dotimes (i 8)
      (setf (aref vector i)
            (ntohs (mem-aref in6-addr :uint16 i))))
    vector))

;;;
;;; Constructors for SOCKADDR_* structs
;;;

(defun make-sockaddr-in (sin ub8-vector &optional (port 0))
  (declare (type ipv4-array ub8-vector)
           (type ub16 port))
  (et:bzero sin et:size-of-sockaddr-in)
  (with-foreign-slots ((et:family et:addr et:port) sin et:sockaddr-in)
    (setf et:family et:af-inet)
    (setf et:addr (htonl (vector-to-ipaddr ub8-vector)))
    (setf et:port (htons port)))
  (values sin))

(defmacro with-sockaddr-in ((var address &optional port) &body body)
  `(with-foreign-object (,var 'et:sockaddr-in)
     (make-sockaddr-in ,var ,address ,port)
     ,@body))

(defun make-sockaddr-in6 (sin6 ub16-vector &optional (port 0))
  (declare (type ipv6-array ub16-vector)
           (type ub16 port))
  (et:bzero sin6 et:size-of-sockaddr-in6)
  (with-foreign-slots ((et:family et:addr et:port) sin6 et:sockaddr-in6)
    (setf et:family et:af-inet6)
    (copy-simple-array-ub16-to-alien-vector ub16-vector et:addr)
    (setf et:port (htons port)))
  (values sin6))

(defmacro with-sockaddr-in6 ((var address &optional port) &body body)
  `(with-foreign-object (,var 'et:sockaddr-in6)
     (make-sockaddr-in6 ,var ,address ,port)
     ,@body))

(defun make-sockaddr-un (sun string)
  (declare (type string string))
  (et:bzero sun et:size-of-sockaddr-un)
  (with-foreign-slots ((et:family et:path) sun et:sockaddr-un)
    (setf et:family et:af-local)
    (with-foreign-string (c-string string)
      (loop
         :for off :below (1- et:unix-path-max)
         :do (setf (mem-aref et:path :uint8 off)
                   (mem-aref c-string :uint8 off)))))
  (values sun))

(defmacro with-sockaddr-un ((var address) &body body)
  `(with-foreign-object (,var 'et:sockaddr-un)
     (make-sockaddr-un ,var ,address)
     ,@body))

;;;
;;; Conversion functions for SOCKADDR_* structs
;;;

(defun sockaddr-in->sockaddr (sin)
  (with-foreign-slots ((et:addr et:port) sin et:sockaddr-in)
    (values (make-instance 'ipv4addr
                           :name (ipaddr-to-vector (ntohl et:addr)))
            (ntohs et:port))))

(defun sockaddr-in6->sockaddr (sin6)
  (with-foreign-slots ((et:addr et:port) sin6 et:sockaddr-in6)
    (values (make-instance 'ipv6addr
                           :name (in6-addr-to-ipv6-array et:addr))
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

;;;
;;; Misc
;;;

(defun %to-octets (buff ef start end)
  (io.encodings:string-to-octets buff :external-format ef
                                 :start start :end end))

(defun parse-number-or-nil (value &optional (type :any) (radix 10))
  (check-type value (or string unsigned-byte))
  (let ((parsed
         (if (stringp value)
             (ignore-errors (parse-integer value :radix radix
                                           :junk-allowed nil))
             value)))
    (and parsed
         ;; if it's a number and its type is ok return it
         (typep parsed (ecase type
                         (:any  t)     (:ub8  'ub8)
                         (:ub16 'ub16) (:ub32 'ub32)))
         (values parsed))))

(defun c->lisp-bool (val)
  (if (zerop val) nil t))

(defun lisp->c-bool (val)
  (if val 1 0))

(defun addrerr-value (keyword)
  (foreign-enum-value 'et:addrinfo-errors keyword))

(defun unixerr-value (keyword)
  (foreign-enum-value 'et:errno-values keyword))

(defmacro with-socklen ((var value) &body body)
  `(with-foreign-object (,var 'et:socklen)
     (setf (mem-ref ,var 'et:socklen) ,value)
     ,@body))
