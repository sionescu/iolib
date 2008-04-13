;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; address-arithmetic.lisp --- Arithmetic with addresses and network masks.
;;;
;;; Copyright (C) 2008, Stelian Ionescu  <sionescu@common-lisp.net>
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

(in-package :net.sockets)

(defun make-subnet-mask (&key cidr class)
  (assert (or cidr class) (cidr class) "You must either specify a CIDR or a network class.")
  (when cidr (check-type cidr (mod 33) "a number between 0 and 32"))
  (when class (check-type class (member :a :b :c) "a valid network class - one of :A, :B or :C"))
  (let ((mask #xFFFFFFFF))
    (declare (type ub32 mask))
    (when class (setf cidr (case class
                             (:a 8)
                             (:b 16)
                             (:c 24))))
    (setf (ldb (byte (- 32 cidr) 0) mask) 0)
    (make-instance 'ipv4-address :name (integer-to-vector mask))))

(defun ensure-subnet-mask (thing)
  "If THING is of type IPV4-ADDRESS it is returned as is; if keyword it must be one of
:A, :B or :C otherwise it's treated as a CIDR suffix."
  (etypecase thing
    (ipv4-address  thing)
    (unsigned-byte (make-subnet-mask :cidr thing))
    (keyword       (make-subnet-mask :class thing))))

(defgeneric inet-address-network-portion (address mask)
  (:documentation "Apply network mask MASK to ADDRESS in order to calculate the
network part of ADDRESS.")
  (:method ((address ipv4-address) mask)
    (setf mask (ensure-subnet-mask mask))
    (let ((v (make-array 4 :element-type 'ub8))
          (av (address-name address))
          (mv (address-name mask)))
      (dotimes (i 4)
        (setf (aref v i)
              (logand (aref av i)
                      (aref mv i))))
      (make-instance 'ipv4-address :name v))))

(defgeneric inet-address-host-portion (address mask)
  (:documentation "Apply network mask MASK to ADDRESS in order to calculate the
host part of ADDRESS.")
  (:method ((address ipv4-address) mask)
    (setf mask (ensure-subnet-mask mask))
    (let ((v (make-array 4 :element-type 'ub8))
          (av (address-name address))
          (mv (address-name mask)))
      (dotimes (i 4)
        (setf (aref v i)
              (logand (aref av i)
                      (logxor (aref mv i) 255))))
      (make-instance 'ipv4-address :name v))))

(defgeneric inet-address-in-network-p (address network mask)
  (:documentation "Return T if ADDRESS is part of the subnet specified by
NETWORK and MASK.")
  (:method ((address ipv4-address) (network ipv4-address) mask)
    (setf mask (ensure-subnet-mask mask))
    (address= (inet-address-network-portion address mask)
              (inet-address-network-portion network mask))))

(defgeneric inet-addresses-in-same-network-p (address1 address2 network mask)
  (:documentation "Return T if ADDRESS1 and ADDRESS2 are both part part of the
subnet specified by NETWORK and MASK.")
  (:method ((address1 ipv4-address) (address2 ipv4-address) (network ipv4-address) mask)
    (setf mask (ensure-subnet-mask mask))
    (let ((address1-network (inet-address-network-portion address1 mask))
          (address2-network (inet-address-network-portion address2 mask))
          (subnet (inet-address-network-portion network mask)))
      (and (address= address1-network subnet)
           (address= address2-network subnet)))))

(defgeneric inet-address-network-class (address)
  (:documentation "Return the network class of ADDRESS: one of :A, :B, :C, :D OR :E .")
  (:method ((address ipv4-address))
    (let ((octet (aref (address-name address) 0)))
      (cond
        ((= #b0000 (ldb (byte 1 7) octet)) :a)
        ((= #b0010 (ldb (byte 2 6) octet)) :b)
        ((= #b0110 (ldb (byte 3 5) octet)) :c)
        ((= #b1110 (ldb (byte 4 4) octet)) :d)
        ((= #b1111 (ldb (byte 4 4) octet)) :e)))))

(defgeneric inet-address-private-p (address)
  (:documentation "Returns T if ADDRESS is in a private network range.
Private IPv4 networks are 10.0.0.0/8, 172.16.0.0/12 and 192.168.0.0/16.
See http://en.wikipedia.org/wiki/Private_network for details.")
  (:method ((address ipv4-address))
    (let* ((address-name (address-name address))
           (first (aref address-name 0))
           (second (aref address-name 1)))
      (values (or (= first 10)
                  (and (= first 172)
                       (<= 16 second 31))
                  (and (= first 192)
                       (= second 168)))
              (inet-address-network-class address))))
  (:method ((address address))
    nil))
