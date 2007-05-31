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


;;;
;;; Conversion functions
;;;

(defun ipaddr-to-dotted (ipaddr)
  "Convert a 32-bit unsigned integer to a dotted string."
  (check-type ipaddr ub32)
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (princ (ldb (byte 8 24) ipaddr) s) (princ #\. s)
      (princ (ldb (byte 8 16) ipaddr) s) (princ #\. s)
      (princ (ldb (byte 8 8)  ipaddr) s) (princ #\. s)
      (princ (ldb (byte 8 0)  ipaddr) s))))

(defun dotted-to-ipaddr (address)
  "Convert a dotted IPv4 address to a 32-bit unsigned integer."
  (vector-to-ipaddr (dotted-to-vector address)))

(defun dotted-to-vector (address)
  "Convert a dotted IPv4 address to a (simple-array (unsigned-byte 8) 4)."
  (check-type address string)
  (let ((addr (make-array 4 :element-type 'ub8 :initial-element 0))
        (split (split-sequence #\. address :count 5)))
    (flet ((set-array-value (index str)
             (setf (aref addr index)
                   (or (parse-number-or-nil str :ub8)
                       (error 'parse-error)))))
      (let ((len (length split)))
        (unless (<= 1 len 4) (error 'parse-error))
        (set-array-value 3 (nth (1- len) split))
        (loop :for n :in split
              :for index :below (1- len)
           :do (set-array-value index n))))
    (values addr)))

(defun vector-to-dotted (vector)
  "Convert an 4-element vector to a dotted string."
  (coercef vector 'ipv4-array)
  (let ((*print-pretty* nil))
    (with-output-to-string (s)
      (princ (aref vector 0) s) (princ #\. s)
      (princ (aref vector 1) s) (princ #\. s)
      (princ (aref vector 2) s) (princ #\. s)
      (princ (aref vector 3) s))))

(defun colon-separated-to-vector (string)
  "Convert a colon-separated IPv6 address to a (simple-array (unsigned-byte 16) 8)."
  (check-type string string)
  (with-foreign-object (in6-addr :uint16 8)
    (with-foreign-string (string-pointer string)
      (et:bzero in6-addr 16)
      (handler-case
          (et:inet-pton et:af-inet6      ; address family
                        string-pointer   ; name
                        in6-addr)        ; pointer to struct in6_addr
        (unix-error () (error 'parse-error))))
    (in6-addr-to-ipv6-array in6-addr)))

(defun ipv4-on-ipv6-mapped-vector-p (vector)
  (and (dotimes (i 5 t) (when (plusp (aref vector i)) (return nil)))
       (= (aref vector 5) #xFFFF)))

(defun vector-to-colon-separated (vector &optional (case :downcase))
  "Convert an 8-element vector to a colon-separated IPv6 address. `CASE' may be :DOWNCASE or :UPCASE."
  (coercef vector 'ipv6-array)
  (check-type case (member :upcase :downcase))
  (labels ((find-zeros ()
             (loop :for i :from 0 :upto 6
                :if (and (zerop (aref vector i))
                         (zerop (aref vector (1+ i))))
                :do (return (values i (or (position-if #'plusp vector :start (1+ i)) 8)))))
           (princ-subvec (start end s)
             (loop :for i :from start :below end :do
                (princ #\: s) (princ (aref vector i) s))))
    (let ((s (make-string-output-stream)))
      (cond
        ((ipv4-on-ipv6-mapped-vector-p vector)
         (princ "::ffff:" s)
         (let ((*print-base* 10))
           (princ (ldb (byte 8 8) (aref vector 6)) s) (princ #\. s)
           (princ (ldb (byte 8 0) (aref vector 6)) s) (princ #\. s)
           (princ (ldb (byte 8 8) (aref vector 7)) s) (princ #\. s)
           (princ (ldb (byte 8 0) (aref vector 7)) s)))
        (t
         (let ((*print-base* 16))
           (multiple-value-bind (start end) (find-zeros)
             (cond (start
                    (when (plusp start)
                      (princ (aref vector 0) s)
                      (princ-subvec 1 start s))
                    (princ #\: s)
                    (if (< end 8)
                        (princ-subvec end 8 s)
                        (princ #\: s)))
                   (t (princ (aref vector 0) s)
                      (princ-subvec 1 8 s)))))))
      (let ((str (get-output-stream-string s)))
        (case case
          (:downcase (nstring-downcase str))
          (:upcase   (nstring-upcase   str)))))))

(defun string-address-to-vector (address)
  "Convert a string address(dotted or colon-separated) to a vector address.
If the string is not a valid address, return NIL."
  (or (ignore-errors (dotted-to-vector address))
      (ignore-errors (colon-separated-to-vector address))))

(defun address-to-vector (address)
  "Convert any reppresentation of an internet address to a vector. Allowed inputs are: unsigned 32-bit integers, strings, vectors and INETADDR objects.
If the address is valid, two values are returned: the vector and the address type(:IPV4 or IPV6), otherwise NIL is returned."
  (let (vector addr-type)
    (typecase address
      (number (and (ignore-errors (setf vector (ipaddr-to-vector address)))
                   (setf addr-type :ipv4)))
      (string (cond
                ((ignore-errors (setf vector (dotted-to-vector address)))
                 (setf addr-type :ipv4))
                ((ignore-errors (setf vector (colon-separated-to-vector address)))
                 (setf addr-type :ipv6))))
      ((vector * 4) (and (ignore-errors (setf vector (coerce address 'ipv4-array)))
                         (setf addr-type :ipv4)))
      ((vector * 8) (and (ignore-errors (setf vector (coerce address 'ipv6-array)))
                         (setf addr-type :ipv6)))
      (ipv4addr (setf vector (name address)
                      addr-type :ipv4))
      (ipv6addr (setf vector (name address)
                      addr-type :ipv6)))
    (when vector (values vector addr-type))))

(defun ensure-address (address &optional (family :internet))
  (ecase family
    (:internet
     (typecase address
       (sockaddr
        (check-type address inetaddr)
        (values address))
       (t
        (make-address (address-to-vector address)))))
    (:local
     (etypecase address
       (string
        (make-instance 'localaddr :name address))
       (sockaddr
        (check-type address localaddr)
        (values address))))))


;;;
;;; Class definitions
;;;

(defclass sockaddr ()
  ((name :initarg :name :reader name :type vector))
  (:documentation "Base class for all socket address classes."))

(defclass inetaddr (sockaddr) ()
  (:documentation "IP addresses."))

(defclass ipv4addr (inetaddr) ()
  (:documentation "IPv4 address."))

(defclass ipv6addr (inetaddr) ()
  (:documentation "IPv6 address."))

(defclass localaddr (sockaddr)
  ((abstract :initform nil :initarg :abstract :reader abstract-p :type boolean))
  (:documentation "UNIX socket address."))


;;;
;;; Print methods
;;;

(defmethod print-object ((address ipv4addr) stream)
  (print-unreadable-object (address stream :type nil :identity nil)
    (format stream "IPv4 address: ~A"
            (sockaddr->presentation address))))

(defmethod print-object ((address ipv6addr) stream)
  (print-unreadable-object (address stream :type nil :identity nil)
    (format stream "IPv6 address: ~A"
            (sockaddr->presentation address))))

(defmethod print-object ((address localaddr) stream)
  (print-unreadable-object (address stream :type nil :identity nil)
    (format stream "Unix socket address: ~A. Abstract: ~:[no~;yes~]"
            (sockaddr->presentation address) (abstract-p address))))

(defgeneric sockaddr->presentation (addr)
  (:documentation "Returns a textual presentation of ADDR."))

(defmethod sockaddr->presentation ((addr ipv4addr))
  (vector-to-dotted (name addr)))

(defmethod sockaddr->presentation ((addr ipv6addr))
  (vector-to-colon-separated (name addr)))

(defmethod sockaddr->presentation ((addr localaddr))
  (if (abstract-p addr)
      "<unknown socket>"
      (name addr)))


;;;
;;; Equality methods
;;;

(defun vector-equal (v1 v2)
  (and (= (length v1) (length v2))
       (every #'eql v1 v2)))

(defgeneric sockaddr= (addr1 addr2)
  (:documentation "Returns T if both arguments are the same socket address."))

(defmethod sockaddr= ((addr1 inetaddr) (addr2 inetaddr))
  (vector-equal (name addr1) (name addr2)))

(defmethod sockaddr= ((addr1 localaddr) (addr2 localaddr))
  (equal (name addr1) (name addr2)))


;;;
;;; Copy methods
;;;

(defgeneric copy-sockaddr (addr)
  (:documentation "Returns a copy of ADDR which is SOCKADDR= to the original."))

(defmethod copy-sockaddr ((addr ipv4addr))
  (make-instance 'ipv4addr
                 :name (copy-seq (name addr))))

(defmethod copy-sockaddr ((addr ipv6addr))
  (make-instance 'ipv6addr
                 :name (copy-seq (name addr))))

(defmethod copy-sockaddr ((addr localaddr))
  (make-instance 'localaddr
                 :name (copy-seq (name addr))
                 :abstract (abstract-p addr)))

(defgeneric map-ipv4-address->ipv6 (addr)
  (:documentation "Returns an IPv6 address by mapping ADDR onto it."))
(defmethod map-ipv4-address->ipv6 ((addr ipv4addr))
  (make-instance 'ipv6addr
                 :name (map-ipv4-vector-to-ipv6 (name addr))))


;;; Constructor
(defun make-address (name)
  (cond
    ((ignore-errors (coercef name 'ipv4-array))
     (make-instance 'ipv4addr :name name))
    ((ignore-errors (coercef name 'ipv6-array))
     (make-instance 'ipv6addr :name name))
    ((stringp name)
     (make-instance 'localaddr :name name))
    (t (error 'type-error :datum name :expected-type '(or string ipv4-array ipv6-array)))))

;;;
;;; Well-known addresses
;;;

(defparameter +ipv4-unspecified+
  (make-address #(0 0 0 0)))

(defparameter +ipv4-loopback+
  (make-address #(127 0 0 1)))

(defparameter +ipv6-unspecified+
  (make-address #(0 0 0 0 0 0 0 0)))

(defparameter +ipv6-loopback+
  (make-address #(0 0 0 0 0 0 0 1)))

;; Multicast addresses replacing IPv4 broadcast addresses
(defparameter +ipv6-interface-local-all-nodes+
  (make-address #(#xFF01 0 0 0 0 0 0 1)))

(defparameter +ipv6-link-local-all-nodes+
  (make-address #(#xFF02 0 0 0 0 0 0 1)))

(defparameter +ipv6-interface-local-all-routers+
  (make-address #(#xFF01 0 0 0 0 0 0 2)))

(defparameter +ipv6-link-local-all-routers+
  (make-address #(#xFF02 0 0 0 0 0 0 2)))

(defparameter +ipv6-site-local-all-routers+
  (make-address #(#xFF05 0 0 0 0 0 0 2)))


;;;
;;; Predicates
;;;

;; General predicates
(defgeneric sockaddrp (address)
  (:documentation "Returns T if ADDRESS is a socket address."))

(defmethod sockaddrp ((address sockaddr))
  (declare (ignore address))
  t)

(defmethod sockaddrp (address)
  (declare (ignore address))
  nil)

(defgeneric ipv4-address-p (address)
  (:documentation "Returns T if ADDRESS is an IPv4 address object."))

(defmethod ipv4-address-p ((address ipv4addr))
  (declare (ignore address))
  t)

(defmethod ipv4-address-p (address)
  (declare (ignore address))
  nil)

(defgeneric ipv6-address-p (address)
  (:documentation "Returns T if ADDRESS is an IPv6 address object."))

(defmethod ipv6-address-p ((address ipv6addr))
  (declare (ignore address))
  t)

(defmethod ipv6-address-p (address)
  (declare (ignore address))
  nil)

(defgeneric local-address-p (address)
  (:documentation "Returns T if ADDRESS is local address object."))

(defmethod local-address-p ((address localaddr))
  (declare (ignore address))
  t)

(defmethod local-address-p (address)
  (declare (ignore address))
  nil)

(defgeneric address-type (address))

(defmethod address-type ((address ipv4addr))
  (declare (ignore address))
  :ipv4)

(defmethod address-type ((address ipv6addr))
  (declare (ignore address))
  :ipv6)

(defmethod address-type ((address localaddr))
  (declare (ignore address))
  :local)

(defmethod address-type (address)
  (declare (ignore address))
  nil)

;; IPv4 predicates

(defgeneric inetaddr-unspecified-p (addr)
  (:documentation "Returns T if ADDR is an \"unspecified\" internet address."))
(defmethod inetaddr-unspecified-p ((addr ipv4addr))
  (sockaddr= addr +ipv4-unspecified+))

(defgeneric inetaddr-loopback-p (addr)
  (:documentation "Returns T if ADDR is a loopback internet address."))
(defmethod inetaddr-loopback-p ((addr ipv4addr))
  (sockaddr= addr +ipv4-loopback+))

(defgeneric inetaddr-multicast-p (addr)
  (:documentation "Returns T if ADDR is an multicast internet address."))
(defmethod inetaddr-multicast-p ((addr ipv4addr))
  (eql (logand (aref (name addr) 0)
               #xE0)
       #xE0))

(defgeneric inetaddr-unicast-p (addr)
  (:documentation "Returns T if ADDR is an unicast internet address."))
(defmethod inetaddr-unicast-p ((addr ipv4addr))
  (and (not (inetaddr-unspecified-p addr))
       (not (inetaddr-loopback-p addr))
       (not (inetaddr-multicast-p addr))))

;; IPv6 predicates
;; definitions taken from RFC 2460

(defmethod inetaddr-unspecified-p ((addr ipv6addr))
  (sockaddr= addr +ipv6-unspecified+))

(defmethod inetaddr-loopback-p ((addr ipv6addr))
  (sockaddr= addr +ipv6-loopback+))

(defgeneric ipv6-ipv4-mapped-p (addr)
  (:documentation "Returns T if ADDR is an IPv6 address representing an IPv4 mapped address."))
(defmethod ipv6-ipv4-mapped-p ((addr ipv6addr))
  (ipv4-on-ipv6-mapped-vector-p (name addr)))

(defmethod inetaddr-multicast-p ((addr ipv6addr))
  (eql (logand (aref (name addr) 0)
               #xFF00)
       #xFF00))

(defgeneric ipv6-interface-local-multicast-p (addr)
  (:documentation "Returns T if ADDR is an interface-local IPv6 address."))
(defmethod ipv6-interface-local-multicast-p ((addr ipv6addr))
  (eql (logand (aref (name addr) 0)
               #xFF0F)
       #xFF01))

(defgeneric ipv6-link-local-multicast-p (addr)
  (:documentation "Returns T if ADDR is a link-local IPv6 address."))
(defmethod ipv6-link-local-multicast-p ((addr ipv6addr))
  (eql (logand (aref (name addr) 0)
               #xFF0F)
       #xFF02))

(defgeneric ipv6-admin-local-multicast-p (addr)
  (:documentation "Returns T if ADDR is a admin-local multicast IPv6 address."))
(defmethod ipv6-admin-local-multicast-p ((addr ipv6addr))
  (eql (logand (aref (name addr) 0)
               #xFF0F)
       #xFF04))

(defgeneric ipv6-site-local-multicast-p (addr)
  (:documentation "Returns T if ADDR is an site-local multicast IPv6 address."))
(defmethod ipv6-site-local-multicast-p ((addr ipv6addr))
  (eql (logand (aref (name addr) 0)
               #xFF0F)
       #xFF05))

(defgeneric ipv6-organization-local-multicast-p (addr)
  (:documentation "Returns T if ADDR is an organization-local multicast IPv6 address."))
(defmethod ipv6-organization-local-multicast-p ((addr ipv6addr))
  (eql (logand (aref (name addr) 0)
               #xFF0F)
       #xFF08))

(defgeneric ipv6-global-multicast-p (addr)
  (:documentation "Returns T if ADDR is a global multicast IPv6 address."))
(defmethod ipv6-global-multicast-p ((addr ipv6addr))
  (eql (logand (aref (name addr) 0)
               #xFF0F)
       #xFF0E))

(defgeneric ipv6-reserved-multicast-p (addr)
  (:documentation "Returns T if ADDR is a reserved multicast IPv6 address."))
(defmethod ipv6-reserved-multicast-p ((addr ipv6addr))
  (member (logand (aref (name addr) 0)
                  #xFF0F)
          (list #xFF00 #xFF03 #xFF0F)))

(defgeneric ipv6-unassigned-multicast-p (addr)
  (:documentation "Returns T if ADDR is an unassigned multicast IPv6 address."))
(defmethod ipv6-unassigned-multicast-p ((addr ipv6addr))
  (member (logand (aref (name addr) 0)
                  #xFF0F)
          (list #xFF06 #xFF07 #xFF09 #xFF0A #xFF0B #xFF0C #xFF0D)))

(defgeneric ipv6-transient-multicast-p (addr)
  (:documentation "Returns T if ADDR is a transient multicast IPv6 address."))
(defmethod ipv6-transient-multicast-p ((addr ipv6addr))
  (eql (logand (aref (name addr) 0)
               #xFF10)
       #xFF10))

(defgeneric ipv6-solicited-node-multicast-p (addr)
  (:documentation "Returns T if ADDR is an solicited-node multicast IPv6 address."))
(defmethod ipv6-solicited-node-multicast-p ((addr ipv6addr))
  (let ((vec (name addr)))
    (and (eql (aref vec 0) #xFF02) ; link-local permanent multicast
         (eql (aref vec 5) 1)
         (eql (logand (aref vec 6)
                      #xFF00)
              #xFF00))))

(defgeneric ipv6-link-local-unicast-p (addr)
  (:documentation "Returns T if ADDR is an link-local unicast IPv6 address."))
(defmethod ipv6-link-local-unicast-p ((addr ipv6addr))
  (eql (aref (name addr) 0) #xFE80))

(defgeneric ipv6-site-local-unicast-p (addr)
  (:documentation "Returns T if ADDR is an site-local unicast IPv6 address."))
(defmethod ipv6-site-local-unicast-p ((addr ipv6addr))
  (eql (aref (name addr) 0) #xFEC0))

(defgeneric ipv6-global-unicast-p (addr)
  (:documentation "Returns T if ADDR is an global unicasst IPv6 address."))
(defmethod ipv6-global-unicast-p ((addr ipv6addr))
  (and (not (inetaddr-unspecified-p addr))
       (not (inetaddr-loopback-p addr))
       (not (inetaddr-multicast-p addr))
       (not (ipv6-link-local-unicast-p addr))))

(defmethod inetaddr-unicast-p ((addr ipv6addr))
  (or (ipv6-link-local-unicast-p addr)
      (and (not (inetaddr-unspecified-p addr))
           (not (inetaddr-loopback-p addr))
           (not (inetaddr-multicast-p addr)))))

(defgeneric ipv6-multicast-type (addr)
  (:documentation "Returns the multicast type of ADDR(which must be IPv6)."))
(defmethod ipv6-multicast-type ((addr ipv6addr))
  (cond
    ((ipv6-interface-local-multicast-p addr)    :interface-local)
    ((ipv6-link-local-multicast-p addr)         :link-local)
    ((ipv6-admin-local-multicast-p addr)        :admin-local)
    ((ipv6-site-local-multicast-p addr)         :site-local)
    ((ipv6-organization-local-multicast-p addr) :organization-local)
    ((ipv6-global-multicast-p addr)             :global)
    ((ipv6-reserved-multicast-p addr)           :reserved)
    ((ipv6-unassigned-multicast-p addr)         :unassigned)))

(defgeneric inetaddr-type (addr)
  (:documentation "Returns the address type of ADDR."))

(defmethod inetaddr-type ((addr ipv6addr))
  (cond
    ((inetaddr-unspecified-p addr)    (values :ipv6 :unspecified))
    ((inetaddr-loopback-p addr)       (values :ipv6 :loopback))
    ((inetaddr-multicast-p addr)      (values :ipv6 :multicast (ipv6-multicast-type addr)))
    ((ipv6-link-local-unicast-p addr) (values :ipv6 :unicast :link-local))
    (t                                (values :ipv6 :unicast :global))))

(defmethod inetaddr-type ((addr ipv4addr))
  (cond
    ((inetaddr-unspecified-p addr) (values :ipv4 :unspecified))
    ((inetaddr-loopback-p addr)    (values :ipv4 :loopback))
    ((inetaddr-multicast-p addr)   (values :ipv4 :multicast))
    ((inetaddr-unicast-p addr)     (values :ipv4 :unicast))))
