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


;;;;;;;;;;;;;;;;
;;;  ERRORS  ;;;
;;;;;;;;;;;;;;;;

(define-condition invalid-address ()
  ((address  :initarg :address  :initform nil :reader address)
   (addrtype :initarg :type     :initform nil :reader address-type))
  (:report (lambda (condition stream)
             (format stream "Invalid ~A address: ~A"
                     (address-type condition) (address condition))))
  (:documentation "Condition raised when an address designator is invalid."))


;;;
;;; Conversion functions
;;;

(defun ipaddr-to-dotted (ipaddr)
  (declare (type ub32 ipaddr))
  (format nil "~A.~A.~A.~A"
          (ldb (byte 8 24) ipaddr)
          (ldb (byte 8 16) ipaddr)
          (ldb (byte 8 8)  ipaddr)
          (ldb (byte 8 0)  ipaddr)))

(defun dotted-to-vector (string &key (errorp t))
  (labels ((err (&rest args)
             (if errorp
                 (apply #'error args)
                 (return-from dotted-to-vector nil)))
           (type-error ()
             (err 'type-error :datum string :expected-type 'string))
           (invalid-address ()
             (err 'invalid-address :address string :type :ipv4)))
    (unless (stringp string)
      (type-error))

    (let ((addr (make-array 4 :element-type 'ub8))
          (split (split-sequence #\. string :count 5)))
      ;; must have exactly 4 tokens
      (when (/= 4 (length split))
        (invalid-address))
      (loop
         :for element :in split
         :for index :below 4
         :for parsed := (parse-number-or-nil element :ub8) :do
         (if parsed
             (setf (aref addr index) parsed)
             (invalid-address)))
      addr)))

(defun dotted-to-ipaddr (string)
  (vector-to-ipaddr (dotted-to-vector string)))

(defun vector-to-dotted (vector)
  (coercef vector 'ipv4-array)
  (format nil "~A.~A.~A.~A"
          (aref vector 0)
          (aref vector 1)
          (aref vector 2)
          (aref vector 3)))

(defun colon-separated-to-vector (string &key (errorp t))
  (when (not (stringp string))
    (if errorp
        (error 'type-error :datum string
               :expected-type 'string)
        (return-from colon-separated-to-vector nil)))

  (with-foreign-object (in6-addr :uint16 8)
    (with-foreign-string (string-pointer string)
      (et:bzero in6-addr 16)
      (handler-case
          (et:inet-pton et:af-inet6        ; address family
                        string-pointer     ; name
                        in6-addr)          ; pointer to struct in6_addr
        (unix-error ()
          (if errorp
              (error 'invalid-address :address string :type :ipv6)
              (return-from colon-separated-to-vector nil)))))
    (in6-addr-to-ipv6-array in6-addr)))

(defun vector-to-colon-separated (vector &key (case :downcase) (errorp t))
  (handler-case
      (coercef vector 'ipv6-array)
    (type-error ()
      (if errorp
          (error 'type-error :datum vector
                 :expected-type 'ipv6-array)
          (return-from vector-to-colon-separated nil))))

  (with-foreign-object (sin6 'et:sockaddr-in6)
    (with-foreign-pointer (namebuf et:inet6-addrstrlen bufsize)
      (make-sockaddr-in6 sin6 vector)
      (et:inet-ntop et:af-inet6                          ; address family
                    (foreign-slot-pointer
                      sin6 'et:sockaddr-in6 'et:addr)    ; pointer to struct in6_addr
                    namebuf                              ; destination buffer
                    bufsize)                             ; INET6_ADDRSTRLEN
      (return-from vector-to-colon-separated
        (let ((str (foreign-string-to-lisp namebuf bufsize)))
          (ecase case
            (:downcase str)
            (:upcase (nstring-upcase str))))))))

(defun string-address->vector (address)
  (or (dotted-to-vector address :errorp nil)
      (colon-separated-to-vector address :errorp nil)))

(defun vector-address-or-nil (address)
  (let (vector addr-type)
    (typecase address
      (string (cond
                ((setf vector (dotted-to-vector address :errorp nil))
                 (setf addr-type :ipv4))
                ((setf vector (colon-separated-to-vector address :errorp nil))
                 (setf addr-type :ipv6))))
      ((vector * 4) (and (ignore-errors (setf vector (coerce address 'ipv4-array)))
                         (setf addr-type :ipv4)))
      ((vector * 8) (and (ignore-errors (setf vector (coerce address 'ipv6-array)))
                         (setf addr-type :ipv6)))
      (ipv4addr (setf vector (name address)
                      addr-type :ipv4))
      (ipv6addr (setf vector (name address)
                      addr-type :ipv6)))
    (values vector addr-type)))


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
    (with-slots (abstract) address
      (format stream "Unix socket address: ~A. Abstract: ~:[no~;yes~]"
              (sockaddr->presentation address) abstract))))

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
  (and (equal (length v1) (length v2))
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
    ((stringp name)
     (make-instance 'localaddr :name name))
    ((ignore-errors (coercef name 'ipv4-array))
     (make-instance 'ipv4addr :name name))
    ((ignore-errors (coercef name 'ipv6-array))
     (make-instance 'ipv6addr :name name))
    (t (error 'invalid-address :address name :type :unknown))))

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
  (with-slots (name) addr
    (and (zerop (aref name 0))
         (zerop (aref name 1))
         (zerop (aref name 2))
         (zerop (aref name 3))
         (zerop (aref name 4))
         (eql (aref name 5) #xFFFF)
         (< (ldb (byte 8 0) (aref name 6))
            255)
         (< (ldb (byte 8 8) (aref name 6))
            255)
         (< (ldb (byte 8 0) (aref name 7))
            255)
         (< (ldb (byte 8 8) (aref name 7))
            255))))

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
