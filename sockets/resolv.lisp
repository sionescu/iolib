;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; resolv.lisp --- Host, protocol and service lookups.
;;;
;;; Copyright (C) 2006-2007, Stelian Ionescu  <sionescu@common-lisp.net>
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

;;;; Resolver Conditions

(define-constant +resolver-error-map+
  '((:eai-again      . resolver-again-error)
    (:eai-fail       . resolver-fail-error)
    (:eai-noname     . resolver-no-name-error)
    (:eai-nodata     . resolver-no-name-error)
    (:eai-addrfamily . resolver-no-name-error)
    (:eai-service    . resolver-no-service-error))
  :test 'equal)

(defun resolver-error-condition (id)
  (cdr (assoc id +resolver-error-map+)))

(defmacro resolver-error-code (id)
  `(addrerr-value ,id))

(define-condition resolver-error (system-error)
  ((data :initarg :data :reader resolver-error-data))
  (:documentation
   "Signaled when an error occurs while trying to resolve an address."))

(defmacro define-resolver-error (name code identifier format-string
                                 &optional documentation)
  `(progn
     (define-condition ,name (resolver-error)
       ((code :initform ,code)
        (identifier :initform ,identifier))
       (:report (lambda (condition stream)
                  (format stream ,format-string (resolver-error-data condition))
                  (print-message-if-not-null condition stream)))
       (:documentation ,documentation))))

(define-resolver-error resolver-again-error (resolver-error-code :eai-again)
  :resolver-again
  "Temporary failure occurred while resolving: ~S"
  "Condition signaled when a temporary failure occurred.")

(define-resolver-error resolver-fail-error (resolver-error-code :eai-fail)
  :resolver-fail
  "Non recoverable error occurred while resolving: ~S"
  "Condition signaled when a non-recoverable error occurred.")

(define-resolver-error resolver-no-name-error (resolver-error-code :eai-noname)
  :resolver-no-name
  "Host or service not found: ~S"
  "Condition signaled when a host or service was not found.")

(define-resolver-error resolver-no-service-error
    (resolver-error-code :eai-service) :resolver-no-service
  "Service not found for specific socket type: ~S"
  "Condition signaled when a service was not found for the socket type
requested.")

(define-resolver-error resolver-unknown-error 0 :resolver-unknown
  "Unknown error while resolving: ~S"
  "Condition signaled when an unknown error is signaled while resolving
an address.")

(defun resolver-error (identifier &key data message)
  (let ((condition-class (resolver-error-condition identifier)))
    (if condition-class
        (error condition-class
               :code (resolver-error-code identifier)
               :identifier identifier
               :data data
               :message message)
        (error 'resolver-unknown-error
               :code (or (ignore-errors
                           (resolver-error-code identifier))
                         0)
               :identifier identifier
               :data data
               :message message))))

(define-constant +max-ipv4-value+ (1- (expt 2 32))
  :documentation "Integer denoting 255.255.255.255")

;;;; Host Lookup

(defun get-address-info (&key (node (null-pointer)) (service (null-pointer))
                         (hint-flags 0) (hint-family 0)
                         (hint-type 0) (hint-protocol 0))
  (with-foreign-objects ((hints 'nix::addrinfo)
                         (res :pointer))
    (nix:bzero hints nix::size-of-addrinfo)
    (with-foreign-slots ((nix::flags nix::family nix::socktype
                          nix::protocol)
                         hints nix::addrinfo)
      (setf nix::flags    hint-flags
            nix::family   hint-family
            nix::socktype hint-type
            nix::protocol hint-protocol)
      (nix:getaddrinfo node service hints res)
      (make-pointer (pointer-address (mem-ref res :pointer))))))

(defun get-name-info (sockaddr &key (want-host t) want-service (flags 0))
  (assert (or want-host want-service))
  (let ((salen nix::size-of-sockaddr-storage))
    (with-foreign-objects ((host :char nix::ni-maxhost)
                           (service :char nix::ni-maxserv))
      (nix:getnameinfo sockaddr salen
                       host (if want-host nix::ni-maxhost 0)
                       service (if want-service nix::ni-maxserv 0)
                       flags)
      (values (and want-host (foreign-string-to-lisp
                              host #|:count nix::ni-maxhost|#))
              (and want-service (foreign-string-to-lisp
                                 service #|:count nix::ni-maxserv|#))))))

(defclass host ()
  ((truename :initarg :truename  :reader host-truename
             :documentation "The name of the host.")
   (aliases :initarg :aliases   :reader host-aliases
            :documentation "A list of aliases.")
   (addresses :initarg :addresses :reader host-addresses
              :documentation "A list of addresses."))
  (:documentation "Class representing a host: name, aliases and addresses."))

(defmethod initialize-instance :after ((host host) &key)
  (with-slots (addresses) host
    (setf addresses (alexandria:ensure-list addresses))))

(defun host-random-address (host)
  "Returns a random address from HOST's address list."
  (alexandria:random-elt (host-addresses host)))

(defun make-host (truename addresses &optional aliases)
  "Instantiates a HOST object."
  (make-instance 'host
                 :truename truename
                 :aliases aliases
                 :addresses addresses))

(defmethod print-object ((host host) stream)
  (print-unreadable-object (host stream :type t :identity nil)
    (with-slots (truename aliases addresses) host
      (format stream "Canonical name: ~S. Aliases: ~:[None~;~:*~{~S~^, ~}~].~%~
                      Addresses: ~{~A~^, ~}"
              truename aliases addresses))))

;;;; Auxiliary Functions

(defun lookup-host-u8-vector-4 (host ipv6)
  (coercef host 'ub8-sarray)
  (handler-case
      (ecase ipv6
        ((nil)
         ;; Darwin's getnameinfo() seems buggy.  Signals a EAI_FAMILY
         ;; error on test LOOKUP-HOST.4.  We use gethostbyaddr() here
         ;; instead as a workaround.  FIXME: handle errors properly.
         #+darwin
         (with-foreign-object (addr 'nix::in-addr-struct)
           (setf (foreign-slot-value addr 'nix::in-addr-struct 'nix::addr)
                 (htonl (vector-to-integer host)))
           (let ((ptr (nix:gethostbyaddr addr 4 nix::af-inet)))
             (if (null-pointer-p ptr)
                 (resolver-error -1 :data host)
                 (make-host (foreign-slot-value
                             ptr 'nix::hostent 'nix::name)
                            (list (make-address (copy-seq host)))))))
         #-darwin
         (with-foreign-object (sin 'nix::sockaddr-storage)
           (make-sockaddr-in sin host)
           (make-host (get-name-info sin :flags nix::ni-namereqd)
                      (list (make-address (copy-seq host))))))
        ((t)
         (with-foreign-object (sin6 'nix::sockaddr-storage)
           (let ((ipv6addr (map-ipv4-vector-to-ipv6 host)))
             (make-sockaddr-in6 sin6 ipv6addr)
             (make-host (get-name-info sin6 :flags nix::ni-namereqd)
                        (list (make-address ipv6addr))))))
        ((:ipv6)
         (resolver-error
          :eai-fail :data host
          :message "Received IPv4 address but IPv6-only was requested.")))
    (nix:resolv-error (err)
      (resolver-error (nix:system-error-identifier err) :data host))))

(defun lookup-host-u16-vector-8 (host ipv6)
  (coercef host 'ub16-sarray)
  (handler-case
      (ecase ipv6
        ((nil)
         (resolver-error
          :eai-fail :data host
          :message "Received IPv6 address but IPv4-only was requested."))
        ((:ipv6 t)
         (with-foreign-object (sin6 'nix::sockaddr-storage)
           (make-sockaddr-in6 sin6 host)
           (make-host (get-name-info sin6 :flags nix::ni-namereqd)
                      (list (make-address (copy-seq host)))))))
    (nix:resolv-error (err)
      (resolver-error (nix:system-error-identifier err) :data host))))

(defun make-host-from-addrinfo (addrinfo)
  (let ((canonname (foreign-slot-value
                    addrinfo 'nix::addrinfo 'nix::canonname))
        (addrlist
         (loop :for addrptr := addrinfo
               :then (foreign-slot-value addrptr 'nix::addrinfo 'nix::next)
               :while (not (null-pointer-p addrptr))
               :collect (sockaddr-storage->sockaddr
                        (foreign-slot-value
                         addrptr 'nix::addrinfo 'nix::addr)))))
    (make-host (if (null-pointer-p canonname)
                   nil
                   (foreign-string-to-lisp canonname))
               addrlist)))

(defun map-host-ipv4-addresses-to-ipv6 (hostobj)
  (declare (type host hostobj))
  (with-slots (addresses) hostobj
    (setf addresses
          (mapcar (lambda (address)
                    (if (ipv4-address-p address)
                        (make-address (map-ipv4-vector-to-ipv6
                                       (address-name address)))
                        address))
                  addresses)))
  hostobj)

;;;; External Interface

(defgeneric lookup-host (name-or-address &key ipv6)
  (:documentation "Looks up a host by name or address.  IPV6
determines the IPv6 behaviour, defaults to *IPV6*."))

(defmethod lookup-host ((host string) &key (ipv6 *ipv6*))
  (check-type ipv6 (member nil :ipv6 t) "valid IPv6 configuration")
  (flet ((decide-family-and-flags ()
           (case ipv6
             ((nil) (values nix::af-inet 0))
             ;; freebsd 6.1 rejects AI_V4MAPPED and AI_ALL (weird thing)
             ;; therefore I'll use AF_UNSPEC and do the mappings myself
             ((t) #-bsd (values nix::af-inet6
                                (logior nix::ai-v4mapped nix::ai-all))
                  #+bsd (values nix::af-unspec 0))
             (:ipv6 (values nix::af-inet6 0)))))
    (multiple-value-bind (vector type) (address-to-vector host)
      (case type
        (:ipv4 (lookup-host-u8-vector-4 vector ipv6))
        (:ipv6 (lookup-host-u16-vector-8 vector ipv6))
        (t (multiple-value-bind (family flags)
               (decide-family-and-flags)
             (setf flags (logior flags nix::ai-canonname
                                 nix::ai-addrconfig))
             (handler-case
                 (let* ((addrinfo (get-address-info
                                   :node host
                                   :hint-flags flags
                                   :hint-family family
                                   :hint-type nix::sock-stream
                                   :hint-protocol nix::ipproto-ip))
                        (hostobj (make-host-from-addrinfo addrinfo)))
                   (when (string-not-equal (host-truename hostobj) host)
                     (setf (slot-value hostobj 'aliases) (list host)))
                   (nix:freeaddrinfo addrinfo)
                   ;; mapping IPv4 addresses onto IPv6
                   #+bsd
                   (when (eq ipv6 t)
                     (map-host-ipv4-addresses-to-ipv6 hostobj))
                   hostobj)
               (nix:resolv-error (err)
                 (resolver-error (nix:system-error-identifier err)
                                 :data host)))))))))

;;; FIXME: Doesn't return aliases, why?
(defmethod lookup-host (host &key (ipv6 *ipv6*))
  (check-type ipv6 (member nil :ipv6 t) "valid IPv6 configuration")
  (multiple-value-bind (vector type) (address-to-vector host)
    (case type
      (:ipv4 (lookup-host-u8-vector-4 vector ipv6))
      (:ipv6 (lookup-host-u16-vector-8 vector ipv6))
      ;; better error?
      (t (error 'parse-error)))))

(defun convert-or-lookup-inet-address (address &optional (ipv6 *ipv6*))
  "If ADDRESS is an inet-address designator, it is converted, if
necessary, to an INET-ADDRESS object and returned.  Otherwise it
is assumed to be a host name which is then looked up in order to
return its primary address as the first return value and the
remaining address list as the second return value."
  (or (ignore-errors (ensure-address address :internet))
      (let ((addresses (host-addresses (lookup-host address :ipv6 ipv6))))
        (values (car addresses) (cdr addresses)))))

;;;; Service Lookup

(defclass service ()
  ((name :initarg :name :reader service-name
         :documentation "The service name.")
   (port :initarg :port :reader service-port
          :documentation "The service's default port.")
   ;; why only these keyword? --luis
   (protocol :initarg :protocol :reader service-protocol
             :documentation "The service's protocol, :TCP or :UDP."))
  (:documentation "Class representing a service."))

(defun make-service (name port protocol)
  "Constructor for SERVICE objects."
  (make-instance 'service :name name :port port :protocol protocol))

(defmethod print-object ((service service) stream)
  (print-unreadable-object (service stream :type t :identity nil)
    (with-slots (name port protocol) service
      (format stream "Name: ~A Port: ~A Protocol: ~A" name port protocol))))

#+darwin
(defun %get-service-name (port protocol)
  (let ((ptr (nix:getservbyport port (ecase protocol
                                       (:tcp "tcp")
                                       (:udp "udp")
                                       (:any (cffi:null-pointer))))))
    (if (null-pointer-p ptr)
        (resolver-error -1 :data port) ; FIXME: wrong error
        (foreign-slot-value ptr 'nix::servent 'nix::name ))))

#-darwin
(defun %get-service-name (port protocol)
  (with-foreign-object (sin 'nix::sockaddr-in)
    (nix:bzero sin nix::size-of-sockaddr-in)
    (with-foreign-slots
        ((nix::family nix::port) sin nix::sockaddr-in)
      (setf nix::family nix::af-inet
            nix::port (htons port)))
    (nth-value 1 (get-name-info sin
                                :flags (case protocol
                                         (:udp nix::ni-dgram)
                                         (t 0))
                                :want-host nil :want-service t))))

(defun lookup-service-number (port-number protocol)
  (declare (type ub32 port-number))
  (let ((service (%get-service-name port-number protocol)))
    (make-service service port-number protocol)))

(defun lookup-service-name (port protocol)
  (flet ((protocol-type-to-int (protocol)
           (case protocol
             (:tcp nix::sock-stream)
             (:udp nix::sock-dgram)
             (:any 0)))
         (socket-type-from-int (alien-val)
           (case alien-val
             (#.nix::sock-stream :tcp)
             (#.nix::sock-dgram :udp)
             (t :unknown))))
    (let* ((addrinfo (get-address-info
                      :service port
                      :hint-type (protocol-type-to-int protocol)))
           (port-number (ntohs (foreign-slot-value
                                (foreign-slot-value addrinfo
                                                    'nix::addrinfo 'nix::addr)
                                'nix::sockaddr-in 'nix::port)))
           (true-protocol
            (socket-type-from-int
             (foreign-slot-value addrinfo 'nix::addrinfo 'nix::socktype))))
      (nix:freeaddrinfo addrinfo)
      (make-service port port-number true-protocol))))

;;; This tries to parse stuff like "22" as a number instead of a
;;; protocol name.  Why bother?  If it does matter, then we should
;;; document this behaviour. --luis
;;;
;;; Hmm, changing the protocol argument to &OPTIONAL might have not
;;; been a great idea on my part. --luis
(defun lookup-service (port-or-name &optional (protocol :tcp))
  "Lookup a service by port or name.  PROTOCOL should be one
of :TCP, :UDP or :ANY."
  (check-type protocol (member :tcp :udp :any))
  (let ((parsed-number (parse-number-or-nil port-or-name :ub16)))
    (handler-case
        (if parsed-number
            (lookup-service-number parsed-number protocol)
            (lookup-service-name port-or-name protocol))
      (nix:resolv-error (err)
        (resolver-error (nix:system-error-identifier err)
                        :data port-or-name)))))

;;;; Protocol Lookup

(defclass protocol ()
  ((name :initarg :name :reader protocol-name
         :documentation "The protocol's primary name.")
   (aliases :initarg :aliases :reader protocol-aliases
            :documentation "A list of aliases for this protocol.")
   (number :initarg :number :reader protocol-number
           :documentation "The protocol number."))
  (:documentation "Class representing a protocol."))

(defun make-protocol (name number &optional aliases)
  "Constructor for PROTOCOL objects."
  (make-instance 'protocol :name name :number number :aliases aliases))

(defmethod print-object ((protocol protocol) stream)
  (print-unreadable-object (protocol stream :type t :identity nil)
    (with-slots (name aliases protonum) protocol
      (format stream "Name: ~S Protocol number: ~A Aliases: ~{~S~^, ~}"
              name protonum aliases))))

(define-condition unknown-protocol (system-error)
  ((name :initarg :name :initform nil :reader protocol-name))
  (:report (lambda (condition stream)
             (format stream "Unknown protocol: ~S" (protocol-name condition))))
  (:documentation "Condition raised when a network protocol is not found."))

(defun make-protocol-from-protoent (protoent)
  (with-foreign-slots ((nix::name nix::proto nix::aliases)
                       protoent nix::protoent)
    (let ((alias-strings
           (loop :for i :from 0
                 :for alias := (mem-aref nix::aliases :string i)
                 :while alias :collect alias)))
      (make-protocol nix::name nix::proto alias-strings))))

;;; Again, why bother parsing numbers in strings? --luis
(defun lookup-protocol (name-or-number)
  "Lookup a protocol by name or number.  Signals an
UNKNOWN-PROTOCOL error if no protocol is found."
  (let ((parsed-number (parse-number-or-nil name-or-number)))
    (handler-case
        (make-protocol-from-protoent
         (if parsed-number
             (nix:getprotobynumber parsed-number)
             (nix:getprotobyname name-or-number)))
      (posix-error ()
        (error 'unknown-protocol :name name-or-number)))))
