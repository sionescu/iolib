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

;; (declaim (optimize (speed 2) (safety 2) (space 1) (debug 2)))
(declaim (optimize (speed 0) (safety 2) (space 0) (debug 2)))

(in-package :net.sockets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     ;;;
;;; RESOLVER CONDITIONS ;;;
;;;                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant +resolver-error-map+
  `((:eai-again      . resolver-again-error)
    (:eai-fail       . resolver-fail-error)
    (:eai-noname     . resolver-no-name-error)
    (:eai-nodata     . resolver-no-name-error)
    (:eai-addrfamily . resolver-no-name-error)
    (:eai-service    . resolver-no-service-error)))

(defun resolver-error-condition (id)
  (cdr (assoc id +resolver-error-map+)))

(defmacro resolver-error-code (id)
  `(addrerr-value ,id))

(define-condition resolver-error (system-error)
  ((data :initarg :data :reader resolver-error-data))
  (:documentation "Signaled when an error occurs while trying to resolve an address."))

(defmacro define-resolver-error (name code identifier format-string &optional documentation)
  `(progn
     (export ',name)
     (define-condition ,name (resolver-error)
       ((code :initform ,code)
        (identifier :initform ,identifier))
       (:report (lambda (condition stream)
                  (format stream ,format-string (resolver-error-data condition))
                  (print-message-if-not-null condition stream)))
       (:documentation ,documentation))))

(define-resolver-error resolver-again-error (resolver-error-code :eai-again) :resolver-again
  "Temporary failure occurred while resolving: ~S"
  "Condition signaled when a temporary failure occurred.")

(define-resolver-error resolver-fail-error (resolver-error-code :eai-fail) :resolver-fail
  "Non recoverable error occurred while resolving: ~S"
  "Condition signaled when a non-recoverable error occurred.")

(define-resolver-error resolver-no-name-error (resolver-error-code :eai-noname) :resolver-no-name
  "Host or service not found: ~S"
  "Condition signaled when a host or service was not found.")

(define-resolver-error resolver-no-service-error (resolver-error-code :eai-service) :resolver-no-service
  "Service not found for specific socket type: ~S"
  "Condition signaled when a service was not found for the socket type requested.")

(define-resolver-error resolver-unknown-error 0 :resolver-unknown
  "Unknown error while resolving: ~S"
  "Condition signaled when an unknown error is signaled while resolving an address.")

(defun resolver-error (identifier &key data message)
  (let ((condition-class
         (resolver-error-condition identifier)))
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

(define-constant +max-ipv4-value+ (1- (expt 2 32)))


;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;
;;;              ;;;
;;;  HOST LOOKUP ;;;
;;;              ;;;
;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;

(defun get-address-info (&key (node (null-pointer)) (service (null-pointer))
                         (hint-flags 0) (hint-family 0)
                         (hint-type 0) (hint-protocol 0))
  (with-foreign-objects ((hints 'et:addrinfo)
                         (res :pointer))
    (et:bzero hints et:size-of-addrinfo)
    (with-foreign-slots ((et:flags et:family et:socktype et:protocol)
                         hints et:addrinfo)
      (setf et:flags hint-flags)
      (setf et:family hint-family)
      (setf et:socktype hint-type)
      (setf et:protocol hint-protocol)
      (et:getaddrinfo node service hints res)
      (make-pointer (pointer-address (mem-ref res :pointer))))))

(defun get-name-info (sockaddr &key (want-host t) want-service (flags 0))
  (assert (or want-host want-service))
  (let ((salen et:size-of-sockaddr-storage))
    (with-foreign-objects ((host :char et:ni-maxhost)
                           (service :char et:ni-maxserv))
      (et:getnameinfo sockaddr salen
                      host (if want-host et:ni-maxhost 0)
                      service (if want-service et:ni-maxserv 0)
                      flags)
      (values (and want-host (foreign-string-to-lisp host et:ni-maxhost))
              (and want-service (foreign-string-to-lisp service et:ni-maxserv))))))

(defclass host ()
  ((truename  :initarg :truename  :reader host-truename)
   (aliases   :initarg :aliases   :reader host-aliases)
   (addresses :initarg :addresses :reader host-addresses)))

(defmethod initialize-instance :after ((host host) &key)
  (with-slots (addresses) host
    (unless (consp addresses)
      (setf addresses (list addresses)))))

(defgeneric random-address (host))
(defmethod random-address ((host host))
  (with-slots (addresses) host
    (nth (random (length addresses))
         addresses)))

(defun make-host (truename addresses &optional aliases)
  (make-instance 'host
                 :truename truename
                 :aliases aliases
                 :addresses addresses))

(defmethod print-object ((host host) stream)
  (print-unreadable-object (host stream :type t :identity nil)
    (with-slots (truename aliases addresses) host
      (format stream "Cannonical name: ~S. Aliases: ~:[None~;~:*~{~S~^, ~}~].~%Addresses: ~{~A~^, ~}"
              truename aliases addresses))))


;;
;; Error management
;;

(defun lookup-host-u8-vector-4 (host ipv6)
  (setf host (coerce host '(simple-array ub8 (4))))

  (handler-case
      (ecase ipv6
        ((nil)
         (with-foreign-object (sin 'et:sockaddr-storage)
           (make-sockaddr-in sin host)
           (return-from lookup-host-u8-vector-4
             (make-host (get-name-info sin :flags et:ni-namereqd)
                        (list (make-address (copy-seq host)))))))

        ((t)
         (with-foreign-object (sin6 'et:sockaddr-storage)
           (let ((ipv6addr (map-ipv4-vector-to-ipv6 host)))
             (make-sockaddr-in6 sin6 ipv6addr)
             (return-from lookup-host-u8-vector-4
               (make-host (get-name-info sin6 :flags et:ni-namereqd)
                          (list (make-address ipv6addr)))))))
        ((:ipv6)
         (resolver-error :eai-fail
                         :data host
                         :message "Received IPv4 address but IPv6-only was requested.")))
    (et:resolv-error (err)
      (resolver-error (et:system-error-identifier err) :data host))))

(defun lookup-host-u16-vector-8 (host ipv6)
  (setf host (coerce host '(simple-array ub16 (8))))

  (handler-case
      (ecase ipv6
        ((nil)
         (resolver-error :eai-fail
                         :data host
                         :message "Received IPv6 address but IPv4-only was requested."))

        ((:ipv6 t)
         (with-foreign-object (sin6 'et:sockaddr-storage)
           (make-sockaddr-in6 sin6 host)
           (return-from lookup-host-u16-vector-8
             (make-host (get-name-info sin6 :flags et:ni-namereqd)
                        (list (make-address (copy-seq host))))))))
    (et:resolv-error (err)
      (resolver-error (et:system-error-identifier err) :data host))))

(defun make-host-from-addrinfo (addrinfo)
  (let ((canonname (foreign-slot-value addrinfo 'et:addrinfo 'et:canonname))
        (addrlist
         (loop
            :for addrptr := addrinfo
                         :then (foreign-slot-value addrptr 'et:addrinfo 'et:next)
            :while (not (null-pointer-p addrptr))
            :collect (sockaddr-storage->sockaddr
                      (foreign-slot-value addrptr 'et:addrinfo 'et:addr)))))
    (make-host (if (null-pointer-p canonname)
                   nil
                   (foreign-string-to-lisp canonname))
               addrlist)))

(defun map-host-ipv4-addresses-to-ipv6 (hostobj)
  (declare (type host hostobj))
  (with-slots (addresses) hostobj
    (setf addresses
          (mapcar #'(lambda (address)
                      (if (ipv4-address-p address)
                          (make-address (map-ipv4-vector-to-ipv6 (name address)))
                          address))
                  addresses)))
  hostobj)

(defgeneric lookup-host (host &key &allow-other-keys))

(defmethod lookup-host :before (host &key (ipv6 *ipv6*))
  (check-type ipv6 (member nil :ipv6 t) "valid IPv6 configuration"))

(defmethod lookup-host ((host string) &key (ipv6 *ipv6*))
  (flet ((decide-family-and-flags ()
           (ecase ipv6
             ((nil) (values et:af-inet 0))
             ;; the freebsd I use rejects AI_V4MAPPED and AI_ALL(weird thing)
             ;; therefore I'll use AF_UNSPEC and do the mappings myself
             ((t)   (values
                     #-freebsd et:af-inet6
                     #+freebsd et:af-unspec
                     #+freebsd 0
                     #-freebsd
                     (logior et:ai-v4mapped
                             #+freebsd et:ai-v4mapped-cfg
                             et:ai-all)))
             (:ipv6 (values et:af-inet6 0)))))

    (let (parsed)
      (cond
        ((setf parsed (dotted-to-vector host :errorp nil))
         (return-from lookup-host
           (lookup-host-u8-vector-4 parsed ipv6)))

        ((setf parsed (colon-separated-to-vector host :errorp nil))
         (return-from lookup-host
           (lookup-host-u16-vector-8 parsed ipv6)))

        ;; FIXME: check for ASCII-only strings or implement IDN
        (t
         (multiple-value-bind (family flags)
             (decide-family-and-flags)
           (setf flags (logior flags et:ai-canonname et:ai-addrconfig))
           (handler-case
               (let* ((addrinfo
                       (get-address-info :node host
                                         :hint-flags flags
                                         :hint-family family
                                         :hint-type et:sock-stream
                                         :hint-protocol et:ipproto-ip))
                      (hostobj (make-host-from-addrinfo addrinfo)))
                 (when (string-not-equal (host-truename hostobj)
                                         host)
                   (setf (slot-value hostobj 'aliases) (list host)))
                 (et:freeaddrinfo addrinfo)
                 ;; mapping IPv4 addresses onto IPv6
                 #+freebsd
                 (when (eql ipv6 t)
                   (map-host-ipv4-addresses-to-ipv6 hostobj))
                 (return-from lookup-host hostobj))
             (et:resolv-error (err)
               (resolver-error (et:system-error-identifier err) :data host)))))))))

(defmethod lookup-host ((host ipv4addr) &key (ipv6 *ipv6*))
  (lookup-host-u8-vector-4 (name host) ipv6))

(defmethod lookup-host ((host ipv6addr) &key (ipv6 *ipv6*))
  (lookup-host-u16-vector-8 (name host) ipv6))

(defmethod lookup-host (host &key (ipv6 *ipv6*))
  (etypecase host
    ((simple-array * (4)) ; IPv4 address
     (lookup-host-u8-vector-4 host ipv6))

    ((simple-array * (8)) ; IPv6 address
     (lookup-host-u16-vector-8 host ipv6))))



;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;
;;;                 ;;;
;;;  SERVICE LOOKUP ;;;
;;;                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defclass service ()
  ((name     :initarg :name     :reader service-name)
   (port     :initarg :port     :reader service-port)
   (protocol :initarg :protocol :reader service-protocol)))

(defun make-service (name port protocol)
  (make-instance 'service
                 :name name
                 :port port
                 :protocol protocol))

(defmethod print-object ((service service) stream)
  (print-unreadable-object (service stream :type t :identity nil)
    (with-slots (name port protocol) service
      (format stream "Name: ~A. Port: ~A. Protocol: ~A" name port protocol))))

(defun socket-type-from-int (alien-val)
  (case alien-val
    (#.et:sock-stream    :tcp)
    (#.et:sock-dgram     :udp)
    (#.et:sock-seqpacket :sctp)
    (#.et:sock-raw       :raw)
    (t                   :unknown)))

(defun lookup-service-number (port-number protocol &key name-required)
  (declare (type ub32 port-number))
  (with-foreign-object (sin 'et:sockaddr-in)
    (let ((service
           (nth-value 1
            (progn
              (et:bzero sin et:size-of-sockaddr-in)
              (with-foreign-slots ((et:family et:port) sin et:sockaddr-in)
                (setf et:family et:af-inet
                      et:port (htons port-number)))
              (get-name-info sin
                             :flags (logior
                                     (case protocol
                                       (:udp et:ni-dgram)
                                       (t 0))
                                     (if name-required
                                         et:ni-namereqd 0))
                             :want-host nil :want-service t)))))
      (make-service service port-number protocol))))

(defun protocol-type-from-int (protocol)
  (case protocol
    (:tcp et:sock-stream)
    (:udp et:sock-dgram)
    (:any 0)))

(defun lookup-service-name (port protocol)
  (let* ((addrinfo
          (get-address-info :service port
                            :hint-type (protocol-type-from-int protocol)))
         (port-number
          (ntohs (foreign-slot-value (foreign-slot-value addrinfo 'et:addrinfo 'et:addr)
                                     'et:sockaddr-in 'et:port)))
         (true-protocol
          (socket-type-from-int (foreign-slot-value addrinfo 'et:addrinfo 'et:socktype))))
    (et:freeaddrinfo addrinfo)
    (return-from lookup-service-name
      (make-service port port-number true-protocol))))

(defun lookup-service (port &key (protocol :tcp) (name-required nil))
  (check-type protocol (member :tcp :udp :any))

  (let ((parsed-number (parse-number-or-nil port :ub16)))
    (handler-case
        (if parsed-number
            (lookup-service-number parsed-number protocol
                                   :name-required name-required)
            (lookup-service-name port protocol))
      (et:resolv-error (err)
        (resolver-error (et:system-error-identifier err) :data port)))))



;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  ;;;
;;;  PROTOCOL LOOKUP ;;;
;;;                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defclass protocol ()
  ((name     :initarg :name     :reader protocol-name)
   (aliases  :initarg :aliases  :reader protocol-aliases)
   (protonum :initarg :protonum :reader protocol-number)))

(defun make-protocol (name protonum &optional aliases)
  (make-instance 'protocol
                 :name name
                 :protonum protonum
                 :aliases aliases))

(defmethod print-object ((protocol protocol) stream)
  (print-unreadable-object (protocol stream :type t :identity nil)
    (with-slots (name aliases protonum) protocol
      (format stream "Name: ~S. Protocol number: ~A. Aliases: ~{~S~^, ~}"
              name protonum aliases))))

(define-condition unknown-protocol (system-error)
  ((name :initarg :name :initform nil :reader protocol-name))
  (:report (lambda (condition stream)
             (format stream "Unknown protocol: ~S"
                     (protocol-name condition))))
  (:documentation "Condition raised when a network protocol is not found."))

(defun make-protocol-from-protoent (protoent)
  (with-foreign-slots ((et:name et:proto et:aliases) protoent et:protoent)
    (let ((alias-strings
           (loop
              :for i :from 0
              :for alias := (mem-aref et:aliases :string i)
              :while alias :collect alias)))
      (make-protocol et:name et:proto alias-strings))))

(defun get-protocol-by-number (protonum)
  (make-protocol-from-protoent (et:getprotobynumber protonum)))

(defun get-protocol-by-name (protoname)
  (make-protocol-from-protoent (et:getprotobyname protoname)))

(defun lookup-protocol (proto)
  (let ((parsed-number (parse-number-or-nil proto)))
    (handler-case
        (if parsed-number
            (get-protocol-by-number parsed-number)
            (get-protocol-by-name proto))
      (unix-error (err)
        (declare (ignore err))
        (error 'unknown-protocol :name proto)))))
