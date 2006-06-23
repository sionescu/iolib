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

;; (declaim (optimize (speed 0) (safety 3) (space 0) (debug 2)))
(declaim (optimize (speed 1) (safety 2) (space 0) (debug 2)))

(in-package #:net.sockets)


(define-constant +resolver-error-map+
  `((#.et::eai-again    . :resolver-again)
    (#.et::eai-fail     . :resolver-fail)
    (#.et::eai-noname   . :resolver-no-name)
    #+linux (#.et::eai-addrfamily . :resolver-no-name)
    (#.et::eai-service  . :resolver-no-service)
    (#.et::eai-system   . :resolver-system)
    (#.et::eai-badflags . :resolver-bad-flags)
    (#.et::eai-family   . :resolver-family)
    (#.et::eai-memory   . :resolver-memory)
    (#.et::eai-socktype . :resolver-socket-type)
    #+linux (#.et::eai-overflow   . :resolver-overflow)
    #+freebsd (#.et::eai-badhints . :resolver-bad-hints)
    #+freebsd (#.et::eai-protocol . :resolver-protocol)
    #+freebsd (#.et::eai-max      . :resolver-max)))

(defun resolver-error-id (code)
  (cdr (assoc code +resolver-error-map+)))

(defun resolver-error-code (id)
  (cdr (rassoc id +resolver-error-map+)))

(defun raise-resolver-error (&key (type 'resolver-error) code identifier data data-presentation message)
  (error type
         :code (unless code (resolver-error-code identifier))
         :identifier identifier
         :data data
         :data-presentation data-presentation
         :message message))

(define-constant +max-ipv4-value+ (1- (expt 2 32)))


;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;
;;;              ;;;
;;;  HOST LOOKUP ;;;
;;;              ;;;
;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;

(defun get-address-info (&key node service
                         (hint-flags 0) (hint-family 0)
                         (hint-type 0) (hint-protocol 0))
  (with-alien ((hints (struct et::addrinfo))
               (res (* (struct et::addrinfo))))
    (let ((retval 0))
      (setf retval (sb-sys:with-pinned-objects (hints res)
                     (et::memset (addr hints) 0 et::size-of-addrinfo)
                     (setf (slot hints 'et::flags) hint-flags)
                     (setf (slot hints 'et::family) hint-family)
                     (setf (slot hints 'et::type) hint-type)
                     (setf (slot hints 'et::protocol) hint-protocol)
                     (et::getaddrinfo node service (addr hints) (addr res))))
      (values retval (if (zerop retval) res nil)))))

(defun get-name-info (sockaddr &key (want-host t) want-service (flags 0))
  (assert (or want-host want-service))
  (let ((salen (etypecase sockaddr
                 ((alien (* (struct et::sockaddr-in))) et::size-of-sockaddr-in)
                 ((alien (* (struct et::sockaddr-in6))) et::size-of-sockaddr-in6)
                 ((alien (* (struct et::sockaddr-storage))) et::size-of-sockaddr-storage))))
    (with-alien ((host (array char #.et::ni-maxhost))
                 (service (array char #.et::ni-maxserv)))
      (values (sb-sys:with-pinned-objects (sockaddr host service)
                (et::getnameinfo sockaddr salen
                                 (alien-sap host) (if want-host et::ni-maxhost 0)
                                 (alien-sap service) (if want-service et::ni-maxserv 0)
                                 flags))
              (and want-host (cast host c-string))
              (and want-service (cast service c-string))))))

(defclass host ()
  ((truename  :initarg :truename  :reader host-truename)
   (aliases   :initarg :aliases   :reader host-aliases)
   (addresses :initarg :addresses :reader host-addresses)))

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
      (format stream "Cannonical name: ~s. Aliases: ~a.~%Addresses: ~{~a~^, ~}"
              truename (unless aliases "None") addresses))))


;;
;; Error management
;;

(defun manage-resolv-error (retval host port)
  (let ((data (or host port)))
    (case retval
      (#.et::eai-fail                   ; non-recoverable error
       (raise-resolver-error :type 'resolver-fail-error
                             :data data))
      (#.et::eai-service                ; non-recoverable error
       (raise-resolver-error :type 'resolver-no-service-error
                             :data (prin1-to-string data)))
      (#.et::eai-again                  ; temporary error
       (raise-resolver-error :type 'resolver-again-error
                             :data (prin1-to-string data)))
      ((#.et::eai-noname #+linux #.et::eai-addrfamily) ; no name found
       (raise-resolver-error :type 'resolver-no-name-error
                             :data (prin1-to-string data)))
      (#.et::eai-system                 ; probably a kernel error
       (raise-resolver-error :type 'system-error
                             :code (get-errno)))
      (otherwise
       (let ((id (resolver-error-id retval)))
         (if id
             (raise-resolver-error :identifier id :data data)
             (error 'possible-bug :message (format nil "Possible bug while looking up ~a"
                                                   (if host "host" "port")))))))))

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

(defun copy-simple-array-ub16-to-alien-vector (lisp-vec alien-vec)
  (declare (type (simple-array ub16 (*)) lisp-vec))
  (dotimes (i (length lisp-vec))
    (setf (deref alien-vec i)
          (aref lisp-vec i))))

(defun lookup-host-u8-vector-4 (host ipv6)
  (setf host (coerce host '(simple-array ub8 (4))))

  (ecase ipv6
    ((nil)
     (with-alien ((sin (struct et::sockaddr-in)))
       (sb-sys:with-pinned-objects (sin)
         (et::memset (addr sin) 0 et::size-of-sockaddr-in)
         (setf (slot sin 'et::family) et::af-inet)
         (setf (slot sin 'et::addr) (htonl (vector-to-ipaddr host)))
         (multiple-value-bind (retval hostname service)
             (get-name-info (addr sin) :flags et::ni-namereqd)
           (declare (ignore service))
           (if (eql retval et::netdb-success)
               (return-from lookup-host-u8-vector-4
                 (make-host hostname
                            (list (make-address :ipv4 (copy-seq host)))))
               (manage-resolv-error retval host nil))))))

    ((:ipv6 t)
     (with-alien ((sin6 (struct et::sockaddr-in6)))
       (sb-sys:with-pinned-objects (sin6)
         (et::memset (addr sin6) 0 et::size-of-sockaddr-in6)
         (setf (slot sin6 'et::family) et::af-inet6)
         (let ((u16-vector (slot (slot (slot sin6 'et::addr)
                                       'et::in6-u)
                                 'et::addr16))
               (ipv6addr (map-ipv4-to-ipv6 host)))

           (copy-simple-array-ub16-to-alien-vector ipv6addr u16-vector)
           (setf (deref u16-vector 6) (htons (deref u16-vector 6)))
           (setf (deref u16-vector 7) (htons (deref u16-vector 7)))
           (multiple-value-bind (retval hostname service)
             (get-name-info (addr sin6) :flags et::ni-namereqd)
           (declare (ignore service))
           (if (eql retval et::netdb-success)
               (return-from lookup-host-u8-vector-4
                 (make-host hostname
                            (list (make-address :ipv6 ipv6addr))))
               (manage-resolv-error retval host nil)))))))))

(defun lookup-host-u16-vector-8 (host ipv6)
  (setf host (coerce host '(simple-array ub16 (8))))

  (ecase ipv6
    ((nil)
     (raise-resolver-error :type 'resolver-fail-error
                           :data host
                           :data-presentation (let ((*print-base* 16)) (format nil "~a" host))
                           :message "Received IPv6 address but IPv4-only was requested."))

    ((:ipv6 t)
     (with-alien ((sin6 (struct et::sockaddr-in6)))
       (sb-sys:with-pinned-objects (sin6)
         (et::memset (addr sin6) 0 et::size-of-sockaddr-in6)
         (setf (slot sin6 'et::family) et::af-inet6)
         (let ((u16-vector (slot (slot (slot sin6 'et::addr)
                                       'et::in6-u)
                                 'et::addr16)))
           (dotimes (i 8)
             (setf (deref u16-vector i) (htons (aref host i)))))
         (multiple-value-bind (retval hostname service)
             (get-name-info (addr sin6) :flags et::ni-namereqd)
           (declare (ignore service))
           (if (eql retval et::netdb-success)
               (return-from lookup-host-u16-vector-8
                 (make-host hostname
                            (list (make-address :ipv6 (copy-seq host)))))
               (manage-resolv-error retval host nil))))))))

(defun make-address-from-addrinfo (ptr)
  (declare (type (alien (* (struct et::addrinfo)))
                 ptr))
  (let ((addr (slot ptr 'et::addr)))
    (case (slot ptr 'et::family)
      (#.et::af-inet (make-address
                      :ipv4
                      (make-vector-u8-4-from-in-addr
                       (slot (cast addr (* (struct et::sockaddr-in)))
                             'et::addr))))
      (#.et::af-inet6 (make-address
                       :ipv6
                       (make-vector-u16-8-from-in6-addr
                        (addr (slot (cast addr (* (struct et::sockaddr-in6)))
                                    'et::addr))))))))

(defun make-host-from-addrinfo-struct (addrinfo)
  (declare (type (alien (* (struct et::addrinfo)))
                 addrinfo))
  (let ((canonname (slot addrinfo 'et::canonname))
        (addrlist (loop
                     for addrptr of-type (alien (* (struct et::addrinfo)))
                       = addrinfo then (slot addrptr 'et::next)
                     while (not (null-alien addrptr))
                     collect (make-address-from-addrinfo addrptr))))
    (make-host canonname addrlist)))

(defmethod lookup-host :around (host &key (ipv6 *ipv6*) all)
  (check-type ipv6 (member nil :ipv6 t) "valid IPv6 configuration")
  (call-next-method))

(defmethod lookup-host ((host string) &key (ipv6 *ipv6*) all)
  (flet ((decide-family-and-flags ()
           (ecase ipv6
             ((nil) (values et::af-inet 0))
             (t     (values et::af-inet6 (if all
                                             et::ai-v4mapped
                                             (logior et::ai-v4mapped
                                                     et::ai-all))))
             (:ipv6 (values et::af-inet6 0)))))

    (let (parsed)
      (cond
        ((setf parsed (dotted-to-vector host :error-p nil))
         (return-from lookup-host
           (lookup-host-u8-vector-4 parsed ipv6)))

        ((setf parsed (colon-separated-to-vector host :error-p nil))
         (return-from lookup-host
           (lookup-host-u16-vector-8 parsed ipv6)))

        (t
         (handler-case
             (setf host (coerce host '(simple-array base-char (*))))
           (type-error (err)
             (declare (ignore err))
             (error 'invalid-argument :argument host
                    :message (format nil "The string ~s contains non-ASCII characters." host))))

         (multiple-value-bind (family flags)
             (decide-family-and-flags)
           (setf flags (logior flags et::ai-canonname et::ai-addrconfig))
           (multiple-value-bind (retval addrinfo)
               (get-address-info :node host
                                 :hint-flags flags :hint-family family
                                 :hint-type et::sock-stream :hint-protocol et::ipproto-ip)
             (unless addrinfo
               (manage-resolv-error retval host nil))
             (let ((hostobj (make-host-from-addrinfo-struct addrinfo)))
               (et::freeaddrinfo addrinfo)
               (return-from lookup-host hostobj)))))))))

(defmethod lookup-host ((host ipv4addr) &key (ipv6 *ipv6*) all)
  (lookup-host-u8-vector-4 (name host) ipv6))

(defmethod lookup-host ((host ipv6addr) &key (ipv6 *ipv6*) all)
  (lookup-host-u8-vector-4 (name host) ipv6))

(defmethod lookup-host (host &key (ipv6 *ipv6*) all)
  (etypecase host
    ((vector * 4) ; IPv4 address
     (lookup-host-u8-vector-4 host ipv6))

    ((vector * 8) ; IPv6 address
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
      (format stream "Name: ~a. Port: ~a. Protocol: ~a" name port protocol))))

(defun socket-type-from-int (alien-val)
  (case alien-val
    (#.et::sock-stream    :tcp)
    (#.et::sock-dgram     :udp)
    (#.et::sock-seqpacket :sctp)
    (#.et::sock-raw       :raw)
    (t                    :unknown)))

(defun lookup-service-number (port-number protocol &key name-required)
  (declare (type ub32 port-number))
  (with-alien ((sin (struct et::sockaddr-in)))
    (multiple-value-bind (retval host service)
        (sb-sys:with-pinned-objects (sin)
          (et::memset (addr sin) 0 et::size-of-sockaddr-in)
          (setf (slot sin 'et::family) et::af-inet)
          (setf (slot sin 'et::port) (htons port-number))
          (get-name-info (addr sin)
                         :flags (logior (case protocol
                                          (:udp et::ni-dgram)
                                          (t 0))
                                        (if name-required et::ni-namereqd 0))
                         :want-host nil :want-service t))
      (declare (ignore host))
      (if (zerop retval)
          (return-from lookup-service-number
            (make-service service port-number protocol))
          (manage-resolv-error retval nil port-number)))))

(defun lookup-service-name (port protocol)
  (multiple-value-bind (retval addrinfo)
      (get-address-info :service port :hint-type (case protocol
                                                   (:tcp et::sock-stream)
                                                   (:udp et::sock-dgram)
                                                   (:any 0)))
    (declare (type (or null (alien (* (struct et::addrinfo)))) addrinfo))
    (if addrinfo
        (let ((port-number
               (ntohs (slot (cast (slot addrinfo 'et::addr)
                                  (* (struct et::sockaddr-in)))
                            'et::port)))
              (true-protocol
               (socket-type-from-int (slot addrinfo 'et::type))))
          (sb-sys:with-pinned-objects (addrinfo)
            (et::freeaddrinfo addrinfo))
          (return-from lookup-service-name
            (make-service port port-number true-protocol)))
        (manage-resolv-error retval nil port))))

(defun lookup-service (port &key (protocol :tcp) (name-required nil))
  (case protocol
    ((:tcp :udp :any) t)
    (t                (setf protocol :any)))

  (multiple-value-bind (port-type port-number)
      (string-or-parsed-number port)
    (case port-type
      (:number
       (lookup-service-number port-number protocol
                              :name-required name-required))
      (:string
       (lookup-service-name port protocol)))))



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
      (format stream "Name: ~s. Protocol number: ~a. Aliases: ~{~s~^, ~}"
              name protonum aliases))))

(defun make-protocol-from-protoent (protoent)
  (declare (type (alien (* (struct et::protoent))) protoent))
  (let* ((name (slot protoent 'et::name))
         (number (slot protoent 'et::proto))
         (aliasptr (slot protoent 'et::aliases))
         (aliases (loop
                    for i from 0
                    for alias = (deref aliasptr i)
                    while alias collect alias)))
    (make-protocol name number aliases)))

(defun get-protocol-by-number (protonum)
  (with-alien ((ptr (* (struct et::protoent))))
    (let ((retval
           (et::getprotobynumber protonum)))
      (if (null-alien retval)
          (error 'unknown-protocol :num protonum)
          (make-protocol-from-protoent retval)))))

(defun get-protocol-by-name (protoname)
  (with-alien ((ptr (* (struct et::protoent))))
    (sb-sys:with-pinned-objects (protoname)
      (let ((retval
             (et::getprotobyname protoname)))
        (if (null-alien retval)
            (error 'unknown-protocol :name protoname)
            (make-protocol-from-protoent retval))))))

(defun lookup-protocol (proto)
  (multiple-value-bind (proto-type proto-val)
      (string-or-parsed-number proto)
    (ecase proto-type
      (:number
       (get-protocol-by-number proto-val))

      (:string
       (get-protocol-by-name proto-val)))))
