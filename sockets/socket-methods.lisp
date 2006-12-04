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

;; (declaim (optimize (speed 2) (safety 2) (space 1) (debug 2)))
(declaim (optimize (speed 0) (safety 2) (space 0) (debug 2)))

(in-package :net.sockets)

(defparameter *socket-type-map*
  '(((:ipv4  :stream   :active  :default) . socket-stream-internet-active)
    ((:ipv6  :stream   :active  :default) . socket-stream-internet-active)
    ((:ipv4  :stream   :passive :default) . socket-stream-internet-passive)
    ((:ipv6  :stream   :passive :default) . socket-stream-internet-passive)
    ((:local :stream   :active  :default) . socket-stream-local-active)
    ((:local :stream   :passive :default) . socket-stream-local-passive)
    ((:local :datagram :active  :default) . socket-datagram-local-active)
    ((:ipv4  :datagram :active  :default) . socket-datagram-internet-active)
    ((:ipv6  :datagram :active  :default) . socket-datagram-internet-active)))

(defun select-socket-type (family type connect protocol)
  (or (cdr (assoc (list family type connect protocol) *socket-type-map*
                  :test #'equal))
      (error "No socket class found !!")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SHARED-INITIALIZE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-make-socket-keywords-to-constants (family type protocol)
  (let ((sf (ecase family
              (:ipv4  et:af-inet)
              (:ipv6  et:af-inet6)
              (:local et:af-local)))
        (st (ecase type
              (:stream   et:sock-stream)
              (:datagram et:sock-dgram)))
        (sp (cond
              ((integerp protocol) protocol)
              ((eql protocol :default) 0)
              ((keywordp protocol)
               (protocol-number
                (get-protocol-by-name (string-downcase
                                       (string protocol))))))))
    (values sf st sp)))

(defmethod shared-initialize :after ((socket socket) slot-names
                                     &key file-descriptor family
                                     type (protocol :default))
  (when (socket-open-p socket)
    (socket-close socket))
  (with-slots (fd (fam family) (proto protocol)) socket
    (multiple-value-bind (sf st sp)
        (translate-make-socket-keywords-to-constants family type protocol)
      (if file-descriptor
          (setf fd file-descriptor)
          (setf fd (with-socket-error-filter
                     (et:socket sf st sp))))
      (setf fam family)
      (setf proto protocol)
      (iomux:finalize-object-closing-fd socket fd))))

(defun make-fd-stream (fd)
  #+sbcl
  (sb-sys:make-fd-stream fd
                         :name (format nil "Socket stream, fd: ~A" fd)
                         :input t :output t :buffering :full :dual-channel-p t
                         :element-type :default :auto-close nil)
  #+cmucl
  (system:make-fd-stream fd
                         :name (format nil "Socket stream, fd: ~A" fd)
                         :input t :output t :buffering :full
                         :binary-stream-p t :auto-close nil))

(defmethod shared-initialize :after ((socket stream-socket) slot-names &key)
  (setf (slot-value socket 'lisp-stream)
        (make-fd-stream (socket-fd socket))))

(defmethod socket-type ((socket stream-socket))
  :stream)

(defmethod socket-type ((socket datagram-socket))
  :datagram)

;;;;;;;;;;;;;;;;;;;;
;;  PRINT-OBJECT  ;;
;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((socket socket-stream-internet-active) stream)
  (print-unreadable-object (socket stream :type nil :identity t)
    (format stream "internet stream socket" )
    (if (slot-boundp socket 'address)
        (format stream " connected to ~A/~A"
                (netaddr->presentation (socket-address socket))
                (socket-port socket))
        (if (slot-boundp socket 'fd)
            (format stream ", unconnected")
            (format stream ", closed")))))

(defmethod print-object ((socket socket-stream-internet-passive) stream)
  (print-unreadable-object (socket stream :type nil :identity t)
    (format stream "internet stream socket" )
    (if (slot-boundp socket 'address)
        (format stream " ~A ~A/~A"
                (if (socket-listening-p socket)
                    "waiting for connections @"
                    "bound to")
                (netaddr->presentation (socket-address socket))
                (socket-port socket))
        (if (slot-boundp socket 'fd)
            (format stream ", unbound")
            (format stream ", closed")))))

(defmethod print-object ((socket socket-stream-local-active) stream)
  (print-unreadable-object (socket stream :type nil :identity t)
    (format stream "local stream socket" )
    (if (slot-boundp socket 'address)
        (format stream " connected to ~S"
                (netaddr->presentation (socket-address socket)))
        (if (slot-boundp socket 'fd)
            (format stream ", unconnected")
            (format stream ", closed")))))

(defmethod print-object ((socket socket-stream-local-passive) stream)
  (print-unreadable-object (socket stream :type nil :identity t)
    (format stream "local stream socket" )
    (if (slot-boundp socket 'address)
        (format stream " ~A ~A"
                (if (socket-listening-p socket)
                    "waiting for connections @"
                    "bound to")
                (netaddr->presentation (socket-address socket)))
        (if (slot-boundp socket 'fd)
            (format stream ", unbound")
            (format stream ", closed")))))

(defmethod print-object ((socket socket-datagram-local-active) stream)
  (print-unreadable-object (socket stream :type nil :identity t)
    (format stream "local datagram socket" )
    (if (slot-boundp socket 'address)
        (format stream " connected to ~S"
                (netaddr->presentation (socket-address socket)))
        (if (slot-boundp socket 'fd)
            (format stream ", unconnected")
            (format stream ", closed")))))

(defmethod print-object ((socket socket-datagram-internet-active) stream)
  (print-unreadable-object (socket stream :type nil :identity t)
    (format stream "internet stream socket" )
    (if (slot-boundp socket 'address)
        (format stream " connected to ~A/~A"
                (netaddr->presentation (socket-address socket))
                (socket-port socket))
        (if (slot-boundp socket 'fd)
            (format stream ", unconnected")
            (format stream ", closed")))))

;;;;;;;;;;;;;
;;  CLOSE  ;;
;;;;;;;;;;;;;

(defmethod socket-close progn ((socket socket))
  (cancel-finalization socket)
  (when (slot-boundp socket 'fd)
    (with-socket-error-filter
      (et:close (socket-fd socket))))
  (mapc #'(lambda (slot)
            (slot-makunbound socket slot))
        '(fd address family protocol))
  (values socket))

(defmethod socket-close progn ((socket stream-socket))
  (slot-makunbound socket 'lisp-stream))

(defmethod socket-close progn ((socket internet-socket))
  (slot-makunbound socket 'port))

(defmethod socket-close progn ((socket passive-socket))
  (slot-makunbound socket 'listening))

(defmethod socket-open-p ((socket socket))
  (unless (slot-boundp socket 'fd)
    (return-from socket-open-p nil))
  (with-socket-error-filter
    (handler-case
        (with-foreign-objects ((ss 'et:sockaddr-storage)
                               (size :socklen
                                     #.(foreign-type-size 'et:sockaddr-storage)))
          (et:memset ss 0 #.(foreign-type-size 'et:sockaddr-storage))
          (et:getsockname (socket-fd socket)
                          ss size)
          t)
      (unix-error (err)
        (case (error-identifier err)
          ((:ebadf
            :enotsock
            :econnreset)
           nil)
          ;; some other error
          (otherwise (error err)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  get and set O_NONBLOCK  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod socket-non-blocking-mode ((socket socket))
  (with-slots (fd) socket
    (let ((file-flags (with-socket-error-filter
                        (et:fcntl fd et:f-getfl))))
      (not (zerop (logand file-flags et:o-nonblock)))))
  (values socket))

(defmethod (setf socket-non-blocking-mode) (value (socket socket))
  (check-type value boolean "a boolean value")
  (with-slots (fd) socket
    (let ((file-flags (et:fcntl fd et:f-getfl)))
      (with-socket-error-filter
        (et:fcntl fd et:f-setfl
                  (logior file-flags
                          (if value et:o-nonblock 0))))))
  (values value))

;;;;;;;;;;;;;;;;;;;
;;  GETSOCKNAME  ;;
;;;;;;;;;;;;;;;;;;;

(defmethod local-name ((socket internet-socket))
  (with-foreign-objects ((ss 'et:sockaddr-storage)
                         (size :socklen
                               #.(foreign-type-size 'et:sockaddr-storage)))
    (et:memset ss 0 #.(foreign-type-size 'et:sockaddr-storage))
    (with-socket-error-filter
      (et:getsockname (socket-fd socket)
                      ss size))
    (return-from local-name
      (values (sockaddr-storage->netaddr ss)
              (ntohs (foreign-slot-value ss 'et:sockaddr-in
                                         'et:port))))))

(defmethod local-name ((socket local-socket))
  (with-foreign-objects ((sun 'et:sockaddr-un)
                         (size :socklen
                               #.(foreign-type-size 'et:sockaddr-un)))
    (et:memset sun 0 #.(foreign-type-size 'et:sockaddr-un))
    (with-socket-error-filter
      (et:getsockname (socket-fd socket)
                      sun size))
    (return-from local-name
      (values (sockaddr-un->netaddr sun)))))

;;;;;;;;;;;;;;;;;;;
;;  GETPEERNAME  ;;
;;;;;;;;;;;;;;;;;;;

(defmethod remote-name ((socket internet-socket))
  (with-foreign-objects ((ss 'et:sockaddr-storage)
                         (size :socklen
                               #.(foreign-type-size 'et:sockaddr-storage)))
    (et:memset ss 0 #.(foreign-type-size 'et:sockaddr-storage))
    (with-socket-error-filter
      (et:getpeername (socket-fd socket)
                      ss size))
    (return-from remote-name
      (values (sockaddr-storage->netaddr ss)
              (ntohs (foreign-slot-value ss 'et:sockaddr-in
                                         'et:port))))))

(defmethod remote-name ((socket local-socket))
  (with-foreign-objects ((sun 'et:sockaddr-un)
                         (size :socklen
                               #.(foreign-type-size 'et:sockaddr-un)))
    (et:memset sun 0 #.(foreign-type-size 'et:sockaddr-un))
    (with-socket-error-filter
      (et:getpeername (socket-fd socket)
                      sun size))
    (return-from remote-name
      (values (sockaddr-un->netaddr sun)))))

;;;;;;;;;;;;
;;  BIND  ;;
;;;;;;;;;;;;

(defmethod bind-address :before ((socket internet-socket)
                                 address &key (reuse-address t))
  (when reuse-address
    (set-socket-option socket :reuse-address :value t)))

(defun bind-ipv4-address (fd address port)
  (with-foreign-object (sin 'et:sockaddr-in)
    (make-sockaddr-in sin address port)
    (with-socket-error-filter
      (et:bind fd sin
               #.(foreign-type-size 'et:sockaddr-in)))))

(defun bind-ipv6-address (fd address port)
  (with-foreign-object (sin6 'et:sockaddr-in6)
    (make-sockaddr-in6 sin6 address port)
    (with-socket-error-filter
      (et:bind fd sin6
               #.(foreign-type-size 'et:sockaddr-in6)))))

(defmethod bind-address ((socket internet-socket)
                         (address ipv4addr)
                         &key (port 0))
  (if (eql (socket-family socket) :ipv6)
      (bind-ipv6-address (socket-fd socket)
                         (map-ipv4-vector-to-ipv6 (name address))
                         port)
      (bind-ipv4-address (socket-fd socket) (name address) port))
  (values socket))

(defmethod bind-address ((socket internet-socket)
                         (address ipv6addr)
                         &key (port 0))
  (bind-ipv6-address (socket-fd socket) (name address) port)
  (values socket))

(defmethod bind-address :before ((socket local-socket)
                                 (address localaddr) &key)
  (when (typep socket 'active-socket)
    (error "You can't bind an active Unix socket.")))

(defmethod bind-address ((socket local-socket)
                         (address localaddr) &key)
  (with-foreign-object (sun 'et:sockaddr-un)
    (make-sockaddr-un sun (name address))
    (with-socket-error-filter
      (et:bind (socket-fd socket)
               sun
               #.(foreign-type-size 'et:sockaddr-un))))
  (values socket))

(defmethod bind-address :after ((socket socket)
                                (address netaddr) &key)
  (setf (slot-value socket 'address) (copy-netaddr address)))

(defmethod bind-address :after ((socket internet-socket)
                                (address netaddr) &key port)
  (setf (slot-value socket 'port) port))


;;;;;;;;;;;;;;
;;  LISTEN  ;;
;;;;;;;;;;;;;;

(defmethod socket-listen ((socket passive-socket)
                          &key (backlog (min *default-backlog-size*
                                             +max-backlog-size+)))
  (check-type backlog unsigned-byte "a non-negative integer")
  (with-socket-error-filter
    (et:listen (socket-fd socket) backlog))
  (setf (slot-value socket 'listening) t)
  (values socket))

(defmethod socket-listen ((socket active-socket)
                          &key backlog)
  (declare (ignore backlog))
  (error "You can't listen on active sockets."))

;;;;;;;;;;;;;;
;;  ACCEPT  ;;
;;;;;;;;;;;;;;

(defmethod accept-connection ((socket active-socket)
                              &key wait)
  (declare (ignore wait))
  (error "You can't accept connections on active sockets."))

(defmethod accept-connection ((socket passive-socket)
                              &key (wait t))
  (with-foreign-objects ((ss 'et:sockaddr-storage)
                         (size :socklen
                               #.(foreign-type-size 'et:sockaddr-storage)))
    (et:memset ss 0 #.(foreign-type-size 'et:sockaddr-storage))
    (let (non-blocking-state
          client-fd)
      (with-socket-error-filter
        (handler-case
            (if wait
                ;; do a "normal" accept
                ;; Note: the socket may already be in non-blocking mode
                (setf client-fd (et:accept (socket-fd socket)
                                           ss size))
                ;; set the socket to non-blocking mode before calling accept()
                ;; if there's no new connection return NIL
                (unwind-protect
                     (progn
                       ;; saving the current non-blocking state
                       (setf non-blocking-state (socket-non-blocking-mode socket))
                       (setf client-fd (et:accept (socket-fd socket)
                                                  ss size)))
                  ;; restoring the socket's non-blocking state
                  (setf (socket-non-blocking-mode socket) non-blocking-state)))
          ;; the socket is marked non-blocking and there's no new connection
          (et:unix-error-wouldblock (err)
            (declare (ignore err))
            (return-from accept-connection nil))))

      (let ((client-socket
             ;; create the client socket object
             (make-instance (active-class socket)
                            :file-descriptor client-fd)))
        ;; setting the socket's remote address and port
        (multiple-value-bind (remote-address remote-port)
            (remote-name client-socket)
          (setf (slot-value client-socket 'address) remote-address)
          ;; when it's an internet socket
          (when remote-port
            (setf (slot-value client-socket 'port) remote-port)))
        (return-from accept-connection client-socket)))))


;;;;;;;;;;;;;;;
;;  CONNECT  ;;
;;;;;;;;;;;;;;;

#+freebsd
(defmethod connect :before ((socket active-socket)
                            netaddr &key)
  (when *no-sigpipe*
    (set-socket-option socket :no-sigpipe :value t)))

(defun ipv4-connect (fd address port)
  (with-foreign-object (sin 'et:sockaddr-in)
    (make-sockaddr-in sin address port)
    (with-socket-error-filter
      (et:connect fd sin
                  #.(foreign-type-size 'et:sockaddr-in)))))

(defun ipv6-connect (fd address port)
  (with-foreign-object (sin6 'et:sockaddr-in6)
    (make-sockaddr-in6 sin6 address port)
    (with-socket-error-filter
      (et:connect fd sin6
                  (foreign-type-size 'et:sockaddr-in6)))))

(defmethod connect ((socket internet-socket)
                    (address ipv4addr) &key (port 0))
  (if (eql (socket-family socket) :ipv6)
      (ipv6-connect (socket-fd socket)
                    (map-ipv4-vector-to-ipv6 (name address))
                    port)
      (ipv4-connect (socket-fd socket) (name address) port))
  (setf (slot-value socket 'port) port)
  (values socket))

(defmethod connect ((socket internet-socket)
                    (address ipv6addr) &key (port 0))
  (ipv6-connect (socket-fd socket) (name address) port)
  (setf (slot-value socket 'port) port)
  (values socket))

(defmethod connect ((socket local-socket)
                    (address localaddr) &key)
  (with-foreign-object (sun 'et:sockaddr-un)
    (make-sockaddr-un sun (name address))
    (with-socket-error-filter
      (et:connect (socket-fd socket)
                  sun
                  #.(foreign-type-size 'et:sockaddr-un))))
  (values socket))

(defmethod connect :after ((socket active-socket)
                           (address netaddr) &key)
  (setf (slot-value socket 'address) (copy-netaddr address)))

(defmethod connect ((socket passive-socket)
                    address &key)
  (error "You cannot connect passive sockets."))

;;;;;;;;;;;;;;;;
;;  SHUTDOWN  ;;
;;;;;;;;;;;;;;;;

(defmethod shutdown ((socket active-socket) direction)
  (check-type direction (member :read :write :read-write)
              "valid shutdown specifier")
  (with-socket-error-filter
    (et:shutdown (socket-fd socket)
                 (ecase direction
                   (:read et:shut-rd)
                   (:write et:shut-wr)
                   (:read-write et:shut-rdwr))))
  (values socket))

(defmethod shutdown ((socket passive-socket) direction)
  (error "You cannot shut down passive sockets."))

;;;;;;;;;;;;
;;  SEND  ;;
;;;;;;;;;;;;

(defun normalize-send-buffer (buff vstart vend)
  (let ((start (or vstart 0))
        (end (if vend
                 (min vend (length buff))
                 (length buff))))
    (assert (<= start end))
    (etypecase buff
      ((simple-array ub8 (*)) (values buff start (- end start)))
      ((vector ub8) (values (coerce buff '(simple-array ub8 (*)))
                            start (- end start)))
      (simple-base-string (values buff start (- end start)))
      (string (values (flexi-streams:string-to-octets buff :start start :end end)
                      0 (- end start))))))

(defmethod socket-send :before ((buffer array)
                                (socket active-socket)
                                &key start end
                                remote-address remote-port)
  (check-type start (or unsigned-byte null)
              "a non-negative value or NIL")
  (check-type end (or unsigned-byte null)
              "a non-negative value or NIL")
  (when (or remote-port remote-address)
    (check-type remote-address netaddr "a network address")
    (check-type remote-port (unsigned-byte 16) "a valid IP port number")))

(defmethod socket-send ((buffer array)
                        (socket active-socket) &key start end
                        remote-address remote-port end-of-record
                        dont-route dont-wait (no-signal *no-sigpipe*)
                        out-of-band #+linux more #+linux confirm)

  (let ((flags (logior (if end-of-record et:msg-eor 0)
                       (if dont-route et:msg-dontroute 0)
                       (if dont-wait  et:msg-dontwait 0)
                       (if no-signal  et:msg-nosignal 0)
                       (if out-of-band et:msg-oob 0)
                       #+linux (if more et:msg-more 0)
                       #+linux (if confirm et:msg-confirm 0))))

    (when (and (ipv4-address-p remote-address)
               (eql (socket-family socket) :ipv6))
      (setf remote-address (map-ipv4-address->ipv6 remote-address)))
    (multiple-value-bind (buff start-offset bufflen)
        (normalize-send-buffer buffer start end)
      (with-foreign-object (ss 'et:sockaddr-storage)
        (et:memset ss 0 #.(foreign-type-size 'et:sockaddr-storage))
        (when remote-address
          (netaddr->sockaddr-storage ss remote-address remote-port))
        (with-pointer-to-vector-data (buff-sap buff)
          (incf-pointer buff-sap start-offset)
          (with-socket-error-filter
            (return-from socket-send
              (et:sendto (socket-fd socket)
                         buff-sap bufflen
                         flags
                         (if remote-address ss nil)
                         (if remote-address #.(foreign-type-size 'et:sockaddr-storage) 0)))))))))

(defmethod socket-send (buffer (socket passive-socket) &key)
  (error "You cannot send data on a passive socket."))

;;;;;;;;;;;;
;;  RECV  ;;
;;;;;;;;;;;;

(defun normalize-receive-buffer (buff vstart vend)
  (let ((start (or vstart 0))
        (end (if vend
                 (min vend (length buff))
                 (length buff))))
    (assert (<= start end))
    (etypecase buff
      ((simple-array ub8 (*)) (values buff start (- end start)))
      (simple-base-string (values buff start (- end start))))))

(defmethod socket-receive :before ((buffer simple-array)
                                   (socket active-socket)
                                   &key start end)
  (check-type start (or unsigned-byte null)
              "a non-negative value or NIL")
  (check-type end (or unsigned-byte null)
              "a non-negative value or NIL"))

(defmethod socket-receive ((buffer simple-array)
                           (socket active-socket) &key start end
                           out-of-band peek wait-all
                           dont-wait (no-signal *no-sigpipe*))

  (let ((flags (logior (if out-of-band et:msg-oob 0)
                       (if peek        et:msg-peek 0)
                       (if wait-all    et:msg-waitall 0)
                       (if dont-wait   et:msg-dontwait 0)
                       (if no-signal   et:msg-nosignal 0)))
        bytes-received)

    (multiple-value-bind (buff start-offset bufflen)
        (normalize-receive-buffer buffer start end)
      (with-foreign-objects ((ss 'et:sockaddr-storage)
                             (size :socklen #.(foreign-type-size 'et:sockaddr-storage)))
        (et:memset ss 0 #.(foreign-type-size 'et:sockaddr-storage))
        (with-pointer-to-vector-data (buff-sap buff)
          (incf-pointer buff-sap start-offset)
          (with-socket-error-filter
            (setf bytes-received
                  (et:recvfrom (socket-fd socket)
                               buff-sap bufflen
                               flags
                               ss size))))

        (return-from socket-receive
          ;; when socket is a datagram socket
          ;; return the sender's address as 3rd value
          (if (typep socket 'datagram-socket)
              (values buffer bytes-received (sockaddr-storage->netaddr ss))
              (values buffer bytes-received)))))))

(defmethod socket-receive (buffer (socket passive-socket) &key)
  (error "You cannot receive data from a passive socket."))


;;
;; Only for datagram sockets
;;

(defmethod unconnect :before ((socket active-socket))
  (unless (typep socket 'datagram-socket)
    (error "You can only unconnect active datagram sockets.")))

(defmethod unconnect ((socket datagram-socket))
  (with-socket-error-filter
    (with-foreign-object (sin 'et:sockaddr-in)
      (et:memset sin 0 #.(foreign-type-size 'et:sockaddr-in))
      (setf (foreign-slot-value sin 'et:sockaddr-in 'et:address) et:af-unspec)
      (et:connect (socket-fd socket)
                  sin
                  #.(foreign-type-size 'et:sockaddr-in))
      (slot-makunbound socket 'address))))

(defmethod unconnect :after ((socket internet-socket))
  (when (typep socket 'internet-socket)
    (slot-makunbound socket 'port)))
