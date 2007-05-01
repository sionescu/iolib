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

(defvar *socket-type-map*
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

(defmethod socket-fd ((socket socket))
  (fd-of socket))
(defmethod (setf socket-fd) (fd (socket socket))
  (setf (fd-of socket) fd))

(defmethod shared-initialize :after ((socket socket) slot-names
                                     &key file-descriptor family
                                     type (protocol :default))
  (declare (ignore slot-names))
  (when (socket-open-p socket)
    (close socket))
  (with-accessors ((fd fd-of)
                   (fam socket-family)
                   (proto socket-protocol)) socket    
    (setf fd (or file-descriptor
                 (multiple-value-bind (sf st sp)
                     (translate-make-socket-keywords-to-constants family type protocol)
                   (with-socket-error-filter
                     (et:socket sf st sp)))))
    (setf fam family
          proto protocol)))

(defmethod socket-type ((socket stream-socket))
  :stream)

(defmethod socket-type ((socket datagram-socket))
  :datagram)


;;;;;;;;;;;;;;;;;;;;
;;  PRINT-OBJECT  ;;
;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((socket socket-stream-internet-active) stream)
  (print-unreadable-object (socket stream :type nil :identity t)
    (format stream "active internet stream socket" )
    (if (socket-connected-p socket)
        (multiple-value-bind (addr port) (remote-name socket)
          (format stream " connected to ~A/~A"
                  (sockaddr->presentation addr) port))
        (if (fd-of socket)
            (format stream ", unconnected")
            (format stream ", closed")))))

(defmethod print-object ((socket socket-stream-internet-passive) stream)
  (print-unreadable-object (socket stream :type nil :identity t)
    (format stream "passive internet stream socket" )
    (if (socket-bound-p socket)
        (multiple-value-bind (addr port) (local-name socket)
          (format stream " ~A ~A/~A"
                  (if (socket-listening-p socket)
                      "waiting for connections @"
                      "bound to")
                  (sockaddr->presentation addr) port))
        (if (fd-of socket)
            (format stream ", unbound")
            (format stream ", closed")))))

(defmethod print-object ((socket socket-stream-local-active) stream)
  (print-unreadable-object (socket stream :type nil :identity t)
    (format stream "active local stream socket" )
    (if (socket-connected-p socket)
        (format stream " connected")
        (if (fd-of socket)
            (format stream ", unconnected")
            (format stream ", closed")))))

(defmethod print-object ((socket socket-stream-local-passive) stream)
  (print-unreadable-object (socket stream :type nil :identity t)
    (format stream "passive local stream socket" )
    (if (socket-bound-p socket)
        (format stream " ~A ~A"
                (if (socket-listening-p socket)
                    "waiting for connections @"
                    "bound to")
                (sockaddr->presentation (socket-address socket)))
        (if (fd-of socket)
            (format stream ", unbound")
            (format stream ", closed")))))

(defmethod print-object ((socket socket-datagram-local-active) stream)
  (print-unreadable-object (socket stream :type nil :identity t)
    (format stream "local datagram socket" )
    (if (socket-connected-p socket)
        (format stream " connected")
        (if (fd-of socket)
            (format stream ", unconnected")
            (format stream ", closed")))))

(defmethod print-object ((socket socket-datagram-internet-active) stream)
  (print-unreadable-object (socket stream :type nil :identity t)
    (format stream "internet datagram socket" )
    (if (socket-connected-p socket)
        (multiple-value-bind (addr port) (remote-name socket)
          (format stream " connected to ~A/~A"
                  (sockaddr->presentation addr) port))
        (if (fd-of socket)
            (format stream ", unconnected")
            (format stream ", closed")))))


;;;;;;;;;;;;;
;;  CLOSE  ;;
;;;;;;;;;;;;;

(defmethod close :around ((socket socket) &key abort)
  (declare (ignore abort))
  (when (fd-of socket)
    (with-socket-error-filter
      (et:close (fd-of socket))))
  (setf (fd-of socket) nil)
  (call-next-method)
  (values socket))

(defmethod close :around ((socket passive-socket) &key abort)
  (declare (ignore abort))
  (call-next-method)
  (setf (slot-value socket 'bound) nil)
  (setf (slot-value socket 'listening) nil)
  (values socket))

(defmethod close ((socket socket) &key abort)
  (declare (ignore socket abort)))

(defmethod socket-open-p ((socket socket))
  (unless (fd-of socket)
    (return-from socket-open-p nil))
  (with-socket-error-filter
    (handler-case
        (with-foreign-object (ss 'et:sockaddr-storage)
          (et:bzero ss et:size-of-sockaddr-storage)
          (with-foreign-pointer (size et:size-of-socklen)
            (setf (mem-ref size :socklen)
                  et:size-of-sockaddr-storage)
            (et:getsockname (fd-of socket) ss size)
            t))
      (unix-error (err)
        (case (error-identifier err)
          ((:ebadf
            #+freebsd :econnreset)
           nil)
          ;; some other error
          (otherwise (error err)))))))


;;;;;;;;;;;;;;;;;;;
;;  GETSOCKNAME  ;;
;;;;;;;;;;;;;;;;;;;

(defmethod local-name ((socket internet-socket))
  (with-foreign-object (ss 'et:sockaddr-storage)
    (et:bzero ss et:size-of-sockaddr-storage)
    (with-foreign-pointer (size et:size-of-socklen)
      (setf (mem-ref size :socklen)
            et:size-of-sockaddr-storage)
      (with-socket-error-filter
        (et:getsockname (fd-of socket) ss size))
      (return-from local-name
        (sockaddr-storage->sockaddr ss)))))

(defmethod local-name ((socket local-socket))
  (with-foreign-object (sun 'et:sockaddr-un)
    (et:bzero sun et:size-of-sockaddr-un)
    (with-foreign-pointer (size et:size-of-socklen)
      (setf (mem-ref size :socklen)
            et:size-of-sockaddr-storage)
      (with-socket-error-filter
        (et:getsockname (fd-of socket) sun size))
      (return-from local-name
        (sockaddr-un->sockaddr sun)))))

(defmethod socket-address ((socket socket))
  (nth-value 0 (local-name socket)))

(defmethod socket-port ((socket internet-socket))
  (nth-value 1 (local-name socket)))


;;;;;;;;;;;;;;;;;;;
;;  GETPEERNAME  ;;
;;;;;;;;;;;;;;;;;;;

(defmethod remote-name ((socket internet-socket))
  (with-foreign-object (ss 'et:sockaddr-storage)
    (et:bzero ss et:size-of-sockaddr-storage)
    (with-foreign-pointer (size et:size-of-socklen)
      (setf (mem-ref size :socklen)
            et:size-of-sockaddr-storage)
      (with-socket-error-filter
        (et:getpeername (fd-of socket) ss size))
      (return-from remote-name
        (sockaddr-storage->sockaddr ss)))))

(defmethod remote-name ((socket local-socket))
  (with-foreign-object (sun 'et:sockaddr-un)
    (et:bzero sun et:size-of-sockaddr-un)
    (with-foreign-pointer (size et:size-of-socklen)
      (setf (mem-ref size :socklen)
            et:size-of-sockaddr-storage)
      (with-socket-error-filter
        (et:getpeername (fd-of socket) sun size))
      (return-from remote-name
        (sockaddr-un->sockaddr sun)))))


;;;;;;;;;;;;
;;  BIND  ;;
;;;;;;;;;;;;

(defmethod bind-address :before ((socket internet-socket)
                                 address &key (reuse-address t))
  (declare (ignore address))
  (when reuse-address
    (set-socket-option socket :reuse-address :value t)))

(defun bind-ipv4-address (fd address port)
  (with-foreign-object (sin 'et:sockaddr-in)
    (make-sockaddr-in sin address port)
    (with-socket-error-filter
      (et:bind fd sin et:size-of-sockaddr-in))))

(defun bind-ipv6-address (fd address port)
  (with-foreign-object (sin6 'et:sockaddr-in6)
    (make-sockaddr-in6 sin6 address port)
    (with-socket-error-filter
      (et:bind fd sin6 et:size-of-sockaddr-in6))))

(defmethod bind-address ((socket internet-socket)
                         (address ipv4addr)
                         &key (port 0))
  (if (eql (socket-family socket) :ipv6)
      (bind-ipv6-address (fd-of socket)
                         (map-ipv4-vector-to-ipv6 (name address))
                         port)
      (bind-ipv4-address (fd-of socket) (name address) port))
  (values socket))

(defmethod bind-address ((socket internet-socket)
                         (address ipv6addr)
                         &key (port 0))
  (bind-ipv6-address (fd-of socket) (name address) port)
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
      (et:bind (fd-of socket) sun et:size-of-sockaddr-un)))
  (values socket))

(defmethod bind-address :after ((socket socket)
                                (address sockaddr) &key)
  (setf (slot-value socket 'bound) t))


;;;;;;;;;;;;;;
;;  LISTEN  ;;
;;;;;;;;;;;;;;

(defmethod socket-listen ((socket passive-socket)
                          &key (backlog (min *default-backlog-size*
                                             +max-backlog-size+)))
  (check-type backlog unsigned-byte "a non-negative integer")
  (with-socket-error-filter
    (et:listen (fd-of socket) backlog))
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
  (with-foreign-object (ss 'et:sockaddr-storage)
    (et:bzero ss et:size-of-sockaddr-storage)
    (with-foreign-pointer (size et:size-of-socklen)
      (setf (mem-ref size :socklen)
            et:size-of-sockaddr-storage)
      (let (non-blocking-state
            client-fd)
        (with-socket-error-filter
          (handler-case
              (if wait
                  ;; do a "normal" accept
                  ;; Note: the socket may already be in non-blocking mode
                  (setf client-fd (et:accept (fd-of socket) ss size))
                  ;; set the socket to non-blocking mode before calling accept()
                  ;; if there's no new connection return NIL
                  (unwind-protect
                       (progn
                         ;; saving the current non-blocking state
                         (setf non-blocking-state (fd-non-blocking socket))
                         ;; switch the socket to non-blocking mode
                         (setf (fd-non-blocking socket) t)
                         (setf client-fd (et:accept (fd-of socket) ss size)))
                    ;; restoring the socket's non-blocking state
                    (setf (fd-non-blocking socket) non-blocking-state)))
            ;; the socket is marked non-blocking and there's no new connection
            (et:ewouldblock ()
              (return-from accept-connection nil))))

        (let ((client-socket
               ;; create the client socket object
               (make-instance (active-class socket)
                              :file-descriptor client-fd)))
          (return-from accept-connection client-socket))))))


;;;;;;;;;;;;;;;
;;  CONNECT  ;;
;;;;;;;;;;;;;;;

#+freebsd
(defmethod connect :before ((socket active-socket)
                            sockaddr &key)
  (declare (ignore sockaddr))
  (when *no-sigpipe*
    (set-socket-option socket :no-sigpipe :value t)))

(defun ipv4-connect (fd address port)
  (with-foreign-object (sin 'et:sockaddr-in)
    (make-sockaddr-in sin address port)
    (with-socket-error-filter
      (et:connect fd sin et:size-of-sockaddr-in))))

(defun ipv6-connect (fd address port)
  (with-foreign-object (sin6 'et:sockaddr-in6)
    (make-sockaddr-in6 sin6 address port)
    (with-socket-error-filter
      (et:connect fd sin6 et:size-of-sockaddr-in6))))

(defmethod connect ((socket internet-socket)
                    (address ipv4addr) &key (port 0))
  (if (eql (socket-family socket) :ipv6)
      (ipv6-connect (fd-of socket)
                    (map-ipv4-vector-to-ipv6 (name address))
                    port)
      (ipv4-connect (fd-of socket) (name address) port))
  (values socket))

(defmethod connect ((socket internet-socket)
                    (address ipv6addr) &key (port 0))
  (ipv6-connect (fd-of socket) (name address) port)
  (values socket))

(defmethod connect ((socket local-socket)
                    (address localaddr) &key)
  (with-foreign-object (sun 'et:sockaddr-un)
    (make-sockaddr-un sun (name address))
    (with-socket-error-filter
      (et:connect (fd-of socket) sun et:size-of-sockaddr-un)))
  (values socket))

(defmethod connect ((socket passive-socket)
                    address &key)
  (declare (ignore address))
  (error "You cannot connect passive sockets."))

(defmethod socket-connected-p ((socket socket))
  (unless (fd-of socket)
    (return-from socket-connected-p nil))
  (with-socket-error-filter
    (handler-case
        (with-foreign-object (ss 'et:sockaddr-storage)
          (et:bzero ss et:size-of-sockaddr-storage)
          (with-foreign-pointer (size et:size-of-socklen)
            (setf (mem-ref size :socklen)
                  et:size-of-sockaddr-storage)
            (et:getpeername (fd-of socket) ss size)
            t))
      (et:enotconn () nil))))


;;;;;;;;;;;;;;;;
;;  SHUTDOWN  ;;
;;;;;;;;;;;;;;;;

(defmethod shutdown ((socket active-socket) direction)
  (check-type direction (member :read :write :read-write)
              "valid direction specifier")
  (with-socket-error-filter
    (et:shutdown (fd-of socket)
                 (ecase direction
                   (:read et:shut-rd)
                   (:write et:shut-wr)
                   (:read-write et:shut-rdwr))))
  (values socket))

(defmethod shutdown ((socket passive-socket) direction)
  (declare (ignore direction))
  (error "You cannot shut down passive sockets."))


;;;;;;;;;;;;
;;  SEND  ;;
;;;;;;;;;;;;

(defun %normalize-send-buffer (buff start end ef)
  (setf (values start end) (%check-bounds buff start end))
  (etypecase buff
    (ub8-sarray (values buff start (- end start)))
    (ub8-vector (values (coerce buff 'ub8-sarray)
                        start (- end start)))
    (string     (values (%to-octets buff ef start end)
                        0 (- end start)))))

(defmethod socket-send ((buffer array)
                        (socket active-socket) &key (start 0) end
                        remote-address remote-port end-of-record
                        dont-route dont-wait (no-signal *no-sigpipe*)
                        out-of-band #+linux more #+linux confirm)
  (check-type start unsigned-byte
              "a non-negative unsigned integer")
  (check-type end (or unsigned-byte null)
              "a non-negative unsigned integer or NIL")
  (when (or remote-port remote-address)
    (check-type remote-address sockaddr "a network address")
    (check-type remote-port (unsigned-byte 16) "a valid IP port number"))
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
        (%normalize-send-buffer buffer start end (external-format-of socket))
      (with-foreign-object (ss 'et:sockaddr-storage)
        (et:bzero ss et:size-of-sockaddr-storage)
        (when remote-address
          (sockaddr->sockaddr-storage ss remote-address remote-port))
        (with-pointer-to-vector-data (buff-sap buff)
          (incf-pointer buff-sap start-offset)
          (with-socket-error-filter
            (return-from socket-send
              (et:sendto (fd-of socket)
                         buff-sap bufflen
                         flags
                         (if remote-address ss (null-pointer))
                         (if remote-address et:size-of-sockaddr-storage 0)))))))))

(defmethod socket-send (buffer (socket passive-socket) &key)
  (declare (ignore buffer))
  (error "You cannot send data on a passive socket."))


;;;;;;;;;;;;
;;  RECV  ;;
;;;;;;;;;;;;

(defun %normalize-receive-buffer (buff start end)
  (setf (values start end) (%check-bounds buff start end))
  (etypecase buff
    ((simple-array ub8 (*)) (values buff start (- end start)))))

(defmethod socket-receive ((buffer array)
                           (socket active-socket) &key (start 0) end
                           out-of-band peek wait-all
                           dont-wait (no-signal *no-sigpipe*))
  (let ((flags (logior (if out-of-band et:msg-oob 0)
                       (if peek        et:msg-peek 0)
                       (if wait-all    et:msg-waitall 0)
                       (if dont-wait   et:msg-dontwait 0)
                       (if no-signal   et:msg-nosignal 0)))
        bytes-received)
    (multiple-value-bind (buff start-offset bufflen)
        (%normalize-receive-buffer buffer start end)
      (with-foreign-object (ss 'et:sockaddr-storage)
        (et:bzero ss et:size-of-sockaddr-storage)
        (with-foreign-pointer (size et:size-of-socklen)
          (setf (mem-ref size :socklen)
                et:size-of-sockaddr-storage)
          (with-pointer-to-vector-data (buff-sap buff)
            (incf-pointer buff-sap start-offset)
            (with-socket-error-filter
              (setf bytes-received
                    (et:recvfrom (fd-of socket)
                                 buff-sap bufflen
                                 flags
                                 ss size)))))

        (return-from socket-receive
          ;; when socket is a datagram socket
          ;; return the sender's address as 3rd value
          (if (typep socket 'datagram-socket)
              (multiple-value-bind (remote-address remote-port)
                  (sockaddr-storage->sockaddr ss)
                (values buffer bytes-received remote-address remote-port))
              (values buffer bytes-received)))))))

(defmethod socket-receive (buffer (socket passive-socket) &key)
  (declare (ignore buffer))
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
      (et:bzero sin et:size-of-sockaddr-in)
      (setf (foreign-slot-value sin 'et:sockaddr-in 'et:addr) et:af-unspec)
      (et:connect (fd-of socket) sin et:size-of-sockaddr-in))))
