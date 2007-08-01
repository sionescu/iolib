;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; socket-methods.lisp --- Various socket methods.
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

;;; FIXME: should match :default to whatever protocol is the default.
(defun select-socket-type (family type connect protocol)
  (or (cdr (assoc (list family type connect protocol) *socket-type-map*
                  :test #'equal))
      (error "No socket class found !!")))

;;;; Shared Initialization

(defun translate-make-socket-keywords-to-constants (family type protocol)
  (let ((sf (ecase family
              (:ipv4  nix::af-inet)
              (:ipv6  nix::af-inet6)
              (:local nix::af-local)))
        (st (ecase type
              (:stream   nix::sock-stream)
              (:datagram nix::sock-dgram)))
        (sp (cond
              ((integerp protocol) protocol)
              ((eql protocol :default) 0)
              ((keywordp protocol)
               (protocol-number
                (lookup-protocol (string-downcase (string protocol))))))))
    (values sf st sp)))

(defmethod socket-fd ((socket socket))
  (fd-of socket))

(defmethod (setf socket-fd) (fd (socket socket))
  (setf (fd-of socket) fd))

;; TODO: we should add some sort of finalizer here to avoid leaking
;; sockets FDs and buffers.  Something along these lines:
;;   (when finalize
;;     (trivial-garbage:finalize socket (lambda () (close socket))))
;;
;; However SBCL's semantics don't allow this, since that reference to
;; the socket will prevent it from being garbage collected.  So we'd
;; need to get all necessary information into a closure or something
;; (foreign pointers, FDs, etc) in order to do that closing.
(defmethod shared-initialize :after ((socket socket) slot-names
                                     &key file-descriptor family type
                                     (protocol :default))
  (declare (ignore slot-names))
  (when (socket-open-p socket)
    (close socket))
  (with-accessors ((fd fd-of) (fam socket-family) (proto socket-protocol))
      socket
    (setf fd (or file-descriptor
                 (multiple-value-bind (sf st sp)
                     (translate-make-socket-keywords-to-constants
                      family type protocol)
                   (with-socket-error-filter
                     (nix:socket sf st sp)))))
    (setf fam family
          proto protocol)))

(defmethod (setf external-format-of) (external-format (socket passive-socket))
  (setf (slot-value socket 'external-format)
        (babel:ensure-external-format external-format)))

(defmethod shared-initialize :after ((socket passive-socket) slot-names
                                     &key external-format)
  (declare (ignore slot-names))
  (setf (external-format-of socket) external-format))

(defmethod socket-type ((socket stream-socket))
  :stream)

(defmethod socket-type ((socket datagram-socket))
  :datagram)

;;;; Printing

(defun sock-fam (socket)
  (ecase (socket-family socket)
    (:ipv4 "IPv4")
    (:ipv6 "IPv6")))

(defmethod print-object ((socket socket-stream-internet-active) stream)
  (print-unreadable-object (socket stream :identity t)
    (format stream "active ~A stream socket" (sock-fam socket))
    (if (socket-connected-p socket)
        (multiple-value-bind (addr port) (remote-name socket)
          (format stream " connected to ~A/~A"
                  (address-to-string addr) port))
        (format stream ", ~:[closed~;unconnected~]" (fd-of socket)))))

(defmethod print-object ((socket socket-stream-internet-passive) stream)
  (print-unreadable-object (socket stream :identity t)
    (format stream "passive ~A stream socket" (sock-fam socket))
    (if (socket-bound-p socket)
        (multiple-value-bind (addr port) (local-name socket)
          (format stream " ~:[bound to~;waiting @~] ~A/~A"
                  (socket-listening-p socket)
                  (address-to-string addr) port))
        (format stream ", ~:[closed~;unbound~]" (fd-of socket)))))

(defmethod print-object ((socket socket-stream-local-active) stream)
  (print-unreadable-object (socket stream :identity t)
    (format stream "active local stream socket")
    (if (socket-connected-p socket)
        (format stream " connected to ~A"
                (address-to-string (remote-address socket)))
        (format stream ", ~:[closed~;unconnected~]" (fd-of socket)))))

(defmethod print-object ((socket socket-stream-local-passive) stream)
  (print-unreadable-object (socket stream :identity t)
    (format stream "passive local stream socket")
    (if (socket-bound-p socket)
        (format stream " ~:[bound to~;waiting @~] ~A"
                  (socket-listening-p socket)
                  (address-to-string (local-address socket)))
        (format stream ", ~:[closed~;unbound~]" (fd-of socket)))))

(defmethod print-object ((socket socket-datagram-local-active) stream)
  (print-unreadable-object (socket stream :identity t)
    (format stream "local datagram socket")
    (if (socket-connected-p socket)
        (format stream " connected to ~A"
                (address-to-string (remote-address socket)))
        (format stream ", ~:[closed~;unconnected~]" (fd-of socket)))))

(defmethod print-object ((socket socket-datagram-internet-active) stream)
  (print-unreadable-object (socket stream :identity t)
    (format stream "~A datagram socket" (sock-fam socket))
    (if (socket-connected-p socket)
        (multiple-value-bind (addr port) (remote-name socket)
          (format stream " connected to ~A/~A"
                  (address-to-string addr) port))
        (format stream ", ~:[closed~;unconnected~]" (fd-of socket)))))

;;;; CLOSE

(defmethod close :around ((socket socket) &key abort)
  (declare (ignore abort))
  (call-next-method)
  (when (fd-of socket)
    (with-socket-error-filter
      (nix:close (fd-of socket))))
  (setf (fd-of socket) nil
        (slot-value socket 'bound) nil)
  (values socket))

(defmethod close :around ((socket passive-socket) &key abort)
  (declare (ignore abort))
  (call-next-method)
  (setf (slot-value socket 'listening) nil)
  (values socket))

(defmethod close ((socket socket) &key abort)
  (declare (ignore socket abort)))

(defmethod socket-open-p ((socket socket))
  (when (fd-of socket)
    (with-socket-error-filter
        (handler-case
            (with-foreign-object (ss 'nix::sockaddr-storage)
              (nix:bzero ss nix::size-of-sockaddr-storage)
              (with-socklen (size nix::size-of-sockaddr-storage)
                (nix:getsockname (fd-of socket) ss size)
                t))
          (nix:ebadf ())
          #+freebsd (nix:econnreset ())))))

;;;; GETSOCKNAME

(defmethod local-name ((socket socket))
  (with-foreign-object (ss 'nix::sockaddr-storage)
    (nix:bzero ss nix::size-of-sockaddr-storage)
    (with-socklen (size nix::size-of-sockaddr-storage)
      (with-socket-error-filter
        (nix:getsockname (fd-of socket) ss size))
      (sockaddr-storage->sockaddr ss))))

(defmethod local-address ((socket socket))
  (nth-value 0 (local-name socket)))

(defmethod local-port ((socket internet-socket))
  (nth-value 1 (local-name socket)))

;;;; GETPEERNAME

(defmethod remote-name ((socket socket))
  (with-foreign-object (ss 'nix::sockaddr-storage)
    (nix:bzero ss nix::size-of-sockaddr-storage)
    (with-socklen (size nix::size-of-sockaddr-storage)
      (with-socket-error-filter
          (nix:getpeername (fd-of socket) ss size))
      (sockaddr-storage->sockaddr ss))))

(defmethod remote-address ((socket socket))
  (nth-value 0 (remote-name socket)))

(defmethod remote-port ((socket internet-socket))
  (nth-value 1 (remote-name socket)))

;;;; BIND

(defmethod bind-address :before ((socket internet-socket) address
                                 &key (reuse-address t))
  (declare (ignore address))
  (when reuse-address
    (set-socket-option socket :reuse-address :value t)))

(defun bind-ipv4-address (fd address port)
  (with-sockaddr-in (sin address port)
    (with-socket-error-filter
      (nix:bind fd sin nix::size-of-sockaddr-in))))

(defun bind-ipv6-address (fd address port)
  (with-sockaddr-in6 (sin6 address port)
    (with-socket-error-filter
      (nix:bind fd sin6 nix::size-of-sockaddr-in6))))

(defmethod bind-address ((socket internet-socket) (address ipv4-address)
                         &key (port 0))
  (if (eql (socket-family socket) :ipv6)
      (bind-ipv6-address (fd-of socket)
                         (map-ipv4-vector-to-ipv6 (address-name address))
                         port)
      (bind-ipv4-address (fd-of socket) (address-name address) port))
  (values socket))

(defmethod bind-address ((socket internet-socket) (address ipv6-address)
                         &key (port 0))
  (bind-ipv6-address (fd-of socket) (address-name address) port)
  (values socket))

(defmethod bind-address ((socket local-socket) (address local-address) &key)
  (with-sockaddr-un (sun (address-name address))
    (with-socket-error-filter
      (nix:bind (fd-of socket) sun nix::size-of-sockaddr-un)))
  (values socket))

(defmethod bind-address :after ((socket socket) (address address) &key)
  (setf (slot-value socket 'bound) t))

;;;; LISTEN

(defmethod socket-listen ((socket passive-socket) &key backlog)
  (unless backlog (setf backlog (min *default-backlog-size*
                                     +max-backlog-size+)))
  (check-type backlog unsigned-byte "a non-negative integer")
  (with-socket-error-filter
    (nix:listen (fd-of socket) backlog))
  (setf (slot-value socket 'listening) t)
  (values socket))

(defmethod socket-listen ((socket active-socket) &key backlog)
  (declare (ignore backlog))
  (error "You can't listen on active sockets."))

;;;; ACCEPT

(defmethod accept-connection ((socket active-socket))
  (error "You can't accept connections on active sockets."))

(defmethod accept-connection ((socket passive-socket))
  (flet ((make-client-socket (fd)
           (make-instance (active-class socket)
                          :external-format (external-format-of socket)
                          :file-descriptor fd)))
    (with-foreign-object (ss 'nix::sockaddr-storage)
      (nix:bzero ss nix::size-of-sockaddr-storage)
      (with-socklen (size nix::size-of-sockaddr-storage)
        (with-socket-error-filter
          (handler-case
              (make-client-socket (nix:accept (fd-of socket) ss size))
            (nix:ewouldblock ())))))))

;;;; CONNECT

#+freebsd
(defmethod connect :before ((socket active-socket) sockaddr &key)
  (declare (ignore sockaddr))
  (set-socket-option socket :no-sigpipe :value t))

(defun ipv4-connect (fd address port)
  (with-sockaddr-in (sin address port)
    (with-socket-error-filter
      (nix:connect fd sin nix::size-of-sockaddr-in))))

(defun ipv6-connect (fd address port)
  (with-sockaddr-in6 (sin6 address port)
    (with-socket-error-filter
      (nix:connect fd sin6 nix::size-of-sockaddr-in6))))

(defmethod connect ((socket internet-socket) (address ipv4-address)
                    &key (port 0))
  (if (eql (socket-family socket) :ipv6)
      (ipv6-connect (fd-of socket)
                    (map-ipv4-vector-to-ipv6 (address-name address))
                    port)
      (ipv4-connect (fd-of socket) (address-name address) port))
  (values socket))

(defmethod connect ((socket internet-socket) (address ipv6-address)
                    &key (port 0))
  (ipv6-connect (fd-of socket) (address-name address) port)
  (values socket))

(defmethod connect ((socket local-socket) (address local-address) &key)
  (with-sockaddr-un (sun (address-name address))
    (with-socket-error-filter
      (nix:connect (fd-of socket) sun nix::size-of-sockaddr-un)))
  (values socket))

(defmethod connect ((socket passive-socket) address &key)
  (declare (ignore address))
  (error "You cannot connect passive sockets."))

(defmethod socket-connected-p ((socket socket))
  (when (fd-of socket)
    (with-socket-error-filter
        (handler-case
            (with-foreign-object (ss 'nix::sockaddr-storage)
              (nix:bzero ss nix::size-of-sockaddr-storage)
              (with-socklen (size nix::size-of-sockaddr-storage)
                (nix:getpeername (fd-of socket) ss size)
                t))
          (nix:enotconn ())))))

;;;; SHUTDOWN

(defmethod shutdown ((socket active-socket) direction)
  (check-type direction (member :read :write :read-write)
              "valid direction specifier")
  (with-socket-error-filter
    (nix:shutdown (fd-of socket)
                  (ecase direction
                    (:read nix::shut-rd)
                    (:write nix::shut-wr)
                    (:read-write nix::shut-rdwr))))
  (values socket))

(defmethod shutdown ((socket passive-socket) direction)
  (declare (ignore direction))
  (error "You cannot shut down passive sockets."))

;;;; SEND

(defun %normalize-send-buffer (buff start end ef)
  (check-bounds buff start end)
  (etypecase buff
    (ub8-sarray (values buff start (- end start)))
    (ub8-vector (values (coerce buff 'ub8-sarray)
                        start (- end start)))
    (string     (values (%to-octets buff ef start end)
                        0 (- end start)))))

(defmethod socket-send ((buffer array) (socket active-socket)
                        &key (start 0) end remote-address remote-port
                        end-of-record dont-route dont-wait no-signal
                        out-of-band #+linux more #+linux confirm)
  #+darwin (declare (ignore no-signal)) ; better warn?
  (check-type start unsigned-byte
              "a non-negative unsigned integer")
  (check-type end (or unsigned-byte null)
              "a non-negative unsigned integer or NIL")
  (when (or remote-port remote-address)
    (check-type remote-address address "a network address")
    (check-type remote-port (unsigned-byte 16) "a valid IP port number"))
  (let ((flags (logior (if end-of-record nix::msg-eor 0)
                       (if dont-route nix::msg-dontroute 0)
                       (if dont-wait  nix::msg-dontwait 0)
                       #-darwin (if no-signal  nix::msg-nosignal 0)
                       (if out-of-band nix::msg-oob 0)
                       #+linux (if more nix::msg-more 0)
                       #+linux (if confirm nix::msg-confirm 0))))
    (when (and (ipv4-address-p remote-address)
               (eql (socket-family socket) :ipv6))
      (setf remote-address (map-ipv4-address-to-ipv6 remote-address)))
    (multiple-value-bind (buff start-offset bufflen)
        (%normalize-send-buffer buffer start end (external-format-of socket))
      (with-foreign-object (ss 'nix::sockaddr-storage)
        (nix:bzero ss nix::size-of-sockaddr-storage)
        (when remote-address
          (sockaddr->sockaddr-storage ss remote-address remote-port))
        (with-pointer-to-vector-data (buff-sap buff)
          (incf-pointer buff-sap start-offset)
          (with-socket-error-filter
            (return-from socket-send
              (nix:sendto
               (fd-of socket)
               buff-sap bufflen
               flags
               (if remote-address ss (null-pointer))
               (if remote-address nix::size-of-sockaddr-storage 0)))))))))

(defmethod socket-send (buffer (socket passive-socket) &key)
  (declare (ignore buffer))
  (error "You cannot send data on a passive socket."))

;;;; RECV

(defun %normalize-receive-buffer (buff start end)
  (check-bounds buff start end)
  (etypecase buff
    ((simple-array ub8 (*)) (values buff start (- end start)))))

(defun calc-recvfrom-flags (out-of-band peek wait-all dont-wait no-signal)
  #+darwin (declare (ignore no-signal)) ; better warn?
  (logior (if out-of-band nix::msg-oob 0)
          (if peek        nix::msg-peek 0)
          (if wait-all    nix::msg-waitall 0)
          (if dont-wait   nix::msg-dontwait 0)
          #-darwin (if no-signal nix::msg-nosignal 0)))

(defun %do-recvfrom (buffer ss fd flags start end)
  (multiple-value-bind (buff start-offset bufflen)
      (%normalize-receive-buffer buffer start end)
    (with-socklen (size nix::size-of-sockaddr-storage)
      (nix:bzero ss nix::size-of-sockaddr-storage)
      (with-pointer-to-vector-data (buff-sap buff)
        (incf-pointer buff-sap start-offset)
        (with-socket-error-filter
          (return-from %do-recvfrom
            (nix:recvfrom fd buff-sap bufflen flags ss size)))))))

(defmethod socket-receive ((buffer array) (socket stream-socket) &key (start 0)
                           end out-of-band peek wait-all dont-wait no-signal)
  (with-foreign-object (ss 'nix::sockaddr-storage)
    (let* ((flags (calc-recvfrom-flags out-of-band peek wait-all
                                       dont-wait no-signal))
           (bytes-received (%do-recvfrom buffer ss (fd-of socket) flags
                                         start end)))
      (values buffer bytes-received))))

(defmethod socket-receive ((buffer array) (socket datagram-socket)
                           &key (start 0) end out-of-band peek wait-all
                           dont-wait no-signal)
  (with-foreign-object (ss 'nix::sockaddr-storage)
    (let* ((flags (calc-recvfrom-flags out-of-band peek wait-all dont-wait
                                       no-signal))
           (bytes-received (%do-recvfrom buffer ss (fd-of socket) flags
                                         start end)))
      (multiple-value-bind (remote-address remote-port)
          (sockaddr-storage->sockaddr ss)
        (values buffer bytes-received remote-address remote-port)))))

(defmethod socket-receive (buffer (socket passive-socket) &key)
  (declare (ignore buffer))
  (error "You cannot receive data from a passive socket."))

;;;; Datagram Sockets

(defmethod disconnect :before ((socket active-socket))
  (unless (typep socket 'datagram-socket)
    (error "You can only disconnect active datagram sockets.")))

(defmethod disconnect ((socket datagram-socket))
  (with-foreign-object (sin 'nix::sockaddr-in)
    (nix:bzero sin nix::size-of-sockaddr-in)
    (setf (foreign-slot-value sin 'nix::sockaddr-in 'nix::addr)
          nix::af-unspec)
    (with-socket-error-filter
      (nix:connect (fd-of socket) sin nix::size-of-sockaddr-in))))
