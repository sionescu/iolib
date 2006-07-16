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

(declaim (optimize (speed 2) (safety 2) (space 1) (debug 2)))

(in-package #:net.sockets)

;;;;;;;;;;;;;
;;  CLOSE  ;;
;;;;;;;;;;;;;

(defmethod socket-close progn ((socket socket))
  (when (slot-boundp socket 'fd)
    (sb-posix:close (socket-fd socket)))
  (sb-ext:cancel-finalization socket)
  (mapc #'(lambda (slot)
            (slot-makunbound socket slot))
        '(fd address family protocol)))

(defmethod socket-close progn ((socket stream-socket))
  (slot-makunbound socket 'lisp-stream))

(defmethod socket-close progn ((socket internet-socket))
  (slot-makunbound socket 'port))

(defmethod socket-open-p ((socket socket))
  (unless (slot-boundp socket 'fd)
    (return-from socket-open-p nil))
  (handler-case
      (with-pinned-aliens ((ss sb-posix::sockaddr-storage)
                           (size sb-posix::socklen-t
                                 #.sb-posix::size-of-sockaddr-storage))
        (let ((ssptr (addr ss)))
          (sb-posix:getsockname (socket-fd socket)
                                ssptr (addr size))
          t))
    (sb-posix:syscall-error (err)
      (case (sb-posix:syscall-errno err)
        ((#.sb-posix:ebadf
          #.sb-posix:enotsock
          #.sb-posix:econnreset)
         nil)
        ;; some other error
        (otherwise (error err))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;  SHARED-INITIALIZE  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-make-socket-keywords-to-constants (family type protocol)
  (let ((sf (ecase family
              (:ipv4 sb-posix::af-inet)
              (:ipv6 sb-posix::af-inet6)
              (:unix sb-posix::af-unix)))
        (st (ecase type
              (:stream   sb-posix::sock-stream)
              (:datagram sb-posix::sock-dgram)))
        (sp (cond
              ((integerp protocol) protocol)
              ((eql protocol :default) 0)
              ((keywordp protocol)
               (protocol-number
                (get-protocol-by-name (string-downcase
                                       (symbol-name protocol))))))))
    (values sf st sp)))

(defmethod shared-initialize :after ((socket socket) slot-names &key family type (protocol :default))
  (when (socket-open-p socket)
    (socket-close socket))
  (with-slots (fd (fam family) (proto protocol)) socket
    (multiple-value-bind (sf st sp)
        (translate-make-socket-keywords-to-constants family type protocol)
      (setf fd (sb-posix::socket sf st sp))
      (setf fam family)
      (setf proto sp)
      (sb-ext:finalize socket #'(lambda () (sb-posix:close fd))))))

(defmethod shared-initialize :after ((socket stream-socket) slot-names &key)
  (setf (slot-value socket 'lisp-stream)
        (sb-sys:make-fd-stream (socket-fd socket)
                               :name (format nil "Socket stream, fd: ~a" (socket-fd socket))
                               :input t :output t :buffering :none :dual-channel-p t
                               :element-type :default :auto-close nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  get and set O_NONBLOCK  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod socket-non-blocking-mode ((socket socket))
  (with-slots (fd) socket
    (let ((fflags (sb-posix:fcntl fd sb-posix::f-getfl)))
      (not (zerop (logand fflags sb-posix:o-nonblock))))))

(defmethod (setf socket-non-blocking-mode) (value (socket socket))
  (check-type value boolean "a boolean value")
  (with-slots (fd) socket
    (let ((fflags (sb-posix:fcntl fd sb-posix::f-getfl)))
      (sb-posix:fcntl fd sb-posix::f-setfl
                      (logior fflags
                              (if value sb-posix:o-nonblock 0))))))

;;;;;;;;;;;;;;;;;;;
;;  GETSOCKNAME  ;;
;;;;;;;;;;;;;;;;;;;

(defmethod local-name ((socket internet-socket))
  (with-pinned-aliens ((ss sb-posix::sockaddr-storage)
                       (size sb-posix::socklen-t
                             #.sb-posix::size-of-sockaddr-storage))
    (let ((ssptr (addr ss)))
      (sb-posix:getsockname (socket-fd socket)
                            ssptr (addr size))
      (values (make-netaddr-from-sockaddr-storage ssptr)
              (ntohs (slot (cast ssptr (* sb-posix::sockaddr-in))
                           'sb-posix::port))))))
(defmethod local-name ((socket unix-socket))
  (with-pinned-aliens ((sun sb-posix::sockaddr-un)
                       (size sb-posix::socklen-t
                             #.sb-posix::size-of-sockaddr-un))
    (let ((sunptr (addr sun)))
      (sb-posix:getsockname (socket-fd socket)
                            sunptr (addr size))
      (values (make-netaddr-from-sockaddr-un sunptr)))))

;;;;;;;;;;;;;;;;;;;
;;  GETPEERNAME  ;;
;;;;;;;;;;;;;;;;;;;

(defmethod remote-name ((socket internet-socket))
  (with-pinned-aliens ((ss sb-posix::sockaddr-storage)
                       (size sb-posix::socklen-t
                             #.sb-posix::size-of-sockaddr-storage))
    (let ((ssptr (addr ss)))
      (sb-posix:getpeername (socket-fd socket)
                            ssptr (addr size))
      (values (make-netaddr-from-sockaddr-storage ssptr)
              (ntohs (slot (cast ssptr (* sb-posix::sockaddr-in))
                           'sb-posix::port))))))

(defmethod remote-name ((socket unix-socket))
  (with-pinned-aliens ((sun sb-posix::sockaddr-un)
                       (size sb-posix::socklen-t
                             #.sb-posix::size-of-sockaddr-un))
    (let ((sunptr (addr sun)))
      (sb-posix:getpeername (socket-fd socket)
                            sunptr (addr size))
      (values (make-netaddr-from-sockaddr-un sunptr)))))

;;;;;;;;;;;;;;
;;  LISTEN  ;;
;;;;;;;;;;;;;;

(defmethod socket-listen ((socket passive-socket)
                          &key (backlog (min *default-backlog-size*
                                             +max-backlog-size+)))
  (check-type backlog unsigned-byte "a non-negative integer")
  (sb-posix:listen (socket-fd socket) backlog))

(defmethod socket-listen ((socket active-socket)
                          &key backlog)
  (declare (ignore backlog))
  (error "You can't listen on active sockets."))

;;;;;;;;;;;;;;
;;  ACCEPT  ;;
;;;;;;;;;;;;;;

(defmethod socket-accept-connection :before ((socket passive-socket)
                                             &key (wait t))
  (when (typep socket 'datagram-socket)
    (error "You can't accept connections on datagram sockets.")))

(defmethod socket-accept-connection ((socket passive-socket)
                                     &key (wait t))
  (with-pinned-aliens ((ss sb-posix::sockaddr-storage)
                       (size sb-posix::socklen-t
                             #.sb-posix::size-of-sockaddr-storage))
    (let ((ssptr (addr ss))
          non-blocking-state)
      (if wait
          (progn
            (sb-posix:accept (socket-fd socket)
                             ssptr (addr size)))
          (handler-case
              (unwind-protect
                   (progn
                     ;; saving the current non-blocking state
                     (setf non-blocking-state (socket-non-blocking-mode socket))
                     (sb-posix:accept (socket-fd socket)
                                      ssptr (addr size)))
                ;; restoring the socket's non-blocking state
                (setf (socket-non-blocking-mode socket) non-blocking-state))
            (sb-posix:syscall-error (err)
              (case (sb-posix:syscall-errno err)
                ;; this means there's no new connection
                ((#.sb-posix:eagain #.sb-posix:ewouldblock)
                 (return-from socket-accept-connection nil))
                ;; some other error
                (otherwise (error err))))))
      (make-netaddr-from-sockaddr-storage ssptr))))

(defmethod socket-accept-connection ((socket active-socket)
                                     &key wait)
  (declare (ignore active-socket)
           (ignore wait))
  (error "You can't accept connections on active sockets."))


;;;;;;;;;;;;;;;
;;  CONNECT  ;;
;;;;;;;;;;;;;;;

(defmethod socket-connect ((socket internet-socket)
                           (address ipv4addr) &key (port 0))
  (with-pinned-aliens ((sin sb-posix::sockaddr-in))
    (make-sockaddr-in (addr sin) (name address) port)
    (sb-posix::connect (socket-fd socket)
                       (addr sin)
                       sb-posix::size-of-sockaddr-in)
    (setf (slot-value socket 'address) (copy-netaddr address))
    (setf (slot-value socket 'port) port)))

(defmethod socket-connect ((socket internet-socket)
                           (address ipv6addr) &key (port 0))
  (with-pinned-aliens ((sin6 sb-posix::sockaddr-in6))
    (make-sockaddr-in6 (addr sin6) (name address) port)
    (sb-posix::connect (socket-fd socket)
                       (addr sin6)
                       sb-posix::size-of-sockaddr-in6)
    (setf (slot-value socket 'address) (copy-netaddr address))
    (setf (slot-value socket 'port) port)))

(defmethod socket-connect ((socket unix-socket)
                           (address unixaddr) &key)
  (with-pinned-aliens ((sun sb-posix::sockaddr-un))
    (make-sockaddr-un (addr sun) (name address))
    (sb-posix::connect (socket-fd socket)
                       (addr sun)
                       sb-posix::size-of-sockaddr-un)
    (setf (slot-value socket 'address) (copy-netaddr address))))

(defmethod socket-connect ((socket passive-socket)
                           address &key)
  (error "You cannot connect passive sockets."))

;;;;;;;;;;;;;;;;
;;  SHUTDOWN  ;;
;;;;;;;;;;;;;;;;

(defmethod socket-shutdown ((socket active-socket) direction)
  (check-type direction (member :read :write :read-write)
              "valid shutdown specifier")
  (sb-posix::shutdown (socket-fd socket)
                      (ecase direction
                        (:read sb-posix::shut-rd)
                        (:write sb-posix::shut-wr)
                        (:read-write sb-posix::shut-rdwr))))

(defmethod socket-shutdown ((socket passive-socket) direction)
  (error "You cannot shut down passive sockets."))

;;;;;;;;;;;;
;;  BIND  ;;
;;;;;;;;;;;;

(defmethod socket-bind-address :before ((socket internet-socket)
                                        address &key (reuse-address t))
  (when reuse-address
    (set-socket-option socket :reuse-address :value t)))

(defmethod socket-bind-address ((socket internet-socket)
                                (address ipv4addr)
                                &key (port 0) reuse-address interface)
  (with-pinned-aliens ((sin sb-posix::sockaddr-in))
    (make-sockaddr-in (addr sin) (name address) port)
    (sb-posix::bind (socket-fd socket)
                    (addr sin)
                    sb-posix::size-of-sockaddr-in)
    (setf (slot-value socket 'address) (copy-netaddr address))))

(defmethod socket-bind-address ((socket internet-socket)
                                (address ipv6addr)
                                &key (port 0) reuse-address interface)
  (with-pinned-aliens ((sin6 sb-posix::sockaddr-in6))
    (make-sockaddr-in6 (addr sin6) (name address) port)
    (sb-posix::bind (socket-fd socket)
                    (addr sin6)
                    sb-posix::size-of-sockaddr-in6)
    (setf (slot-value socket 'address) (copy-netaddr address))))

(defmethod socket-bind-address :before ((socket unix-socket)
                                        (address unixaddr) &key)
  (when (typep socket 'active-socket)
    (error "You can't bind an active Unix socket.")))

(defmethod socket-bind-address ((socket unix-socket)
                                (address unixaddr) &key)
  (with-pinned-aliens ((sun sb-posix::sockaddr-un))
    (make-sockaddr-un (addr sun) (name address))
    (sb-posix::bind (socket-fd socket)
                    (addr sun)
                    sb-posix::size-of-sockaddr-un)
    (setf (slot-value socket 'address) (copy-netaddr address))))


;;;;;;;;;;;;
;;  SEND  ;;
;;;;;;;;;;;;

(defmethod socket-send ((buffer simple-array)
                        (socket socket-stream-internet-active) &key
                        dont-route dont-wait (no-signal *no-sigpipe*)
                        out-of-band #+linux more &allow-other-keys)
  )

;;;;;;;;;;;;
;;  RECV  ;;
;;;;;;;;;;;;

(defmethod socket-receive ((buffer simple-array)
                           (socket socket-stream-internet-active) &key
                           out-of-band peek wait-all dont-wait &allow-other-keys)
  )


;;
;; Only for datagram sockets ATM
;;

(defmethod socket-unconnect :before ((socket active-socket))
  (unless (typep socket 'datagram-socket)
    (error "You can only unconnect active datagram sockets.")))

(defmethod socket-unconnect ((socket datagram-socket))
  (with-pinned-aliens ((sin sb-posix::sockaddr-in))
    (sb-posix::memset (addr sin) 0 sb-posix::size-of-sockaddr-in)
    (setf (slot sin 'sb-posix::addr) sb-posix::af-unspec)
    (sb-posix::connect (socket-fd socket)
                       (addr sin)
                       sb-posix::size-of-sockaddr-in)
    (slot-makunbound socket 'address)
    (when (typep socket 'internet-socket)
      (slot-makunbound socket 'port))))
