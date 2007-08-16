;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; make-socket.lisp --- Socket creation.
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

(defun xnor (x1 x2)
  (eq (not x1) (not x2)))

;;; FIXME: protocol is sort of misinterpreted.
;;;
;;; CREATE-SOCKET is a a bit of a confusing name as it is too similar
;;; to MAKE-SOCKET.  I thought about different names and considered
;;; the option of moving this "factory" code could be moved into
;;; MAKE-INSTANCE methods.  However, AFAICT, MAKE-SOCKET can achieve
;;; the exact same effect as CREATE-SOCKET when some of the parameters
;;; are ommitted so I have opted to remove CREATE-SOCKET from the
;;; export list. --luis
(defun create-socket (&key (family :internet) (type :stream) (connect :active)
                      (protocol :default) (ipv6 *ipv6*)
                      (external-format :default))
  ;; (check-type address-family (member :internet :local))
  (check-type type (member :stream :datagram))
  (check-type connect (member :active :passive))
  (check-type ipv6 (member nil t :ipv6))
  (when (or (null family) (eq family :internet))
    (setf family (if ipv6 :ipv6 :ipv4)))
  (let ((class (select-socket-type family type connect protocol)))
    (make-instance class
                   :family family
                   :external-format external-format)))

(defmacro %close-on-error ((obj) &body body)
  (alexandria:with-unique-names (flag)
    `(let ((,flag t))
       (unwind-protect (multiple-value-prog1 (progn ,@body) (setf ,flag nil))
         (when (and ,obj ,flag) (close ,obj :abort t))))))

(declaim (inline %make-internet-stream-socket))
(defun %make-internet-stream-socket (args connect ef)
  (let (socket address)
    (destructuring-bind (&key local-host local-port remote-host remote-port
                              backlog reuse-address keepalive nodelay family)
        args
      (ecase connect
        (:active
         (assert (xnor remote-host remote-port))
         (%close-on-error (socket)
           (setf socket (create-socket :family family :type :stream
                                       :connect :active :external-format ef))
           (when keepalive (set-socket-option socket :keep-alive :value t))
           (when nodelay (set-socket-option socket :tcp-nodelay :value t))
           (when local-host
             (setf address (convert-or-lookup-inet-address local-host))
             (bind-address socket address :port (or local-port 0)
                           :reuse-address reuse-address))
           (when remote-host
             (setf address (convert-or-lookup-inet-address remote-host))
             (connect socket address :port remote-port))))
        (:passive
         (%close-on-error (socket)
           (setf socket (create-socket :family family :type :stream
                                       :connect :passive :external-format ef))
           (when local-host
             (setf address (convert-or-lookup-inet-address local-host))
             (bind-address socket address :port (or local-port 0)
                           :reuse-address reuse-address)
             (socket-listen socket :backlog backlog))))))
    (values socket)))

(declaim (inline %make-local-stream-socket))
(defun %make-local-stream-socket (args connect ef)
  (let (socket)
    (destructuring-bind (&key local-filename remote-filename backlog family)
        args
      (ecase connect
        (:active
         (assert remote-filename)
         (%close-on-error (socket)
           (setf socket (create-socket :family family :type :stream
                                       :connect :active :external-format ef))
           (when local-filename
             (bind-address socket (make-address local-filename)))
           (connect socket (make-address remote-filename))))
        (:passive
         (assert local-filename)
         (%close-on-error (socket)
           (setf socket (create-socket :family family :type :stream
                                       :connect :passive
                                       :external-format ef))
           (bind-address socket (make-address local-filename))
           (socket-listen socket :backlog backlog)))))
    (values socket)))

(declaim (inline %make-internet-datagram-socket))
(defun %make-internet-datagram-socket (args ef)
  (let (socket address)
    (destructuring-bind (&key local-host local-port remote-host remote-port
                              reuse-address broadcast family)
        args
      (assert (xnor local-host local-port))
      (assert (xnor remote-host remote-port))
      (%close-on-error (socket)
        (setf socket (create-socket :family family :type :datagram
                                    :connect :active :external-format ef))
        (when broadcast (set-socket-option socket :broadcast :value t))
        (when local-host
          (setf address (convert-or-lookup-inet-address local-host))
          (bind-address socket address :port local-port
                        :reuse-address reuse-address))
        (when remote-host
          (setf address (convert-or-lookup-inet-address remote-host))
          (connect socket address :port remote-port))))
    (values socket)))

(declaim (inline %make-local-datagram-socket))
(defun %make-local-datagram-socket (args ef)
  (let (socket address)
    (destructuring-bind (&key local-filename remote-filename family) args
      (%close-on-error (socket)
        (setf socket (create-socket :family family :type :datagram
                                    :connect :active :external-format ef))
        (when local-filename
          (bind-address socket (make-address address)))
        (when remote-filename
          (connect socket (make-address address)))))
    (values socket)))

;;; Changed ADDRESS-FAMILY to FAMILY and accept :IPV4 and :IPV6 as
;;; arguments so we can create an IPv4 socket with
;;;   (make-socket :family :ipv4)
;;; instead of
;;;   (make-socket #|:family :internet|# :ipv6 nil)
;;; This is not compatible with Allegro's MAKE-SOCKET behaviour.
;;; Is the renaming and the acceptance of extra values a problem?
;;; (We need to be careful with *IPV6* for starters.)
(defun make-socket (&rest args &key (family :internet) (type :stream)
                    (connect :active) (ipv6 *ipv6*) format eol
                    (external-format :default) scope-id &allow-other-keys)
  "Creates a socket instance of the appropriate subclass of SOCKET."
  (declare (ignore format eol scope-id))
  (check-type family (member :internet :local :ipv4 :ipv6))
  (check-type type (member :stream :datagram))
  (check-type connect (member :active :passive))
  (remf args :ipv6)
  (remf args :external-format)
  (remf args :type)
  (remf args :connect)
  (let ((*ipv6* (if (eq family :ipv4) nil ipv6))
        (address-family (if (or (eq family :ipv4) (eq family :ipv6))
                            :internet
                            family)))
    (cond
      ((and (eq address-family :internet) (eq type :stream))
       (%make-internet-stream-socket args connect external-format))
      ((and (eq address-family :local) (eq type :stream))
       (%make-local-stream-socket args connect external-format))
      ((and (eq address-family :internet) (eq type :datagram))
       (%make-internet-datagram-socket args external-format))
      ((and (eq address-family :local) (eq type :datagram))
       (%make-local-datagram-socket args external-format)))))

(defmacro with-socket ((var &rest args) &body body)
  "VAR is bound to a socket created by passing ARGS to
MAKE-SOCKET and BODY is executed as implicit PROGN.  The socket
is automatically closed upon exit."
  `(with-open-stream (,var (make-socket ,@args)) ,@body))
