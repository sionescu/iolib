;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; make-socket.lisp --- Socket creation.
;;;
;;; Copyright (C) 2006-2008, Stelian Ionescu  <sionescu@common-lisp.net>
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

(defun create-socket (family type connect external-format)
  (make-instance (select-socket-class family type connect :default)
                 :family family
                 :external-format external-format))

(defmacro %with-close-on-error ((var value) &body body)
  "Bind VAR to VALUE, execute BODY as implicit PROGN and return VAR.
On error call CLOSE with :ABORT T on VAR."
  (with-gensyms (errorp)
    `(let ((,var ,value) (,errorp t))
       (unwind-protect
            (multiple-value-prog1 (locally ,@body ,var) (setf ,errorp nil))
         (when (and ,var ,errorp) (close ,var :abort t))))))

(defun convert-or-lookup-inet-address (address &optional (ipv6 *ipv6*))
  "If ADDRESS is an inet-address designator, it is converted, if
necessary, to an INET-ADDRESS object and returned.  Otherwise it
is assumed to be a host name which is then looked up in order to
return its primary address as the first return value and the
remaining address list as the second return value."
  (or (ignore-parse-errors (ensure-address address :internet))
      (let ((addresses (lookup-host address :ipv6 ipv6)))
        (values (car addresses) (cdr addresses)))))

(define-symbol-macro +default-host+
    (if *ipv6* +ipv6-unspecified+ +ipv4-unspecified+))

(defun %make-internet-stream-active-socket (args family ef)
  (destructuring-bind (&key keepalive nodelay (reuse-address t)
                            (local-host +default-host+) (local-port 0)
                            (remote-host +default-host+) (remote-port 0))
      args
    (let ((local-port  (ensure-numerical-service local-port))
          (remote-port (ensure-numerical-service remote-port)))
      (%with-close-on-error (socket (create-socket family :stream :active ef))
        (when keepalive (set-socket-option socket :keep-alive :value t))
        (when nodelay (set-socket-option socket :tcp-nodelay :value t))
        (when local-host
          (bind-address socket (convert-or-lookup-inet-address local-host)
                        :port local-port
                        :reuse-address reuse-address))
        (when (plusp remote-port)
          (connect socket (convert-or-lookup-inet-address remote-host)
                   :port remote-port))))))

(defun %make-internet-stream-passive-socket (args family ef)
  (destructuring-bind (&key interface (reuse-address t)
                            (local-host +default-host+) (local-port 0)
                            (backlog *default-backlog-size*))
      args
    (let ((local-port  (ensure-numerical-service local-port)))
      (%with-close-on-error (socket (create-socket family :stream :passive ef))
        (when local-host
          (when interface
            (set-socket-option socket :bind-to-device :value interface))
          (bind-address socket (convert-or-lookup-inet-address local-host)
                        :port local-port
                        :reuse-address reuse-address)
          (socket-listen socket :backlog backlog))))))

(defun %make-local-stream-active-socket (args family ef)
  (destructuring-bind (&key local-filename remote-filename)
      args
    (%with-close-on-error (socket (create-socket family :stream :active ef))
      (when local-filename
        (bind-address socket (ensure-address local-filename :local)))
      (when remote-filename
        (connect socket (ensure-address remote-filename :local))))))

(defun %make-local-stream-passive-socket (args family ef)
  (destructuring-bind (&key local-filename (reuse-address t)
                            (backlog *default-backlog-size*))
      args
    (%with-close-on-error (socket (create-socket family :stream :passive ef))
      (when local-filename
        (bind-address socket (ensure-address local-filename :local)
                      :reuse-address reuse-address)
        (socket-listen socket :backlog backlog)))))

(defun %make-internet-datagram-socket (args family ef)
  (destructuring-bind (&key broadcast interface (reuse-address t)
                            (local-host +default-host+) (local-port 0)
                            (remote-host +default-host+) (remote-port 0))
      args
    (let ((local-port  (ensure-numerical-service local-port))
          (remote-port (ensure-numerical-service remote-port)))
      (%with-close-on-error (socket (create-socket family :datagram :active ef))
        (when broadcast (set-socket-option socket :broadcast :value t))
        (when local-host
          (bind-address socket (convert-or-lookup-inet-address local-host)
                        :port local-port
                        :reuse-address reuse-address)
          (when interface
            (set-socket-option socket :bind-to-device :value interface)))
        (when (plusp remote-port)
          (connect socket (convert-or-lookup-inet-address remote-host)
                   :port remote-port))))))

(defun %make-local-datagram-socket (args family ef)
  (destructuring-bind (&key local-filename remote-filename) args
    (%with-close-on-error (socket (create-socket family :datagram :active ef))
      (when local-filename
        (bind-address socket (ensure-address local-filename :local)))
      (when remote-filename
        (connect socket (ensure-address remote-filename :local))))))

(defun make-socket (&rest args &key (family :internet) (type :stream)
                    (connect :active) (ipv6 *ipv6*)
                    (external-format :default) &allow-other-keys)
  "Creates a socket instance of the appropriate subclass of SOCKET."
  (check-type family (member :internet :local :ipv4 :ipv6) "one of :INTERNET, :LOCAL, :IPV4 or :IPV6")
  (check-type type (member :stream :datagram) "either :STREAM or :DATAGRAM")
  (check-type connect (member :active :passive) "either :ACTIVE or :PASSIVE")
  (let ((args (remove-properties args '(:family :type :connect :external-format :ipv6))))
    (case family
      (:internet (setf family (if ipv6 :ipv6 :ipv4)))
      (:ipv4     (setf ipv6 nil)))
    (let ((*ipv6* ipv6))
      (multiple-value-case (family type connect)
        (((:ipv4 :ipv6) :stream :active)
         (%make-internet-stream-active-socket args family external-format))
        (((:ipv4 :ipv6) :stream :passive)
         (%make-internet-stream-passive-socket args family external-format))
        ((:local :stream :active)
         (%make-local-stream-active-socket args :local external-format))
        ((:local :stream :passive)
         (%make-local-stream-passive-socket args :local external-format))
        (((:ipv4 :ipv6) :datagram)
         (%make-internet-datagram-socket args family external-format))
        ((:local :datagram)
         (%make-local-datagram-socket args :local external-format))))))

(defmacro with-open-socket ((var &rest args) &body body)
  "VAR is bound to a socket created by passing ARGS to
MAKE-SOCKET and BODY is executed as implicit PROGN.  The socket
is automatically closed upon exit."
  `(with-open-stream (,var (make-socket ,@args)) ,@body))

(defmacro with-accept-connection ((var passive-socket &rest args) &body body)
  "VAR is bound to a socket created by passing PASSIVE-SOCKET and ARGS to
ACCEPT-CONNECTION and BODY is executed as implicit PROGN.  The socket
is automatically closed upon exit."
  `(with-open-stream (,var (accept-connection ,passive-socket ,@args)) ,@body))
