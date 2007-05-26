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

(defun create-socket (&key
                      (address-family :internet)
                      (type :stream)
                      (connect :active)
                      (protocol :default)
                      (ipv6 *ipv6*)
                      (external-format :default))
  (check-type address-family (member :internet :local))
  (check-type type (member :stream :datagram))
  (check-type connect (member :active :passive))
  (check-type ipv6 (member nil t :ipv6))
  (when (eq address-family :internet)
    (setf address-family (if ipv6 :ipv6 :ipv4)))
  (let ((socket-class
         (select-socket-type address-family type connect protocol)))
    (make-instance socket-class :family address-family
                   :external-format external-format)))

(declaim (inline %make-internet-stream-socket))
(defun %make-internet-stream-socket (args connect ipv6 ef)
  (let (socket address)
    (destructuring-bind (&key local-host local-port remote-host remote-port
                              backlog reuse-address keepalive nodelay &allow-other-keys) args
      (ecase connect
        (:active
         (assert (xnor local-host local-port))
         (assert (xnor remote-host remote-port))
         (setf socket (create-socket :address-family :internet :type :stream
                                     :connect :active :ipv6 ipv6
                                     :external-format ef))
         (when keepalive (set-socket-option socket :keep-alive :value t))
         (when nodelay (set-socket-option socket :tcp-nodelay :value t))
         (when local-host
           (setf address (convert-or-lookup-inet-address local-host ipv6))
           (bind-address socket address :port local-port
                         :reuse-address reuse-address))
         (when remote-host
           (setf address (convert-or-lookup-inet-address remote-host ipv6))
           (connect socket address :port remote-port)))
        (:passive
         (assert (xnor local-host local-port))
         (setf socket (create-socket :address-family :internet :type :stream
                                     :connect :passive :ipv6 ipv6))
         (when local-host
           (setf address (convert-or-lookup-inet-address local-host ipv6))
           (bind-address socket address :port local-port
                         :reuse-address reuse-address)
           (socket-listen socket :backlog backlog)))))
    (values socket)))

(declaim (inline %make-local-stream-socket))
(defun %make-local-stream-socket (args connect ef)
  (let (socket)
    (destructuring-bind (&key local-filename remote-filename backlog &allow-other-keys) args
      (ecase connect
        (:active
         (assert remote-filename)
         (setf socket (create-socket :address-family :local :type :stream
                                     :connect :active :external-format ef))
         (when local-filename (bind-address socket (make-address local-filename)))
         (connect socket (make-address remote-filename)))
        (:passive
         (assert local-filename)
         (setf socket (create-socket :address-family :local :type :stream
                                     :connect :passive))
         (bind-address socket (make-address local-filename))
         (socket-listen socket :backlog backlog))))
    (values socket)))

(declaim (inline %make-internet-datagram-socket))
(defun %make-internet-datagram-socket (args ipv6 ef)
  (let (socket address)
    (destructuring-bind (&key local-host local-port remote-host remote-port
                              reuse-address broadcast &allow-other-keys) args
      (assert (xnor local-host local-port))
      (assert (xnor remote-host remote-port))
      (setf socket (create-socket :address-family :internet :type :datagram
                                  :connect :active :ipv6 ipv6
                                  :external-format ef))
      (when broadcast (set-socket-option socket :broadcast :value t))
      (when local-host
        (setf address (convert-or-lookup-inet-address local-host ipv6))
        (bind-address socket address :port local-port
                      :reuse-address reuse-address))
      (when remote-host
        (setf address (convert-or-lookup-inet-address remote-host ipv6))
        (connect socket address :port remote-port)))
    (values socket)))

(declaim (inline %make-local-datagram-socket))
(defun %make-local-datagram-socket (args ef)
  (let (socket address)
    (destructuring-bind (&key local-filename remote-filename &allow-other-keys) args
      (setf socket (create-socket :address-family :local :type :datagram
                                  :connect :active :external-format ef))
      (when local-filename
        (bind-address socket (make-address address)))
      (when remote-filename
        (connect socket (make-address address))))
    (values socket)))

(defun make-socket (&rest args &key address-family type connect (ipv6 *ipv6*)
                    format eol (external-format :default) scope-id &allow-other-keys)
  (declare (ignore format eol scope-id))
  (cond
    ((and (eq address-family :internet) (eq type :stream))
     (%make-internet-stream-socket args connect ipv6 external-format))
    ((and (eq address-family :local) (eq type :stream))
     (%make-local-stream-socket args connect external-format))
    ((and (eq address-family :internet) (eq type :datagram))
     (%make-internet-datagram-socket args ipv6 external-format))
    ((and (eq address-family :local) (eq type :datagram))
     (%make-local-datagram-socket args external-format))))

(defmacro with-socket ((var &rest args) &body body)
  `(with-open-stream (,var (make-socket ,@args)) ,@body))
