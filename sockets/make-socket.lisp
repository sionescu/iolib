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

(defun make-socket (&key
                    (address-family :internet)
                    (type :stream)
                    (connect :active)
                    (protocol :default)
                    (ipv6 *ipv6*))
  (check-type address-family (member :internet :local))
  (check-type type (member :stream :datagram))
  (check-type connect (member :active :passive))
  (check-type ipv6 (member nil t :ipv6))
  (when (eql address-family :internet)
    (setf address-family (if ipv6 :ipv6 :ipv4)))
  (let ((socket-class
         (select-socket-type address-family type connect protocol)))
    (make-instance socket-class :family address-family)))

(defun open-client-socket (family type address &optional port (ipv6 *ipv6*))
  (let* ((addr (ensure-address address family))
         (socket (make-socket :address-family family
                              :type type
                              :connect :active
                              :protocol :default
                              :ipv6 ipv6)))
    (connect socket addr :port port)))

(defun open-server-socket (family address &key
                           port reuse-address
                           backlog (ipv6 *ipv6*))
  (let* ((addr (ensure-address address family))
         (socket (make-socket :address-family family
                              :connect :passive
                              :protocol :default
                              :ipv6 ipv6)))
    (bind-address socket addr :port port
                  :reuse-address reuse-address)
    (socket-listen socket :backlog backlog)))

(defmacro with-socket ((var &rest args) &body body)
  `(with-open-stream (,var (make-socket ,@args)) ,@body))

(defmacro with-client-socket ((var &key family type address port (ipv6 nil ipv6p)) &body body)
  `(with-open-stream (,var  ,(if ipv6p `(open-client-socket ,family ,type ,address ,port ,ipv6)
                                       `(open-client-socket ,family ,type ,address ,port)))
     ,@body))

(defmacro with-server-socket ((var &key address port reuse-address
                                   backlog (ipv6 nil ipv6p)) &body body)
  `(with-open-stream (,var  ,(if ipv6p `(open-server-socket ,address :port ,port
                                                            :reuse-address ,reuse-address
                                                            :backlog ,backlog
                                                            :ipv6 ,ipv6)
                                       `(open-server-socket ,address :port ,port
                                                            :reuse-address ,reuse-address
                                                            :backlog ,backlog)))
     ,@body))
