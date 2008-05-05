;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: NIL -*-
;;;
;;; Copyright (C) 2006-2008, Attila Lendvai  <attila.lendvai@gmail.com>
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

(in-package :net.sockets.cc)

(defclass connection-multiplexer ()
  ((name
    :initform "unnamed connection-multiplexer"
    :initarg :name
    :accessor name-of)
   (lock
    :initform (bordeaux-threads:make-recursive-lock "connection-multiplexer lock")
    :accessor lock-of)
   (fd->connection
    :initform (make-array 128 :adjustable t :initial-element nil)
    :accessor fd->connection-of)
   (fd-multiplexer
    :initform nil
    :accessor fd-multiplexer-of)
   (shutdown-requested
    :initform nil
    :accessor shutdown-requested-p)
   (workers
    :initform (make-array 4 :adjustable t :fill-pointer 0)
    :accessor workers-of)
   ;; configuration slots
   (worker-count
    :initform 4
    :initarg :worker-count
    :accessor worker-count-of)
   (external-format
    :initform :default
    :initarg :external-format
    :accessor external-format-of)
   (fd-multiplexer-type
    :initarg :fd-multiplexer-type
    :accessor fd-multiplexer-type-of)))

;; TODO use progn combination
(defgeneric startup-connection-multiplexer (multiplexer &key &allow-other-keys))
(defgeneric shutdown-connection-multiplexer (multiplexer &key force))

(defgeneric register-connection (multiplexer connection))
(defgeneric unregister-connection (multiplexer connection))
(defgeneric connection-registered-p (multiplexer connection))

(defclass connection-acceptor (connection-multiplexer)
  (;; slots for the runtime state
   (accepting-connection
    :initform nil
    :initarg :accepting-connection
    :accessor accepting-connection-of)
   ;; configuration slots
   (connection-handler
    :initarg :connection-handler
    :accessor connection-handler-of
    :type (or symbol function))))

(defclass connection-with-continuation-mixin ()
  ((continuation
    :initform nil
    :initarg :continuation
    :accessor continuation-of)
   (wait-reason
    :accessor wait-reason-of
    :initarg :wait-reason
    :type (member :read :write))))

(defclass bandwidth-information-mixin ()
  ((created-at
    :initform (get-internal-real-time)
    :accessor created-at-of)
   (total-bytes-written
    :initform 0
    :accessor total-bytes-written-of)
   (total-bytes-read
    :initform 0
    :accessor total-bytes-read-of)))

(defgeneric notify-bytes-written (connection count))
(defgeneric average-writing-speed-of (connection))
(defgeneric notify-bytes-read (connection count))
(defgeneric average-reading-speed-of (connection))

(defclass connection (connection-with-continuation-mixin
                      bandwidth-information-mixin
                      socket-stream-internet-active)
  ())

;; TODO why does connection inherit from socket-stream-internet-active and
;; accepting-connection only contain a socket slot?
(defclass accepting-connection (connection-with-continuation-mixin)
  ((socket
    :initarg :socket
    :reader socket-of)))

(defgeneric startup-acceptor (acceptor &key address port))
(defgeneric shutdown-acceptor (acceptor &key force))

