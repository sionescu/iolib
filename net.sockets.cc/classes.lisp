;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
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
  ((lock
    :initform (bordeaux-threads:make-recursive-lock "connection-multiplexer lock")
    :accessor lock-of)
   (fd->connection
    :initform (make-array 128 :adjustable t :initial-element nil)
    :accessor fd->connection-of)
   (fd-multiplexer
    :accessor fd-multiplexer-of)))

(defgeneric register-connection (multiplexer connection))
(defgeneric unregister-connection (multiplexer connection))
(defgeneric connection-registered-p (multiplexer connection))
(defgeneric close-connection-multiplexer (multiplexer))

(defclass connection-acceptor ()
  (;; slots for the runtime state
   (connection-multiplexer
    :accessor connection-multiplexer-of)
   (lock
    :initform (bordeaux-threads:make-lock "acceptor lock")
    :accessor lock-of)
   (shutdown-requested
    :initform nil
    :accessor shutdown-requested-p)
   (workers
    :initform (make-array 4 :adjustable t :fill-pointer 0)
    :accessor workers-of)
   (accepting-connection
    :initform nil
    :initarg :accepting-connection
    :accessor accepting-connection-of)
   ;; configuration slots
   (connection-handler
    :initarg :connection-handler
    :accessor connection-handler-of
    :type (or symbol function))
   (worker-count
    :initform 4
    :initarg :worker-count
    :accessor worker-count-of)
   (external-format
    :initform :default
    :initarg :external-format
    :reader external-format-of)))

(defclass connection-with-continuation-mixin ()
  ((continuation
    :initform nil
    :initarg :continuation
    :accessor continuation-of)
   (wait-reason
    :accessor wait-reason-of
    :initarg :wait-reason
    :type (member :read :write))))

(defclass connection (connection-with-continuation-mixin
                      socket-stream-internet-active)
  ())

(defclass accepting-connection (connection-with-continuation-mixin)
  ((socket
    :initarg :socket
    :reader socket-of)))

(defgeneric startup-acceptor (acceptor &key address port))
(defgeneric shutdown-acceptor (acceptor &key force))

