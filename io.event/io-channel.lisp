;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; io-channel.lisp - Manage a single I/O channel.
;;;
;;; Copyright (C) 2007, Stelian Ionescu  <sionescu@common-lisp.net>
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

(in-package :io.event)

;;;; IO Channel

(defclass io-channel ()
  ((event-loop :initarg :event-loop
               :accessor event-loop-of)
   (protocol :accessor protocol-of)
   (read-handler :accessor read-handler-of)
   (write-handler :accessor write-handler-of)
   (error-handler :accessor error-handler-of)))

(defconstant +default-read-window-size+ 8192)

(defclass io-buffered-channel (io-channel)
  ((read-buffer :accessor read-buffer-of)
   (read-buffered-p :initarg :read-buffered-p
                    :accessor read-buffered-p)
   (read-window-size :initarg :read-window-size
                     :accessor read-window-size-of)
   (write-buffer :accessor write-buffer-of)
   (write-buffered-p :initarg :write-buffered-p
                     :accessor write-buffered-p))
  (:default-initargs :read-buffered-p t
                     :write-buffered-p t
                     :read-window-size +default-read-window-size+))

(defmethod initialize-instance :after ((channel io-buffered-channel) &key
                                       read-buffer-size write-buffer-size)
  (when (read-buffered-p channel)
    (setf (read-buffer-of channel)
          (make-instance 'io-buffer :size read-buffer-size)))
  (when (write-buffered-p channel)
    (setf (write-buffer-of channel)
          (make-instance 'io-buffer :size write-buffer-size))))

;;;; Socket Transport

;;; probably a bad idea.  maybe a sign that having different classes
;;; for different kinds of sockets is a funky abstraction?
(defclass socket-transport (io-channel sockets::socket-stream-internet-active)
  ())

(defgeneric on-transport-readable (transport))
(defgeneric on-transport-writable (transport))
(defgeneric on-transport-error (transport))

;;;; TCP Transport

(defclass tcp-transport (io-buffered-channel socket-transport)
  ((status :initform :unconnected :accessor status-of)))

(defmethod shared-initialize :after ((transport tcp-transport) slots &key)
  (declare (ignore slots))
  (write-line "tcp-transport initialize-instance")
  (setf (read-handler-of transport)
        (add-fd (event-loop-of transport) (fd-of transport) :read
                (lambda (fd event)
                  (declare (ignore fd event))
                  (on-transport-readable transport))))
  (setf (write-handler-of transport)
        (add-fd (event-loop-of transport) (fd-of transport) :write
                (lambda (fd event)
                  (declare (ignore fd event))
                  (on-transport-writable transport))))
  (setf (error-handler-of transport)
        (add-fd (event-loop-of transport) (fd-of transport) :error
                (lambda (fd event)
                  (declare (ignore fd event))
                  (on-transport-error transport)))))

(defmethod on-transport-readable ((c tcp-transport))
  (with-accessors ((proto protocol-of) (status status-of)) c
    (assert (eq status :connected))
    (let ((buffer (make-array +default-read-window-size+
                              :element-type '(unsigned-byte 8)))
          (byte-num 0))
      (declare (type unsigned-byte byte-num))
      (handler-case
          (setf (values buffer byte-num) (socket-receive buffer c))
        ;; a spurious event !
        (nix:ewouldblock ()
          (error "Got a transport-readable event but recv() returned ~
                  EWOULDBLOCK !"))
        ;; FIXME: perhaps we might be a little more sophisticated here
        (socket-error (err)
          (setf status :disconnected)
          (on-connection-lost proto err)))
      (cond
        ;; EOF
        ((zerop byte-num)
         (setf status :disconnected)
         (on-connection-end proto))
        ;; good data
        ((plusp byte-num)
         (on-data-received proto
                           (make-array byte-num
                                       :element-type (array-element-type buffer)
                                       :displaced-to buffer
                                       :displaced-index-offset 0)))))))

;;; FIXME: deal with full write kernel buffers
(defmethod on-transport-writable ((c tcp-transport))
  (with-accessors ((proto protocol-of) (status status-of)) c
    ;; not exactly complete: infact subsequent :WRITE
    ;; events must be handled
    (when (eq status :unconnected)
      (on-connection-made proto)
      (setf status :connected))))

;;; FIXME: complete it
(defmethod on-transport-error ((c tcp-transport))
  (let ((error-code (get-socket-option c :error)))
    ))

;;;; UDP Transport

(defclass udp-transport (socket-transport)
  ())
