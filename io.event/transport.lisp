;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; io-channel.lisp --- Transport protocol.
;;;
;;; Copyright (C) 2007, Stelian Ionescu  <sionescu@common-lisp.net>
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
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

;;;; Transport

(defclass transport ()
  ((event-base :initarg :event-base :accessor event-base-of)
   (protocol :initarg :protocol :accessor protocol-of)
   (read-handler :accessor read-handler-of)
   (write-handler :accessor write-handler-of)
   (error-handler :accessor error-handler-of)))

(defgeneric on-transport-readable (transport)
  (:documentation ""))

(defgeneric on-transport-writable (transport)
  (:documentation ""))

(defgeneric on-transport-error (transport)
  (:documentation ""))

(defgeneric write-data (data transport &key &allow-other-keys)
  (:documentation ""))

(defgeneric close-transport (transport &key &allow-other-keys)
  (:documentation ""))

(defconstant +default-read-window-size+ 8192
  "")

(defclass buffered-transport (transport)
  ((read-buffer :accessor read-buffer-of)
   (read-buffered-p :initarg :read-buffered-p
                    :accessor read-buffered-p
                    :initform t)
   (read-window-size :initarg :read-window-size
                     :accessor read-window-size-of
                     :initform t)
   (write-buffer :accessor write-buffer-of)
   (write-buffered-p :initarg :write-buffered-p
                     :accessor write-buffered-p
                     :initform +default-read-window-size+))
  (:documentation ""))

(defmethod initialize-instance :after ((channel buffered-transport) &key
                                       read-buffer-size write-buffer-size)
  (when (read-buffered-p channel)
    (setf (read-buffer-of channel)
          (make-instance 'io-buffer :size read-buffer-size)))
  (when (write-buffered-p channel)
    (setf (write-buffer-of channel)
          (make-instance 'io-buffer :size write-buffer-size))))

;;;; Socket Transport

(defclass socket-transport (transport)
  ((socket :initarg :socket :accessor socket-of))
  (:documentation ""))

(defmethod initialize-instance :after ((transport socket-transport) &key)
  (macrolet ((handler (event callback)
               `(add-fd (event-base-of transport) (fd-of (socket-of transport))
                        ,event (lambda (fd event)
                                 (declare (ignore fd event))
                                 (,callback transport)))))
    (setf (read-handler-of transport) (handler :read on-transport-readable)
          (write-handler-of transport) (handler :write on-transport-writable)
          (error-handler-of transport) (handler :error on-transport-error))))

(defmethod close-transport ((c socket-transport) &key abort)
  (remove-event (event-base-of c) (read-handler-of c))
  (remove-event (event-base-of c) (write-handler-of c))
  (remove-event (event-base-of c) (error-handler-of c))
  (close (socket-of c) :abort abort))

;;;; TCP Transport

;;; Unbuffered, for now.
(defclass tcp-transport (#-(and) buffered-transport socket-transport)
  ((status :initform :unconnected :accessor status-of))
  (:documentation ""))

(defmethod on-transport-readable ((c tcp-transport))
  (unless (eq (status-of c) :connected)
    (warn "ON-TRANSPORT-READABLE on non-connected socket")
    (return-from on-transport-readable))
  (let ((buffer (make-array +default-read-window-size+
                            :element-type '(unsigned-byte 8)))
        (byte-num 0))
    (declare (type unsigned-byte byte-num))
    (handler-case
        (setf (values buffer byte-num) (socket-receive buffer (socket-of c)))
      ;; a spurious event!
      (nix:ewouldblock ()
        (warn "Got a transport-readable event but recv() returned ~
               EWOULDBLOCK!"))
      ;; FIXME: perhaps we might be a little more sophisticated here
      (socket-error (err)
        (setf (status-of c) :disconnected)
        (on-connection-lost (protocol-of c) c err)))
    (cond
      ;; EOF
      ((zerop byte-num)
       (setf (status-of c) :disconnected)
       (on-connection-end (protocol-of c) c))
      ;; good data
      ((plusp byte-num)
       (on-data-received (protocol-of c) c
                         (make-array byte-num
                                     :element-type (array-element-type buffer)
                                     :displaced-to buffer
                                     :displaced-index-offset 0))))))

(defmethod write-data (data (c tcp-transport) &key)
  (handler-case
      (let ((count (socket-send data (socket-of c))))
        (when (/= count (length data))
          ;; here it should copy what it didn't manage to send,
          ;; and then write it out ON-TRANSPORT-WRITABLE.
          (warn "WRITE-DATA didn't send everything")))
    (nix:ewouldblock ()
      (warn "WRITE-DATA EWOULDBLOCK"))))

;;; FIXME: deal with full write kernel buffers
(defmethod on-transport-writable ((c tcp-transport))
  ;; not exactly complete: infact subsequent :WRITE
  ;; events must be handled
  (cond ((eq (status-of c) :unconnected)
         (on-connection-made (protocol-of c) c)
         (setf (status-of c) :connected))
        (t (warn "tcp on-transport-writable: implement me"))))

(defmethod on-transport-error ((transport tcp-transport))
  ;; are we supposed to close the socket now?
  (on-connection-lost (protocol-of transport)
                      transport
                      (sockets::lookup-socket-error
                       (get-socket-option (socket-of transport) :error))))

;;;; UDP Transport

(defclass udp-transport (socket-transport)
  ()
  (:documentation ""))

(defmethod on-transport-readable ((c udp-transport))
  (handler-case
      (multiple-value-bind (buffer byte-num address port)
          (socket-receive (make-array +default-read-window-size+
                                      :element-type '(unsigned-byte 8))
                          (socket-of c))
        (on-datagram-received
         (protocol-of c) c
         (make-array byte-num
                     :element-type (array-element-type buffer)
                     :displaced-to buffer
                     :displaced-index-offset 0)
         address
         port))
    ;; a spurious event!
    (nix:ewouldblock ()
      (warn "Got a transport-readable event but recv() returned ~
             EWOULDBLOCK!"))
    ;; FIXME: perhaps we might be a little more sophisticated here
    (socket-error (err)
      (warn "got error: ~S" err))))

;;; we can probably just use WRITE-DATA with :REMOTE-ADDRESS and
;;; :REMOTE-PORT instead of this separate function.
(defgeneric write-datagram (datagram address port transport
                            &key &allow-other-keys)
  (:documentation "")
  (:method (datagram address port (c udp-transport) &key)
    (handler-case
        (let ((count (socket-send datagram (socket-of c)
                                  :remote-address address
                                  :remote-port port)))
          (when (/= count (length datagram))
            ;; here it should copy what it didn't manage to send,
            ;; and then write it out ON-TRANSPORT-WRITABLE.
            (warn "WRITE-DATA didn't send everything")))
      (nix:ewouldblock ()
        (warn "WRITE-DATA EWOULDBLOCK"))
      (socket-error (err)
        (warn "write-datagram: got ~S" err)))))

;;; FIXME: deal with full write kernel buffers
(defmethod on-transport-writable ((c udp-transport))
  (warn "udp on-transport-writable: implement me"))

;;; FIXME: complete it
(defmethod on-transport-error ((c udp-transport))
  (let ((error-code (get-socket-option (socket-of c) :error)))
    (warn "got socket error: ~A" error-code)))
