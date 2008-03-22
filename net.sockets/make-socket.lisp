;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
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

(defun create-socket (family type connect external-format &key fd ibs obs)
  (cond
    ;; this is necessary because passive sockets don't inherit from
    ;; stream classes, therefore keyword args :INPUT-BUFFER-SIZE and
    ;; :OUTPUT-BUFFER-SIZE are invalid for them
    ((or ibs obs)
     (assert (eq connect :active))
     (make-instance (select-socket-class family type connect :default)
                    :family family :file-descriptor fd
                    :external-format external-format
                    :input-buffer-size ibs
                    :output-buffer-size obs))
    (t (make-instance (select-socket-class family type connect :default)
                      :family family :file-descriptor fd
                      :external-format external-format))))

(defmacro with-close-on-error ((var value) &body body)
  "Bind `VAR' to `VALUE', execute `BODY' as implicit PROGN and return `VAR'.
If a non-local exit occurs during the execution of `BODY' call CLOSE with :ABORT T on `VAR'."
  (with-gensyms (errorp)
    `(let ((,var ,value) (,errorp t))
       (unwind-protect
            (multiple-value-prog1 (locally ,@body ,var) (setf ,errorp nil))
         (when (and ,var ,errorp) (close ,var :abort t))))))

(defmacro with-guard-again-non-list-args-and-destructuring-bind-errors
    (form args &body body)
  `(if (listp ,args)
       (handler-case (progn ,@body)
         (error (err) `(error ,err)))
       ,form))

;;; Internet Stream Active Socket creation

(defun %%make-internet-stream-active-socket (family ef keepalive nodelay reuse-address
                                             local-host local-port remote-host remote-port
                                             input-buffer-size output-buffer-size)
  (let ((local-port  (ensure-numerical-service local-port))
        (remote-port (ensure-numerical-service remote-port)))
    (with-close-on-error (socket (create-socket family :stream :active ef
                                                :ibs input-buffer-size
                                                :obs output-buffer-size))
      (when keepalive (setf (socket-option socket :keep-alive) t))
      (when nodelay (setf (socket-option socket :tcp-nodelay) t))
      (when local-host
        (bind-address socket (ensure-hostname local-host)
                      :port local-port
                      :reuse-address reuse-address))
      (when (plusp remote-port)
        (connect socket (ensure-hostname remote-host)
                 :port remote-port)))))

(defun %make-internet-stream-active-socket (args family ef)
  (destructuring-bind (&key keepalive nodelay (reuse-address t)
                            local-host (local-port 0)
                            (remote-host +any-host+) (remote-port 0)
                            input-buffer-size output-buffer-size)
      args
    (%%make-internet-stream-active-socket family ef keepalive nodelay reuse-address
                                          local-host local-port remote-host remote-port
                                          input-buffer-size output-buffer-size)))

(define-compiler-macro %make-internet-stream-active-socket (&whole form args family ef)
  (with-guard-again-non-list-args-and-destructuring-bind-errors
      form args
    (destructuring-bind (&key keepalive nodelay (reuse-address t)
                              local-host (local-port 0)
                              (remote-host +any-host+) (remote-port 0)
                              input-buffer-size output-buffer-size)
        (cdr args)
      `(%%make-internet-stream-active-socket ,family ,ef ,keepalive ,nodelay ,reuse-address
                                             ,local-host ,local-port ,remote-host ,remote-port
                                             ,input-buffer-size ,output-buffer-size))))

;;; Internet Stream Passive Socket creation

(defun %%make-internet-stream-passive-socket (family ef interface reuse-address
                                              local-host local-port backlog)
  (let ((local-port  (ensure-numerical-service local-port)))
    (with-close-on-error (socket (create-socket family :stream :passive ef))
      (when local-host
        (when interface
          (setf (socket-option socket :bind-to-device) interface))
        (bind-address socket (ensure-hostname local-host)
                      :port local-port
                      :reuse-address reuse-address)
        (listen-on socket :backlog backlog)))))

(defun %make-internet-stream-passive-socket (args family ef)
  (destructuring-bind (&key interface (reuse-address t)
                            (local-host +any-host+) (local-port 0)
                            (backlog *default-backlog-size*))
      args
    (%%make-internet-stream-passive-socket family ef interface reuse-address
                                           local-host local-port backlog)))

(define-compiler-macro %make-internet-stream-passive-socket (&whole form args family ef)
  (with-guard-again-non-list-args-and-destructuring-bind-errors
      form args
    (destructuring-bind (&key interface (reuse-address t)
                              (local-host +any-host+) (local-port 0)
                              (backlog *default-backlog-size*))
        (cdr args)
      `(%%make-internet-stream-passive-socket ,family ,ef ,interface ,reuse-address
                                              ,local-host ,local-port ,backlog))))

;;; Local Stream Active Socket creation

(defun %%make-local-stream-active-socket (family ef local-filename remote-filename
                                          input-buffer-size output-buffer-size)
  (with-close-on-error (socket (create-socket family :stream :active ef
                                              :ibs input-buffer-size
                                              :obs output-buffer-size))
    (when local-filename
      (bind-address socket (ensure-address local-filename :family :local)))
    (when remote-filename
      (connect socket (ensure-address remote-filename :family :local)))))

(defun %make-local-stream-active-socket (args family ef)
  (destructuring-bind (&key local-filename remote-filename
                            input-buffer-size output-buffer-size)
      args
    (%%make-local-stream-active-socket family ef local-filename remote-filename
                                       input-buffer-size output-buffer-size)))

(define-compiler-macro %make-local-stream-active-socket (&whole form args family ef)
  (with-guard-again-non-list-args-and-destructuring-bind-errors
      form args
    (destructuring-bind (&key local-filename remote-filename
                              input-buffer-size output-buffer-size)
        (cdr args)
      `(%%make-local-stream-active-socket ,family ,ef ,local-filename ,remote-filename
                                          ,input-buffer-size ,output-buffer-size))))

;;; Local Stream Passive Socket creation

(defun %%make-local-stream-passive-socket (family ef local-filename reuse-address backlog)
  (with-close-on-error (socket (create-socket family :stream :passive ef))
    (when local-filename
      (bind-address socket (ensure-address local-filename :family :local)
                    :reuse-address reuse-address)
      (listen-on socket :backlog backlog))))

(defun %make-local-stream-passive-socket (args family ef)
  (destructuring-bind (&key local-filename (reuse-address t)
                            (backlog *default-backlog-size*))
      args
    (%%make-local-stream-passive-socket family ef local-filename reuse-address backlog)))

(define-compiler-macro %make-local-stream-passive-socket (&whole form args family ef)
  (with-guard-again-non-list-args-and-destructuring-bind-errors
      form args
    (destructuring-bind (&key local-filename (reuse-address t)
                              (backlog *default-backlog-size*))
        (cdr args)
      `(%%make-local-stream-passive-socket ,family ,ef ,local-filename ,reuse-address ,backlog))))

;;; Internet Datagram Socket creation

(defun %%make-internet-datagram-socket (family ef broadcast interface reuse-address
                                        local-host local-port remote-host remote-port)
  (let ((local-port  (ensure-numerical-service local-port))
        (remote-port (ensure-numerical-service remote-port)))
    (with-close-on-error (socket (create-socket family :datagram :active ef))
      (when broadcast (setf (socket-option socket :broadcast) t))
      (when local-host
        (bind-address socket (ensure-hostname local-host)
                      :port local-port
                      :reuse-address reuse-address)
        (when interface
          (setf (socket-option socket :bind-to-device) interface)))
      (when (plusp remote-port)
        (connect socket (ensure-hostname remote-host)
                 :port remote-port)))))

(defun %make-internet-datagram-socket (args family ef)
  (destructuring-bind (&key broadcast interface (reuse-address t)
                            local-host (local-port 0)
                            (remote-host +any-host+) (remote-port 0))
      args
    (%%make-internet-datagram-socket family ef broadcast interface reuse-address
                                     local-host local-port remote-host remote-port)))

(define-compiler-macro %make-internet-datagram-socket (&whole form args family ef)
  (with-guard-again-non-list-args-and-destructuring-bind-errors
      form args
    (destructuring-bind (&key broadcast interface (reuse-address t)
                              local-host (local-port 0)
                              (remote-host +any-host+) (remote-port 0))
        (cdr args)
      `(%%make-internet-datagram-socket ,family ,ef ,broadcast ,interface ,reuse-address
                                        ,local-host ,local-port ,remote-host ,remote-port))))

;;; Local Datagram Socket creation

(defun %%make-local-datagram-socket (family ef local-filename remote-filename)
  (with-close-on-error (socket (create-socket family :datagram :active ef))
    (when local-filename
      (bind-address socket (ensure-address local-filename :family :local)))
    (when remote-filename
      (connect socket (ensure-address remote-filename :family :local)))))

(defun %make-local-datagram-socket (args family ef)
  (destructuring-bind (&key local-filename remote-filename)
      args
    (%%make-local-datagram-socket family ef local-filename remote-filename)))

(define-compiler-macro %make-local-datagram-socket (&whole form args family ef)
  (with-guard-again-non-list-args-and-destructuring-bind-errors
      form args
    (destructuring-bind (&key local-filename remote-filename)
        (cdr args)
      `(%%make-local-datagram-socket ,family ,ef ,local-filename ,remote-filename))))

;;; MAKE-SOCKET

(defun make-socket (&rest args &key (family :internet) (type :stream)
                    (connect :active) (ipv6 *ipv6*)
                    (external-format :default) &allow-other-keys)
  "Creates a socket instance of the appropriate subclass of SOCKET."
  (check-type family (member :internet :local :ipv4 :ipv6) "one of :INTERNET, :LOCAL, :IPV4 or :IPV6")
  (check-type type (member :stream :datagram) "either :STREAM or :DATAGRAM")
  (check-type connect (member :active :passive) "either :ACTIVE or :PASSIVE")
  (let ((args (remove-from-plist args :family :type :connect :external-format :ipv6)))
    (when (eq :ipv4 family) (setf ipv6 nil))
    (let ((*ipv6* ipv6))
      (when (eq :internet family) (setf family +default-inet-family+))
      (multiple-value-case ((family type connect) :test #'eq)
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

(define-compiler-macro make-socket (&whole form &rest args &key (family :internet) (type :stream)
                                    (connect :active) (ipv6 '*ipv6* ipv6p)
                                    (external-format :default) &allow-other-keys)
  (cond
    ((and (constantp family) (constantp type) (constantp connect))
     (check-type family (member :internet :local :ipv4 :ipv6) "one of :INTERNET, :LOCAL, :IPV4 or :IPV6")
     (check-type type (member :stream :datagram) "either :STREAM or :DATAGRAM")
     (check-type connect (member :active :passive) "either :ACTIVE or :PASSIVE")
     (let ((lower-function
            (multiple-value-case ((family type connect) :test #'eq)
              (((:ipv4 :ipv6 :internet) :stream :active) '%make-internet-stream-active-socket)
              (((:ipv4 :ipv6 :internet) :stream :passive) '%make-internet-stream-passive-socket)
              ((:local :stream :active) '%make-local-stream-active-socket)
              ((:local :stream :passive) '%make-local-stream-passive-socket)
              (((:ipv4 :ipv6 :internet) :datagram) '%make-internet-datagram-socket)
              ((:local :datagram) '%make-local-datagram-socket)))
           (newargs (remove-from-plist args :family :type :connect :external-format :ipv6)))
       (multiple-value-case (family)
         (:internet (setf family '+default-inet-family+))
         (:ipv4     (setf ipv6 nil)))
       (let ((expansion `(,lower-function (list ,@newargs) ,family ,external-format)))
         (if (or ipv6p (eq :ipv4 family))
             `(let ((*ipv6* ,ipv6)) ,expansion)
             expansion))))
    (t form)))

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

;;; MAKE-SOCKET-STREAM

(defun get-address-family (fd)
  (with-sockaddr-storage (ss)
    (with-socklen (size size-of-sockaddr-storage)
      (%getsockname fd ss size)
      (foreign-slot-value ss 'sockaddr-storage 'family))))

(defun make-socket-stream (fd &key (external-format :default) (errorp t)
                           input-buffer-size output-buffer-size)
  "Creates an active stream socket instance of the appropriate subclass of SOCKET using `FD'.
The address family of the sockets is automatically discovered using OS functions. Buffer sizes
for the new socket can also be specified using `INPUT-BUFFER-SIZE' and `OUTPUT-BUFFER-SIZE'.
If `FD' is an invalid socket descriptor and `ERRORP' is not NIL a condition subtype of POSIX-ERROR
is signaled, otherwise two values are returned: NIL and the specific condition object."
  (flet ((%make-socket-stream ()
           (let ((family (switch ((get-address-family fd) :test #'=)
                           (af-inet :ipv4)
                           (af-inet6 :ipv6)
                           (af-local :local))))
             (create-socket family :stream :active external-format :fd fd
                            :input-buffer-size input-buffer-size
                            :output-buffer-size output-buffer-size))))
    (if errorp
        (%make-socket-stream)
        (ignore-some-conditions (posix-error)
          (%make-socket-stream)))))
