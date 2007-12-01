;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; hosts.lisp --- Static host lookup.
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

(defvar *hosts-file* "/etc/hosts")

(defclass host ()
  ((truename :initform nil :initarg :truename
             :accessor host-truename
             :documentation "The name of the host.")
   (aliases :initform nil :initarg :aliases
            :accessor host-aliases
            :documentation "A list of aliases.")
   (addresses :initform nil :initarg :addresses
              :accessor host-addresses
              :documentation "A list of addresses."))
  (:documentation "Class representing a host: name, aliases and addresses."))

(defmethod initialize-instance :after ((host host) &key)
  (with-accessors ((name host-truename) (aliases host-aliases)
                   (addresses host-addresses)) host
    (flet ((namep (h) (and (stringp h) (plusp (length h)))))
      (assert (namep name))
      (assert (every #'namep aliases))
      (assert addresses)
      (setf addresses (ensure-list addresses))
      (map-into addresses #'ensure-address addresses))))

(defun host-random-address (host)
  "Returns a random address from HOST's address list."
  (random-elt (host-addresses host)))

(defun make-host (truename addresses &optional aliases)
  "Instantiates a HOST object."
  (make-instance 'host
                 :truename truename
                 :aliases aliases
                 :addresses addresses))

(defmethod print-object ((host host) stream)
  (print-unreadable-object (host stream :type t :identity nil)
    (with-slots (truename aliases addresses) host
      (format stream "Canonical name: ~S. Aliases: ~:[None~;~:*~{~S~^, ~}~]. Addresses: ~{~A~^, ~}"
              truename aliases addresses))))

(defvar *hosts-cache* ())

(defun map-host-ipv4-addresses-to-ipv6 (hostobj)
  (declare (type host hostobj))
  (with-accessors ((addresses host-addresses)) hostobj
    (setf addresses
          (mapcar (lambda (address)
                    (if (ipv4-address-p address)
                        (make-address (map-ipv4-vector-to-ipv6
                                       (address-name address)))
                        address))
                  addresses)))
  (values hostobj))

(defun parse-/etc/hosts (file)
  (let (hosts)
    (flet ((parse-one-line (tokens)
             (when (< (length tokens) 2) (error 'parse-error))
             (destructuring-bind (address cname &rest aliases) tokens
               (push (make-host cname (ensure-address address) aliases)
                     hosts))))
      (iterate ((tokens (serialize-etc-file file)))
        (ignore-errors (parse-one-line tokens)))
      (nreverse hosts))))

(defun update-hosts-list (file)
  (setf *hosts-cache* (parse-/etc/hosts file)))

(defun search-host-by-name (name ipv6)
  (labels ((compatible-address-p (address)
             (ecase ipv6
               ((t)   (inet-address-p address))
               ((nil) (ipv4-address-p address))
               (:ipv6 (ipv6-address-p address))))
           (compatible-host-p (host)
             (and (or (string= name (host-truename host))
                      (member name (host-aliases host)
                              :test #'string=))
                  (compatible-address-p (car (host-addresses host))))))
    (let ((hosts (remove-if-not #'compatible-host-p *hosts-cache*))
          addresses aliases)
      (when hosts
        (mapc #'(lambda (host)
                  (let ((address (car (host-addresses host))))
                    (push address addresses)
                    (push (cons (host-truename host) address) aliases)
                    (mapc #'(lambda (alias) (push (cons alias address) aliases))
                          (host-aliases host))))
              hosts)
        (values (nreverse addresses)
                (nreverse aliases))))))

(defun search-host-by-address (address)
  (let* ((address (ensure-address address))
         (host (find-if #'(lambda (host)
                            (address= (car (host-addresses host))
                                      address))
                        *hosts-cache*)))
    (when host
      (values address
              (list* (cons (host-truename host) address)
                     (mapcar #'(lambda (alias) (cons alias address))
                             (host-aliases host)))))))

(defvar *hosts-monitor*
  (make-instance 'file-monitor
                 :file *hosts-file*
                 :update-fn 'update-hosts-list))
