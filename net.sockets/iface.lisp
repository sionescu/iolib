;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Network interface class and operators.
;;;

(in-package :net.sockets)

(defclass interface ()
  ((name  :initarg :name :reader interface-name)
   (index :initarg :index :reader interface-index))
  (:documentation "Class describing a network interface."))
(unset-method-docstring #'interface-name () '(interface))
(set-function-docstring 'interface-name "Return the name of an INTERFACE.")
(unset-method-docstring #'interface-index () '(interface))
(set-function-docstring 'interface-index "Return the index number of an INTERFACE.")

(defmethod print-object ((interface interface) stream)
  (print-unreadable-object (interface stream :type nil :identity nil)
    (with-slots (name index) interface
      (format stream "Network Interface: ~S Index: ~A" name index))))

(defun make-interface (name index)
  "Constructor for INTERFACE objects."
  (make-instance 'interface :name name :index index))

(define-condition unknown-interface (system-error)
  ((datum :initarg :datum :initform nil :reader unknown-interface-datum))
  (:report (lambda (condition stream)
             (format stream "Unknown interface: ~A"
                     (unknown-interface-datum condition))))
  (:documentation "Condition raised when a network interface is not found."))
(setf (documentation 'unknown-interface-datum 'function)
      "Return the datum that caused the signalling of an UNKNOWN-INTERFACE condition.")

(defun signal-unknown-interface-error (system-error datum)
  (error 'unknown-interface
         :code (osicat-sys:system-error-code system-error)
         :identifier (osicat-sys:system-error-identifier system-error)
         :datum datum))

(defun list-network-interfaces ()
  "Returns a list of network interfaces currently available."
  (let ((ifptr (null-pointer)))
    (macrolet ((%if-slot-value (slot index)
                 `(foreign-slot-value
                   (mem-aref ifptr 'if-nameindex ,index)
                   'if-nameindex ,slot)))
      (unwind-protect
           (progn
             (setf ifptr (%if-nameindex))
             (loop :for i :from 0
                   :for name := (%if-slot-value 'name i)
                   :for index := (%if-slot-value 'index i)
               :while (plusp index) :collect (make-interface name index)))
        (unless (null-pointer-p ifptr) (%if-freenameindex ifptr))))))

(defun get-interface-by-index (index)
  (with-foreign-object (buffer :uint8 ifnamesize)
    (handler-case
        (%if-indextoname index buffer)
      (nix:enxio (error)
        (signal-unknown-interface-error error index))
      (:no-error (name)
        (make-interface name index)))))

(defun get-interface-by-name (name)
  (handler-case
      (%if-nametoindex name)
    (nix:enxio (error)
      (signal-unknown-interface-error error name))
    (:no-error (index)
      (make-interface (copy-seq name) index))))

(defun lookup-interface (interface)
  "Lookup an interface by name or index.  UNKNOWN-INTERFACE is
signalled if an interface is not found."
  (check-type interface (or unsigned-byte string symbol) "non-negative integer, a string or a symbol")
  (let ((interface (ensure-string-or-unsigned-byte interface)))
    (etypecase interface
      (unsigned-byte (get-interface-by-index interface))
      (string        (get-interface-by-name interface)))))
