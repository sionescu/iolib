;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- DNS client constants.
;;;

(in-package :net.sockets)

(defconstant +dns-max-datagram-size+ 4096)

(defconstant +opcode-standard+ 0)

;;; Query types

(define-constant +query-type-map+
    '((:a . 1) (:ns . 2) (:cname . 5) (:soa . 6)
      (:wks . 11) (:ptr . 12) (:hinfo . 13) (:mx . 15)
      (:txt . 16) (:aaaa . 28) (:any . 255))
    :test #'equal)

(defun query-type-number (id)
  (cdr (assoc id +query-type-map+)))

(defun query-type-id (number)
  (car (rassoc number +query-type-map+)))

(defun dns-record-type-p (id)
  (query-type-number id))

;;; Query classes

(define-constant +query-class-map+
    '((:in . 1) (:any . 255))
    :test #'equal)

(defun query-class-number (id)
  (cdr (assoc id +query-class-map+)))

(defun query-class-id (number)
  (car (rassoc number +query-class-map+)))

;;; Error codes

(define-constant +rcode-map+
    '((:no-error . 0) (:format-error . 1)
      (:server-failure . 2) (:name-error . 3)
      (:not-implemented . 4) (:refused . 5))
    :test #'equal)

(defun rcode-number (id)
  (cdr (assoc id +rcode-map+)))

(defun rcode-id (number)
  (car (rassoc number +rcode-map+)))
