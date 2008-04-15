;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- DNS client constants.
;;;

(in-package :net.sockets)

(enable-reader-macro 'literal-hash-table)

(defconstant +dns-max-datagram-size+ 4096)

(defconstant +opcode-standard+ 0)

;;; Query types

(define-constant +query-type-to-value-map+
    #h:eq((:a . 1) (:ns . 2) (:cname . 5) (:soa . 6)
          (:wks . 11) (:ptr . 12) (:hinfo . 13) (:mx . 15)
          (:txt . 16) (:aaaa . 28) (:any . 255))
    :test #'equalp)

(define-constant +query-value-to-type-map+
    #h((1 . :a) (2 . :ns) (5 . :cname) (6 . :soa)
       (11 . :wks) (12 . :ptr) (13 . :hinfo) (15 . :mx)
       (16 . :txt) (28 . :aaaa) (255 . :any))
    :test #'equalp)

(defun query-type-number (id)
  (gethash id +query-type-to-value-map+))

(defun query-type-id (number)
  (gethash number +query-value-to-type-map+))

(defun dns-record-type-p (id)
  (query-type-number id))

;;; Query classes

(define-constant +query-class-to-value-map+
    #h:eq((:in . 1) (:any . 255))
    :test #'equalp)

(define-constant +query-value-to-class-map+
    #h((1 . :in) (255 . :any))
    :test #'equalp)

(defun query-class-number (id)
  (gethash id +query-class-to-value-map+))

(defun query-class-id (number)
  (gethash number +query-value-to-class-map+))

;;; Error codes

(define-constant +rcode-to-value-map+
    #h:eq((:no-error . 0) (:format-error . 1)
          (:server-failure . 2) (:name-error . 3)
          (:not-implemented . 4) (:refused . 5))
    :test #'equalp)

(define-constant +value-to-rcode-map+
    #h((0 . :no-error) (1 . :format-error)
       (2 . :server-failure) (3 . :name-error)
       (4 . :not-implemented) (5 . :refused))
    :test #'equalp)

(defun rcode-number (id)
  (gethash id +rcode-to-value-map+))

(defun rcode-id (number)
  (gethash number +value-to-rcode-map+))

(disable-reader-macro 'literal-hash-table)
