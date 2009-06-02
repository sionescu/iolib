;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Surrogates of chars.
;;;

(in-package :iolib.base)

;;;-------------------------------------------------------------------------
;;; Constants
;;;-------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant uchar-code-limit #x110000))


;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(deftype uchar ()
  '(mod #.uchar-code-limit))

(deftype ustring (&optional (size '*))
  `(simple-array uchar (,size)))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

;; FIXME: USELESS ?
(defun code-uchar (code)
  (check-type code (mod #.uchar-code-limit))
  code)

;; FIXME: USELESS ?
(defun uchar-code (uchar)
  (check-type uchar uchar)
  uchar)

(defun char-to-uchar (character)
  (char-code character))

(defun uchar-to-char (uchar)
  (code-char uchar))

(defun digit-uchar (digit &optional (radix 10))
  (check-type digit unsigned-byte)
  (check-type radix (integer 2 36))
  (if (< digit radix)
      (+ digit #x30)
      nil))

(defun uchar (thing)
  (etypecase thing
    (uchar thing)
    ((ustring 1) (aref thing 0))
    (character-designator
     (char-to-uchar (character thing)))))


;;;-------------------------------------------------------------------------
;;; Predicates
;;;-------------------------------------------------------------------------

(defun ucharp (uchar)
  (typep uchar 'uchar))

(defun unicode-uchar-p (uchar)
  (check-type uchar uchar)
  (or (< uchar #xD800)
      (> uchar #xDFFF)))

(defun uchar/= (uchar &rest more-uchars)
  (check-type uchar uchar)
  (assert (every #'ucharp more-uchars))
  (= (1+ (length more-uchars))
     (length (remove-duplicates (list* uchar more-uchars)
                                :test #'=))))

(defun uchar-not-equal (uchar &rest more-uchars)
  (check-type uchar uchar)
  (assert (every #'ucharp more-uchars))
  (= (1+ (length more-uchars))
     (length (remove-duplicates (list* uchar more-uchars)
                                :test #'= :key #'uchar-downcase))))

(macrolet
    ((define-uchar-comparison (name test &key (key 'identity))
       `(defun ,name (uchar &rest more-uchars)
          (check-type uchar uchar)
          (assert (every #'ucharp more-uchars))
          (do* ((r (,key uchar) (,key (car list)))
                (list more-uchars (cdr list)))
               ((null list) t)
            (unless (,test r (,key (car list)))
              (return nil))))))
  (define-uchar-comparison uchar=              =                   )
  (define-uchar-comparison uchar-equal         = :key uchar-downcase)
  (define-uchar-comparison uchar<              <                   )
  (define-uchar-comparison uchar-lessp         < :key uchar-downcase)
  (define-uchar-comparison uchar>              >                   )
  (define-uchar-comparison uchar-greaterp      > :key uchar-downcase)
  (define-uchar-comparison uchar<=            <=                   )
  (define-uchar-comparison uchar-not-greaterp <= :key uchar-downcase)
  (define-uchar-comparison uchar>=            >=                   )
  (define-uchar-comparison uchar-not-lessp    >= :key uchar-downcase))

(defun alpha-uchar-p (uchar)
  (and (unicode-uchar-p uchar)
       (alpha-char-p (uchar-to-char uchar))))

(defun alphanumeric-uchar-p (uchar)
  (and (unicode-uchar-p uchar)
       (alphanumericp (uchar-to-char uchar))))

(defun digit-uchar-p (uchar &optional (radix 10))
  (digit-char-p (uchar-to-char uchar) radix))

(defun graphic-uchar-p (uchar)
  (and (unicode-uchar-p uchar)
       (graphic-char-p (uchar-to-char uchar))))

(defun upper-case-uchar-p (uchar)
  (and (unicode-uchar-p uchar)
       (upper-case-p (uchar-to-char uchar))))

(defun lower-case-uchar-p (uchar)
  (and (unicode-uchar-p uchar)
       (lower-case-p (uchar-to-char uchar))))

(defun both-case-uchar-p (uchar)
  (and (unicode-uchar-p uchar)
       (both-case-p (uchar-to-char uchar))))


;;;-------------------------------------------------------------------------
;;; Operators
;;;-------------------------------------------------------------------------

(defun uchar-upcase (uchar)
  (if (unicode-uchar-p uchar)
      (char-to-uchar (char-upcase (uchar-to-char uchar)))
      uchar))

(defun uchar-downcase (uchar)
  (if (unicode-uchar-p uchar)
      (char-to-uchar (char-downcase (uchar-to-char uchar)))
      uchar))

