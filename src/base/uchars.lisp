;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Surrogates of chars.
;;;

(in-package :iolib.base)

;;;-------------------------------------------------------------------------
;;; Constants
;;;-------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant uchar-code-limit (+ #x110000 #xFF)))


;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(deftype uchar ()
  `(mod ,uchar-code-limit))

(deftype simple-ustring (&optional (size '*))
  `(simple-array uchar (,size)))

(deftype complex-ustring (&optional (size '*))
  `(and (vector uchar ,size) (not simple-array)))

(deftype ustring (&optional (size '*))
  `(or (simple-ustring ,size) (complex-ustring ,size)))

(deftype uchar-designator ()
  '(or uchar (ustring 1) character-designator))

(deftype ustring-designator ()
  '(or ustring uchar string-designator))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

;; FIXME: USELESS ?
(defun code-uchar (code)
  (check-type code uchar)
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
  (if (typep uchar 'uchar)
      uchar
      nil))

(defun unicode-uchar-p (uchar)
  (check-type uchar uchar)
  (if (or (<= #xD800   uchar #xDFFF)
          (<= #x110000 uchar #x1100FF))
      nil
      uchar))

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
  (if (and (unicode-uchar-p uchar)
           (alpha-char-p (uchar-to-char uchar)))
      uchar
      nil))

(defun alphanumeric-uchar-p (uchar)
  (if (and (unicode-uchar-p uchar)
           (alphanumericp (uchar-to-char uchar)))
      uchar
      nil))

(defun digit-uchar-p (uchar &optional (radix 10))
  (digit-char-p (uchar-to-char uchar) radix))

(defun graphic-uchar-p (uchar)
  (if (and (unicode-uchar-p uchar)
           (graphic-char-p (uchar-to-char uchar)))
      uchar
      nil))

(defun upper-case-uchar-p (uchar)
  (if (and (unicode-uchar-p uchar)
           (upper-case-p (uchar-to-char uchar)))
      uchar
      nil))

(defun lower-case-uchar-p (uchar)
  (if (and (unicode-uchar-p uchar)
           (lower-case-p (uchar-to-char uchar)))
      uchar
      nil))

(defun both-case-uchar-p (uchar)
  (if (and (unicode-uchar-p uchar)
           (both-case-p (uchar-to-char uchar)))
      uchar
      nil))


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

