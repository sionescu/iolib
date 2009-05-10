;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Surrogates of chars.
;;;

(in-package :iolib.base)

;;;-------------------------------------------------------------------------
;;; Constants
;;;-------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant rune-code-limit #x110000))


;;;-------------------------------------------------------------------------
;;; Classes and Types
;;;-------------------------------------------------------------------------

(deftype rune ()
  '(mod #.rune-code-limit))

(deftype rod (&optional (size '*))
  `(simple-array rune (,size)))


;;;-------------------------------------------------------------------------
;;; Constructors
;;;-------------------------------------------------------------------------

;; FIXME: USELESS ?
(defun code-rune (code)
  (check-type code (mod #.rune-code-limit))
  code)

;; FIXME: USELESS ?
(defun rune-code (rune)
  (check-type rune rune)
  rune)

(defun char-rune (character)
  (char-code character))

(defun rune-char (rune)
  (code-char rune))

(defun digit-rune (digit &optional (radix 10))
  (if (< -1 digit radix)
      digit
      nil))

(defun rune (thing)
  (etypecase thing
    (rune    thing)
    ((rod 1) (aref thing 0))
    ((or character
         string
         symbol)
     (char-rune (character thing)))))


;;;-------------------------------------------------------------------------
;;; Predicates
;;;-------------------------------------------------------------------------

(defun runep (rune)
  (typep rune 'rune))

(defun unicode-rune-p (rune)
  (check-type rune rune)
  (or (< rune #xD800)
      (> rune #xDFFF)))

(defun rune= (rune &rest more-runes)
  (check-type rune rune)
  (assert (every #'runep more-runes))
  (dolist (r more-runes t)
    (unless (= r rune) (return nil))))

(defun rune/= (rune &rest more-runes)
  (check-type rune rune)
  (assert (every #'runep more-runes))
  (= (1+ (length more-runes))
     (length (remove-duplicates (list* rune more-runes)
                                :test #'=))))

(defun rune< (rune &rest more-runes)
  (check-type rune rune)
  (assert (every #'runep more-runes))
  (do* ((r rune (car list))
        (list more-runes (cdr list)))
       ((null list) t)
    (unless (< r (car list))
      (return nil))))

(defun rune> (rune &rest more-runes)
  (check-type rune rune)
  (assert (every #'runep more-runes))
  (do* ((r rune (car list))
        (list more-runes (cdr list)))
       ((null list) t)
    (unless (> r (car list))
      (return nil))))

(defun rune<= (rune &rest more-runes)
  (check-type rune rune)
  (assert (every #'runep more-runes))
  (do* ((r rune (car list))
        (list more-runes (cdr list)))
       ((null list) t)
    (unless (<= r (car list))
      (return nil))))

(defun rune>= (rune &rest more-runes)
  (check-type rune rune)
  (assert (every #'runep more-runes))
  (do* ((r rune (car list))
        (list more-runes (cdr list)))
       ((null list) t)
    (unless (>= r (car list))
      (return nil))))

(defun rune-equal (rune &rest more-runes)
  (check-type rune rune)
  (assert (every #'runep more-runes))
  (let ((rune (rune-downcase rune)))
    (dolist (r more-runes t)
      (unless (= (rune-downcase r) rune) (return nil)))))

(defun rune-not-equal (rune &rest more-runes)
  (check-type rune rune)
  (assert (every #'runep more-runes))
  (= (1+ (length more-runes))
     (length (remove-duplicates (list* rune more-runes)
                                :test #'= :key #'rune-downcase))))

(defun rune-lessp (rune &rest more-runes)
  (check-type rune rune)
  (assert (every #'runep more-runes))
  (reduce #'< more-runes :initial-value rune :key #'rune-downcase))

(defun rune-greaterp (rune &rest more-runes)
  (check-type rune rune)
  (assert (every #'runep more-runes))
  (reduce #'> more-runes :initial-value rune :key #'rune-downcase))

(defun rune-not-greaterp (rune &rest more-runes)
  (check-type rune rune)
  (assert (every #'runep more-runes))
  (reduce #'<= more-runes :initial-value rune :key #'rune-downcase))

(defun rune-not-lessp (rune &rest more-runes)
  (check-type rune rune)
  (assert (every #'runep more-runes))
  (reduce #'>= more-runes :initial-value rune :key #'rune-downcase))

(defun alpha-rune-p (rune)
  (and (unicode-rune-p rune)
       (alpha-char-p (rune-char rune))))

(defun alphanumeric-rune-p (rune)
  (and (unicode-rune-p rune)
       (alphanumericp (rune-char rune))))

(defun digit-rune-p (rune)
  (and (unicode-rune-p rune)
       (digit-char-p (rune-char rune))))

(defun graphic-rune-p (rune)
  (and (unicode-rune-p rune)
       (graphic-char-p (rune-char rune))))

(defun upper-case-rune-p (rune)
  (and (unicode-rune-p rune)
       (upper-case-p (rune-char rune))))

(defun lower-case-rune-p (rune)
  (and (unicode-rune-p rune)
       (lower-case-p (rune-char rune))))

(defun both-case-rune-p (rune)
  (and (unicode-rune-p rune)
       (both-case-p (rune-char rune))))


;;;-------------------------------------------------------------------------
;;; Operators
;;;-------------------------------------------------------------------------

(defun rune-upcase (rune)
  (if (unicode-rune-p rune)
      (char-rune (char-upcase (rune-char rune)))
      rune))

(defun rune-downcase (rune)
  (if (unicode-rune-p rune)
      (char-rune (char-downcase (rune-char rune)))
      rune))

