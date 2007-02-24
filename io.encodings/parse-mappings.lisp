;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;   This code is free software; you can redistribute it and/or
;;   modify it under the terms of the version 2.1 of
;;   the GNU Lesser General Public License as published by
;;   the Free Software Foundation, as clarified by the
;;   preamble found here:
;;       http://opensource.franz.com/preamble.html
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU Lesser General
;;   Public License along with this library; if not, write to the
;;   Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;   Boston, MA 02110-1301, USA

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :split-sequence))

(defpackage :parse-encodings
  (:use :cl :split-sequence)
  (:export #:parse-unicode-mapping))

(in-package :parse-encodings)

(defvar +unicode-replacement-char-code+ #xFFFD)

(defun parse-unicode-mapping (file &optional (array-size 256))
  (flet ((parse-hex (str)
           (assert (string-equal "0x" (string-downcase (subseq str 0 2))))
           (parse-integer (subseq str 2) :radix 16))
         (sanitize-line (line)
           (when line
             (string-left-trim '(#\space)
                               (nsubstitute #\space #\tab line)))))
    (let ((arr (make-array array-size :element-type '(unsigned-byte 32)
                           :initial-element +unicode-replacement-char-code+)))
      (with-open-file (fin file)
        (loop :for line = (sanitize-line (read-line fin nil nil))
           :while line :do
           (when (and (plusp (length line)) (char= #\0 (char line 0)))
             (let* ((split (split-sequence #\space line :remove-empty-subseqs t))
                    (index (parse-hex (first split)))
                    (code (parse-hex (second split))))
               (setf (aref arr index) code)))))
      arr)))
