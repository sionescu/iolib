;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Strings used for decoding Unix pathnames: invalid UTF8 octets
;;;     are encoded in the invalid range #x110000 - #x1100FF.
;;;

(in-package :iolib.syscalls)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +cstring-path-max+ 65535))

(defun ustring-to-cstring (ustring c-ptr c-len)
  (let ((index 0))
    (labels
        ((output-octet (octet)
           (setf (cffi:mem-aref c-ptr :unsigned-char index) octet))
         (check-space (chars-required)
           (unless (<= (+ index chars-required) c-len)
             (return* (values c-ptr index))))
         (put-one-uchar (code)
           (cond
             ((< code #x80)
              (check-space 1)
              (output-octet code)
              (incf index 1))
             ((< code #x800)
              (check-space 2)
              (output-octet (logior #xC0 (ldb (byte 5 6) code)))
              (output-octet (logior #x80 (ldb (byte 6 0) code)))
              (incf index 2))
             ((< code #x10000)
              (check-space 3)
              (output-octet (logior #xE0 (ldb (byte 4 12) code)))
              (output-octet (logior #x80 (ldb (byte 6 6) code)))
              (output-octet (logior #x80 (ldb (byte 6 0) code)))
              (incf index 3))
             ((< code #x110000)
              (check-space 4)
              (output-octet (logior #xF0 (ldb (byte 3 18) code)))
              (output-octet (logior #x80 (ldb (byte 6 12) code)))
              (output-octet (logior #x80 (ldb (byte 6 6) code)))
              (output-octet (logior #x80 (ldb (byte 6 0) code)))
              (incf index 4))
             ((<= code uchar-code-limit)
              (check-space 1)
              (output-octet (logand code #xFF))
              (incf index 1))
             (t (error "BUG! Uchars should be smaller than ~S" uchar-code-limit)))))
      (map 'nil #'put-one-uchar ustring)
      (values c-ptr nil))))

(deftype cstr-offset ()
  `(integer 0 ,(1+ path-max)))

(defun utf8-extra-bytes (code)
  (declare (type (unsigned-byte 8) code)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((vec (load-time-value
              (coerce #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                        0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
                        2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2  3 3 3 3 3 0 0 0 0 0 0 0 0 0 0 0)
                      '(simple-array (unsigned-byte 8) (256))))))
    (aref (the (simple-array (unsigned-byte 8) (256)) vec) code)))

(defun offsets-from-utf8 (extra-bytes)
  (declare (type (mod 4) extra-bytes)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((vec (load-time-value
              (coerce #(#x00000000 #x00003080 #x000E2080 #x03C82080)
                      '(simple-array (unsigned-byte 26) (4))))))
    (aref (the (simple-array (unsigned-byte 26) (4)) vec) extra-bytes)))

(defun legal-utf8-cstring (ptr start len)
  (declare (type cstr-offset start len)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((end (+ start len))
        (srchr (mem-aref ptr :unsigned-char start))
        c)
    (flet ((getch ()
             (mem-aref ptr :unsigned-char (decf (the (unsigned-byte 17) end)))))
      (declare (inline getch))
      (when (=  len 4) (setf c (getch)) (unless (<= #x80 c #xBF) (return* nil)))
      (when (>= len 3) (setf c (getch)) (unless (<= #x80 c #xBF) (return* nil)))
      (when (>= len 2) (setf c (getch)) (unless (<= #x00 c #xBF) (return* nil))
        (case srchr
          (#xE0 (when (< c #xA0) (return* nil)))
          (#xED (when (> c #x9F) (return* nil)))
          (#xF0 (when (< c #x90) (return* nil)))
          (#xF4 (when (> c #x8F) (return* nil)))
          (t    (when (< c #x80) (return* nil)))))
      (when (>= len 1) (when (<= #x80 srchr #xC1) (return* nil)))
      (when (> srchr #xF4) (return* nil))
      t)))

(defun cstring-to-ustring (c-ptr c-len)
  (declare (type cstr-offset c-len)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((index 0) (uindex 0)
        (ustring (make-array c-len :element-type 'uchar)))
    (declare (type cstr-offset index uindex)
             (type ustring ustring))
    (flet ((input-char ()
             (prog1 (mem-aref c-ptr :unsigned-char index)
               (incf index)))
           (output-uchar (code)
             (setf (aref ustring uindex) code) (incf uindex)))
      (declare (inline input-char output-uchar))
      (loop :for byte0 := (mem-aref c-ptr :unsigned-char index)
            :until (or (>= index c-len) (zerop byte0)) :do
            (block decode-one-char
              (let* ((code 0)
                     (extra-bytes (min (utf8-extra-bytes byte0)))
                     (legalp (and (legal-utf8-cstring c-ptr index (1+ extra-bytes))
                                  (< extra-bytes (- c-len index)))))
                (declare (type (mod 4) extra-bytes)
                         (type (integer 0 #.uchar-code-limit) code))
                (labels ((finish-seq (extra-bytes offset)
                           (decf code (the (integer 0 #.uchar-code-limit) (offsets-from-utf8 extra-bytes)))
                           (output-uchar (+ code offset)))
                         (legalchk ()
                           (unless legalp (finish-seq 0 #x110000) (return-from decode-one-char))))
                  (when (>= extra-bytes 3) (setf code (ash (+ code (input-char)) 6)) (legalchk))
                  (when (>= extra-bytes 2) (setf code (ash (+ code (input-char)) 6)) (legalchk))
                  (when (>= extra-bytes 1) (setf code (ash (+ code (input-char)) 6)) (legalchk))
                  (when (>= extra-bytes 0) (setf code (ash (+ code (input-char)) 0)) (legalchk))
                  (finish-seq extra-bytes 0))))))
    (shrink-vector ustring uindex)))
