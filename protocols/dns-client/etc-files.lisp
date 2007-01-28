;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Copyright (C) 2006 by Stelian Ionescu                                 ;
;                                                                         ;
;   This program is free software; you can redistribute it and/or modify  ;
;   it under the terms of the GNU General Public License as published by  ;
;   the Free Software Foundation; either version 2 of the License, or     ;
;   (at your option) any later version.                                   ;
;                                                                         ;
;   This program is distributed in the hope that it will be useful,       ;
;   but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;   GNU General Public License for more details.                          ;
;                                                                         ;
;   You should have received a copy of the GNU General Public License     ;
;   along with this program; if not, write to the                         ;
;   Free Software Foundation, Inc.,                                       ;
;   51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :net.sockets)

(defun load-file (path)
  (with-open-file (fin path)
    (let ((big-string (make-string (file-length fin))))
      (read-sequence big-string fin)
      big-string)))

(defun space-char-p (char)
  (declare (type character char))
  (or (char-equal char #\Space)
      (char-equal char #\Tab)))

(defun split-string-by-spaces (string &key (start 0) end empty-seqs)
  (declare (type string string)
           (type unsigned-byte start)
           (type (or unsigned-byte null) end))
  (let ((substring-length (or end (length string))))
    (assert (>= substring-length start))
    (loop
       :with substr-start := (1- start) :and substr-end := (1- start)
       :with dummy-char := #\Space
       :for index :upto substring-length
       :for char := (if (eql index substring-length) dummy-char (char string index))
       :when (and (space-char-p char)
                  (setf substr-start (1+ substr-end)
                        substr-end   index)
                  (or (> substr-end substr-start) empty-seqs))
       :collect (subseq string substr-start substr-end))))

(defun search-in-etc-file (path predicate &optional (match-all t))
  (let ((file (load-file path))
        results)
    (with-input-from-string (string-stream file)
      (loop
         :for line := (read-line string-stream nil nil)
         :for comment-start := (or (position #\# line)
                                   (length line))
         :while line :do
         (destructuring-bind (&optional col1 col2 &rest other-cols)
             (split-string-by-spaces line :empty-seqs nil :end comment-start)
           (when col2 ; skip invalid lines
             (let ((result (funcall predicate col1 col2 other-cols)))
               (when result
                 (push result results)
                 (unless match-all
                   (loop-finish))))))
         :finally (setf results (nreverse results))))
    results))

(defun vector-ipv6-good-p (vector ipv6)
  (when vector
    (let ((len (length vector)))
      (case ipv6
        (:ipv6 (eql len 8))
        ((nil) (eql len 4))
        (otherwise t)))))

(defun search-etc-hosts-ip (file ip ipv6)
  (let ((line (search-in-etc-file file
                                  #'(lambda (col1 col2 other-cols)
                                      (let ((vector (string-address->vector col1)))
                                        (when (and (vector-ipv6-good-p vector ipv6)
                                                   (vector-equal vector ip))
                                          (let ((host
                                                 (make-host col2 (make-address vector) other-cols)))
                                            (if (eql ipv6 t)
                                                (map-host-ipv4-addresses-to-ipv6 host)
                                                host)))))
                                  nil)))
    (car line)))

(defun merge-lines-into-one-host (lines ipv6)
  (flet ((pushnew-alias (alias place cname)
           (when (string-not-equal alias cname)
             (pushnew alias place :test #'string-equal)
             place)))

    (let (ips aliases host)
      (destructuring-bind (first-ip cname first-aliases) (car lines)
        (setf ips (list first-ip))
        (mapc #'(lambda (alias) (setf aliases (pushnew-alias alias aliases cname)))
              first-aliases)
        (mapc #'(lambda (line)
                  (destructuring-bind (ip alias more-aliases) line
                    (pushnew ip ips)
                    (mapc #'(lambda (alias) (setf aliases (pushnew-alias alias aliases cname)))
                          (cons alias more-aliases))))
              (cdr lines))
        (setf host (make-host cname
                              (mapcar #'make-address (nreverse ips))
                              (nreverse aliases)))
        (if (eql ipv6 t)
            (map-host-ipv4-addresses-to-ipv6 host)
            host)))))

(defun search-etc-hosts-name (file name ipv6)
  (let ((lines (search-in-etc-file file
                                   #'(lambda (col1 col2 other-cols)
                                       (let ((vector (string-address->vector col1)))
                                         (when (and (vector-ipv6-good-p vector ipv6)
                                                    (or (string-equal name col2)
                                                        (member name other-cols
                                                                :test #'string-equal)))
                                           (list vector col2 other-cols))))
                                   t)))
    (when lines
      (merge-lines-into-one-host lines ipv6))))
