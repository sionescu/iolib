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

(in-package :io.encodings)

;; taken from kmrcl
(defmacro shrink-vector (str size)
  #+allegro `(excl::.primcall 'sys::shrink-svector ,str ,size)
  #+cmu `(lisp::shrink-vector ,str ,size)
  #+lispworks `(system::shrink-vector$vector ,str ,size)
  #+sbcl `(sb-kernel:shrink-vector ,str ,size)
  #+scl `(common-lisp::shrink-vector ,str ,size)
  #-(or allegro cmu lispworks sbcl scl) `(subseq ,str 0 ,size))

(defun missing-arg ()
  (error "A required &KEY or &OPTIONAL argument was not supplied."))

