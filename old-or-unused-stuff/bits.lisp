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

(defun filter-adrinfo-according-to-hints (addrinfo hint-family
                                          hint-type hint-protocol)
  (labels ((addrinfo-good-p (addr)
             (and (or (zerop hint-family) (eql hint-family (slot addr 'et::family)))
                  (or (zerop hint-type) (eql hint-type (slot addr 'et::type)))
                  (eql hint-protocol (slot addr 'et::protocol))))

           (filter-addrinfo-recursive (addr list)
             (if (sb-alien::null-alien addr)
                 (nreverse list)
                 (filter-addrinfo-recursive
                  (slot addr 'et::next)
                  (if (addrinfo-good-p addr)
                      (cons addr list)
                      list)))))

    (filter-addrinfo-recursive addrinfo nil)))
