;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Parsing URIs.
;;;

(in-package :io.uri)

(defmacro register-bind ((&rest bindings) (regex target-string &key (start 0) end) &body body)
  (flet ((compute-bindings (fun)
           (loop :for index :from 0 :for b :in bindings
                 :if (string/= b "_") :collect `(,b (,fun ,index)))))
    (with-gensyms (string match-start match-end registers-start registers-end register-fun)
      (let ((bindings (compute-bindings register-fun)))
        `(let ((,string ,target-string))
           (multiple-value-bind (,match-start ,match-end ,registers-start ,registers-end)
               (scan ,regex ,string :start ,start :end (or ,end (length ,string)))
             (declare (ignore ,match-end))
             (flet ((,register-fun (index)
                      (when ,match-start
                        (let ((start (aref ,registers-start index))
                              (end (aref ,registers-end index)))
                          (when start (subseq ,string start end))))))
               (let ,bindings
                 (declare (ignorable ,@(mapcar #'car bindings)))
                 ,@body))))))))

(define-constant +uri-regexp+
    "^<?(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?>?$"
  :test #'string=)

(defun %parse-uri (uri)
  (multiple-value-bind (start end svec evec)
      (scan +uri-regexp+ uri)
    (declare (ignore end))
    (flet ((reg (index)
             (let ((s (aref svec index))
                   (e (aref evec index)))
               (when s (subseq uri s e)))))
      (when start
        (values (reg 1) (reg 3) (reg 4) (reg 6) (reg 8))))))

(defparameter +userinfo-regexp+ "(((%[0-9a-fA-F]{2}|[-$!&'()*+,;=:a-zA-Z0-9._~])*)@)?")
(defparameter +host-regexp+ "([-~a-zA-Z0-9]+|\\[.*:.*:.*\\])")
(defparameter +port-regexp+ "(:([0-9]*))?")
(defparameter +authority-regexp+
  (concatenate 'string +userinfo-regexp+ +host-regexp+ +port-regexp+))

(defun %parse-authority (authority)
  (multiple-value-bind (start end svec evec)
      (scan +authority-regexp+ authority)
    (declare (ignore end))
    (flet ((reg (index)
             (let ((s (aref svec index))
                   (e (aref evec index)))
               (when s (subseq authority s e)))))
      (when start
        (values (reg 1) (reg 3) (reg 5))))))

(define-abnf-syntax :uri
  (uri . (scheme ":" hier-part (:opt ("?" query)) (:opt ("#" fragment))))
  (hier-part . (:or ("//" authority path-abempty) path-absolute path-rootless path-empty))
  (uri-reference . (:or uri relative-ref))
  (absolute-uri . (scheme ":" hier-part (:opt ("?" query))))
  (relative-ref . (relative-part (:opt ("?" query)) (:opt ("#" fragment))))
  (relative-part . (:or ("//" authority path-abempty) path-absolute path-noscheme path-empty))
  (scheme . (:alpha (:any (:or :alpha :digit "+" "-" "."))))
  (authority . ((:opt (userinfo "@")) host (:opt (":" port))))
  (userinfo . (:any (:or unreserved pct-encoded sub-delims ":")))
  (host . (:or ip-literal ipv4-address reg-name))
  (port . (:any :digit))
  (ip-literal . ("[" (:or ipv6address ipvfuture) "]"))
  (ipvfuture . ("v" (:req :hexdig) "." (:req (:or unreserved sub-delims ":"))))
  (ipv6address . (:or (                                               (:repeat (:= 6) (h16 ":")) ls32)
                      (                                         "::"  (:repeat (:= 5) (h16 ":")) ls32)
                      ((:opt                               h16) "::"  (:repeat (:= 4) (h16 ":")) ls32)
                      ((:opt ((:repeat (:to 1) (h16 ":")) h16)) "::"  (:repeat (:= 3) (h16 ":")) ls32)
                      ((:opt ((:repeat (:to 2) (h16 ":")) h16)) "::"  (:repeat (:= 2) (h16 ":")) ls32)
                      ((:opt ((:repeat (:to 3) (h16 ":")) h16)) "::"                  (h16 ":")  ls32)
                      ((:opt ((:repeat (:to 4) (h16 ":")) h16)) "::"                             ls32)
                      ((:opt ((:repeat (:to 5) (h16 ":")) h16)) "::"                             h16)
                      ((:opt ((:repeat (:to 6) (h16 ":")) h16)) "::")))
  (h16 . (:repeat (:from 1 4) :hexdig))
  (ls32 . (:or (h16 ":" h16) ipv4address))
  (ipv4address . (dec-octet "." dec-octet "." dec-octet "." dec-octet))
  (dec-octet . (:or :digit
                    ((:range #\1 #\9) :digit)
                    ("1" (:repeat (:= 2) :digit))
                    ("2" (:range #\0 #\4) :digit)
                    ("25" (:range #\0 #\5))))
  (reg-name . (:any (:or unreserved pct-encoded sub-delims)))
  (path . (:or path-abempty path-absolute path-noscheme path-rootless path-empty))
  (path-abempty . (:any ("/" segment)))
  (path-absolute . ("/" (:opt (segment-nz (:any ("/" segment))))))
  (path-noscheme . (segment-nz-nc (:any ("/" segment))))
  (path-rootless . (segment-nz (:any ("/" segment))))
  (path-empty . (:not pchar))
  (segment . (:any pchar))
  (segment-nz . (:req pchar))
  (segment-nz-nc . (:req (:or unreserved pct-encoded sub-delims "@")))
  (pchar . (:or unreserved pct-encoded sub-delims ":" "@"))
  (query . (:any (:or pchar "/" "?")))
  (fragment . (:any (:or pchar "/" "?")))
  (pct-encoded . ("%" :hexdig :hexdig))
  (unreserved . (:or :alpha :digit "-" "." "_" "~"))
  (reserved . (:or gen-delims sub-delims))
  (gen-delims . (:or ":" "/" "?" "#" "[" "]" "@"))
  (sub-delims . (:or "!" "$" "&" "'" "(" ")" "*" "+" "," ";" "=")))

(define-condition uri-parse-error (parse-error) ())

(defun uri-parse-error (message &rest args)
  (error 'uri-parse-error
         :format-control message
         :format-arguments args))
