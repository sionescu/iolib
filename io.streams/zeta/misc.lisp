(in-package :io.zeta-streams)

(defun device-write-string (device string)
  (let ((octets (babel:string-to-octets string :encoding :utf-8)))
    (device-write device octets 0 (length octets))))

(defun device-terpri (device)
  (device-write device #.(coerce #(#xA) 'ub8-simple-vector) 0 1))

(defun device-write-line (device string)
  (device-write-string device string)
  (device-terpri device))
