;;; TBinaryProtocol.el ---Very basic Thrift transport layer based on string.

;; Author: Sylvain Roy <sylvain.roy@m4x.org>

(defun create-string-transport (in)
  (list in ""))

(defun open-string-transport (io)
  io)

(defun close-string-transport (io)
  io)

(defun read-string-transport (io)
  (car io))

(defun write-string-transport (io out)
  (list (car io) (concat (car (cdr io)) out)))

(defun flush-string-transport (io)
  io)

(provide 'thrift-string-transport)
