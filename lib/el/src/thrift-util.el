(defun thrift-pformat (in all)
  "Return a printable string with non alphanum (or all) char escaped."
  (let ((len (length in))
	(i 0)
	(out "")
	char)
    (while (< i len)
      (setq char (string-to-char (substring in i (+ i 1))))
      (if (or all
	      (not (or (and (< 47 char) (< char 58))
		       (and (< 64 char) (< char 91))
		       (and (< 96 char) (< char 123)))))
	  (setq out (concat out (format "\\x%x" char)))
	(setq out (concat out (char-to-string char))))
      (setq i (+ i 1)))
    out))

(provide 'thrift-util)
