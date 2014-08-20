;;; thrift.el ---Thrift implementation

;; Author: Sylvain Roy <sylvain.roy@m4x.org>

(defun thrift-constant-message-type (name)
  (cond   ((equal name 'call)      1)
	  ((equal name 'reply)     2)
	  ((equal name 'exception) 3)
	  ((equal name 'oneway)    4)))

(defun thrift-constant-type (name)
  (cond ((equal name 'stop)    0)
	((equal name 'void)    1)
	((equal name 'bool)    2)
	((equal name 'byte)    3)
	((equal name 'i08)     3)
	((equal name 'double)  4)
	((equal name 'i16)     6)
	((equal name 'i32)     8)
	((equal name 'i64)     10)
	((equal name 'string)  11)
	((equal name 'utf7)    11)
	((equal name 'struct)  12)
	((equal name 'map)     13)
	((equal name 'set)     14)
	((equal name 'list)    15)
	((equal name 'utf8)    16)
	((equal name 'utf16)   17)))

(provide 'thrift)
