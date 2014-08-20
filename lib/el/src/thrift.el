;;; thrift.el ---Thrift implementation

;; Author: Sylvain Roy <sylvain.roy@m4x.org>

(defconst thrift-cst-message-type-call      1)
(defconst thrift-cst-message-type-reply     2)
(defconst thrift-cst-message-type-exception 3)
(defconst thrift-cst-message-type-oneway    4)

(defun thrift-constant-type-value (name)
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
