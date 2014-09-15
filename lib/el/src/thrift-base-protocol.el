(require 'eieio)


(defclass thrift-base-protocol ()
  ((transport :initarg :transport
	      :document "Transport object to send/recv data."))
  "Base class of thrift protocols.")

(defmethod thrift-protocol-skip ((prot thrift-base-protocol) type)
  (cond ((equal type (thrift-constant-type 'stop)) nil)
	((equal type (thrift-constant-type 'bool)) (thrift-protocol-read-bool prot))
	((equal type (thrift-constant-type 'byte)) (thrift-protocol-read-byte prot))
	((equal type (thrift-constant-type 'i16)) (thrift-protocol-read-i16 prot))
	((equal type (thrift-constant-type 'i32)) (thrift-protocol-read-i32 prot))
	((equal type (thrift-constant-type 'i64)) (thrift-protocol-read-i64 prot))
	((equal type (thrift-constant-type 'double)) (thrift-protocol-read-double prot))
	((equal type (thrift-constant-type 'string)) (thrift-protocol-read-string prot))
	((equal type (thrift-constant-type 'struct))
	 (progn
	   (thrift-protocol-read-struct-begin prot)
	   (while (not (equal (setq ttype (car (cdr (thrift-protocol-read-field-begin prot))))
			      (thrift-constant-type 'stop)))
	     (thrift-protocol-skip prot ttype)
	     (thrift-protocol-read-field-end prot))
	   (thrift-protocol-read-struct-end prot)))
	((equal type (thrift-constant-type 'map))
	 (progn
	   (setq r (thrift-protocol-read-map-begin prot))
	   (setq ktype (pop r))
	   (setq vtype (pop r))
	   (setq size (pop r))
	   (dotimes (index size nil)
	     (thrift-protocol-skip prot ktype)
	     (thrift-protocol-skip prot vtype))
	   (thrift-protocol-read-map-end prot)))
	((equal type (thrift-constant-type 'set))
	 (progn
	   (setq r (thrift-protocol-read-set-begin prot))
	   (setq etype (pop r))
	   (setq size (pop r))
	   (dotimes (index size nil)
	     (thrift-protocol-skip prot etype))
	   (thrift-protocol-read-set-end prot)))
	((equal type (thrift-constant-type 'list))
	 (progn
	   (setq r (thrift-protocol-read-list-begin prot))
	   (setq etype (pop r))
	   (setq size (pop r))
	   (dotimes (index size nil)
	     (thrift-protocol-skip prot etype))
	   (thrift-protocol-read-list-end prot)))))

(defmethod thrift-protocol-write-message-begin ((prot thrift-base-protocol) name type seq)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-message-end ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-struct-begin ((prot thrift-base-protocol) name)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-struct-end ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-field-begin ((prot thrift-base-protocol) name type id)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-field-end ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-field-stop ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-map-begin ((prot thrift-base-protocol) ktype vtype size)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-map-end ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-list-begin ((prot thrift-base-protocol) etype size)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-list-end ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-set-begin ((prot thrift-base-protocol) etype size)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-set-end ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-bool ((prot thrift-base-protocol) bool)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-byte ((prot thrift-base-protocol) byte)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-i16 ((prot thrift-base-protocol) i16)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-i32 ((prot thrift-base-protocol) i32)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-i64 ((prot thrift-base-protocol) i64)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-double ((prot thrift-base-protocol) double)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-write-string ((prot thrift-base-protocol) string)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-message-begin ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-message-end ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-struct-begin ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-struct-end ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-field-begin ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-field-end ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-map-begin ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-map-end ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-list-begin ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-list-end ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-set-begin ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-set-end ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-bool ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-byte ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-i16 ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-i32 ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-i64 ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-double ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-read-string ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(provide 'thrift-base-protocol)
