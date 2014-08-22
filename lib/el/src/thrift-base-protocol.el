(require 'eieio)


(defclass thrift-base-protocol ()
  ((transport :initarg :transport
	      :document "Transport object to send/recv data."))
  "Base class of thrift protocols.")

(defmethod thrift-protocol-skip ((prot thrift-base-protocol) type)
  (cond ((equal type (thrift-constant-type 'stop)) nil)
	((equal type (thrift-constant-type 'bool)) (thrift-protocol-readBool prot))
	((equal type (thrift-constant-type 'byte)) (thrift-protocol-readByte prot))
	((equal type (thrift-constant-type 'i16)) (thrift-protocol-readI16 prot))
	((equal type (thrift-constant-type 'i32)) (thrift-protocol-readI32 prot))
	((equal type (thrift-constant-type 'i64)) (thrift-protocol-readI64 prot))
	((equal type (thrift-constant-type 'double)) (thrift-protocol-readDouble prot))
	((equal type (thrift-constant-type 'string)) (thrift-protocol-readString prot))
	((equal type (thrift-constant-type 'struct))
	 (progn
	   (thrift-protocol-readStructBegin prot)
	   (while (not (equal (setq ttype (car (cdr (thrift-protocol-readFieldBegin prot))))
			      (thrift-constant-type 'stop)))
	     (thrift-protocol-skip prot ttype)
	     (thrift-protocol-readFieldEnd prot))
	   (thrift-protocol-readStructEnd prot)))
	((equal type (thrift-constant-type 'map))
	 (progn
	   (setq r (thrift-protocol-readMapBegin prot))
	   (setq ktype (pop r))
	   (setq vtype (pop r))
	   (setq size (pop r))
	   (dotimes (index size nil)
	     (thrift-protocol-skip prot ktype)
	     (thrift-protocol-skip prot vtype))
	   (thrift-protocol-readMapEnd prot)))
	((equal type (thrift-constant-type 'set))
	 (progn
	   (setq r (thrift-protocol-readSetBegin prot))
	   (setq etype (pop r))
	   (setq size (pop r))
	   (dotimes (index size nil)
	     (thrift-protocol-skip prot etype))
	   (thrift-protocol-readSetEnd prot)))
	((equal type (thrift-constant-type 'list))
	 (progn
	   (setq r (thrift-protocol-readListBegin prot))
	   (setq etype (pop r))
	   (setq size (pop r))
	   (dotimes (index size nil)
	     (thrift-protocol-skip prot etype))
	   (thrift-protocol-readListEnd prot)))))

(defmethod thrift-protocol-writeMessageBegin ((prot thrift-base-protocol) name type seq)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeMessageEnd ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeStructBegin ((prot thrift-base-protocol) name)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeStructEnd ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeFieldBegin ((prot thrift-base-protocol) name type id)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeFieldEnd ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeFieldStop ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeMapBegin ((prot thrift-base-protocol) ktype vtype size)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeMapEnd ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeListBegin ((prot thrift-base-protocol) etype size)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeListEnd ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeSetBegin ((prot thrift-base-protocol) etype size)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeSetEnd ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeBool ((prot thrift-base-protocol) bool)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeByte ((prot thrift-base-protocol) byte)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeI16 ((prot thrift-base-protocol) i16)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeI32 ((prot thrift-base-protocol) i32)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeI64 ((prot thrift-base-protocol) i64)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeDouble ((prot thrift-base-protocol) double)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-writeString ((prot thrift-base-protocol) string)
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readMessageBegin ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readMessageEnd ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readStructBegin ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readStructEnd ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readFieldBegin ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readFieldEnd ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readMapBegin ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readMapEnd ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readListBegin ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readListEnd ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readSetBegin ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readSetEnd ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readBool ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readByte ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readI16 ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readI32 ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readI64 ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readDouble ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(defmethod thrift-protocol-readString ((prot thrift-base-protocol))
  (error "This method should be defined in subclasses."))

(provide 'thrift-base-protocol)
