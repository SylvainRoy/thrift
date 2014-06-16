(require 'eieio)


(defclass thrift-base-protocol ()
  ()
  "A 'virtual' class for thrift protocols.")


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
