(require 'eieio)


(defclass thrift-base-protocol ()
  ((transport :initarg :transport
	      :document "Transport object to send/recv data."))
  "Base class of thrift protocols.")

(defmethod thrift-protocol-skip ((prot thrift-base-protocol) type)
  (cond ((equal type (thrift-constant-type 'stop)) nil)
	((equal type (thrift-constant-type 'bool)) (thrift-protocol-readBool))
	((equal type (thrift-constant-type 'byte)) (thrift-protocol-readByte))
	((equal type (thrift-constant-type 'i16)) (thrift-protocol-readI16))
	((equal type (thrift-constant-type 'i32)) (thrift-protocol-readI32))
	((equal type (thrift-constant-type 'i64)) (thrift-protocol-readI64))
	((equal type (thrift-constant-type 'double)) (thrift-protocol-readDouble))
	((equal type (thrift-constant-type 'string)) (thrift-protoco`l-readString))))

;; todo: this needs to be integrated in the 'skip' method above...
;;     elif ttype == TType.STRUCT:
;;       name = self.readStructBegin()
;;       while True:
;;         (name, ttype, id) = self.readFieldBegin()
;;         if ttype == TType.STOP:
;;           break
;;         self.skip(ttype)
;;         self.readFieldEnd()
;;       self.readStructEnd()
;;     elif ttype == TType.MAP:
;;       (ktype, vtype, size) = self.readMapBegin()
;;       for i in xrange(size):
;;         self.skip(ktype)
;;         self.skip(vtype)
;;       self.readMapEnd()
;;     elif ttype == TType.SET:
;;       (etype, size) = self.readSetBegin()
;;       for i in xrange(size):
;;         self.skip(etype)
;;       self.readSetEnd()
;;     elif ttype == TType.LIST:
;;       (etype, size) = self.readListBegin()
;;       for i in xrange(size):
;;         self.skip(etype)
;;       self.readListEnd()


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
