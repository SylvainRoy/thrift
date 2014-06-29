;;; TBinaryProtocol.el ---Encode/Decode data using Thrift BinaryProtocol.

;; Author: Sylvain Roy <sylvain.roy@m4x.org>

(require 'thrift-base-protocol)
(require 'bindat)


(setq thrift-binary-protocol-version-1 #x8001)


(defclass thrift-binary-protocol (thrift-base-protocol)
  ((transport :initarg :transport
	      :document "Transport object to send/recv data.")
   (strictRead :initarg :strictRead
	       :initform nil)
   (strictWrite :initarg :strictWrite
		:initform t))
  "Implementation of the Thrift binary protocol.")


(defmethod thrift-protocol-writeMessageBegin ((prot thrift-binary-protocol) name type seq)
  (if (oref prot strictWrite)
      (progn 
	(thrift-protocol-writeI16 prot thrift-binary-protocol-version-1)
	(thrift-protocol-writeByte prot 0)
	(thrift-protocol-writeByte prot type)
	(thrift-protocol-writeString prot name)
	(thrift-protocol-writeI32 prot seq))
    (progn
      (thrift-protocol-writeString prot name)
      (thrift-protocol-writeByte prot type)
	(thrift-protocol-writeI32 prot seq))))

(defmethod thrift-protocol-writeMessageEnd ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-writeStructBegin ((prot thrift-binary-protocol) name)
  nil)

(defmethod thrift-protocol-writeStructEnd ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-writeFieldBegin ((prot thrift-binary-protocol) name type id)
  (thrift-protocol-writeByte prot type)
  (thrift-protocol-writeI16  prot id))

(defmethod thrift-protocol-writeFieldEnd ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-writeFieldStop ((prot thrift-binary-protocol))
  (error "todo"))

(defmethod thrift-protocol-writeMapBegin ((prot thrift-binary-protocol) ktype vtype size)
  (error "todo"))

(defmethod thrift-protocol-writeMapEnd ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-writeListBegin ((prot thrift-binary-protocol) etype size)
  (error "todo"))

(defmethod thrift-protocol-writeListEnd ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-writeSetBegin ((prot thrift-binary-protocol) etype size)
  (error "todo"))

(defmethod thrift-protocol-writeSetEnd ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-writeBool ((prot thrift-binary-protocol) bool)
  (thrift-protocol-writeByte prot (if bool 1 0)))

(defmethod thrift-protocol-writeByte ((prot thrift-binary-protocol) byte)
  (thrift-transport-write (oref prot transport)
			  (bindat-pack '((data u8))
				       `((data . ,byte)))))

(defmethod thrift-protocol-writeI16 ((prot thrift-binary-protocol) i16)
  (thrift-transport-write (oref prot transport)
			  (bindat-pack '((data u16))
				       `((data . ,i16)))))

(defmethod thrift-protocol-writeI32 ((prot thrift-binary-protocol) i32)
  ;; Emacs does not cope with integer on 32bits.
  ;; This function only works for signed number that can be encoded on 24bits.
  (let ((of (if (>= i32 0) 0 #xff)))
    (thrift-transport-write (oref prot transport)
			    (bindat-pack '((overflow u8)
					   (data     u24))
					 `((overflow . ,of)
					   (data     . ,i32))))))

(defmethod thrift-protocol-writeI64 ((prot thrift-binary-protocol) i64)
  ;; Emacs does not cope with integer on 64bits.
  ;; This function only works for signed number that can be encoded on 24bits.
  (let ((of1 (if (>= i64 0) 0 #xffffff))
	(of2 (if (>= i64 0) 0 #xffff)))
    (thrift-transport-write (oref prot transport)
			    (bindat-pack '((overflow1   u24)
					   (overflow2   u16)
					   (data        u24))
					 `((overflow1 . ,of1)
					   (overflow2 . ,of2)
					   (data      . ,i64))))))

(defmethod thrift-protocol-writeDouble ((prot thrift-binary-protocol) double)
  (error "todo"))

(defmethod thrift-protocol-writeString ((prot thrift-binary-protocol) string)
  (thrift-transport-write (oref prot transport)
			  (bindat-pack '((len1	   u8)
					 (len2	   u24)
					 (data     str (len2)))
				       `((len1   . 0)
					 (len2   . ,(length string))
					 (data   . ,string)))))


(defmethod thrift-protocol-readMessageBegin ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readMessageEnd ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readStructBegin ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readStructEnd ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readFieldBegin ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readFieldEnd ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readMapBegin ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readMapEnd ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readListBegin ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readListEnd ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readSetBegin ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readSetEnd ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readBool ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readByte ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readI16 ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readI32 ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readI64 ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readDouble ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readString ((prot thrift-binary-protocol))
  (error "todo."))


(provide 'thrift-binary-protocol)
