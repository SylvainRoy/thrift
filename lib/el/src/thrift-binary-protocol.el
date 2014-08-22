;;; TBinaryProtocol.el ---Encode/Decode data using Thrift BinaryProtocol.

;; Author: Sylvain Roy <sylvain.roy@m4x.org>

(require 'thrift)
(require 'thrift-base-protocol)
(require 'bindat)


(setq thrift-binary-protocol-version-1 #x8001)


(defclass thrift-binary-protocol (thrift-base-protocol)
  ((strictRead :initarg :strictRead
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
  (thrift-protocol-writeByte prot (thrift-constant-type 'stop)))

(defmethod thrift-protocol-writeMapBegin ((prot thrift-binary-protocol) ktype vtype size)
  (thrift-protocol-writeByte prot ktype)
  (thrift-protocol-writeByte prot vtype)
  (thrift-protocol-writeI32 prot size))

(defmethod thrift-protocol-writeMapEnd ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-writeListBegin ((prot thrift-binary-protocol) etype size)
  (thrift-protocol-writeByte prot etype)
  (thrift-protocol-writeI32 prot size))

(defmethod thrift-protocol-writeListEnd ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-writeSetBegin ((prot thrift-binary-protocol) etype size)
  (thrift-protocol-writeByte prot etype)
  (thrift-protocol-writeI32 prot size))

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
  ;; Emacs is limited to integer on 30 bits. So, this function only
  ;; consider the 24 first bits
  ;; todo: improve that to go to 30bits.
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
  (error "todo (see javascript implementation in nodejs/../../binary.js implem...)"))

(defmethod thrift-protocol-writeString ((prot thrift-binary-protocol) string)
  (thrift-transport-write (oref prot transport)
			  (bindat-pack '((len1     u8)
					 (len2     u24)
					 (data     str (len2)))
				       `((len1   . 0)
					 (len2   . ,(length string))
					 (data   . ,string)))))

(defmethod thrift-protocol-readMessageBegin ((prot thrift-binary-protocol))
  (setq version1 (thrift-protocol-readByte prot))
  (if (not (equal 0 (logand version1 #x80)))
      ;; 'strict write' applied
      (progn
	(setq version2 (thrift-protocol-readByte prot))
	(if (not (and (equal version1 #x80)
		      (equal version2 #x01)))
	    (error "Bad version in readMessageBegin"))
	(thrift-protocol-readByte prot)             ; useless byte
	(setq type (thrift-protocol-readByte prot))
	(setq name (thrift-protocol-readString prot))
	(setq seqid (thrift-protocol-readI32 prot)))
    ;; 'strict write' not applied
    (progn
      (if (oref prot strictRead)
	  (error "No protocol header found while 'strict read' asked"))
      ;; Decode length of name
      (setq data (thrift-transport-read (oref prot transport) 3))
      (setq decoded (bindat-unpack '((d u24)) data))
      (setq len (bindat-get-field decoded 'd))
      ;; Decode name
      (setq data (thrift-transport-read (oref prot transport) len))
      (setq decoded (bindat-unpack `((d str ,len)) data))
      (setq name (bindat-get-field decoded 'd))
      ;; Decode type & seqid
      (setq type (thrift-protocol-readByte prot))
      (setq seqid (thrift-protocol-readI32 prot))))
  (list name type seqid))

(defmethod thrift-protocol-readMessageEnd ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-readStructBegin ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-readStructEnd ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-readFieldBegin ((prot thrift-binary-protocol))
  (setq type (thrift-protocol-readByte prot))
  (if (equal type (thrift-constant-type 'stop))
      (list nil type 0)
    (progn
      (setq id (thrift-protocol-readI16  prot))
      (list nil type id))))

(defmethod thrift-protocol-readFieldEnd ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-readFieldStop ((prot thrift-binary-protocol))
  (thrift-protocol-writeByte (thrift-constant-type 'stop)))

(defmethod thrift-protocol-readMapBegin ((prot thrift-binary-protocol))
  (setq ktype (thrift-protocol-readByte prot))
  (setq vtype (thrift-protocol-readByte prot))
  (setq size (thrift-protocol-readI32 prot))
  (list ktype vtype size))

(defmethod thrift-protocol-readMapEnd ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-readListBegin ((prot thrift-binary-protocol))
  (setq etype (thrift-protocol-readByte prot))
  (setq size (thrift-protocol-readI32 prot))
  (list etype size))

(defmethod thrift-protocol-readListEnd ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-readSetBegin ((prot thrift-binary-protocol))
  (setq etype (thrift-protocol-readByte prot))
  (setq size (thrift-protocol-readI32 prot))
  (list etype size))

(defmethod thrift-protocol-readSetEnd ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-readBool ((prot thrift-binary-protocol))
  (not (equal 0 (thrift-protocol-readByte prot))))

(defmethod thrift-protocol-readByte ((prot thrift-binary-protocol))
  (setq data (thrift-transport-read (oref prot transport) 1))
  (setq decoded (bindat-unpack '((d u8)) data))
  (bindat-get-field decoded 'd))

(defmethod thrift-protocol-readI16 ((prot thrift-binary-protocol))
  (setq data (thrift-transport-read (oref prot transport) 2))
  (setq decoded (bindat-unpack '((d u16)) data))
  (bindat-get-field decoded 'd))

(defmethod thrift-protocol-readI32 ((prot thrift-binary-protocol))
  ;; Emacs is limited to integer on 30 bits. Same limitation applies here.
  (setq data (thrift-transport-read (oref prot transport) 4))
  (setq decoded (bindat-unpack '((d u32)) data))
  (bindat-get-field decoded 'd))

(defmethod thrift-protocol-readI64 ((prot thrift-binary-protocol))
  ;; Emacs is limited to integer on 30 bits. Same limitation applies here.
  (thrift-transport-read (oref prot transport) 4)
  (setq data (thrift-transport-read (oref prot transport) 4))
  (setq decoded (bindat-unpack '((d u32)) data))
  (bindat-get-field decoded 'd))

(defmethod thrift-protocol-readDouble ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readString ((prot thrift-binary-protocol))
  (setq len (thrift-protocol-readI32 prot))
  (setq data (thrift-transport-read (oref prot transport) len))
  (setq decoded (bindat-unpack `((d str ,len)) data))
  (bindat-get-field decoded 'd))

(provide 'thrift-binary-protocol)
