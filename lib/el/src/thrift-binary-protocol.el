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
  (thrift-protocol-writeByte prot 0)) ; todo: 0 should be replaced by something like TType.STOP

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
  (error "todo (see javascript implementation in nodejs/../../binary.js implem...)"))

(defmethod thrift-protocol-writeString ((prot thrift-binary-protocol) string)
  (thrift-transport-write (oref prot transport)
			  (bindat-pack '((len1     u8)
					 (len2     u24)
					 (data     str (len2)))
				       `((len1   . 0)
					 (len2   . ,(length string))
					 (data   . ,string)))))

;;;;; todo: clean the mess below... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Experiment on elisp internal representation of numbers ;;;;;;;;;;;;;;;;;
(defun repr (x s)
  (if (< (length s) 32)
      (repr (lsh x -1) (concat (number-to-string (logand x 1)) s))
    s))

; Bigger int:
(repr #x1fffffff "")
"00011111111111111111111111111111"
;10987654321098765432109876543210

(repr -1 "")
"00111111111111111111111111111111"

;; a negative i32 should start by a 1.
;; Not the case in elisp cause it's 30 bits.


;;;; python implementation of readMessageBegin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; sz = self.readI32()
    ;; if sz < 0:
    ;;   version = sz & TBinaryProtocol.VERSION_MASK
    ;;   if version != TBinaryProtocol.VERSION_1:
    ;;     raise TProtocolException(
    ;;       type=TProtocolException.BAD_VERSION,
    ;;       message='Bad version in readMessageBegin: %d' % (sz))
    ;;   type = sz & TBinaryProtocol.TYPE_MASK
    ;;   name = self.readString()
    ;;   seqid = self.readI32()
    ;; else:
    ;;   if self.strictRead:
    ;;     raise TProtocolException(type=TProtocolException.BAD_VERSION,
    ;;                              message='No protocol version header')
    ;;   name = self.trans.readAll(sz)
    ;;   type = self.readByte()
    ;;   seqid = self.readI32()
    ;; return (name, type, seqid)


;;;; Some example of data generated by the python implem of writeMessageBegin ;;

;;def writeMessageBegin(self, name, type, seqid):

;;;def __init__(self, trans, strictRead=False, strictWrite=True):
;;prot.writeMessageBegin("name", 1, 2)
; ==> \x80\x01\x00\x01 \x00\x00\x00\x04 name \x00\x00\x00\x02
;;prot.writeMessageBegin("name", 33, 44)
; ==> \x80\x01\x00! \x00\x00\x00\x04 name \x00\x00\x00,

;;;def __init__(self, trans, strictRead=False, strictWrite=False):
;;prot.writeMessageBegin("name", 1, 2)
; ==> \x00\x00\x00\x04 name \x01 \x00\x00\x00\x02
;;prot.writeMessageBegin("name", 33, 44)
; ==> \x00\x00\x00\x04 name ! \x00\x00\x00,

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
  (setq data (thrift-transport-read (oref prot transport) 2))
  (setq decoded (bindat-unpack '((d u16)) data))
  (bindat-get-field decoded 'd))

(defmethod thrift-protocol-readI32 ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readI64 ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readDouble ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-readString ((prot thrift-binary-protocol))
  (error "todo."))


(provide 'thrift-binary-protocol)
