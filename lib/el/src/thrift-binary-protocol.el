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


(defmethod thrift-protocol-write-message-begin ((prot thrift-binary-protocol) name type seq)
  (message (concat "writemessage : " name
		   ", type: " (int-to-string type)
		   ", seqid: " (int-to-string seq)))
  (if (oref prot strictWrite)
      (progn
	(thrift-protocol-write-i16 prot thrift-binary-protocol-version-1)
	(thrift-protocol-write-byte prot 0)
	(thrift-protocol-write-byte prot type)
	(thrift-protocol-write-string prot name)
	(thrift-protocol-write-i32 prot seq))
    (progn
      (thrift-protocol-write-string prot name)
      (thrift-protocol-write-byte prot type)
      (thrift-protocol-write-i32 prot seq))))

(defmethod thrift-protocol-write-message-end ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-write-struct-begin ((prot thrift-binary-protocol) name)
  nil)

(defmethod thrift-protocol-write-struct-end ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-write-field-begin ((prot thrift-binary-protocol) name type id)
  (thrift-protocol-write-byte prot type)
  (thrift-protocol-write-i16  prot id))

(defmethod thrift-protocol-write-field-end ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-write-field-stop ((prot thrift-binary-protocol))
  (thrift-protocol-write-byte prot (thrift-constant-type 'stop)))

(defmethod thrift-protocol-write-map-begin ((prot thrift-binary-protocol) ktype vtype size)
  (thrift-protocol-write-byte prot ktype)
  (thrift-protocol-write-byte prot vtype)
  (thrift-protocol-write-i32 prot size))

(defmethod thrift-protocol-write-map-end ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-write-list-begin ((prot thrift-binary-protocol) etype size)
  (thrift-protocol-write-byte prot etype)
  (thrift-protocol-write-i32 prot size))

(defmethod thrift-protocol-write-list-end ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-write-set-begin ((prot thrift-binary-protocol) etype size)
  (thrift-protocol-write-byte prot etype)
  (thrift-protocol-write-i32 prot size))

(defmethod thrift-protocol-write-set-end ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-write-bool ((prot thrift-binary-protocol) bool)
  (thrift-protocol-write-byte prot (if bool 1 0)))

(defmethod thrift-protocol-write-byte ((prot thrift-binary-protocol) byte)
  (thrift-transport-write (oref prot transport)
			  (bindat-pack '((data u8))
				       `((data . ,byte)))))

(defmethod thrift-protocol-write-i16 ((prot thrift-binary-protocol) i16)
  (thrift-transport-write (oref prot transport)
			  (bindat-pack '((data u16))
				       `((data . ,i16)))))

(defmethod thrift-protocol-write-i32 ((prot thrift-binary-protocol) i32)
  ;; Emacs does not cope with integer on 32bits.
  ;; This function only works for signed number that can be encoded on 24bits.
  (let ((of (if (>= i32 0) 0 #xff)))
    (thrift-transport-write (oref prot transport)
			    (bindat-pack '((overflow u8)
					   (data     u24))
					 `((overflow . ,of)
					   (data     . ,i32))))))

(defmethod thrift-protocol-write-i64 ((prot thrift-binary-protocol) i64)
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

(defmethod thrift-protocol-write-double ((prot thrift-binary-protocol) double)
  (error "todo (see javascript implementation in nodejs/../../binary.js implem...)"))

(defmethod thrift-protocol-write-string ((prot thrift-binary-protocol) string)
  (thrift-transport-write (oref prot transport)
			  (bindat-pack '((len1     u8)
					 (len2     u24)
					 (data     str (len2)))
				       `((len1   . 0)
					 (len2   . ,(length string))
					 (data   . ,string)))))

(defmethod thrift-protocol-read-message-begin ((prot thrift-binary-protocol))
  (setq version1 (thrift-protocol-read-byte prot))
  (if (not (equal 0 (logand version1 #x80)))
      ;; 'strict write' applied
      (progn
	(setq version2 (thrift-protocol-read-byte prot))
	(if (not (and (equal version1 #x80)
		      (equal version2 #x01)))
	    (error "Bad version in read-message-begin"))
	(thrift-protocol-read-byte prot)             ; useless byte
	(setq type (thrift-protocol-read-byte prot))
	(setq name (thrift-protocol-read-string prot))
	(setq seqid (thrift-protocol-read-i32 prot)))
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
      (setq type (thrift-protocol-read-byte prot))
      (setq seqid (thrift-protocol-read-i32 prot))))
  (list name type seqid))

(defmethod thrift-protocol-read-message-end ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-read-struct-begin ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-read-struct-end ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-read-field-begin ((prot thrift-binary-protocol))
  (setq type (thrift-protocol-read-byte prot))
  (if (equal type (thrift-constant-type 'stop))
      (list nil type 0)
    (progn
      (setq id (thrift-protocol-read-i16  prot))
      (list nil type id))))

(defmethod thrift-protocol-read-field-end ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-read-field-stop ((prot thrift-binary-protocol))
  (thrift-protocol-write-byte (thrift-constant-type 'stop)))

(defmethod thrift-protocol-read-map-begin ((prot thrift-binary-protocol))
  (setq ktype (thrift-protocol-read-byte prot))
  (setq vtype (thrift-protocol-read-byte prot))
  (setq size (thrift-protocol-read-i32 prot))
  (list ktype vtype size))

(defmethod thrift-protocol-read-map-end ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-read-list-begin ((prot thrift-binary-protocol))
  (setq etype (thrift-protocol-read-byte prot))
  (setq size (thrift-protocol-read-i32 prot))
  (list etype size))

(defmethod thrift-protocol-read-list-end ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-read-set-begin ((prot thrift-binary-protocol))
  (setq etype (thrift-protocol-read-byte prot))
  (setq size (thrift-protocol-read-i32 prot))
  (list etype size))

(defmethod thrift-protocol-read-set-end ((prot thrift-binary-protocol))
  nil)

(defmethod thrift-protocol-read-bool ((prot thrift-binary-protocol))
  (not (equal 0 (thrift-protocol-read-byte prot))))

(defmethod thrift-protocol-read-byte ((prot thrift-binary-protocol))
  (setq data (thrift-transport-read (oref prot transport) 1))
  (setq decoded (bindat-unpack '((d u8)) data))
  (bindat-get-field decoded 'd))

(defmethod thrift-protocol-read-i16 ((prot thrift-binary-protocol))
  (setq data (thrift-transport-read (oref prot transport) 2))
  (setq decoded (bindat-unpack '((d u16)) data))
  (bindat-get-field decoded 'd))

(defmethod thrift-protocol-read-i32 ((prot thrift-binary-protocol))
  ;; Emacs is limited to integer on 30 bits. Same limitation applies here.
  (setq data (thrift-transport-read (oref prot transport) 4))
  (setq decoded (bindat-unpack '((d u32)) data))
  (bindat-get-field decoded 'd))

(defmethod thrift-protocol-read-i64 ((prot thrift-binary-protocol))
  ;; Emacs is limited to integer on 30 bits. Same limitation applies here.
  (thrift-transport-read (oref prot transport) 4)
  (setq data (thrift-transport-read (oref prot transport) 4))
  (setq decoded (bindat-unpack '((d u32)) data))
  (bindat-get-field decoded 'd))

(defmethod thrift-protocol-read-double ((prot thrift-binary-protocol))
  (error "todo."))

(defmethod thrift-protocol-read-string ((prot thrift-binary-protocol))
  (setq len (thrift-protocol-read-i32 prot))
  (setq data (thrift-transport-read (oref prot transport) len))
  (setq decoded (bindat-unpack `((d str ,len)) data))
  (bindat-get-field decoded 'd))

(provide 'thrift-binary-protocol)
