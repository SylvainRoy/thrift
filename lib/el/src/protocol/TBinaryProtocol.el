;;; TBinaryProtocol.el ---Encode/Decode data using Thrift BinaryProtocol.

;; Author: Sylvain Roy <sylvain.roy@m4x.org>

(require 'bindat)


(defun writeMessageBegin (buffer name type seqid)
  (error "todo: not yet implemented"))

(defun writeMessageEnd (buffer)
  buffer)

(defun writeStructBegin (buffer)
  buffer)

(defun writeStructEnd (buffer)
  buffer)

(defun writeFieldBegin (buffer name type id)
  (writeI16 (writeByte buffer type) id))

(defun writeFieldEnd (buffer)
  buffer)

(defun writeFieldStop (buffer)
  (error "todo: not yet implemented."))

(defun writeMapBegin (buffer ktype vtype size)
  (error "todo: not yet implemented."))

(defun writeMapEnd (buffer)
  buffer)

(defun writeListBegin (buffer ktype vtype size)
  (error "todo: not yet implemented."))

(defun writeListEnd (buffer)
  buffer)

(defun writeSetBegin (buffer ktype vtype size)
  (error "todo: not yet implemented."))

(defun writeSetEnd (buffer)
  buffer)

(defun writeBool (buffer bool)
  "Encode a boolean."
  (writeByte buffer (if bool 1 0)))

(defun writeByte (buffer value)
  "Encode a signed integer on one bytes."
  (concat buffer
	  (bindat-pack '((data u8))
		       `((data . ,value)))))

(defun writeI16 (buffer value)
  "Encode a signed integer on 2 bytes."
  (concat buffer
	  (bindat-pack '((data u16))
		       `((data . ,value)))))

(defun writeI32 (buffer value)
  "Encode a signed integer on 4 bytes."
  ;; Emacs does not cope with integer on 32bits.
  ;; This function only works for signed number that can be encoded on 24bits.
  (let ((of (if (>= value 0) 0 #xff)))
    (concat buffer
	    (bindat-pack '((overflow u8)
			   (data     u24))
			 `((overflow . ,of)
			   (data     . ,value))))))

(defun writeI64 (buffer value)
  "Encode a signed integer on 4 bytes."
  ;; Emacs does not cope with integer on 32bits.
  ;; This function only works for signed number that can be encoded on 24bits.
  (let ((of1 (if (>= value 0) 0 #xffffff))
	(of2 (if (>= value 0) 0 #xffff)))
    (concat buffer
	    (bindat-pack '((overflow1   u24)
			   (overflow2   u16)
			   (data        u24))
			 `((overflow1 . ,of1)
			   (overflow2 . ,of2)
			   (data      . ,value))))))

(defun writeDouble (buffer value)
  (error "todo: writeDouble not yet implemented"))

(defun writeString (buffer string)
  "Encode a string."
  (concat buffer
	  (bindat-pack '((len1	   u8)
			 (len2	   u24)
			 (data     str (len2)))
		       `((len1   . 0)
			 (len2   . ,(length string))
			 (data   . ,string)))))
