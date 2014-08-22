(require 'eieio)


(defclass thrift-base-client ()
  ((protocol :initarg :protocol
	     :document "Protocol to encode/decode and send/recv data.")
   (seqid    :initform 0
	     :document "The sequence ID of the client")
   (functions :initform nil
	      :document "Helper functions for the various operations supported by the client."))
  "Base class for clients.")


(defmethod thrift-client-call ((client thrift-base-client) function parameters callback)
  "Calls a thrift service of the client."
  (setq handler callback)
  ;; Build handler to call upon reception of the data of the reply
  (defun reply-data-handler ()
    (setq res (thrift-client-recv client))
    (funcall handler nil res))
  ;; Register the handler in the transport
  (oset (oref (oref client protocol) transport)
	on-data-received
	'reply-data-handler)
  ;; Write query to the protocol/transport with ad-hoc function
  (setq send-fun (car (plist-get (oref client functions) function)))
  (funcall send-fun client parameters))


(defmethod thrift-client-recv ((client thrift-base-client))
  "Decode the header of an incoming reply and call the associated decoder."
  ;; Decode message header
  (setq header (thrift-protocol-readMessageBegin (oref client protocol)))
  (setq name (car header))
  (setq type (car (cdr header)))
  (setq seqid (car (cdr (cdr header))))
  ;; Decode and return reply content with ad-hoc decoder
  (setq recv-fun (car (cdr (plist-get (oref client functions) (intern name)))))
  (funcall recv-fun client))


(provide 'thrift-base-client)
