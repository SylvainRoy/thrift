(require 'eieio)


(defclass thrift-base-client ()
  ((protocol :initarg :protocol
	     :document "Protocol to encode/decode and send/recv data.")
   (seqid    :initform 12
	     :document "The sequence ID of the client.")
   (callbacks :initform (list)
	      :document "The callbacks, indexed by seqid, to call once the reply has been received.")
   (functions :initform nil
	      :document "Helper functions for the various operations supported by the client."))
  "Base class for clients.")


(defmethod thrift-client-new-seqid ((client thrift-base-client))
  "Returns the next sequence ID to use."
  (setq seqid (oref client seqid))
  (oset client seqid (+ 1 seqid))
  seqid)


(defmethod initialize-instance ((client thrift-base-client) &rest slots)
  "Initializes of client object."
  (apply 'shared-initialize client slots)
  ;; Link this client in the associated transport.
  (setq trans (oref (oref client protocol) transport))
  (oset trans client client))


(defmethod thrift-client-reply-handler ((client thrift-base-client))
  "Callbacks to call upon reception of data by the tranport layer."
  (message "reply handler")
  ;; Recv and decode reply
  (setq res (thrift-client-recv client))
  (setq seqid (car res))
  (setq result (car (cdr res)))
  ;; retrieve callback and fire it
  (setq callback (plist-get (oref client callbacks) seqid))
  (funcall callback nil result)
  (throw 'done-decoding t))


(defmethod thrift-client-call ((client thrift-base-client) function parameters callback)
  "Calls a thrift service of the client."
  (setq seqid (thrift-client-new-seqid client))
  (message (concat "client call of " (pp function)
		   " with seqid " (pp seqid)))
  ;; Save callback
  (oset client callbacks
	(plist-put (oref client callbacks) seqid callback))
  ;; Retrieve ad-hoc function to encode/send the query
  (setq send-fun (car (plist-get (oref client functions) function)))
  ;; send query
  (funcall send-fun client seqid parameters))


(defmethod thrift-client-recv ((client thrift-base-client))
  "Decode the header of an incoming reply and call the associated decoder."
  ;; Decode message header
  (setq header (thrift-protocol-readMessageBegin (oref client protocol)))
  (setq name (car header))
  (setq type (car (cdr header)))
  (setq seqid (car (cdr (cdr header))))
  (message (concat "decoding incoming reply (name: " name
		   ", type: " (int-to-string type)
		   ", seqid: " (int-to-string seqid) ")"))
  ;; Retrieve ad-hoc function to read/decode the reply
  (setq recv-fun (car (cdr (plist-get (oref client functions) (intern name)))))
  ;; read query
  (setq result (funcall recv-fun client))
  (list seqid result))


(provide 'thrift-base-client)
