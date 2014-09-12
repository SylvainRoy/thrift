(require 'eieio)


(defclass thrift-service ()
  ((protocol  :initarg :protocol
	      :document "Protocol to encode/decode and send/recv data.")
   (seqid     :initform 12
	      :document "The sequence ID of the client.")
   (callbacks :initform (list)
	      :document "The callbacks, indexed by seqid, to call once the reply has been received.")
   (functions :initform nil
	      :document "Helper functions for the various operations supported by the client."))
  "Base class for clients.")


(defmethod initialize-instance ((client thrift-service) &rest slots)
  "Initializes of client object."
  (apply 'shared-initialize client slots)
  ;; Link this client in the associated transport.
  (setq trans (oref (oref client protocol) transport))
  (oset trans client client))


(defmethod thrift-call ((client thrift-service) function parameters callback)
  "Calls a thrift service of the client."
  (setq seqid (thrift-client-new-seqid client))
  ;; Save callback
  (oset client callbacks
	(plist-put (oref client callbacks) seqid callback))
  ;; Retrieve ad-hoc function to encode and send the query
  (setq send-fun (car (plist-get (oref client functions) function)))
  ;; Send query
  (funcall send-fun client seqid parameters)
  (thrift-transport-flush (oref (oref client protocol) transport)))


(defmethod thrift-client-reply-handler ((client thrift-service))
  "Callbacks to call upon reception of data by the tranport layer."
  (setq protocol (oref client protocol))
  (setq tranport (oref protocol transport))
  (catch 'not-enough-data
    (while t
      ;; Receive and decode reply
      (setq res (thrift-client-recv client))
      (setq seqid (pop res))
      (setq error (pop res))
      (setq result (pop res))
      ;; Retrieve associated callback and fire it
      (setq callback (plist-get (oref client callbacks) seqid))
      (funcall callback error result)
      ;; Flush the data successfuly read from recv buffer
      (thrift-transport-confirm-reads transport)))
  ;; Cancel reads that could not complete if any
  (thrift-transport-cancel-reads transport))


(defmethod thrift-client-recv ((client thrift-service))
  "Decode the header of an incoming reply and call the associated decoder."
  ;; Decode message header
  (setq header (thrift-protocol-readMessageBegin (oref client protocol)))
  (setq name (pop header))
  (setq type (pop header))
  (setq seqid (pop header))
  ;; Retrieve ad-hoc function to read/decode the reply
  (setq recv-fun (car (cdr (plist-get (oref client functions) (intern name)))))
  ;; Read query
  (setq result (funcall recv-fun client))
  ;; Return (seqid, error, result)
  (cons seqid result))


(defmethod thrift-client-new-seqid ((client thrift-service))
  "Returns the next sequence ID to use."
  (setq seqid (oref client seqid))
  (oset client seqid (+ 1 seqid))
  seqid)


(provide 'thrift-service)
