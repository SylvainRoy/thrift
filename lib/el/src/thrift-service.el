(require 'eieio)


(defclass thrift-service ()
  ((protocol  :initarg :protocol
	      :document "Protocol to encode/decode and send/recv data.")
   (seqid     :initform 0
	      :document "The sequence ID of the service.")
   (callbacks :initform (list)
	      :document "The callbacks, indexed by seqid, to call once the reply has been received.")
   (functions :initform nil
	      :document "Encoders/decoders of the service operations."))
  "Base class for services.")


(defmethod initialize-instance ((service thrift-service) &rest slots)
  "Initialize the service object."
  (apply 'shared-initialize service slots)
  ;; Register a callback in transport to be warned upon data reception
  (setq trans (oref (oref service protocol) transport))
  (setq c service) ; required to have 'service' visible in the lambda function
  (oset trans callback (lambda () (thrift-service-reply-handler c))))


(defmethod thrift-call ((service thrift-service) function parameters callback)
  "Calls a thrift service of the service."
  (setq seqid (thrift-service-new-seqid service))
  ;; Save callback
  (oset service callbacks
	(plist-put (oref service callbacks) seqid callback))
  ;; Retrieve ad-hoc function to encode and send the query
  (setq send-fun (car (plist-get (oref service functions) function)))
  ;; Send query
  (funcall send-fun (oref service protocol) seqid parameters)
  (thrift-transport-flush (oref (oref service protocol) transport)))


(defmethod thrift-service-reply-handler ((service thrift-service))
  "Callbacks to call upon reception of data by the tranport layer."
  (setq protocol (oref service protocol))
  (setq tranport (oref protocol transport))
  (catch 'not-enough-data
    (while t
      ;; Receive and decode reply
      (setq res (thrift-service-recv service))
      (setq seqid (pop res))
      (setq error (pop res))
      (setq result (pop res))
      ;; Retrieve associated callback and fire it
      (setq callback (plist-get (oref service callbacks) seqid))
      (funcall callback error result)
      ;; Flush the data successfuly read from recv buffer
      (thrift-transport-confirm-reads transport)))
  ;; Cancel reads that could not complete if any
  (thrift-transport-cancel-reads transport))


(defmethod thrift-service-recv ((service thrift-service))
  "Decode the header of an incoming reply and call the associated decoder."
  ;; Decode message header
  (setq header (thrift-protocol-readMessageBegin (oref service protocol)))
  (setq name (pop header))
  (setq type (pop header))
  (setq seqid (pop header))
  ;; Retrieve ad-hoc function to read/decode the reply
  (setq recv-fun (car (cdr (plist-get (oref service functions) (intern name)))))
  ;; Read query
  (setq result (funcall recv-fun (oref service protocol)))
  ;; Return (seqid, error, result)
  (cons seqid result))


(defmethod thrift-service-new-seqid ((service thrift-service))
  "Returns the next sequence ID to use."
  (setq seqid (oref service seqid))
  (oset service seqid (+ 1 seqid))
  seqid)


(provide 'thrift-service)
