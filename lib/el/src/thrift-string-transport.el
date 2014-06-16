(require 'thrift-base-transport)

(defclass thrift-string-transport (thrift-base-transport)
  ((recv :initarg :recv
	 :initform ""
	 :writer set-recv
	 :reader get-recv
	 :document "Received from the transport.")
   (sent :initarg :sent
	 :initform ""
	 :writer set-sent
	 :reader get-sent
	 :document "Sent to the transport."))
  "A basic string implementation of a transport for testing.")

(defmethod get-sent-and-reset((trans thrift-string-transport))
  "Receive data."
  (setq o (oref trans sent))
  (oset trans sent "")
  o)

(defmethod thrift-transport-read ((trans thrift-string-transport))
  "Receive data."
  (setq o (oref trans recv))
  (oset trans recv "")
  o)

(defmethod thrift-transport-write ((trans thrift-string-transport) data)
  "Write data."
  (oset trans sent (concat (oref trans sent)
			   data)))

(defmethod thrift-transport-open ((trans thrift-string-transport))
  "")

(defmethod thrift-transport-close ((trans thrift-string-transport))
  "")

(defmethod thrift-transport-flush ((trans thrift-string-transport))
  "")

(provide 'thrift-string-transport)
