(require 'eieio)

(defclass thrift-base-transport ()
  ((client :initform nil
	   :document "The client associated to this transport."))
  "A 'virtual' class for thrift transports.")


(defmethod thrift-transport-open ((trans thrift-base-transport))
  "Open the transport."
  (error "The method 'open' should be defined in subclasses."))

(defmethod thrift-transport-close ((trans thrift-base-transport))
  "Close the transport."
  (error "The method 'close' should be defined in subclasses."))

(defmethod thrift-transport-read ((trans thrift-base-transport) size)
  "Receive data."
  (error "The method 'read' should be defined in subclasses."))

(defmethod thrift-transport-write ((trans thrift-base-transport) data)
  "Write data."
  (error "The method 'write' should be defined in subclasses."))

(defmethod thrift-transport-flush ((trans thrift-base-transport))
  "Flush the transport."
  (error "The method 'flush' should be defined in subclasses."))

(defmethod thrift-transport-cancel-reads ((trans thrift-base-transport))
  "Cancel the reads done since last read confirmation."
  (error "The method 'cancel-reads' should be defined in subclasses."))

(defmethod thrift-transport-confirm-reads ((trans thrift-base-transport))
  "Cancel the reads done since last read confirmation."
  (error "The method 'confirm-reads' should be defined in subclasses."))


(provide 'thrift-base-transport)
