(require 'eieio)

(defclass thrift-base-transport ()
  ()
  "A 'virtual' class for thrift transports.")


(defmethod thrift-transport-open ((trans thrift-base-transport))
  "Open the transport."
  (error "This method should be defined in subclasses."))

(defmethod thrift-transport-close ((trans thrift-base-transport))
  "Close the transport."
  (error "This method should be defined in subclasses."))

(defmethod thrift-transport-read ((trans thrift-base-transport) size)
  "Receive data."
  (error "This method should be defined in subclasses."))

(defmethod thrift-transport-write ((trans thrift-base-transport) data)
  "Write data."
  (error "This method should be defined in subclasses."))

(defmethod thrift-transport-flush ((trans thrift-base-transport))
  "Flush the transport."
  (error "This method should be defined in subclasses."))

(provide 'thrift-base-transport)
