(require 'thrift-base-transport)

(defclass thrift-base-server-transport (thrift-base-transport)
  ()
  "A 'virtual' class for thrift server-transports.")

(defmethod thrift-transport-listen ((trans thrift-base-server-transport))
  "Receive data."
  (error "This method should be defined in subclasses."))

(defmethod thrift-transport-accept ((trans thrift-base-server-transport) data)
  "Write data."
  (error "This method should be defined in subclasses."))

(provide 'thrift-base-server-transport)
