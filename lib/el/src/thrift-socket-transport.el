(require 'thrift-base-transport)


(defclass thrift-socket-transport (thrift-base-transport)
  ((network-process :initform nil
		    :document "The underlying network-process object.")
   (recv-buffer :initform ""
		:document "Buffer to store incoming data."))
  "TCP tranport implementation.")


(defmethod thrift-transport-open ((trans thrift-base-transport))
  "Open the transport."
  (defun trans-filter (process data)
    (oset trans recv-buffer
	  (concat (oref trans recv-buffer) data)))
  (defun trans-sentinel (process event)
    (message event))
  (oset trans network-process (make-network-process :name "*thrift*"
						    :type nil
						    :server nil
						    :host "127.0.0.1"
						    :service 10000
						    :filter 'trans-filter
						    :sentinel 'trans-sentinel)))

(defmethod thrift-transport-close ((trans thrift-base-transport))
  "Close the transport."
  (delete-process (oref trans network-process)))

(defmethod thrift-transport-read ((trans thrift-base-transport))
  "Receive data."
  (setq o (oref trans recv-buffer))
  (oset trans recv-buffer "")
  o)

(defmethod thrift-transport-write ((trans thrift-base-transport) data)
  "Write data."
  (process-send-string (oref trans network-process) data))

(defmethod thrift-transport-flush ((trans thrift-base-transport))
  "Flush the transport.")


(provide 'thrift-socket-transport)
