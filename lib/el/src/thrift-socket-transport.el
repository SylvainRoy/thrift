(require 'thrift-base-transport)


(defclass thrift-socket-transport (thrift-base-transport)
  ((network-process :initform nil
		    :document "The underlying network-process object.")
   (recv-buffer :initform ""
		:document "Buffer to store incoming data.")
   (host :initarg :host
	 :document "the host to connect to as a name or an internet address.")
   (port :initarg :port
	 :document "the port number to connect to."))
  "TCP tranport implementation.")


(defmethod thrift-transport-open ((trans thrift-base-transport))
  "Open the transport."
  (setq transport trans)
  (defun trans-filter (process data)
    (message (concat "Raw reply received: '" data "'"))
    ;; Stored data received in buffer
    (oset transport recv-buffer
	  (concat (oref transport recv-buffer) data))
    ;; calls client handler
    (funcall (oref transport on-data-received))
    )
  (defun trans-sentinel (process event)
    (message (concat "transport event: " event)))
  (oset trans network-process (make-network-process :name "*thrift*"
						    :type nil
						    :server nil
						    :host (oref trans host)
						    :service (oref trans port)
						    :filter 'trans-filter
						    :sentinel 'trans-sentinel)))

(defmethod thrift-transport-close ((trans thrift-base-transport))
  "Close the transport."
  (delete-process (oref trans network-process)))

(defmethod thrift-transport-read ((trans thrift-base-transport) size)
  "Receive data."
  (setq data (oref trans recv-buffer))
  (setq toread (min size (length data)))
  (setq out (substring data 0 toread))
  (oset trans recv-buffer (substring data toread (length data)))
  (string-to-unibyte out))

(defmethod thrift-transport-write ((trans thrift-base-transport) data)
  "Write data."
  (process-send-string (oref trans network-process) data)
  )

(defmethod thrift-transport-flush ((trans thrift-base-transport))
  "Flush the transport.")


(provide 'thrift-socket-transport)
