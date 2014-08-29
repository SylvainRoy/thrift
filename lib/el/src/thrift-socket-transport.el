(require 'thrift-base-transport)


(defclass thrift-socket-transport (thrift-base-transport)
  ((network-process :initform nil
		    :document "The underlying network-process object.")
   (recv-buffer :initform ""
		:document "Buffer to store incoming data.")
   (recv-cursor :initform 0
		:document "The position of the cursor in the recv buffer.")
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
    ;; Stores received data in buffer
    (oset transport recv-buffer
	  (concat (oref transport recv-buffer) data))
    ;; notifies client that received data are available
    (thrift-client-reply-handler (oref trans client)))
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
  "Read data."
  (setq buffer (oref trans recv-buffer))
  (setq cursor (oref trans recv-cursor))
  (if (< (- (length buffer) cursor) size)
      (throw 'not-enough-data t))
  (setq out (substring buffer cursor (+ cursor size)))
  (oset trans recv-cursor (+ cursor size))
  (string-to-unibyte out))

(defmethod thrift-transport-write ((trans thrift-base-transport) data)
  "Write data."
  (process-send-string (oref trans network-process) data)
  )

(defmethod thrift-transport-flush ((trans thrift-base-transport))
  "Flush the transport.")

(defmethod thrift-transport-cancel-reads ((trans thrift-base-transport))
  "Cancel the reads done since last read confirmation."
  (oset trans recv-cursor 0))

(defmethod thrift-transport-confirm-reads ((trans thrift-base-transport))
  "Cancel the reads done since last read confirmation."
  ;; Flush read data
  (oset trans recv-buffer (substring (oref trans recv-buffer)
				     (oref trans recv-cursor)
				     (length (oref trans recv-buffer))))
  (oset trans recv-cursor 0))


(provide 'thrift-socket-transport)
