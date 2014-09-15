;;; Emacslispclient.el ---Tutorial of Thrift usage.

;; Licensed to the Apache Software Foundation (ASF) under one
;; or more contributor license agreements. See the NOTICE file
;; distributed with this work for additional information
;; regarding copyright ownership. The ASF licenses this file
;; to you under the Apache License, Version 2.0 (the
;; "License"); you may not use this file except in compliance
;; with the License. You may obtain a copy of the License at
;;
;;   http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing,
;; software distributed under the License is distributed on an
;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;; KIND, either express or implied. See the License for the
;; specific language governing permissions and limitations
;; under the License.


;; Require thrift library modules
(add-to-list 'load-path "../../lib/el/src")
(require 'thrift)
(require 'thrift-socket-transport)
(require 'thrift-binary-protocol)

;; Require module generated for this tutorial
(add-to-list 'load-path "./gen-el/")
(require 'thrift-gen-tutorial-Calculator)

;; Create a Calculator client using TCP transport and binary protocol
(setq mytransport (thrift-socket-transport "MyTransport"
					   :host "localhost"
					   :port 9090))
(setq myprotocol (thrift-binary-protocol "MyProtocol"
					 :transport mytransport))
(setq myclient (thrift-gen-tutorial-Calculator "MyService"
					       :protocol myprotocol))

(thrift-transport-open mytransport)

(thrift-call myclient
	     'ping
	     '()
	     (lambda (err response)
	       (message "ping()")))

(thrift-call myclient
	     'add
	     '(:num1 1 :num2 1)
	     (lambda (err response)
	       (message "1+1=%d" response)))

(thrift-call myclient
	     'calculate
	     '(:logid 1
	       :w (:num1    1
		   :num2    0
		   :op      divide
		   :comment ""))
	     (lambda (err response)
	       (message "1/0=%s"
			(if err
			    (format "InvalidOperation %S" err)
			  "Whoa? You know how to divide by zero?"))))

(thrift-call myclient
	     'calculate
	     '(:logid 1
	       :w (:num1    15
		   :num2    10
		   :op      substract
		   :comment "comment"))
	     (lambda (err response)
	       (message "15-10=%d" response)
	       (thrift-transport-close mytransport)))
