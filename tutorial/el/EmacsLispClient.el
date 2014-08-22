;;; EmacsLispClient.el ---Tutorial of Thrift usage.

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
(require 'thrift-client-calculator)


;; Create a Calculator client using tcp transport and the thrift binary protocol
(setq transport (thrift-socket-transport "MyTransport"
					 :host "localhost"
					 :port 9090))
(thrift-transport-open transport)

(setq protocol (thrift-binary-protocol "MyProtocol" :transport transport))
(setq client (thrift-client-calculator "MyCalculator" :protocol protocol))

(defun handler (err response)
  "Function to handle the response from the thrift library."
  (setq result "-")
  (if (equal 1 (length response))
      (setq result (int-to-string (car response))))
  (message (concat "response received: " result))
  (thrift-transport-close transport))

;; Uncomment the operation you want to use.
;;(thrift-client-call client 'ping '() 'handler)
 (thrift-client-call client 'add '(17 12) 'handler)
;; (thrift-client-call client 'divide '(1 0) 'handler)
;; (thrift-client-call client 'substract '(1 0) 'handler)
