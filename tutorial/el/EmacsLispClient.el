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
(require 'thrift-socket-transport)
(require 'thrift-binary-protocol)
(require 'thrift)

;; Require module generated for this tutorial
(add-to-list 'load-path "./gen-el/")
(require 'thrift-module-calculator)

;; Create a Calculator client using tcp transport and the thrift binary protocol
; todo: ip/port should be given here
(setq transport (thrift-socket-transport "MyTransport"))
(thrift-transport-open transport)

(setq protocol (thrift-binary-protocol "MyProtocol" :transport transport))
(setq client (thrift-client-calculator "MyCalculator" :protocol protocol))

(defun handler (err, response)
  "Function to handle the response from the thrift library."
  (message "response received!!!")
  (thrift-transport-close transport))

(thrift-client-call client 'ping '() 'handler)
;; todo: uncomment when ready...
;; (thrift-client-call client 'add '(1 1) 'handler)
;; (thrift-client-call client 'divide '(1 0) 'handler)
;; (thrift-client-call client 'substract '(1 0) 'handler)

(message "This is the end...")
