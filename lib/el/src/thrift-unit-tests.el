;;; unittest.el ---Unit test Thrift elisp implementation.

;; To run the test in batch mode:
;;  emacs -batch -l ert -l unittest.el -f ert-run-tests-batch-and-exit

;; To run the test from emacs:
;;  M-x ert RET t RET

;; Author: Sylvain Roy <sylvain.roy@m4x.org>

(require 'ert)

(add-to-list 'load-path "./")
(require 'thrift-string-transport)
(require 'thrift-binary-protocol)

;; Init transport and protocol
(setq transport (thrift-string-transport "TestTrans"))
(setq protocol (thrift-binary-protocol "TestProt" :transport transport))

(ert-deftest thrift-string-transport-test ()
  "Tests the string-transport transport."
  ;; test the read part
  (set-recv transport "0123456789")
  (should (equal (thrift-transport-read transport 5) "01234"))
  (should (equal (thrift-transport-read transport 10) "56789"))
  (should (equal (thrift-transport-read transport 10) ""))
  (set-recv transport "0123456789")
  (should (equal (thrift-transport-read transport 5) "01234"))
  (should (equal (thrift-transport-read transport 10) "56789"))
  (should (equal (thrift-transport-read transport 10) ""))
  ;; test the write part
  (thrift-transport-write transport "012345")
  (thrift-transport-write transport "6789")
  (should (equal (get-sent transport) "0123456789"))
  (should (equal (get-sent-and-reset transport) "0123456789"))
  (should (equal (get-sent transport) ""))
  (should (equal (get-sent-and-reset transport) "")))

(ert-deftest thrift-binary-protocol-writeMessageBegin-test ()
  "Tests the encoding of 'writeMessageBegin' using the thrift-binary-protocol."
  (thrift-protocol-writeMessageBegin protocol "name" 33 44)
  (should (equal (get-sent-and-reset transport)
		 "\x80\x01\x00!\x00\x00\x00\x04name\x00\x00\x00,"))
  (thrift-protocol-writeMessageBegin protocol "name" 1 2)
  (should (equal (get-sent-and-reset transport)
		 "\x80\x01\x00\x01\x00\x00\x00\x04name\x00\x00\x00\x02")))

(ert-deftest thrift-binary-protocol-writeMessageEnd-test ()
  "Tests the encoding of 'writeMessageEnd' using the thrift-binary-protocol."
  (thrift-protocol-writeMessageEnd protocol)
  (should (equal (get-sent-and-reset transport) "")))

(ert-deftest thrift-binary-protocol-writeFieldBegin-test ()
  "Tests the encoding of 'writeFieldBegin' using the thrift-binary-protocol."
  (thrift-protocol-writeFieldBegin protocol "name" 1 2)
  (should (equal (get-sent-and-reset transport) "\x01\x00\x02")))

(ert-deftest thrift-binary-protocol-writeFieldStop-test ()
  "Tests the encoding of 'writeFieldStop' using the thrift-binary-protocol."
  (thrift-protocol-writeFieldStop protocol)
  (should (equal (get-sent-and-reset transport) "\x00")))

(ert-deftest thrift-binary-protocol-writeMapBegin-test ()
  "Tests the encoding of 'writeMapBegin' using the thrift-binary-protocol."
  (thrift-protocol-writeMapBegin protocol 1 2 3)
  (should (equal (get-sent-and-reset transport) "\x01\x02\x00\x00\x00\x03")))

(ert-deftest thrift-binary-protocol-writeMapEnd-test ()
  nil)

(ert-deftest thrift-binary-protocol-writeListBegin-test ()
  "Tests the encoding of Lists using the thrift-binary-protocol."
  (thrift-protocol-writeListBegin protocol 1 2)
  (should (equal (get-sent-and-reset transport) "\x01\x00\x00\x00\x02")))

(ert-deftest thrift-binary-protocol-writeListEnd-test ()
  nil)

(ert-deftest thrift-binary-protocol-writeSetBegin-test ()
  "Tests the encoding of Sets using the thrift-binary-protocol."
  (thrift-protocol-writeSetBegin protocol 1 2)
  (should (equal (get-sent-and-reset transport) "\x01\x00\x00\x00\x02")))

(ert-deftest thrift-binary-protocol-writeSetEnd-test ()
  nil)

(ert-deftest thrift-binary-protocol-writeBool-test ()
  "Tests the encoding of Bool using the thrift-binary-protocol."
  ;; Test encoding of bool=true
  (thrift-protocol-writeBool protocol t)
  (should (equal (get-sent-and-reset transport) "\x01"))
  ;; Test encoding of bool=false
  (thrift-protocol-writeBool protocol nil)
  (should (equal (get-sent-and-reset transport) "\x00")))

(ert-deftest thrift-binary-protocol-writeByte-test ()
  "Tests the encoding of Bytes using the thrift-binary-protocol."
  (thrift-protocol-writeByte protocol 0)
  (should (equal (get-sent-and-reset transport) "\x00"))
  (thrift-protocol-writeByte protocol -1)
  (should (equal (get-sent-and-reset transport) "\xff"))
  (thrift-protocol-writeByte protocol 1)
  (should (equal (get-sent-and-reset transport) "\x01")))

(ert-deftest thrift-binary-protocol-writeI16-test ()
  "Tests the encoding of i16 using the thrift-binary-protocol."
  (thrift-protocol-writeI16 protocol 0)
  (should (equal (get-sent-and-reset transport) "\x00\x00"))
  (thrift-protocol-writeI16 protocol 1)
  (should (equal (get-sent-and-reset transport) "\x00\x01"))
  (thrift-protocol-writeI16 protocol -1)
  (should (equal (get-sent-and-reset transport) "\xff\xff")))

(ert-deftest thrift-binary-protocol-writeI32-test ()
  "Tests the encoding of i32 using the thrift-binary-protocol."
  (thrift-protocol-writeI32 protocol 0)
  (should (equal (get-sent-and-reset transport) "\x00\x00\x00\x00"))
  (thrift-protocol-writeI32 protocol 1)
  (should (equal (get-sent-and-reset transport) "\x00\x00\x00\x01"))
  (thrift-protocol-writeI32 protocol -1)
  (should (equal (get-sent-and-reset transport) "\xff\xff\xff\xff")))

(ert-deftest thrift-binary-protocol-writeI64-test ()
  "Tests the encoding of i32 using the thrift-binary-protocol."
  (thrift-protocol-writeI64 protocol 0)
  (should (equal (get-sent-and-reset transport) "\x00\x00\x00\x00\x00\x00\x00\x00"))
  (thrift-protocol-writeI64 protocol 1)
  (should (equal (get-sent-and-reset transport) "\x00\x00\x00\x00\x00\x00\x00\x01"))
  (thrift-protocol-writeI64 protocol -1)
  (should (equal (get-sent-and-reset transport) "\xff\xff\xff\xff\xff\xff\xff\xff")))

(ert-deftest thrift-binary-protocol-writeDouble-test ()
  "todo"
  nil)

(ert-deftest thrift-binary-protocol-writeString-test ()
  "Tests the encoding of strings using the thrift-binary-protocol."
  (thrift-protocol-writeString protocol "a")
  (should (equal (get-sent-and-reset transport) (concat "\x00\x00\x00\x01" "a")))
  (thrift-protocol-writeString protocol "bb")
  (should (equal (get-sent-and-reset transport) (concat "\x00\x00\x00\x02" "bb"))))

(ert-deftest thrift-binary-protocol-readI16-test ()
  "Tests the decoding of I16 using the thrift-binary-protocol."
  (set-recv transport "\x00\x00")
  (should (equal (thrift-protocol-readI16 protocol) 0))
  (set-recv transport "\x00\x01\x00\x02")
  (should (equal (thrift-protocol-readI16 protocol) 1))
  (should (equal (thrift-protocol-readI16 protocol) 2))
  (set-recv transport "\xFF\xFF")
  (should (equal (thrift-protocol-readI16 protocol) 65535))
)

;;todo: test of read functions
