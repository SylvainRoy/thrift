;;; unittest.el ---Unit test Thrift elisp implementation.

;; To run the test in batch mode:
;; emacs -batch -l ert -l thrift-unit-tests.el -f ert-run-tests-batch-and-exit

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

;;; Testing of the 'string' transport ;;;

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


;;; Testing of the 'base' protocol ;;;

(ert-deftest thrift-protocol-skip-test ()
  "Tests the 'skip' method defined in thrift-base-protocol."
  ;; skip a struct with a byte only
  (set-recv transport "\x03\x00\x00\x00\x00")
  (thrift-protocol-skip protocol (thrift-constant-type 'struct))
  (should (equal (oref transport recv) ""))
  ;; skip a struct with a byte and an i16
  (set-recv transport "\x03\x00\x00\x00\x06\x00\x01\x00\x01\x00")
  (thrift-protocol-skip protocol (thrift-constant-type 'struct))
  (should (equal (oref transport recv) ""))
  ;; skip a struct with: bool, byte, i16, i32, i64 and a string.
  (set-recv transport "\x02\x00\x00\x01\x03\x00\x00\x00\x06\x00\x00\x00\x01\x08\x00\x00\x00\x00\x00\x00\n\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00\x00\x00\x06string\x00")
  (thrift-protocol-skip protocol (thrift-constant-type 'struct))
  (should (equal (oref transport recv) ""))
  ;; skip a struct of a map(i16->i32) with two elements
  (set-recv transport (concat "\r" "\x00\x00\x06\x08\x00\x00\x00\x02\x00\x01\x00\x00\x00" "@" "\x00\x02\x00\x00\x00" "*" "\x00"))
  (thrift-protocol-skip protocol (thrift-constant-type 'struct))
  (should (equal (oref transport recv) ""))
  ;; skip a map(i16->i32) with two elements
  (set-recv transport "\x06\x08\x00\x00\x00\x02\x00\x01\x00\x00\x00@\x00\x02\x00\x00\x00*")
  (thrift-protocol-skip protocol (thrift-constant-type 'map))
  (should (equal (oref transport recv) ""))
  ;; skip a map(string->i32) with two elements
  (set-recv transport "\x0b\x08\x00\x00\x00\x02\x00\x00\x00\x04key1\x00\x00\x04\xd2\x00\x00\x00\x04key2\x00\x00\x16.")
  (thrift-protocol-skip protocol (thrift-constant-type 'map))
  (should (equal (oref transport recv) ""))
  ;; skip a list of i32 of 3 elements
  (set-recv transport "\x08\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02")
  (thrift-protocol-skip protocol (thrift-constant-type 'list))
  (should (equal (oref transport recv) ""))
  ;; skip a set of i32 of 3 elements
  (set-recv transport "\x08\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02")
  (thrift-protocol-skip protocol (thrift-constant-type 'set))
  (should (equal (oref transport recv) "")))



;;; Testing of the 'binary' protocol ;;;

(ert-deftest thrift-binary-protocol-write-message-begin-test ()
  "Tests the encoding of 'write-message-begin' using the thrift-binary-protocol."
  (thrift-protocol-write-message-begin protocol "name" 33 44)
  (should (equal (get-sent-and-reset transport)
		 "\x80\x01\x00!\x00\x00\x00\x04name\x00\x00\x00,"))
  (thrift-protocol-write-message-begin protocol "name" 1 2)
  (should (equal (get-sent-and-reset transport)
		 "\x80\x01\x00\x01\x00\x00\x00\x04name\x00\x00\x00\x02")))

(ert-deftest thrift-binary-protocol-write-message-end-test ()
  "Tests the encoding of 'write-message-end' using the thrift-binary-protocol."
  (thrift-protocol-write-message-end protocol)
  (should (equal (get-sent-and-reset transport) "")))

(ert-deftest thrift-binary-protocol-write-field-begin-test ()
  "Tests the encoding of 'write-field-begin' using the thrift-binary-protocol."
  (thrift-protocol-write-field-begin protocol "name" 1 2)
  (should (equal (get-sent-and-reset transport) "\x01\x00\x02")))

(ert-deftest thrift-binary-protocol-write-field-stop-test ()
  "Tests the encoding of 'write-field-stop' using the thrift-binary-protocol."
  (thrift-protocol-write-field-stop protocol)
  (should (equal (get-sent-and-reset transport) "\x00")))

(ert-deftest thrift-binary-protocol-write-map-begin-test ()
  "Tests the encoding of 'write-map-begin' using the thrift-binary-protocol."
  (thrift-protocol-write-map-begin protocol 1 2 3)
  (should (equal (get-sent-and-reset transport) "\x01\x02\x00\x00\x00\x03")))

(ert-deftest thrift-binary-protocol-write-map-end-test ()
  nil)

(ert-deftest thrift-binary-protocol-write-list-begin-test ()
  "Tests the encoding of Lists using the thrift-binary-protocol."
  (thrift-protocol-write-list-begin protocol 1 2)
  (should (equal (get-sent-and-reset transport) "\x01\x00\x00\x00\x02")))

(ert-deftest thrift-binary-protocol-write-list-end-test ()
  nil)

(ert-deftest thrift-binary-protocol-write-set-begin-test ()
  "Tests the encoding of Sets using the thrift-binary-protocol."
  (thrift-protocol-write-set-begin protocol 1 2)
  (should (equal (get-sent-and-reset transport) "\x01\x00\x00\x00\x02")))

(ert-deftest thrift-binary-protocol-write-set-end-test ()
  nil)

(ert-deftest thrift-binary-protocol-write-bool-test ()
  "Tests the encoding of Bool using the thrift-binary-protocol."
  ;; Test encoding of bool=true
  (thrift-protocol-write-bool protocol t)
  (should (equal (get-sent-and-reset transport) "\x01"))
  ;; Test encoding of bool=false
  (thrift-protocol-write-bool protocol nil)
  (should (equal (get-sent-and-reset transport) "\x00")))

(ert-deftest thrift-binary-protocol-write-byte-test ()
  "Tests the encoding of Bytes using the thrift-binary-protocol."
  (thrift-protocol-write-byte protocol 0)
  (should (equal (get-sent-and-reset transport) "\x00"))
  (thrift-protocol-write-byte protocol -1)
  (should (equal (get-sent-and-reset transport) "\xff"))
  (thrift-protocol-write-byte protocol 1)
  (should (equal (get-sent-and-reset transport) "\x01")))

(ert-deftest thrift-binary-protocol-write-i16-test ()
  "Tests the encoding of i16 using the thrift-binary-protocol."
  (thrift-protocol-write-i16 protocol 0)
  (should (equal (get-sent-and-reset transport) "\x00\x00"))
  (thrift-protocol-write-i16 protocol 1)
  (should (equal (get-sent-and-reset transport) "\x00\x01"))
  (thrift-protocol-write-i16 protocol -1)
  (should (equal (get-sent-and-reset transport) "\xff\xff")))

(ert-deftest thrift-binary-protocol-write-i32-test ()
  "Tests the encoding of i32 using the thrift-binary-protocol."
  (thrift-protocol-write-i32 protocol 0)
  (should (equal (get-sent-and-reset transport) "\x00\x00\x00\x00"))
  (thrift-protocol-write-i32 protocol 1)
  (should (equal (get-sent-and-reset transport) "\x00\x00\x00\x01"))
  (thrift-protocol-write-i32 protocol -1)
  (should (equal (get-sent-and-reset transport) "\xff\xff\xff\xff")))

(ert-deftest thrift-binary-protocol-write-i64-test ()
  "Tests the encoding of i32 using the thrift-binary-protocol."
  (thrift-protocol-write-i64 protocol 0)
  (should (equal (get-sent-and-reset transport) "\x00\x00\x00\x00\x00\x00\x00\x00"))
  (thrift-protocol-write-i64 protocol 1)
  (should (equal (get-sent-and-reset transport) "\x00\x00\x00\x00\x00\x00\x00\x01"))
  (thrift-protocol-write-i64 protocol -1)
  (should (equal (get-sent-and-reset transport) "\xff\xff\xff\xff\xff\xff\xff\xff")))

(ert-deftest thrift-binary-protocol-write-double-test ()
  "todo"
  nil)

(ert-deftest thrift-binary-protocol-write-string-test ()
  "Tests the encoding of strings using the thrift-binary-protocol."
  (thrift-protocol-write-string protocol "a")
  (should (equal (get-sent-and-reset transport) (concat "\x00\x00\x00\x01" "a")))
  (thrift-protocol-write-string protocol "bb")
  (should (equal (get-sent-and-reset transport) (concat "\x00\x00\x00\x02" "bb"))))

(ert-deftest thrift-binary-protocol-read-byte-test ()
  "Tests the decoding of Byte using the thrift-binary-protocol."
  (set-recv transport "\x00")
  (should (equal (thrift-protocol-read-byte protocol) 0))
  (set-recv transport "\x01\x02")
  (should (equal (thrift-protocol-read-byte protocol) 1))
  (should (equal (thrift-protocol-read-byte protocol) 2))
  (set-recv transport "\xFF")
  (should (equal (thrift-protocol-read-byte protocol) 255))
  )

(ert-deftest thrift-binary-protocol-read-i16-test ()
  "Tests the decoding of I16 using the thrift-binary-protocol."
  (set-recv transport "\x00\x00")
  (should (equal (thrift-protocol-read-i16 protocol) 0))
  (set-recv transport "\x00\x01\x00\x02")
  (should (equal (thrift-protocol-read-i16 protocol) 1))
  (should (equal (thrift-protocol-read-i16 protocol) 2))
  (set-recv transport "\xFF\xFF")
  (should (equal (thrift-protocol-read-i16 protocol) 65535))
  )

(ert-deftest thrift-binary-protocol-read-i32-test ()
  "Tests the decoding of I32 using the thrift-binary-protocol."
  (set-recv transport "\x00\x00\x00\x00")
  (should (equal (thrift-protocol-read-i32 protocol) 0))
  (set-recv transport "\x00\x00\x00\x01\x00\x00\x00\x02")
  (should (equal (thrift-protocol-read-i32 protocol) 1))
  (should (equal (thrift-protocol-read-i32 protocol) 2))
  (set-recv transport "\x00\x00\xFF\xFF")
  (should (equal (thrift-protocol-read-i32 protocol) 65535))
  (set-recv transport "\x1F\xFF\xFF\xFF")
  (should (equal (thrift-protocol-read-i32 protocol) 536870911))
  )

(ert-deftest thrift-binary-protocol-read-i64-test ()
  "Tests the decoding of I64 using the thrift-binary-protocol."
  (set-recv transport "\x00\x00\x00\x00\x00\x00\x00\x00")
  (should (equal (thrift-protocol-read-i64 protocol) 0))
  (set-recv transport
	    "\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x02")
  (should (equal (thrift-protocol-read-i64 protocol) 1))
  (should (equal (thrift-protocol-read-i64 protocol) 2))
  (set-recv transport "\x00\x00\x00\x00\x00\x00\xFF\xFF")
  (should (equal (thrift-protocol-read-i64 protocol) 65535))
  (set-recv transport "\x00\x00\x00\x00\x1F\xFF\xFF\xFF")
  (should (equal (thrift-protocol-read-i64 protocol) 536870911))
  )

(ert-deftest thrift-binary-protocol-read-bool-test ()
  "Tests the decoding of Bool using the thrift-binary-protocol."
  (set-recv transport "\x00")
  (should (equal (thrift-protocol-read-bool protocol) nil))
  (set-recv transport "\x01")
  (should (equal (thrift-protocol-read-bool protocol) t)))

(ert-deftest thrift-binary-protocol-read-set-begin-test ()
  "Tests the decoding of read-set-begin using the thrift-binary-protocol."
  (set-recv transport "\x01\x00\x00\x00\x02")
  (should (equal (thrift-protocol-read-set-begin protocol) (list 1 2))))

(ert-deftest thrift-binary-protocol-read-list-begin-test ()
  "Tests the decoding of read-list-begin using the thrift-binary-protocol."
  (set-recv transport "\x01\x00\x00\x00\x02")
  (should (equal (thrift-protocol-read-list-begin protocol) (list 1 2))))

(ert-deftest thrift-binary-protocol-read-map-begin-test ()
  "Tests the decoding of read-map-begin using the thrift-binary-protocol."
  (set-recv transport "\x01\x02\x00\x00\x00\x03")
  (should (equal (thrift-protocol-read-map-begin protocol) (list 1 2 3))))

(ert-deftest thrift-binary-protocol-read-field-begin-test ()
  "Tests the decoding of read-field-begin using the thrift-binary-protocol."
  (set-recv transport "\x01\x00\x02")
  (should (equal (thrift-protocol-read-field-begin protocol) (list nil 1 2)))
  (set-recv transport "\x00\x00\x02")
  (should (equal (thrift-protocol-read-field-begin protocol) (list nil 0 0)))
  (set-recv transport "\x00\x00\x00")
  (should (equal (thrift-protocol-read-field-begin protocol) (list nil 0 0))))

(ert-deftest thrift-binary-protocol-read-string-test ()
  "Tests the decoding of read-string using the thrift-binary-protocol."
  (set-recv transport (concat "\x00\x00\x00\x01" "a"))
  (should (equal (thrift-protocol-read-string protocol) "a"))
  (set-recv transport (concat "\x00\x00\x00\x02" "bb"))
  (should (equal (thrift-protocol-read-string protocol) "bb")))

(ert-deftest thrift-binary-protocol-read-message-begin-test ()
  "Tests the decoding of read-map-begin using the thrift-binary-protocol."
  (set-recv transport (concat "\x80\x01\x00\x01"
			      "\x00\x00\x00\x04"
			      "name"
			      "\x00\x00\x00\x02"))
  (should (equal (thrift-protocol-read-message-begin protocol)
		 (list "name" 1 2)))
  (set-recv transport (concat "\x80\x01\x00" "!"
			      "\x00\x00\x00\x04"
			      "name"
			      "\x00\x00\x00" ","))
  (should (equal (thrift-protocol-read-message-begin protocol)
		 (list "name" 33 44)))
  (set-recv transport (concat "\x00\x00\x00\x04"
			      "name"
			      "\x01"
			      "\x00\x00\x00\x02"))
  (should (equal (thrift-protocol-read-message-begin protocol)
		 (list "name" 1 2)))
  (set-recv transport (concat "\x00\x00\x00\x04"
			      "name"
			      "!"
			      "\x00\x00\x00" ","))
  (should (equal (thrift-protocol-read-message-begin protocol)
		 (list "name" 33 44))))
