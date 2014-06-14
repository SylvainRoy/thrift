;;; unittest.el ---Unit test Thrift elisp implementation.

;; To run the test in batch mode:
;;  emacs -batch -l ert -l unittest.el -f ert-run-tests-batch-and-exit

;; To run the test from emacs:
;;  M-x ert RET t RET

;; Author: Sylvain Roy <sylvain.roy@m4x.org>

(require 'ert)

(add-to-list 'load-path "./protocol")
(load "TBinaryProtocol.el")

(ert-deftest TBinaryProtocol-writeBool-test ()
  "Tests the encoding/decoding of Bool using the TBinaryProtocol."
  (should (equal (writeBool "" t) "\x01"))
  (should (equal (writeBool "" nil) "\x00"))
  )

(ert-deftest TBinaryProtocol-writeByte-test ()
  "Tests the encoding/decoding of Bytes using the TBinaryProtocol."
  (should (equal (writeByte "" 0) "\x00"))
  (should (equal (writeByte "" 1) "\x01"))
  (should (equal (writeByte "" -1) "\xff"))
  )

(ert-deftest TBinaryProtocol-writei16-test ()
  "Tests the encoding/decoding of i16 using the TBinaryProtocol."
  (should (equal (writeI16 "" 0) "\x00\x00"))
  (should (equal (writeI16 "" 1) "\x00\x01"))
  (should (equal (writeI16 "" -1) "\xff\xff"))
  )

(ert-deftest TBinaryProtocol-writei32-test ()
  "Tests the encoding/decoding of i32 using the TBinaryProtocol."
  (should (equal (writeI32 "" 0) "\x00\x00\x00\x00"))
  (should (equal (writeI32 "" 1) "\x00\x00\x00\x01"))
  (should (equal (writeI32 "" -1) "\xff\xff\xff\xff"))
  )

(ert-deftest TBinaryProtocol-writei64-test ()
  "Tests the encoding/decoding of i32 using the TBinaryProtocol."
  (should (equal (writeI64 "" 0) "\x00\x00\x00\x00\x00\x00\x00\x00"))
  (should (equal (writeI64 "" 1) "\x00\x00\x00\x00\x00\x00\x00\x01"))
  (should (equal (writeI64 "" -1) "\xff\xff\xff\xff\xff\xff\xff\xff"))
  )

(ert-deftest TBinaryProtocol-string-test ()
  "Tests the encoding/decoding of strings using the TBinaryProtocol."
  (should (equal (writeString "" "a") (concat "\x00\x00\x00\x01" "a")))
  (should (equal (writeString "" "ab") (concat "\x00\x00\x00\x02" "ab")))
  )
