
(require 'ffi)

(define-ffi-library test.so "test.so")

(define-ffi-function test-function "test_function" :int nil test.so)

(ert-deftest ffi-basic ()
  (should (= (test-function) 27)))

(define-ffi-function test-c-string "test_c_string" :pointer nil test.so)

(ert-deftest ffi-pointer-type ()
  (should (eq (type-of (test-c-string)) 'user-ptr)))

(ert-deftest ffi-c-string ()
  (should (equal (ffi-get-c-string (test-c-string)) "hello")))
