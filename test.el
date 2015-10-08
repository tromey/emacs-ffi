
(require 'ffi)

(define-ffi-library test.so "test.so")

(define-ffi-function test-function "test_function" :int nil test.so)

(ert-deftest ffi-basic ()
  (should (= (test-function) 27)))
