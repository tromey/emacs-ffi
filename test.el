
(require 'ffi)

(define-ffi-library test.so "test")

(define-ffi-function test-function "test_function" :int nil test.so)

(ert-deftest ffi-basic ()
  (should (= (test-function) 27)))

(define-ffi-function test-c-string "test_c_string" :pointer nil test.so)

(ert-deftest ffi-pointer-type ()
  (should (eq (type-of (test-c-string)) 'user-ptr)))

(ert-deftest ffi-c-string ()
  (should (equal (ffi-get-c-string (test-c-string)) "hello")))

(defun callback (arg)
  (1+ arg))

(define-ffi-function test-call-callback "test_call_callback"
  :int [:pointer] test.so)

(ert-deftest ffi-call-callback ()
  (let* ((cif (ffi--prep-cif :int [:int]))
	 (pointer-to-callback (ffi-make-closure cif #'callback)))
    (should (eq (test-call-callback pointer-to-callback) 23))))

(define-ffi-function test-add "test_add" :int [:int :int] test.so)

(ert-deftest ffi-add ()
  (should (eq (test-add 23 -23) 0)))

(ert-deftest ffi-struct-layout ()
  (let ((struct-type (ffi--define-struct :int)))
    (should (eq (ffi--type-size struct-type)
		(ffi--type-size :int)))
    (should (eq (ffi--type-alignment struct-type)
		(ffi--type-alignment :int)))))
    
