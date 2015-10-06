;; -*- lexical-binding:t -*-

(require 'cl-macs)
(require 'libffi)

(module-load "ffi-module.so")

(defmacro define-ffi-library (symbol name)
  (let ((library nil))
    `(defun symbol ()
       (unless library
	 (setf library (ffi--dlopen name)))
       library)))

(defun ffi--make-finalized-cif (return-type arg-types)
  (let ((cif (ffi--prep-cif return-type arg-types)))
    (cons cif (make-finalizer (lambda () (ffi--free cif))))))

(defmacro define-ffi-function (name c-name return-type arg-types library)
  (let* ((arg-names (mapcar #'cl-gensym arg-types))
	 (arg-types (vconcat arg-types))
	 (function nil)
	 (cif (ffi--make-finalized-cif return-type arg-types)))
    `(defun name (,@arg-names)
       (unless function
	 (setf function (ffi--dlsym ,c-name (,library))))
       ;; FIXME change the convention of ffi--prep-cif
       ;; or do we even need a separate prep?
       (apply #'ffi--call (car ,cif) function return-type arg-types
	      ,@arg-names))))
