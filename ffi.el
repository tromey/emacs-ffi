;; -*- lexical-binding:t -*-

(require 'cl-macs)

(module-load "ffi-module.so")

(defmacro define-ffi-library (symbol name)
  (let ((library (cl-gensym)))
    (set library nil)
    `(defun ,symbol ()
       (or ,library
	   (setq ,library (ffi--dlopen ,name))))))

(defun ffi--make-finalized-cif (return-type arg-types)
  (let ((cif (ffi--prep-cif return-type arg-types)))
    (cons cif (make-finalizer (lambda () (ffi--free cif))))))

(defmacro define-ffi-function (name c-name return-type arg-types library)
  (let* ((arg-names (mapcar #'cl-gensym arg-types))
	 (arg-types (vconcat arg-types))
	 (function (cl-gensym))
	 (cif (ffi--make-finalized-cif return-type arg-types)))
    (set function nil)
    `(defun ,name (,@arg-names)
       (unless ,function
	 (setq ,function (ffi--dlsym ,c-name (,library))))
       ;; FIXME change the convention of ffi--prep-cif
       ;; or do we even need a separate prep?
       (ffi--call (car ',cif) ,function ,return-type ,arg-types
		  ,@arg-names))))

(provide 'ffi)
