;; -*- lexical-binding:t -*-

(require 'cl-macs)

(module-load "ffi-module.so")

(gv-define-simple-setter ffi--mem-ref ffi--mem-set)

(defmacro define-ffi-library (symbol name)
  (let ((library (cl-gensym)))
    (set library nil)
    `(defun ,symbol ()
       (or ,library
	   (setq ,library (ffi--dlopen ,name))))))

(defmacro define-ffi-function (name c-name return-type arg-types library)
  (let* ((arg-names (mapcar #'cl-gensym arg-types))
	 (arg-types (vconcat arg-types))
	 (function (cl-gensym))
	 (cif (ffi--prep-cif return-type arg-types)))
    (set function nil)
    `(defun ,name (,@arg-names)
       (unless ,function
	 (setq ,function (ffi--dlsym ,c-name (,library))))
       ;; FIXME do we even need a separate prep?
       (ffi--call ,cif ,function ,@arg-names))))

(provide 'ffi)
