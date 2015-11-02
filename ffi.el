;; -*- lexical-binding:t -*-

(require 'cl-macs)

(module-load "ffi-module.so")

(gv-define-simple-setter ffi--mem-ref ffi--mem-set t)

(defmacro define-ffi-library (symbol name)
  (let ((library (cl-gensym)))
    (set library nil)
    `(defun ,symbol ()
       (or ,library
	   (setq ,library (ffi--dlopen ,name))))))

(defmacro define-ffi-function (name c-name return-type arg-types library)
  (let* (
	 ;; Turn variable references into actual types; while keeping
	 ;; keywords the same.
	 (arg-types (mapcar #'symbol-value arg-types))
	 (arg-names (mapcar (lambda (_ignore) (cl-gensym)) arg-types))
	 (arg-types (vconcat arg-types))
	 (function (cl-gensym))
	 (cif (ffi--prep-cif (symbol-value return-type) arg-types)))
    (set function nil)
    `(defun ,name (,@arg-names)
       (unless ,function
	 (setq ,function (ffi--dlsym ,c-name (,library))))
       ;; FIXME do we even need a separate prep?
       (ffi--call ,cif ,function ,@arg-names))))

(defsubst ffi--align (offset align)
  (+ offset (mod (- align (mod offset align)) align)))

(defun ffi--lay-out-struct (types)
  (let ((offset 0))
    (mapcar (lambda (this-type)
	      (setf offset (ffi--align offset
				       (ffi--type-alignment this-type)))
	      (let ((here offset))
		(cl-incf offset (ffi--type-size this-type))
		here))
	    types)))

(defmacro define-ffi-struct (name &rest slots)
  "Like a limited form of `cl-defstruct', but works with foreign objects.

NAME must be a symbol.
Each SLOT must be of the form `(SLOT-NAME :type TYPE)', where
SLOT-NAME is a symbol and TYPE is an FFI type descriptor."
  (cl-assert (symbolp name))
  (let* ((docstring (if (stringp (car slots))
			(pop slots)))
	 (conc-name (concat (symbol-name name) "-"))
	 (result-forms ())
	 (field-types (mapcar (lambda (slot)
				(cl-assert (eq (cadr slot) :type))
				(symbol-value (cl-caddr slot)))
			      slots))
	 (struct-type (apply #'ffi--define-struct field-types))
	 (field-offsets (ffi--lay-out-struct field-types)))
    (push `(defvar ,name ,struct-type ,docstring)
	  result-forms)
    (cl-mapc
     (lambda (slot type offset)
       (let* ((getter-name (intern (concat conc-name
					   (symbol-name (car slot))))))
	 ;; One benefit of using defsubst here is that we don't have
	 ;; to provide a GV setter.
	 (push `(cl-defsubst ,getter-name (struct)
		  (ffi--mem-ref (ffi-pointer+ struct ,offset) ,type))
	       result-forms)))
     slots field-types field-offsets)
    (cons 'progn (nreverse result-forms))))

(provide 'ffi)
