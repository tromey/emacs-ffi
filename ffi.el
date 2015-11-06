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

(defun ffi--struct-union-helper (name slots definer-function layout-function)
  (cl-assert (symbolp name))
  (let* ((docstring (if (stringp (car slots))
			(pop slots)))
	 (conc-name (concat (symbol-name name) "-"))
	 (result-forms ())
	 (field-types (mapcar (lambda (slot)
				(cl-assert (eq (cadr slot) :type))
				(symbol-value (cl-caddr slot)))
			      slots))
	 (the-type (apply definer-function field-types))
	 (field-offsets (funcall layout-function field-types)))
    (push `(defvar ,name ,the-type ,docstring)
	  result-forms)
    (cl-mapc
     (lambda (slot type offset)
       (let ((getter-name (intern (concat conc-name
					  (symbol-name (car slot)))))
	     (offsetter (if (> offset 0)
			    `(ffi-pointer+ object ,offset)
			  'object)))
	 ;; One benefit of using defsubst here is that we don't have
	 ;; to provide a GV setter.
	 (push `(cl-defsubst ,getter-name (object)
		  (ffi--mem-ref ,offsetter ,type))
	       result-forms)))
     slots field-types field-offsets)
    (cons 'progn (nreverse result-forms))))

(defmacro define-ffi-struct (name &rest slots)
  "Like a limited form of `cl-defstruct', but works with foreign objects.

NAME must be a symbol.
Each SLOT must be of the form `(SLOT-NAME :type TYPE)', where
SLOT-NAME is a symbol and TYPE is an FFI type descriptor."
  (ffi--struct-union-helper name slots #'ffi--define-struct
			    #'ffi--lay-out-struct))

(defmacro define-ffi-union (name &rest slots)
  "Like a limited form of `cl-defstruct', but works with foreign objects.

NAME must be a symbol.
Each SLOT must be of the form `(SLOT-NAME :type TYPE)', where
SLOT-NAME is a symbol and TYPE is an FFI type descriptor."
  (ffi--struct-union-helper name slots #'ffi--define-union
			    (lambda (types)
			      (make-list (length types) 0))))

(defmacro define-ffi-array (name type length &optional docstring)
  ;; This is a hack until libffi gives us direct support.
  (let ((type-description
	 (apply #'ffi--define-struct (make-list (eval length)
						(symbol-name type)))))
    `(defvar ,name ,type-description ,docstring)))

(defsubst ffi-aref (array type index)
  (ffi--mem-ref (ffi-pointer+ array (* index (ffi--type-size type))) type))

(defmacro with-ffi-temporary (binding &rest body)
  (declare (indent defun))
  `(let ((,(car binding) (ffi-allocate ,@(cdr binding))))
     (unwind-protect
	 ,@body
       (ffi-free ,(car binding)))))

(defmacro with-ffi-string (binding &rest body)
  (declare (indent defun))
  `(let ((,(car binding) (ffi-make-c-string ,@(cdr binding))))
     (unwind-protect
	 ,@body
       (ffi-free ,(car binding)))))

(provide 'ffi)
