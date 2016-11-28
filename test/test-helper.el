;;; test-helper --- Test helper for ffi

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar ffi-test-path
  (f-dirname (f-this-file)))

(defvar ffi-root-path
  (f-parent ffi-test-path))

(defvar ffi-sandbox-path
  (f-expand "sandbox" ffi-test-path))

(when (f-exists? ffi-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" ffi-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory ffi-sandbox-path))
     (when (f-exists? ffi-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir ffi-sandbox-path)
     ,@body
     (f-delete default-directory :force)))


(require 'ert)
(require 'el-mock)
(eval-when-compile
  (require 'cl))
(require 'undercover)
(undercover "*.el"
            (:exclude "*-test.el")
            (:report-file "/tmp/undercover-report.json"))
(require 'ffi)

(provide 'test-helper)
;;; test-helper.el ends here
