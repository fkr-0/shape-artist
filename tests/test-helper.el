;;; test-helper.el --- Shared test utilities -*- lexical-binding: t; -*-

(require 'ert)

(defmacro test-artist-with-buffer (content &rest body)
  "Create a temp buffer with CONTENT and run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     ,@body))

(provide 'test-helper)
;;; test-helper.el ends here
