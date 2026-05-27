;;; test-runner.el --- ERT test runner for shape-artist -*- lexical-binding: t; -*-

;; Usage: emacs --batch -l tests/test-runner.el

(add-to-list 'load-path (expand-file-name ".." (file-name-directory
                                                 (or load-file-name
                                                     default-directory))))
(add-to-list 'load-path (file-name-directory
                          (or load-file-name default-directory)))

(require 'ert)
(require 'test-helper)
(require 'artist-diagram-parse)
(require 'shape-artist-core)

;; Load test files
(load (expand-file-name "test-artist-diagram-parse.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-core.el"
                        (file-name-directory
                         (or load-file-name default-directory))))

;; Run tests
(ert-run-tests-batch-and-exit)

;;; test-runner.el ends here
