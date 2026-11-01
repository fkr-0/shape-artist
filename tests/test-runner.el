;;; test-runner.el --- ERT test runner for shape-artist -*- lexical-binding: t; -*-

;; Usage: emacs --batch -l tests/test-runner.el

(setq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory
                                                 (or load-file-name
                                                     default-directory))))
(add-to-list 'load-path (file-name-directory
                          (or load-file-name default-directory)))

(require 'ert)
(require 'test-helper)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-core)

;; Load test files
(load (expand-file-name "test-shape-artist-diagram-parse.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-core.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-shapes.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-select.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-shape-edit.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-ui.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-keys.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-roundtrip.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-embed.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-mouse.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-help.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-tutorial.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-explain.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-export.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-compat.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-guides.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-metadata.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-persistence.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-diagram-outline.el"
                        (file-name-directory
                         (or load-file-name default-directory))))
(load (expand-file-name "test-shape-artist-e2e.el"
                        (file-name-directory
                         (or load-file-name default-directory))))

;; Run tests.  SHAPE_ARTIST_TEST_SELECTOR (a regexp over test names) narrows the
;; run, which is handy for timing or debugging one area under coverage.
(ert-run-tests-batch-and-exit
 (let ((selector (getenv "SHAPE_ARTIST_TEST_SELECTOR")))
   (if (and selector (not (string-empty-p selector))) selector t)))

;;; test-runner.el ends here
