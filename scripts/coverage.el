;;; coverage.el --- Batch coverage driver for Shape Artist -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Shape Artist Contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Instrument the package sources with `undercover', run the ERT suite, and
;; write an lcov report.  Normally invoked through scripts/coverage.sh, which
;; provides the environment (Eldev supplies `undercover' and its dependencies).
;;
;; Environment:
;;   SHAPE_ARTIST_COVERAGE_REPORT  absolute path of the lcov file to write.

;;; Code:

;; scripts/coverage.sh runs this from a scratch copy that contains only .el
;; sources: byte-compiled files are never instrumented, so a stale .elc next to a
;; source would silently hide it from the report.
(setq load-prefer-newer t)

(require 'undercover)

(let ((report (or (getenv "SHAPE_ARTIST_COVERAGE_REPORT")
                  (expand-file-name "coverage/lcov.info"))))
  (make-directory (file-name-directory report) t)
  (undercover "shape-artist*.el" "artist-diagram-*.el"
              (:report-format 'lcov)
              (:report-file report)
              (:send-report nil)))

(load (expand-file-name "tests/test-runner.el") nil t)

;;; coverage.el ends here
