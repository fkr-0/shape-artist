;;; shape-artist.el --- Rapid ASCII diagram prototyping -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Shape Artist Contributors

;; Author: Shape Artist Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, diagrams, art
;; URL: https://github.com/example/shape-artist
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shape Artist provides rapid ASCII diagram prototyping with
;; auto-discovered shape detection, fuzzy selection, and shape
;; manipulation.  It layers on top of `picture-mode' and integrates
;; with Evil when available.
;;
;; Entry points:
;;   `shape-artist-scratch'  -- open a new artist buffer
;;   `shape-artist-mode'     -- toggle artist mode in current buffer
;;
;; See the Info manual (doc/shape-artist.org) for full documentation.

;;; Code:

(require 'shape-artist-core)
(require 'shape-artist-guides)
(require 'shape-artist-ui)
(require 'shape-artist-keys)
(require 'shape-artist-select)
(require 'shape-artist-shape-edit)
(require 'shape-artist-shapes)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-diagram-outline)

;;;###autoload
(defun shape-artist-scratch ()
  "Create a new scratch buffer for shape-artist."
  (interactive)
  (let ((buf (generate-new-buffer "*shape-artist*")))
    (switch-to-buffer buf)
    (shape-artist-core-mode 1)
    (shape-artist-ui-show)))

;;;###autoload
(defun shape-artist-mode (&optional arg)
  "Toggle shape-artist mode.  With ARG, enable if positive."
  (interactive (list (or current-prefix-arg 'toggle)))
  (if (eq arg 'toggle)
      (if shape-artist-core-mode
          (shape-artist-core-mode -1)
        (shape-artist-core-mode 1))
    (shape-artist-core-mode (if (and arg (> (prefix-numeric-value arg) 0)) 1 -1))))

;;; --- Provide ---

(provide 'shape-artist)
;;; shape-artist.el ends here
