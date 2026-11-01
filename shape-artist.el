;;; shape-artist.el --- Rapid ASCII diagram prototyping -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Shape Artist Contributors

;; Author: Shape Artist Contributors
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, diagrams, art
;; URL: https://github.com/fkr-0/shape-artist
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shape Artist provides rapid ASCII diagram prototyping with
;; auto-discovered shape detection, fuzzy selection, and shape
;; manipulation.  It layers on top of `picture-mode' and integrates
;; with Evil when available.
;;
;; Entry points:
;;   `shape-artist-scratch'       -- open a new artist buffer
;;   `shape-artist-mode'          -- toggle artist mode in current buffer
;;   `shape-artist-edit-at-point' -- edit the diagram at point, in place, in any
;;                                   buffer (comments, Org, Markdown, text)
;;   `shape-artist-insert-diagram' -- start a new diagram below the current line
;;   `shape-artist-tutorial'      -- learn the basics interactively
;;
;; See the Info manual (doc/shape-artist.org) for full documentation.

;;; Code:

(require 'shape-artist-core)
(require 'shape-artist-metadata)
(require 'shape-artist-guides)
(require 'shape-artist-ui)
(require 'shape-artist-keys)
(require 'shape-artist-select)
(require 'shape-artist-shape-edit)
(require 'shape-artist-shapes)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-diagram-outline)
(require 'shape-artist-embed)
(require 'shape-artist-mouse)
(require 'shape-artist-help)
(require 'shape-artist-tutorial)
(require 'shape-artist-explain)
(require 'shape-artist-export)

;;;###autoload
(defun shape-artist-scratch ()
  "Create a new scratch buffer for shape-artist."
  (interactive)
  (let ((buf (generate-new-buffer "*shape-artist*")))
    (switch-to-buffer buf)
    (shape-artist-core-mode 1)
    (shape-artist-ui-show)
    ;; Last, so the one-time hint is not overwritten by other messages.
    (shape-artist-tutorial-maybe-hint)))

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
