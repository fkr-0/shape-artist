;;; shape-artist-keys.el --- Keybindings for all modes -*- lexical-binding: t; -*-

;;; Commentary:

;;; This module defines keybindings for all artist modes (MOVE, DRAW,
;;; SELECT, SHAPE-EDIT).  It integrates with Evil states and provides
;;; context-sensitive keymaps.

;;; Code:

(require 'shape-artist-core)
(require 'shape-artist-ui)
(require 'shape-artist-select)
(require 'shape-artist-shape-edit)
(require 'shape-artist-shapes)

;;; --- MOVE Mode Keybindings (Evil normal + picture-mode) ---

(defvar shape-artist-move-map (make-sparse-keymap)
  "Keymap for MOVE mode.")

(define-key shape-artist-move-map (kbd "h") #'shape-artist-move-left)
(define-key shape-artist-move-map (kbd "j") #'shape-artist-move-down)
(define-key shape-artist-move-map (kbd "k") #'shape-artist-move-up)
(define-key shape-artist-move-map (kbd "l") #'shape-artist-move-right)
(define-key shape-artist-move-map (kbd "SPC") #'shape-artist-enter-draw-and-anchor)
(define-key shape-artist-move-map (kbd "RET") #'shape-artist-commit-preview)
(define-key shape-artist-move-map (kbd "d") #'shape-artist-enter-draw)
(define-key shape-artist-move-map (kbd "q") #'shape-artist-quit)
(define-key shape-artist-move-map (kbd "a") #'shape-artist-ui-show)

;;; --- DRAW Mode Keybindings (Evil artist) ---

(defvar shape-artist-draw-map (make-sparse-keymap)
  "Keymap for DRAW mode.")

(define-key shape-artist-draw-map (kbd "h") #'shape-artist-draw-move-left)
(define-key shape-artist-draw-map (kbd "j") #'shape-artist-draw-move-down)
(define-key shape-artist-draw-map (kbd "k") #'shape-artist-draw-move-up)
(define-key shape-artist-draw-map (kbd "l") #'shape-artist-draw-move-right)
(define-key shape-artist-draw-map (kbd "SPC") #'shape-artist-set-anchor-here)
(define-key shape-artist-draw-map (kbd "RET") #'shape-artist-commit-and-exit)
(define-key shape-artist-draw-map (kbd "<escape>") #'shape-artist-cancel-and-exit)
(define-key shape-artist-draw-map (kbd "p") #'shape-artist-pick-tool)

;;; --- SELECT Mode Keybindings (Evil select) ---

(defvar shape-artist-select-map (make-sparse-keymap)
  "Keymap for SELECT mode.")

(define-key shape-artist-select-map (kbd "n") #'shape-artist-select-next)
(define-key shape-artist-select-map (kbd "p") #'shape-artist-select-prev)
(define-key shape-artist-select-map (kbd "g") #'shape-artist-shapes-parse)
(define-key shape-artist-select-map (kbd "RET") #'shape-artist-select-confirm)
(define-key shape-artist-select-map (kbd "<escape>") #'shape-artist-select-cancel)

;;; --- SHAPE-EDIT Mode Keybindings (Evil shape-edit) ---

(defvar shape-artist-shape-edit-map (make-sparse-keymap)
  "Keymap for SHAPE-EDIT mode.")

(define-key shape-artist-shape-edit-map (kbd "e") #'shape-artist-shape-edit-label)
(define-key shape-artist-shape-edit-map (kbd "m") #'shape-artist-shape-move)
(define-key shape-artist-shape-edit-map (kbd "r") #'shape-artist-shape-resize)
(define-key shape-artist-shape-edit-map (kbd "x") #'shape-artist-shape-replace)
(define-key shape-artist-shape-edit-map (kbd "h") #'shape-artist-shape-edit-left)
(define-key shape-artist-shape-edit-map (kbd "j") #'shape-artist-shape-edit-down)
(define-key shape-artist-shape-edit-map (kbd "k") #'shape-artist-shape-edit-up)
(define-key shape-artist-shape-edit-map (kbd "l") #'shape-artist-shape-edit-right)
(define-key shape-artist-shape-edit-map (kbd "<escape>") #'shape-artist-shape-edit-exit)

;;; --- Evil State Integration ---

(defun shape-artist--setup-evil-keymaps ()
  "Setup Evil state keymaps for artist modes."
  (when (fboundp 'evil-define-key*)
    ;; MOVE mode: override normal state in artist buffers
    (evil-define-key* 'normal shape-artist-core-mode-map
      "h" #'shape-artist-move-left
      "j" #'shape-artist-move-down
      "k" #'shape-artist-move-up
      "l" #'shape-artist-move-right
      (kbd "SPC") #'shape-artist-enter-draw-and-anchor
      (kbd "RET") #'shape-artist-commit-preview
      "d" #'shape-artist-enter-draw
      (kbd "C-n") #'shape-artist-select-start
      "u" #'shape-artist-ui-toggle
      "q" #'shape-artist-quit
      "a" #'shape-artist-ui-show)

    ;; DRAW mode
    (evil-define-key* 'artist shape-artist-core-mode-map
      "h" #'shape-artist-draw-move-left
      "j" #'shape-artist-draw-move-down
      "k" #'shape-artist-draw-move-up
      "l" #'shape-artist-draw-move-right
      (kbd "SPC") #'shape-artist-set-anchor-here
      (kbd "RET") #'shape-artist-commit-and-exit
      (kbd "C-RET") #'shape-artist-commit-stay
      (kbd "C-<return>") #'shape-artist-commit-stay
      (kbd "<escape>") #'shape-artist-cancel-and-exit
      "p" #'shape-artist-pick-tool
      "u" #'shape-artist-undo)

    ;; SELECT mode
    (evil-define-key* 'select shape-artist-core-mode-map
      "n" #'shape-artist-select-next
      "p" #'shape-artist-select-prev
      "g" #'shape-artist-shapes-parse
      (kbd "RET") #'shape-artist-select-confirm
      (kbd "<escape>") #'shape-artist-select-cancel
      "u" #'shape-artist-ui-toggle)

    ;; SHAPE-EDIT mode
    (evil-define-key* 'shape-edit shape-artist-core-mode-map
      "e" #'shape-artist-shape-edit-label
      "m" #'shape-artist-shape-move
      "r" #'shape-artist-shape-resize
      "x" #'shape-artist-shape-replace
      "h" #'shape-artist-shape-edit-left
      "j" #'shape-artist-shape-edit-down
      "k" #'shape-artist-shape-edit-up
      "l" #'shape-artist-shape-edit-right
      (kbd "<escape>") #'shape-artist-shape-edit-exit
      "u" #'shape-artist-ui-toggle)))

;; Setup keymaps immediately when Evil is already available.
(when (featurep 'evil)
  (shape-artist--setup-evil-keymaps))

;;; --- Provide ---

(provide 'shape-artist-keys)
;;; shape-artist-keys.el ends here
