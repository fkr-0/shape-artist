;;; shape-artist-keys.el --- Keybindings for all modes -*- lexical-binding: t; -*-

;;; Commentary:

;;; This module defines keybindings for all Shape Artist modes.  It provides a
;;; non-Evil modal dispatch keymap and optional Evil state integration.

;;; Code:

(require 'shape-artist-core)
(require 'shape-artist-ui)
(require 'shape-artist-select)
(require 'shape-artist-shape-edit)
(require 'shape-artist-shapes)

;;; --- Descriptive Mode Keymaps ---

(defvar shape-artist-move-map (make-sparse-keymap)
  "Keymap for MOVE mode documentation and embedding.")

(define-key shape-artist-move-map (kbd "h") #'shape-artist-move-left)
(define-key shape-artist-move-map (kbd "j") #'shape-artist-move-down)
(define-key shape-artist-move-map (kbd "k") #'shape-artist-move-up)
(define-key shape-artist-move-map (kbd "l") #'shape-artist-move-right)
(define-key shape-artist-move-map (kbd "SPC") #'shape-artist-enter-draw-and-anchor)
(define-key shape-artist-move-map (kbd "RET") #'shape-artist-commit-preview)
(define-key shape-artist-move-map (kbd "d") #'shape-artist-enter-draw)
(define-key shape-artist-move-map (kbd "q") #'shape-artist-quit)
(define-key shape-artist-move-map (kbd "a") #'shape-artist-ui-show)

(defvar shape-artist-draw-map (make-sparse-keymap)
  "Keymap for DRAW mode documentation and embedding.")

(define-key shape-artist-draw-map (kbd "h") #'shape-artist-draw-move-left)
(define-key shape-artist-draw-map (kbd "j") #'shape-artist-draw-move-down)
(define-key shape-artist-draw-map (kbd "k") #'shape-artist-draw-move-up)
(define-key shape-artist-draw-map (kbd "l") #'shape-artist-draw-move-right)
(define-key shape-artist-draw-map (kbd "SPC") #'shape-artist-set-anchor-here)
(define-key shape-artist-draw-map (kbd "RET") #'shape-artist-commit-and-exit)
(define-key shape-artist-draw-map (kbd "<escape>") #'shape-artist-cancel-and-exit)
(define-key shape-artist-draw-map (kbd "p") #'shape-artist-pick-tool)

(defvar shape-artist-select-map (make-sparse-keymap)
  "Keymap for SELECT mode documentation and embedding.")

(define-key shape-artist-select-map (kbd "n") #'shape-artist-select-next)
(define-key shape-artist-select-map (kbd "p") #'shape-artist-select-prev)
(define-key shape-artist-select-map (kbd "g") #'shape-artist-shapes-parse)
(define-key shape-artist-select-map (kbd "t") #'shape-artist-select-toggle-current)
(define-key shape-artist-select-map (kbd "T") #'shape-artist-select-all)
(define-key shape-artist-select-map (kbd "c") #'shape-artist-select-clear-selected)
(define-key shape-artist-select-map (kbd "RET") #'shape-artist-select-confirm)
(define-key shape-artist-select-map (kbd "<escape>") #'shape-artist-select-cancel)

(defvar shape-artist-shape-edit-map (make-sparse-keymap)
  "Keymap for SHAPE-EDIT mode documentation and embedding.")

(define-key shape-artist-shape-edit-map (kbd "e") #'shape-artist-shape-edit-label)
(define-key shape-artist-shape-edit-map (kbd "m") #'shape-artist-shape-move)
(define-key shape-artist-shape-edit-map (kbd "r") #'shape-artist-shape-resize)
(define-key shape-artist-shape-edit-map (kbd "x") #'shape-artist-shape-cycle-style)
(define-key shape-artist-shape-edit-map (kbd "s") #'shape-artist-shape-set-style)
(define-key shape-artist-shape-edit-map (kbd "h") #'shape-artist-shape-edit-left)
(define-key shape-artist-shape-edit-map (kbd "j") #'shape-artist-shape-edit-down)
(define-key shape-artist-shape-edit-map (kbd "k") #'shape-artist-shape-edit-up)
(define-key shape-artist-shape-edit-map (kbd "l") #'shape-artist-shape-edit-right)
(define-key shape-artist-shape-edit-map (kbd "H") #'shape-artist-shape-move-left)
(define-key shape-artist-shape-edit-map (kbd "J") #'shape-artist-shape-move-down)
(define-key shape-artist-shape-edit-map (kbd "K") #'shape-artist-shape-move-up)
(define-key shape-artist-shape-edit-map (kbd "L") #'shape-artist-shape-move-right)
(define-key shape-artist-shape-edit-map (kbd ">") #'shape-artist-shape-resize-wider)
(define-key shape-artist-shape-edit-map (kbd "<") #'shape-artist-shape-resize-narrower)
(define-key shape-artist-shape-edit-map (kbd "+") #'shape-artist-shape-resize-taller)
(define-key shape-artist-shape-edit-map (kbd "_") #'shape-artist-shape-resize-shorter)
(define-key shape-artist-shape-edit-map (kbd "[") #'shape-artist-shape-resize-left)
(define-key shape-artist-shape-edit-map (kbd "]") #'shape-artist-shape-shrink-left)
(define-key shape-artist-shape-edit-map (kbd "{") #'shape-artist-shape-resize-up)
(define-key shape-artist-shape-edit-map (kbd "}") #'shape-artist-shape-shrink-up)
(define-key shape-artist-shape-edit-map (kbd "<escape>") #'shape-artist-shape-edit-exit)

;;; --- Modal Dispatch Helpers ---

(defun shape-artist--dispatch-left ()
  "Dispatch left key according to current Shape Artist mode."
  (interactive)
  (pcase shape-artist--mode
    ('draw (shape-artist-draw-move-left))
    ('shape-edit (shape-artist-shape-edit-left))
    (_ (shape-artist-move-left))))

(defun shape-artist--dispatch-down ()
  "Dispatch down key according to current Shape Artist mode."
  (interactive)
  (pcase shape-artist--mode
    ('draw (shape-artist-draw-move-down))
    ('shape-edit (shape-artist-shape-edit-down))
    (_ (shape-artist-move-down))))

(defun shape-artist--dispatch-up ()
  "Dispatch up key according to current Shape Artist mode."
  (interactive)
  (pcase shape-artist--mode
    ('draw (shape-artist-draw-move-up))
    ('shape-edit (shape-artist-shape-edit-up))
    (_ (shape-artist-move-up))))

(defun shape-artist--dispatch-right ()
  "Dispatch right key according to current Shape Artist mode."
  (interactive)
  (pcase shape-artist--mode
    ('draw (shape-artist-draw-move-right))
    ('shape-edit (shape-artist-shape-edit-right))
    (_ (shape-artist-move-right))))

(defun shape-artist--dispatch-space ()
  "Dispatch space according to current Shape Artist mode."
  (interactive)
  (pcase shape-artist--mode
    ('draw (shape-artist-set-anchor-here))
    (_ (shape-artist-enter-draw-and-anchor))))

(defun shape-artist--dispatch-return ()
  "Dispatch return according to current Shape Artist mode."
  (interactive)
  (pcase shape-artist--mode
    ('draw (shape-artist-commit-and-exit))
    ('select (shape-artist-select-confirm))
    (_ (shape-artist-commit-preview))))

(defun shape-artist--dispatch-escape ()
  "Dispatch escape according to current Shape Artist mode."
  (interactive)
  (pcase shape-artist--mode
    ('draw (shape-artist-cancel-and-exit))
    ('select (shape-artist-select-cancel))
    ('shape-edit (shape-artist-shape-edit-exit))))

(defun shape-artist--dispatch-utility ()
  "Dispatch utility key according to current Shape Artist mode."
  (interactive)
  (if (eq shape-artist--mode 'draw)
      (shape-artist-undo)
    (shape-artist-ui-toggle)))

(defun shape-artist--dispatch-select-next ()
  "Advance selection when in SELECT mode."
  (interactive)
  (when (eq shape-artist--mode 'select)
    (shape-artist-select-next)))

(defun shape-artist--dispatch-select-prev ()
  "Go to previous selection when in SELECT mode."
  (interactive)
  (when (eq shape-artist--mode 'select)
    (shape-artist-select-prev)))

(defun shape-artist--dispatch-select-refresh ()
  "Refresh parsed shapes when in SELECT mode."
  (interactive)
  (when (eq shape-artist--mode 'select)
    (shape-artist-shapes-parse)))

(defun shape-artist--dispatch-shape-edit-label ()
  "Edit label when a shape is locked."
  (interactive)
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-edit-label)))

(defun shape-artist--dispatch-shape-move ()
  "Prompt-move locked shape when in SHAPE-EDIT mode."
  (interactive)
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-move)))

(defun shape-artist--dispatch-shape-resize ()
  "Prompt-resize locked shape when in SHAPE-EDIT mode."
  (interactive)
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-resize)))

(defun shape-artist--dispatch-shape-cycle-style ()
  "Cycle locked shape style when in SHAPE-EDIT mode."
  (interactive)
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-cycle-style)))

(defun shape-artist--dispatch-shape-set-style ()
  "Prompt for locked shape style when in SHAPE-EDIT mode."
  (interactive)
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-set-style)))

(defun shape-artist--dispatch-shape-move-left (&optional count)
  "Move locked shape left by COUNT cells when in SHAPE-EDIT mode."
  (interactive "p")
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-move-left count)))

(defun shape-artist--dispatch-shape-move-down (&optional count)
  "Move locked shape down by COUNT cells when in SHAPE-EDIT mode."
  (interactive "p")
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-move-down count)))

(defun shape-artist--dispatch-shape-move-up (&optional count)
  "Move locked shape up by COUNT cells when in SHAPE-EDIT mode."
  (interactive "p")
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-move-up count)))

(defun shape-artist--dispatch-shape-move-right (&optional count)
  "Move locked shape right by COUNT cells when in SHAPE-EDIT mode."
  (interactive "p")
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-move-right count)))

(defun shape-artist--dispatch-shape-resize-wider (&optional count)
  "Widen locked shape by COUNT cells when in SHAPE-EDIT mode."
  (interactive "p")
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-resize-wider count)))

(defun shape-artist--dispatch-shape-resize-narrower (&optional count)
  "Narrow locked shape by COUNT cells when in SHAPE-EDIT mode."
  (interactive "p")
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-resize-narrower count)))

(defun shape-artist--dispatch-shape-resize-taller (&optional count)
  "Make locked shape taller by COUNT cells when in SHAPE-EDIT mode."
  (interactive "p")
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-resize-taller count)))

(defun shape-artist--dispatch-shape-resize-shorter (&optional count)
  "Make locked shape shorter by COUNT cells when in SHAPE-EDIT mode."
  (interactive "p")
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-resize-shorter count)))


(defun shape-artist--dispatch-select-toggle ()
  "Toggle current selection candidate when in SELECT mode."
  (interactive)
  (when (eq shape-artist--mode 'select)
    (shape-artist-select-toggle-current)))

(defun shape-artist--dispatch-select-all ()
  "Select all candidates when in SELECT mode."
  (interactive)
  (when (eq shape-artist--mode 'select)
    (shape-artist-select-all)))

(defun shape-artist--dispatch-select-clear ()
  "Clear selected candidates when in SELECT mode."
  (interactive)
  (when (eq shape-artist--mode 'select)
    (shape-artist-select-clear-selected)))

(defun shape-artist--dispatch-shape-resize-left (&optional count)
  "Grow locked rectangle left by COUNT cells when in SHAPE-EDIT mode."
  (interactive "p")
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-resize-left count)))

(defun shape-artist--dispatch-shape-shrink-left (&optional count)
  "Shrink locked rectangle from the left by COUNT cells."
  (interactive "p")
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-shrink-left count)))

(defun shape-artist--dispatch-shape-resize-up (&optional count)
  "Grow locked rectangle upward by COUNT cells."
  (interactive "p")
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-resize-up count)))

(defun shape-artist--dispatch-shape-shrink-up (&optional count)
  "Shrink locked rectangle from top by COUNT cells."
  (interactive "p")
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-shrink-up count)))

(defun shape-artist--install-modal-dispatch-keymap ()
  "Install non-Evil modal dispatch bindings on `shape-artist-core-mode-map'."
  (when (boundp 'shape-artist-core-mode-map)
    (set-keymap-parent shape-artist-core-mode-map nil)
    (define-key shape-artist-core-mode-map (kbd "h") #'shape-artist--dispatch-left)
    (define-key shape-artist-core-mode-map (kbd "j") #'shape-artist--dispatch-down)
    (define-key shape-artist-core-mode-map (kbd "k") #'shape-artist--dispatch-up)
    (define-key shape-artist-core-mode-map (kbd "l") #'shape-artist--dispatch-right)
    (define-key shape-artist-core-mode-map (kbd "SPC") #'shape-artist--dispatch-space)
    (define-key shape-artist-core-mode-map (kbd "RET") #'shape-artist--dispatch-return)
    (define-key shape-artist-core-mode-map (kbd "<escape>") #'shape-artist--dispatch-escape)
    (define-key shape-artist-core-mode-map (kbd "d") #'shape-artist-enter-draw)
    (define-key shape-artist-core-mode-map (kbd "C-n") #'shape-artist--dispatch-select-start)
    (define-key shape-artist-core-mode-map (kbd "u") #'shape-artist--dispatch-utility)
    (define-key shape-artist-core-mode-map (kbd "q") #'shape-artist-quit)
    (define-key shape-artist-core-mode-map (kbd "a") #'shape-artist-ui-show)
    (define-key shape-artist-core-mode-map (kbd "C-RET") #'shape-artist--dispatch-commit-stay)
    (define-key shape-artist-core-mode-map (kbd "C-<return>") #'shape-artist--dispatch-commit-stay)
    (define-key shape-artist-core-mode-map (kbd "n") #'shape-artist--dispatch-select-next)
    (define-key shape-artist-core-mode-map (kbd "p") #'shape-artist--dispatch-select-prev)
    (define-key shape-artist-core-mode-map (kbd "g") #'shape-artist--dispatch-select-refresh)
    (define-key shape-artist-core-mode-map (kbd "t") #'shape-artist--dispatch-select-toggle)
    (define-key shape-artist-core-mode-map (kbd "T") #'shape-artist--dispatch-select-all)
    (define-key shape-artist-core-mode-map (kbd "c") #'shape-artist--dispatch-select-clear)
    (define-key shape-artist-core-mode-map (kbd "e") #'shape-artist--dispatch-shape-edit-label)
    (define-key shape-artist-core-mode-map (kbd "m") #'shape-artist--dispatch-shape-move)
    (define-key shape-artist-core-mode-map (kbd "r") #'shape-artist--dispatch-shape-resize)
    (define-key shape-artist-core-mode-map (kbd "x") #'shape-artist--dispatch-shape-cycle-style)
    (define-key shape-artist-core-mode-map (kbd "s") #'shape-artist--dispatch-shape-set-style)
    (define-key shape-artist-core-mode-map (kbd "H") #'shape-artist--dispatch-shape-move-left)
    (define-key shape-artist-core-mode-map (kbd "J") #'shape-artist--dispatch-shape-move-down)
    (define-key shape-artist-core-mode-map (kbd "K") #'shape-artist--dispatch-shape-move-up)
    (define-key shape-artist-core-mode-map (kbd "L") #'shape-artist--dispatch-shape-move-right)
    (define-key shape-artist-core-mode-map (kbd ">") #'shape-artist--dispatch-shape-resize-wider)
    (define-key shape-artist-core-mode-map (kbd "<") #'shape-artist--dispatch-shape-resize-narrower)
    (define-key shape-artist-core-mode-map (kbd "+") #'shape-artist--dispatch-shape-resize-taller)
    (define-key shape-artist-core-mode-map (kbd "_") #'shape-artist--dispatch-shape-resize-shorter)
    (define-key shape-artist-core-mode-map (kbd "[") #'shape-artist--dispatch-shape-resize-left)
    (define-key shape-artist-core-mode-map (kbd "]") #'shape-artist--dispatch-shape-shrink-left)
    (define-key shape-artist-core-mode-map (kbd "{") #'shape-artist--dispatch-shape-resize-up)
    (define-key shape-artist-core-mode-map (kbd "}") #'shape-artist--dispatch-shape-shrink-up)))

;;; --- Evil State Integration ---

(defun shape-artist--setup-evil-keymaps ()
  "Setup Evil state keymaps for artist modes."
  (when (fboundp 'evil-define-key*)
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

    (evil-define-key* 'select shape-artist-core-mode-map
      "n" #'shape-artist-select-next
      "p" #'shape-artist-select-prev
      "g" #'shape-artist-shapes-parse
      "t" #'shape-artist-select-toggle-current
      "T" #'shape-artist-select-all
      "c" #'shape-artist-select-clear-selected
      (kbd "RET") #'shape-artist-select-confirm
      (kbd "<escape>") #'shape-artist-select-cancel
      "u" #'shape-artist-ui-toggle)

    (evil-define-key* 'shape-edit shape-artist-core-mode-map
      "e" #'shape-artist-shape-edit-label
      "m" #'shape-artist-shape-move
      "r" #'shape-artist-shape-resize
      "x" #'shape-artist-shape-cycle-style
      "s" #'shape-artist-shape-set-style
      "h" #'shape-artist-shape-edit-left
      "j" #'shape-artist-shape-edit-down
      "k" #'shape-artist-shape-edit-up
      "l" #'shape-artist-shape-edit-right
      "H" #'shape-artist-shape-move-left
      "J" #'shape-artist-shape-move-down
      "K" #'shape-artist-shape-move-up
      "L" #'shape-artist-shape-move-right
      ">" #'shape-artist-shape-resize-wider
      "<" #'shape-artist-shape-resize-narrower
      "+" #'shape-artist-shape-resize-taller
      "_" #'shape-artist-shape-resize-shorter
      "[" #'shape-artist-shape-resize-left
      "]" #'shape-artist-shape-shrink-left
      "{" #'shape-artist-shape-resize-up
      "}" #'shape-artist-shape-shrink-up
      (kbd "<escape>") #'shape-artist-shape-edit-exit
      "u" #'shape-artist-ui-toggle)))

(shape-artist--install-modal-dispatch-keymap)

(when (featurep 'evil)
  (shape-artist--setup-evil-keymaps))

;;; --- Provide ---

(provide 'shape-artist-keys)
;;; shape-artist-keys.el ends here
