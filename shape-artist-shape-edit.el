;;; shape-artist-shape-edit.el --- SHAPE-EDIT evil state: move/resize/label -*- lexical-binding: t; -*-

;;; Commentary:

;;; This module implements SHAPE-EDIT mode for locked-in shapes.
;;; It supports prompt-based editing commands and repeatable Evil-style
;;; directional commands for moving, resizing, labeling, and border styling.

;;; Code:

(require 'cl-lib)
(require 'shape-artist-core)
(require 'shape-artist-shapes)

(declare-function evil-shape-edit-state "evil")
(declare-function evil-normal-state "evil")

;;; --- SHAPE-EDIT Mode Entry/Exit ---

(defun shape-artist-shape-edit-exit ()
  "Exit SHAPE-EDIT mode, return to MOVE."
  (interactive)
  (setq shape-artist--locked-shape nil)
  (setq shape-artist--mode 'move)
  (when (fboundp 'evil-normal-state)
    (evil-normal-state))
  (shape-artist--update-mode-line)
  (shape-artist--maybe-update-sidebar))

;;; --- Locked Shape Helpers ---

(defun shape-artist-shape-edit--set-locked-shape (shape)
  "Set SHAPE as the current locked shape and refresh edit UI."
  (when shape
    (setq shape-artist--locked-shape shape)
    (shape-artist--maybe-update-sidebar))
  shape)

(defun shape-artist-shape-edit--count (count)
  "Normalize interactive COUNT to a positive integer."
  (max 1 (prefix-numeric-value (or count 1))))

;;; --- Label Editing ---

(defun shape-artist-shape-edit-label ()
  "Edit the label of the locked-in shape."
  (interactive)
  (when shape-artist--locked-shape
    (shape-artist-shape-edit--set-locked-shape
     (shape-artist-shapes-edit-label shape-artist--locked-shape))))

;;; --- Move Shape ---

(defun shape-artist-shape-move (&optional dx dy)
  "Move the locked-in shape by DX columns and DY rows.
When called interactively, prompt for DX and DY.  Directional commands such as
`shape-artist-shape-move-right' are better for repeatable Evil-style editing."
  (interactive)
  (when shape-artist--locked-shape
    (let ((dx (or dx (read-number "dx: " 0)))
          (dy (or dy (read-number "dy: " 0))))
      (shape-artist-shape-edit--set-locked-shape
       (shape-artist-shapes-move shape-artist--locked-shape dx dy)))))

(defun shape-artist-shape-move-left (&optional count)
  "Move the locked shape left by COUNT cells."
  (interactive "p")
  (shape-artist-shape-move (- (shape-artist-shape-edit--count count)) 0))

(defun shape-artist-shape-move-right (&optional count)
  "Move the locked shape right by COUNT cells."
  (interactive "p")
  (shape-artist-shape-move (shape-artist-shape-edit--count count) 0))

(defun shape-artist-shape-move-up (&optional count)
  "Move the locked shape up by COUNT cells."
  (interactive "p")
  (shape-artist-shape-move 0 (- (shape-artist-shape-edit--count count))))

(defun shape-artist-shape-move-down (&optional count)
  "Move the locked shape down by COUNT cells."
  (interactive "p")
  (shape-artist-shape-move 0 (shape-artist-shape-edit--count count)))

;;; --- Resize Shape ---

(defun shape-artist-shape-resize (&optional width height)
  "Resize the locked-in shape to WIDTH and HEIGHT.
When called interactively, prompt for WIDTH and HEIGHT."
  (interactive)
  (when shape-artist--locked-shape
    (let ((width (or width (read-number "New width: ")))
          (height (or height (read-number "New height: "))))
      (shape-artist-shape-edit--set-locked-shape
       (shape-artist-shapes-resize shape-artist--locked-shape width height)))))

(defun shape-artist-shape-resize-wider (&optional count)
  "Increase locked shape width by COUNT cells."
  (interactive "p")
  (when shape-artist--locked-shape
    (shape-artist-shape-resize
     (+ (oref shape-artist--locked-shape width)
        (shape-artist-shape-edit--count count))
     (oref shape-artist--locked-shape height))))

(defun shape-artist-shape-resize-narrower (&optional count)
  "Decrease locked shape width by COUNT cells, stopping at width 2."
  (interactive "p")
  (when shape-artist--locked-shape
    (shape-artist-shape-resize
     (max 2 (- (oref shape-artist--locked-shape width)
               (shape-artist-shape-edit--count count)))
     (oref shape-artist--locked-shape height))))

(defun shape-artist-shape-resize-taller (&optional count)
  "Increase locked shape height by COUNT cells."
  (interactive "p")
  (when shape-artist--locked-shape
    (shape-artist-shape-resize
     (oref shape-artist--locked-shape width)
     (+ (oref shape-artist--locked-shape height)
        (shape-artist-shape-edit--count count)))))

(defun shape-artist-shape-resize-shorter (&optional count)
  "Decrease locked shape height by COUNT cells, stopping at height 2."
  (interactive "p")
  (when shape-artist--locked-shape
    (shape-artist-shape-resize
     (oref shape-artist--locked-shape width)
     (max 2 (- (oref shape-artist--locked-shape height)
               (shape-artist-shape-edit--count count))))))

;;; --- Replace Border Style ---

(defun shape-artist-shape-set-style (&optional style)
  "Set locked shape border STYLE.
STYLE is one of `standard', `heavy', or `light'.  Interactively, prompt for it."
  (interactive)
  (when shape-artist--locked-shape
    (let ((style (or style
                     (intern (completing-read
                              "Border style: "
                              '("standard" "heavy" "light") nil t)))))
      (shape-artist-shape-edit--set-locked-shape
       (shape-artist-shapes-replace shape-artist--locked-shape style)))))

(defun shape-artist-shape-cycle-style ()
  "Cycle locked shape border style through standard, heavy, and light."
  (interactive)
  (when shape-artist--locked-shape
    (let* ((current (shape-artist-shapes-border-style shape-artist--locked-shape))
           (next (pcase current
                   ('standard 'heavy)
                   ('heavy 'light)
                   (_ 'standard))))
      (shape-artist-shape-set-style next))))

(defun shape-artist-shape-replace ()
  "Cycle the locked-in shape's border style."
  (interactive)
  (shape-artist-shape-cycle-style))

;;; --- Interior Navigation ---

(defun shape-artist-shape-edit-up ()
  "Navigate up within shape interior."
  (interactive)
  (when shape-artist--locked-shape
    (let* ((shape shape-artist--locked-shape)
           (pos (shape-artist--current-row-col))
           (min-y (1+ (oref shape y))))
      (when (> (car pos) min-y)
        (shape-artist--goto-row-col (1- (car pos)) (cdr pos))))))

(defun shape-artist-shape-edit-down ()
  "Navigate down within shape interior."
  (interactive)
  (when shape-artist--locked-shape
    (let* ((shape shape-artist--locked-shape)
           (pos (shape-artist--current-row-col))
           (max-y (1- (+ (oref shape y) (oref shape height)))))
      (when (< (car pos) max-y)
        (shape-artist--goto-row-col (1+ (car pos)) (cdr pos))))))

(defun shape-artist-shape-edit-left ()
  "Navigate left within shape interior."
  (interactive)
  (when shape-artist--locked-shape
    (let* ((shape shape-artist--locked-shape)
           (pos (shape-artist--current-row-col))
           (min-x (1+ (oref shape x))))
      (when (> (cdr pos) min-x)
        (shape-artist--goto-row-col (car pos) (1- (cdr pos)))))))

(defun shape-artist-shape-edit-right ()
  "Navigate right within shape interior."
  (interactive)
  (when shape-artist--locked-shape
    (let* ((shape shape-artist--locked-shape)
           (pos (shape-artist--current-row-col))
           (max-x (1- (+ (oref shape x) (oref shape width)))))
      (when (< (cdr pos) max-x)
        (shape-artist--goto-row-col (car pos) (1+ (cdr pos)))))))

;;; --- Provide ---

(provide 'shape-artist-shape-edit)
;;; shape-artist-shape-edit.el ends here
