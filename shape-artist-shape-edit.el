;;; shape-artist-shape-edit.el --- SHAPE-EDIT evil state: move/resize/label -*- lexical-binding: t; -*-

;;; Commentary:

;;; This module implements the SHAPE-EDIT mode, allowing users to
;;; move, resize, replace, or label a locked-in shape.

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

;;; --- Label Editing ---

(defun shape-artist-shape-edit-label ()
  "Edit the label of the locked-in shape."
  (interactive)
  (when shape-artist--locked-shape
    (shape-artist-shapes-edit-label shape-artist--locked-shape)))

;;; --- Move Shape (stub) ---

(defun shape-artist-shape-move ()
  "Move the locked-in shape.  Stub."
  (interactive)
  (when shape-artist--locked-shape
    (let ((dx (read-number "dx: " 0))
          (dy (read-number "dy: " 0)))
      (shape-artist-shapes-move shape-artist--locked-shape dx dy))))

;;; --- Resize Shape (stub) ---

(defun shape-artist-shape-resize ()
  "Resize the locked-in shape.  Stub."
  (interactive)
  (when shape-artist--locked-shape
    (let ((w (read-number "New width: "))
          (h (read-number "New height: ")))
      (shape-artist-shapes-resize shape-artist--locked-shape w h))))

;;; --- Replace Shape Type (stub) ---

(defun shape-artist-shape-replace ()
  "Replace the locked-in shape type.  Stub."
  (interactive)
  (when shape-artist--locked-shape
    (shape-artist-shapes-replace shape-artist--locked-shape 'diamond)))

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
