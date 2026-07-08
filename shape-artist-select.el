;;; shape-artist-select.el --- SELECT evil state: shape candidate cycling -*- lexical-binding: t; -*-

;;; Commentary:

;;; SELECT mode: cycle candidates, optionally build a multi-selection, and lock
;;; one or more shapes for SHAPE-EDIT operations.

;;; Code:

(require 'cl-lib)
(require 'shape-artist-core)
(require 'shape-artist-shapes)
(require 'shape-artist-diagram-parse)

(declare-function evil-select-state "evil")
(declare-function evil-normal-state "evil")
(declare-function evil-shape-edit-state "evil")

(defvar-local shape-artist--select-overlays nil
  "Overlays used for current candidate highlighting in SELECT mode.")

(defvar-local shape-artist--selected-overlays nil
  "Overlays used for persistent multi-selection highlighting.")

(defun shape-artist--select-current-signature ()
  "Return the current candidate signature, or nil."
  (let ((shape (shape-artist-shapes-current)))
    (when shape (shape-artist-shapes-signature shape))))

(defun shape-artist-select-start ()
  "Enter SELECT mode and highlight the first detected shape."
  (interactive)
  (unless shape-artist--last-shapes-list
    (shape-artist-shapes-parse))
  (when (and shape-artist--last-shapes-list
             (> (length shape-artist--last-shapes-list) 0))
    (setq shape-artist--pre-select-pos (shape-artist--current-row-col))
    (setq shape-artist--select-index 0)
    (setq shape-artist--mode 'select)
    (when (fboundp 'evil-select-state)
      (evil-select-state))
    (shape-artist--select-show-current)
    (shape-artist--select-refresh-selected-overlays)
    (shape-artist--update-mode-line)
    (shape-artist--maybe-update-sidebar)))

(defun shape-artist-select-cancel ()
  "Exit SELECT mode, clear multi-selection, return to MOVE, restore cursor."
  (interactive)
  (shape-artist--select-clear-overlays)
  (shape-artist--select-clear-selected-overlays)
  (setq shape-artist--selected-shape-signatures nil)
  (setq shape-artist--mode 'move)
  (when (fboundp 'evil-normal-state)
    (evil-normal-state))
  (when shape-artist--pre-select-pos
    (shape-artist--goto-row-col (car shape-artist--pre-select-pos)
                                (cdr shape-artist--pre-select-pos)))
  (shape-artist--update-mode-line)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist-select-confirm ()
  "Lock current shape or multi-selection and enter SHAPE-EDIT mode."
  (interactive)
  (let* ((current (shape-artist-shapes-current))
         (targets (shape-artist-shapes-selected)))
    (when (or current targets)
      (setq shape-artist--locked-shape (or current (car targets)))
      (shape-artist--select-clear-overlays)
      (shape-artist--select-clear-selected-overlays)
      (setq shape-artist--mode 'shape-edit)
      (when (fboundp 'evil-shape-edit-state)
        (evil-shape-edit-state))
      (when shape-artist--locked-shape
        (let ((cx (+ (oref shape-artist--locked-shape x)
                     (/ (oref shape-artist--locked-shape width) 2)))
              (cy (+ (oref shape-artist--locked-shape y)
                     (/ (oref shape-artist--locked-shape height) 2))))
          (shape-artist--goto-row-col cy cx)))
      (shape-artist--update-mode-line)
      (shape-artist--maybe-update-sidebar))))

(defun shape-artist-select-next ()
  "Advance to the next shape candidate."
  (interactive)
  (when shape-artist--last-shapes-list
    (setq shape-artist--select-index
          (mod (1+ shape-artist--select-index)
               (length shape-artist--last-shapes-list)))
    (shape-artist--select-show-current)
    (shape-artist--select-refresh-selected-overlays)
    (shape-artist--maybe-update-sidebar)))

(defun shape-artist-select-prev ()
  "Go to the previous shape candidate."
  (interactive)
  (when shape-artist--last-shapes-list
    (setq shape-artist--select-index
          (mod (1- shape-artist--select-index)
               (length shape-artist--last-shapes-list)))
    (shape-artist--select-show-current)
    (shape-artist--select-refresh-selected-overlays)
    (shape-artist--maybe-update-sidebar)))

(defun shape-artist-select-toggle-current ()
  "Toggle current candidate in the persistent multi-selection set."
  (interactive)
  (let ((sig (shape-artist--select-current-signature)))
    (when sig
      (if (member sig shape-artist--selected-shape-signatures)
          (setq shape-artist--selected-shape-signatures
                (delete sig shape-artist--selected-shape-signatures))
        (push sig shape-artist--selected-shape-signatures))
      (shape-artist--select-refresh-selected-overlays)
      (shape-artist--maybe-update-sidebar))))

(defun shape-artist-select-all ()
  "Select all non-label shapes in the current parse result."
  (interactive)
  (setq shape-artist--selected-shape-signatures
        (mapcar #'shape-artist-shapes-signature
                (cl-remove-if #'shape-artist-diagram-label-p
                              shape-artist--last-shapes-list)))
  (shape-artist--select-refresh-selected-overlays)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist-select-clear-selected ()
  "Clear persistent multi-selection while staying in SELECT mode."
  (interactive)
  (setq shape-artist--selected-shape-signatures nil)
  (shape-artist--select-clear-selected-overlays)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist--select-show-current ()
  "Highlight the current shape candidate and move cursor."
  (shape-artist--select-clear-overlays)
  (let ((shape (shape-artist-shapes-current)))
    (when shape
      (setq shape-artist--select-overlays
            (shape-artist-diagram--highlight-shape shape 'region))
      (let ((cx (+ (oref shape x) (/ (oref shape width) 2)))
            (cy (+ (oref shape y) (/ (oref shape height) 2))))
        (shape-artist--goto-row-col cy cx))
      (message "Shape %d/%d: %s at (%d,%d) size %dx%d%s%s"
               (1+ shape-artist--select-index)
               (length shape-artist--last-shapes-list)
               (cond
                ((shape-artist-diagram-rectangle-p shape) "rectangle")
                ((shape-artist-diagram-edge-p shape) "edge")
                ((shape-artist-diagram-label-p shape) "label")
                (t "node"))
               (oref shape x) (oref shape y)
               (oref shape width) (oref shape height)
               (if (shape-artist-diagram-edge-p shape)
                   (format " [%s]" (oref shape edge-type))
                 "")
               (if (member (shape-artist-shapes-signature shape)
                           shape-artist--selected-shape-signatures)
                   " [selected]" "")))))

(defun shape-artist--select-refresh-selected-overlays ()
  "Refresh persistent selected shape overlays."
  (shape-artist--select-clear-selected-overlays)
  (dolist (shape (shape-artist-shapes-selected))
    (setq shape-artist--selected-overlays
          (append (shape-artist-diagram--highlight-shape
                   shape 'secondary-selection)
                  shape-artist--selected-overlays))))

(defun shape-artist--select-clear-overlays ()
  "Clear current SELECT mode candidate overlays."
  (mapc #'delete-overlay shape-artist--select-overlays)
  (setq shape-artist--select-overlays nil))

(defun shape-artist--select-clear-selected-overlays ()
  "Clear persistent SELECT mode multi-selection overlays."
  (mapc #'delete-overlay shape-artist--selected-overlays)
  (setq shape-artist--selected-overlays nil))

(provide 'shape-artist-select)
;;; shape-artist-select.el ends here
