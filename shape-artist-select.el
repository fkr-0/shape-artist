;;; shape-artist-select.el --- SELECT evil state: shape candidate cycling -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'shape-artist-core)
(require 'artist-diagram-parse)

(declare-function evil-select-state "evil")
(declare-function evil-normal-state "evil")

;;; --- State ---

(defvar-local shape-artist--select-overlays nil
  "Overlays used for shape highlighting in SELECT mode.")

;;; --- SELECT Mode Entry/Exit ---

(defun shape-artist-select-start ()
  "Enter SELECT mode and highlight the first detected shape."
  (interactive)
  ;; Parse shapes if not already done
  (unless shape-artist--last-shapes-list
    (when (fboundp 'shape-artist-shapes-parse)
      (shape-artist-shapes-parse)))
  (when (and shape-artist--last-shapes-list
             (> (length shape-artist--last-shapes-list) 0))
    ;; Save current position
    (setq shape-artist--pre-select-pos (shape-artist--current-row-col))
    (setq shape-artist--select-index 0)
    (setq shape-artist--mode 'select)
    (when (fboundp 'evil-select-state)
      (evil-select-state))
    (shape-artist--select-show-current)
    (shape-artist--update-mode-line)
    (shape-artist--maybe-update-sidebar)))

(defun shape-artist-select-cancel ()
  "Exit SELECT mode, return to MOVE, restore cursor."
  (interactive)
  (shape-artist--select-clear-overlays)
  (setq shape-artist--mode 'move)
  (when (fboundp 'evil-normal-state)
    (evil-normal-state))
  ;; Restore pre-selection position
  (when shape-artist--pre-select-pos
    (shape-artist--goto-row-col (car shape-artist--pre-select-pos)
                                (cdr shape-artist--pre-select-pos)))
  (shape-artist--update-mode-line)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist-select-confirm ()
  "Lock in current shape and enter SHAPE-EDIT mode."
  (interactive)
  (let ((shape (shape-artist-shapes-current)))
    (when shape
      (setq shape-artist--locked-shape shape)
      (shape-artist--select-clear-overlays)
      (setq shape-artist--mode 'shape-edit)
      (when (fboundp 'evil-shape-edit-state)
        (evil-shape-edit-state))
      ;; Move cursor to center of shape
      (let ((cx (+ (oref shape x) (/ (oref shape width) 2)))
            (cy (+ (oref shape y) (/ (oref shape height) 2))))
        (shape-artist--goto-row-col cy cx))
      (shape-artist--update-mode-line)
      (shape-artist--maybe-update-sidebar))))

;;; --- Navigation ---

(defun shape-artist-select-next ()
  "Advance to the next shape candidate."
  (interactive)
  (when shape-artist--last-shapes-list
    (setq shape-artist--select-index
          (mod (1+ shape-artist--select-index)
               (length shape-artist--last-shapes-list)))
    (shape-artist--select-show-current)
    (shape-artist--maybe-update-sidebar)))

(defun shape-artist-select-prev ()
  "Go to the previous shape candidate."
  (interactive)
  (when shape-artist--last-shapes-list
    (setq shape-artist--select-index
          (mod (1- shape-artist--select-index)
               (length shape-artist--last-shapes-list)))
    (shape-artist--select-show-current)
    (shape-artist--maybe-update-sidebar)))

;;; --- Display ---

(defun shape-artist--select-show-current ()
  "Highlight the current shape candidate and move cursor."
  (shape-artist--select-clear-overlays)
  (let ((shape (shape-artist-shapes-current)))
    (when shape
      ;; Highlight
      (setq shape-artist--select-overlays
            (artist-diagram--highlight-shape shape 'region))
      ;; Move cursor to center
      (let ((cx (+ (oref shape x) (/ (oref shape width) 2)))
            (cy (+ (oref shape y) (/ (oref shape height) 2))))
        (shape-artist--goto-row-col cy cx))
      ;; Show summary in minibuffer
      (message "Shape %d/%d: %s at (%d,%d) size %dx%d%s"
               (1+ shape-artist--select-index)
               (length shape-artist--last-shapes-list)
               (if (artist-diagram-rectangle-p shape) "rectangle" "edge")
               (oref shape x) (oref shape y)
               (oref shape width) (oref shape height)
               (if (artist-diagram-edge-p shape)
                   (format " [%s]" (oref shape edge-type))
                 "")))))

(defun shape-artist--select-clear-overlays ()
  "Clear all SELECT mode overlays."
  (mapc #'delete-overlay shape-artist--select-overlays)
  (setq shape-artist--select-overlays nil))

;;; --- Provide ---

(provide 'shape-artist-select)
;;; shape-artist-select.el ends here
