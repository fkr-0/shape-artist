;;; shape-artist-diagram-outline.el --- Outline buffer for shape tree -*- lexical-binding: t; -*-

;;; Commentary:

;;; This module provides an outline buffer that displays the shape tree
;;; hierarchy detected by shape-artist-diagram-parse.  It allows navigation
;;; back to specific shapes in the main artist buffer.

;;; Code:

(require 'shape-artist-diagram-parse)
(require 'shape-artist-core)

;;; --- Variables ---

(defvar shape-artist-diagram--outline-buffer-name "*artist-outline*"
  "Name of the outline buffer.")

(defvar-local shape-artist-diagram--outline-parent-buffer nil
  "The artist buffer this outline is associated with.")

;;; --- Derived Mode ---

(define-derived-mode shape-artist-diagram-outline-mode special-mode "Artist-Outline"
  "Major mode for the shape-artist outline buffer."
  (setq buffer-read-only t))

;;; --- Outline Buffer ---

(defun shape-artist-diagram-outline-show ()
  "Show or create the outline buffer for the current artist buffer."
  (interactive)
  (let* ((parent-buf (current-buffer))
         (outline-buf (get-buffer-create shape-artist-diagram--outline-buffer-name)))
    (with-current-buffer outline-buf
      (shape-artist-diagram-outline-mode)
      (setq-local shape-artist-diagram--outline-parent-buffer parent-buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (shape-artist-diagram--outline-insert-tree parent-buf))
      (goto-char (point-min)))
    (display-buffer outline-buf)))

(defun shape-artist-diagram--outline-insert-tree (buffer)
  "Insert the shape tree from BUFFER into current outline buffer."
  (let ((root nil))
    (with-current-buffer buffer
      (setq root shape-artist--last-root))
    (if root
        (shape-artist-diagram--outline-insert-node root 0)
      (insert "(no shapes detected)\n"))))

(defun shape-artist-diagram--outline-insert-node (node depth)
  "Insert NODE at DEPTH into outline buffer."
  (let ((indent (make-string (* 2 depth) ?\s)))
    (insert (format "%s%s %s\n"
                    indent
                    (cond
                     ((shape-artist-diagram-root-p node) "[root]")
                     ((shape-artist-diagram-rectangle-p node)
                      (format "[rect (%d,%d) %dx%d]"
                              (oref node x) (oref node y)
                              (oref node width) (oref node height)))
                     ((shape-artist-diagram-edge-p node)
                      (format "[edge (%d,%d)->(%d,%d) %s]"
                              (oref node x) (oref node y)
                              (oref node x2) (oref node y2)
                              (oref node edge-type)))
                     ((shape-artist-diagram-label-p node)
                      (format "[label \"%s\"]" (oref node text)))
                     (t "[node]"))
                    ""))
    (dolist (child (oref node children))
      (shape-artist-diagram--outline-insert-node child (1+ depth)))))

;;; --- Navigation ---

(defun shape-artist-diagram-outline-goto ()
  "Navigate to the shape at point in the outline buffer."
  (interactive)
  (when shape-artist-diagram--outline-parent-buffer
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
      (when (string-match "(\\([0-9]+\\),\\([0-9]+\\))" line)
        (let ((x (string-to-number (match-string 1 line)))
              (y (string-to-number (match-string 2 line))))
          (with-current-buffer shape-artist-diagram--outline-parent-buffer
            (shape-artist--goto-row-col y x)
            (select-window (get-buffer-window shape-artist-diagram--outline-parent-buffer))))))))

;;; --- Keymap ---

(let ((map shape-artist-diagram-outline-mode-map))
  (define-key map (kbd "RET") #'shape-artist-diagram-outline-goto)
  (define-key map (kbd "g") #'shape-artist-diagram-outline-refresh))

;;; --- Refresh ---

(defun shape-artist-diagram-outline-refresh ()
  "Refresh the outline buffer from the parent artist buffer."
  (interactive)
  (when shape-artist-diagram--outline-parent-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (shape-artist-diagram--outline-insert-tree shape-artist-diagram--outline-parent-buffer)
      (goto-char (point-min)))))

;;; --- Hook ---

(defun shape-artist-diagram--outline-update ()
  "Update outline buffer if it exists and is visible."
  (let ((outline-buf (get-buffer shape-artist-diagram--outline-buffer-name)))
    (when (and outline-buf (buffer-live-p outline-buf))
      (with-current-buffer outline-buf
        (shape-artist-diagram-outline-refresh)))))

;;; --- Provide ---

(provide 'shape-artist-diagram-outline)
;;; shape-artist-diagram-outline.el ends here
