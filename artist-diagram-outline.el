;;; artist-diagram-outline.el --- Outline buffer for shape tree -*- lexical-binding: t; -*-

(require 'artist-diagram-parse)

;;; --- Variables ---

(defvar artist-diagram--outline-buffer-name "*artist-outline*"
  "Name of the outline buffer.")

(defvar-local artist-diagram--outline-parent-buffer nil
  "The artist buffer this outline is associated with.")

;;; --- Outline Buffer ---

(defun artist-diagram-outline-show ()
  "Show or create the outline buffer for the current artist buffer."
  (interactive)
  (let* ((parent-buf (current-buffer))
         (outline-buf (get-buffer-create artist-diagram--outline-buffer-name)))
    (with-current-buffer outline-buf
      (special-mode)
      (setq-local artist-diagram--outline-parent-buffer parent-buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (artist-diagram--outline-insert-tree parent-buf))
      (goto-char (point-min)))
    (display-buffer outline-buf)))

(defun artist-diagram--outline-insert-tree (buffer)
  "Insert the shape tree from BUFFER into current outline buffer."
  (with-current-buffer buffer
    (if shape-artist--last-root
        (artist-diagram--outline-insert-node shape-artist--last-root 0)
      (insert "(no shapes detected)\n"))))

(defun artist-diagram--outline-insert-node (node depth)
  "Insert NODE at DEPTH into outline buffer."
  (let ((indent (make-string (* 2 depth) ?\s)))
    (insert (format "%s%s %s\n"
                    indent
                    (cond
                     ((artist-diagram-root-p node) "[root]")
                     ((artist-diagram-rectangle-p node)
                      (format "[rect (%d,%d) %dx%d]"
                              (oref node x) (oref node y)
                              (oref node width) (oref node height)))
                     ((artist-diagram-edge-p node)
                      (format "[edge (%d,%d)->(%d,%d) %s]"
                              (oref node x) (oref node y)
                              (oref node x2) (oref node y2)
                              (oref node edge-type)))
                     ((artist-diagram-label-p node)
                      (format "[label \"%s\"]" (oref node text)))
                     (t "[node]"))
                    ""))
    (dolist (child (oref node children))
      (artist-diagram--outline-insert-node child (1+ depth)))))

;;; --- Navigation ---

(defun artist-diagram-outline-goto ()
  "Navigate to the shape at point in the outline buffer."
  (interactive)
  (when artist-diagram--outline-parent-buffer
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
      (when (string-match "(\\([0-9]+\\),\\([0-9]+\\))" line)
        (let ((x (string-to-number (match-string 1 line)))
              (y (string-to-number (match-string 2 line))))
          (with-current-buffer artist-diagram--outline-parent-buffer
            (shape-artist--goto-row-col y x)
            (select-window (get-buffer-window artist-diagram--outline-parent-buffer))))))))

;;; --- Keymap ---

(defvar artist-diagram-outline-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'artist-diagram-outline-goto)
    map)
  "Keymap for the outline buffer.")

;;; --- Hook ---

(defun artist-diagram--outline-update ()
  "Update outline buffer if it exists."
  (let ((outline-buf (get-buffer artist-diagram--outline-buffer-name)))
    (when (and outline-buf (buffer-live-p outline-buf))
      (with-current-buffer outline-buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (artist-diagram--outline-insert-tree artist-diagram--outline-parent-buffer))))))

;;; --- Provide ---

(provide 'artist-diagram-outline)
;;; artist-diagram-outline.el ends here
