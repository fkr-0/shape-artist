;;; shape-artist-ui.el --- Persistent sidebar UI -*- lexical-binding: t; -*-

;;; Commentary:

;;; This module provides a persistent sidebar UI that displays the
;;; current mode, cursor position, anchor state, and detected shapes.

;;; Code:

(require 'cl-lib)
(require 'shape-artist-core)
(require 'shape-artist-diagram-parse)

;;; --- Variables ---

(defvar-local shape-artist--sidebar-window nil
  "The sidebar window for this artist buffer.")

(defvar-local shape-artist--sidebar-buffer-name nil
  "Name of the sidebar buffer for this artist buffer.")

;;; --- Sidebar Buffer ---

(defun shape-artist--get-or-create-sidebar-buffer ()
  "Get or create the sidebar buffer for the current artist buffer."
  (unless shape-artist--sidebar-buffer-name
    (setq shape-artist--sidebar-buffer-name
          (format "*artist-sidebar:%s*" (buffer-name))))
  (let ((buf (get-buffer-create shape-artist--sidebar-buffer-name)))
    (with-current-buffer buf
      (special-mode)
      (setq-local truncate-lines t)
      (setq-local word-wrap nil)
      (buffer-face-set '(:height 0.9)))
    buf))

;;; --- Content Generators ---

(defun shape-artist--ui-state-section ()
  "Generate state section text."
  (format "=== State ===\nMode: %s\n"
          (pcase shape-artist--mode
            ('move "MOVE")
            ('draw "DRAW")
            ('select "SELECT")
            ('shape-edit "SHAPE-EDIT")
            (_ "UNKNOWN"))))

(defun shape-artist--ui-cursor-section ()
  "Generate cursor position section."
  (let ((pos (shape-artist--current-row-col)))
    (format "Cursor: (%d . %d)\n" (car pos) (cdr pos))))

(defun shape-artist--ui-anchor-section ()
  "Generate anchor state section."
  (format "Anchor: %s\n"
          (if shape-artist--anchor
              (format "(%d, %d)" (car shape-artist--anchor) (cdr shape-artist--anchor))
            "none")))

(defun shape-artist--ui-nonblank-section ()
  "Generate non-whitespace character count."
  (format "Non-blank chars: %d\n"
          (let ((count 0))
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (let ((line (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))))
                  (cl-incf count (length (replace-regexp-in-string "[[:space:]]" "" line))))
                (forward-line 1)))
            count)))

(defun shape-artist--ui-shapes-section ()
  "Generate shape candidates section."
  (if (and shape-artist--last-shapes-list
           (> (length shape-artist--last-shapes-list) 0))
      (let ((text "=== Shapes ===\n"))
        (dotimes (i (length shape-artist--last-shapes-list))
          (let* ((shape (nth i shape-artist--last-shapes-list))
                 (marker (if (and (eq shape-artist--mode 'select)
                                  (= i shape-artist--select-index))
                             ">>>" "   "))
                 (locked (if (and (eq shape-artist--locked-shape shape)
                                  (eq shape-artist--mode 'shape-edit))
                             " [LOCKED]" "")))
            (setq text
                  (concat text
                          (format "%s %d: %s at (%d,%d) %dx%d%s\n"
                                  marker (1+ i)
                                  (cond
                                   ((shape-artist-diagram-rectangle-p shape) "rect")
                                   ((shape-artist-diagram-edge-p shape)
                                    (format "edge[%s]" (oref shape edge-type)))
                                   ((shape-artist-diagram-label-p shape) "label")
                                   (t "node"))
                                  (oref shape x) (oref shape y)
                                  (oref shape width) (oref shape height)
                                  locked)))))
        text)
    "=== Shapes ===\n(none detected)\n"))

(defun shape-artist--ui-labels-section ()
  "Generate labels section."
  (let ((labels (cl-remove-if-not #'shape-artist-diagram-label-p
                                  shape-artist--last-shapes-list)))
    (if labels
        (let ((text "=== Labels ===\n"))
          (dolist (label labels)
            (setq text (concat text (format "  \"%s\" at (%d,%d)\n"
                                            (oref label text)
                                            (oref label x) (oref label y)))))
          text)
      "=== Labels ===\n(none)\n")))

(defun shape-artist--ui-keys-section ()
  "Generate context-sensitive keybindings section."
  (format "=== Keys ===\n%s"
          (pcase shape-artist--mode
            ('move
             "h/j/k/l  Move\nSPC      Enter DRAW + anchor\nRET      Commit preview\nd        Enter DRAW\nC-n      Select shapes\nu        Toggle sidebar\nq        Quit\n")
            ('draw
             "h/j/k/l  Draw\nSPC      Set anchor\nRET      Commit + exit\nC-RET    Commit + stay\nESC      Cancel + exit\np        Pick tool\nu        Undo\n")
            ('select
             "n        Next shape\np        Prev shape\ng        Re-parse\nRET      Lock in shape\nESC      Cancel\nu        Toggle sidebar\n")
            ('shape-edit
             "e        Edit label\nm        Move shape\nr        Resize\nx        Replace type\nh/j/k/l  Navigate interior\nESC      Exit to MOVE\nu        Toggle sidebar\n")
            (_ ""))))

(defun shape-artist--ui-locked-shape-section ()
  "Generate locked shape details section (for SHAPE-EDIT mode)."
  (if (and (eq shape-artist--mode 'shape-edit) shape-artist--locked-shape)
      (let ((s shape-artist--locked-shape))
        (format "=== Locked Shape ===\nType: %s\nPos: (%d, %d)\nSize: %dx%d\n"
                (cond
                 ((shape-artist-diagram-rectangle-p s) "rectangle")
                 ((shape-artist-diagram-edge-p s) "edge")
                 ((shape-artist-diagram-label-p s) "label")
                 (t "node"))
                (oref s x) (oref s y)
                (oref s width) (oref s height)))
    ""))

;;; --- Update ---

(defun shape-artist--update-sidebar ()
  "Update the sidebar buffer content."
  (when shape-artist--sidebar-buffer-name
    (let ((sidebar-buf (get-buffer shape-artist--sidebar-buffer-name)))
      (when (and sidebar-buf (buffer-live-p sidebar-buf))
        (let ((artist-buf (current-buffer)))
          (with-current-buffer sidebar-buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (with-current-buffer artist-buf
                        (concat
                         (shape-artist--ui-state-section)
                         (shape-artist--ui-cursor-section)
                         (shape-artist--ui-anchor-section)
                         (shape-artist--ui-nonblank-section)
                         "\n"
                         (shape-artist--ui-shapes-section)
                         "\n"
                         (shape-artist--ui-labels-section)
                         "\n"
                         (shape-artist--ui-locked-shape-section)
                         "\n"
                         (shape-artist--ui-keys-section)))))))))))

;; Register the update function as the global default for artist buffers.
(setq-default shape-artist--update-sidebar-fn #'shape-artist--update-sidebar)

;;; --- Toggle ---

(defun shape-artist-ui-toggle ()
  "Toggle the sidebar UI."
  (interactive)
  (let ((sidebar-buf (when shape-artist--sidebar-buffer-name
                       (get-buffer shape-artist--sidebar-buffer-name))))
    (if (and sidebar-buf
             (get-buffer-window sidebar-buf))
        ;; Hide sidebar
        (progn
          (delete-window (get-buffer-window sidebar-buf))
          (setq shape-artist--sidebar-window nil))
      ;; Show sidebar
      (shape-artist-ui-show))))

(defun shape-artist-ui-show ()
  "Force-show the sidebar UI."
  (interactive)
  (let* ((sidebar-buf (shape-artist--get-or-create-sidebar-buffer))
         (win (display-buffer-in-side-window
               sidebar-buf
               '((side . right)
                 (window-width . 35)
                 (slot . 0)))))
    (setq shape-artist--sidebar-window win)
    (shape-artist--update-sidebar)))

;;; --- Provide ---

(provide 'shape-artist-ui)
;;; shape-artist-ui.el ends here
