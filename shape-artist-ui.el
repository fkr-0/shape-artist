;;; shape-artist-ui.el --- Persistent sidebar UI -*- lexical-binding: t; -*-

;;; Commentary:

;;; This module provides a persistent sidebar UI that displays the
;;; current mode, cursor position, anchor state, and detected shapes.

;;; Code:

(require 'cl-lib)
(require 'shape-artist-core)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-shapes)
(require 'shape-artist-guides)
(require 'shape-artist-metadata)

(declare-function shape-artist-diagram-outline-show "shape-artist-diagram-outline")

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
  (let* ((label (shape-artist--mode-property :label))
         (activity (shape-artist--mode-property :activity))
         (face (shape-artist--mode-property :face))
         (hint (shape-artist--mode-property :hint))
         (canonical (pcase shape-artist--mode
                      ('move "MOVE")
                      ('draw "DRAW")
                      ('select "SELECT")
                      ('shape-edit "SHAPE-EDIT")
                      (_ "UNKNOWN"))))
    (if label
        (concat "=== State ===\n"
                (propertize (format " %s · %s " label activity) 'face face)
                (format "\nMode: %s\n" canonical)
                (propertize hint 'face 'shadow)
                "\n")
      (format "=== State ===\nMode: %s\n" canonical))))

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

(defun shape-artist--ui-shape-extents (shape)
  "Return (MIN-X MIN-Y MAX-X MAX-Y) for SHAPE."
  (if (shape-artist-diagram-edge-p shape)
      (let ((x1 (oref shape x))
            (y1 (oref shape y))
            (x2 (oref shape x2))
            (y2 (oref shape y2)))
        (list (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)))
    (list (oref shape x)
          (oref shape y)
          (+ (oref shape x) (oref shape width))
          (+ (oref shape y) (oref shape height)))))

(defun shape-artist--ui-diagram-bounds (&optional shapes)
  "Return diagram bounds for SHAPES as (MIN-X MIN-Y MAX-X MAX-Y COUNT).

SHAPES defaults to `shape-artist--last-shapes-list'.  Return nil when
there are no known shapes."
  (let ((items (or shapes shape-artist--last-shapes-list))
        (min-x nil)
        (min-y nil)
        (max-x nil)
        (max-y nil)
        (count 0))
    (dolist (shape items)
      (unless (shape-artist-diagram-root-p shape)
        (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (shape-artist--ui-shape-extents shape)))
          (setq min-x (if min-x (min min-x x1) x1))
          (setq min-y (if min-y (min min-y y1) y1))
          (setq max-x (if max-x (max max-x x2) x2))
          (setq max-y (if max-y (max max-y y2) y2))
          (cl-incf count))))
    (when (> count 0)
      (list min-x min-y max-x max-y count))))

(defun shape-artist--ui-layout-section ()
  "Generate diagram layout/bounds section text."
  (let ((bounds (shape-artist--ui-diagram-bounds)))
    (if bounds
        (pcase-let ((`(,min-x ,min-y ,max-x ,max-y ,count) bounds))
          (let* ((graph shape-artist--last-graph)
                 (connectors (and graph
                                  (shape-artist-diagram-graph-connectors graph)))
                 (connections (cl-count-if
                               (lambda (connector)
                                 (> (length (shape-artist-diagram-connector-shape-ids
                                             connector))
                                    1))
                               connectors))
                 (components (and graph
                                  (shape-artist-diagram-graph-components graph))))
            (format (concat "=== Layout ===\n"
                            "Bounds: (%d,%d)-(%d,%d)\n"
                            "Size: %dx%d\nShapes: %d\n"
                            "Graph: %d nodes · %d connections · %d components\n")
                    min-x min-y max-x max-y
                    (- max-x min-x) (- max-y min-y) count
                    (if graph (length (shape-artist-diagram-graph-nodes graph)) 0)
                    connections
                    (length components))))
      "=== Layout ===\n(no diagram bounds)\n")))

(defun shape-artist-ui-center-diagram ()
  "Move point and window view to the center of the parsed diagram."
  (interactive)
  (let ((bounds (shape-artist--ui-diagram-bounds)))
    (if (not bounds)
        (message "No parsed Shape Artist diagram to center.")
      (pcase-let* ((`(,min-x ,min-y ,max-x ,max-y ,_) bounds)
                   (center-x (round (+ min-x (/ (- max-x min-x) 2.0))))
                   (center-y (round (+ min-y (/ (- max-y min-y) 2.0))))
                   (win (selected-window)))
        (shape-artist--goto-row-col center-y center-x)
        (when (window-live-p win)
          (set-window-hscroll
           win (max 0 (- center-x (/ (window-width win) 2))))
          (ignore-errors (recenter)))
        (message "Centered diagram at (%d, %d)." center-y center-x)))))

(defun shape-artist--ui-shapes-section ()
  "Generate shape tree section."
  (if shape-artist--last-root
      (concat "=== Shape Tree ===\n"
              "Markers: > current, L locked, * selected\n"
              (shape-artist--ui-tree-lines shape-artist--last-root 0))
    "=== Shape Tree ===\n(none detected)\n"))

(defun shape-artist--ui-tree-lines (node depth)
  "Return sidebar tree lines for NODE and its children at DEPTH."
  (let ((text (shape-artist--ui-tree-line node depth)))
    (dolist (child (oref node children))
      (setq text (concat text (shape-artist--ui-tree-lines child (1+ depth)))))
    text))

(defun shape-artist--ui-tree-line (node depth)
  "Return one sidebar tree line for NODE at DEPTH."
  (format "%s%s %s\n"
          (make-string (* 2 depth) ?\s)
          (shape-artist--ui-shape-markers node)
          (shape-artist--ui-shape-summary node)))

(defun shape-artist--ui-shape-markers (shape)
  "Return compact current/locked/selected markers for SHAPE."
  (if (shape-artist-diagram-root-p shape)
      "   "
    (let ((current (and (eq shape-artist--mode 'select)
                        (eq shape (nth shape-artist--select-index
                                       shape-artist--last-shapes-list))))
          (locked (and (eq shape-artist--mode 'shape-edit)
                       (eq shape shape-artist--locked-shape)))
          (selected (and shape-artist--selected-shape-signatures
                         (member (shape-artist-shapes-signature shape)
                                 shape-artist--selected-shape-signatures))))
      (format "%c%c%c"
              (if current ?> ?\s)
              (if locked ?L ?\s)
              (if selected ?* ?\s)))))

(defun shape-artist--ui-shape-summary (shape)
  "Return a concise sidebar summary for SHAPE."
  (cond
   ((shape-artist-diagram-root-p shape)
    "root")
   ((shape-artist-diagram-rectangle-p shape)
    (format "rect#%s (%d,%d) %dx%d"
            (or (shape-artist-metadata-shape-id shape) "?")
            (oref shape x) (oref shape y)
            (oref shape width) (oref shape height)))
   ((shape-artist-diagram-edge-p shape)
    (format "edge#%s[%s] (%d,%d)->(%d,%d)"
            (or (shape-artist-metadata-shape-id shape) "?")
            (oref shape edge-type)
            (oref shape x) (oref shape y)
            (oref shape x2) (oref shape y2)))
   ((shape-artist-diagram-label-p shape)
    (format "label#%s \"%s\" (%d,%d)"
            (or (shape-artist-metadata-shape-id shape) "?")
            (oref shape text) (oref shape x) (oref shape y)))
   (t
    (format "node (%d,%d) %dx%d"
            (oref shape x) (oref shape y)
            (oref shape width) (oref shape height)))))

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
             "h/j/k/l  Move\nSPC      Enter DRAW + anchor\nRET      Commit preview\nd        Enter DRAW\nC-n      Select shapes\n?        Quick actions\no        Outline tree\nz        Center diagram\nG h/v    Add row/column guide via menu\nu        Toggle sidebar\nq        Quit\n")
            ('draw
             "h/j/k/l  Draw\nSPC      Set anchor\nRET      Commit + exit\nC-RET    Commit + stay\nESC      Cancel + exit\np        Pick tool\n?        Quick actions\nz        Center diagram\nG h/v    Add row/column guide via menu\nu        Undo\n")
            ('select
             "n/p      Next/prev shape\nt        Toggle candidate\nT        Select all\nc        Clear selection\ng        Re-parse\nRET      Lock/edit\nESC      Cancel\n?        Quick actions\no        Outline tree\nz        Center diagram\nG h/v    Add row/column guide via menu\nu        Toggle sidebar\n")
            ('shape-edit
             "H/J/K/L  Move shape/group\n>/<      Wider/narrower or edge +/-\n+/_      Taller/shorter or edge +/-\n[/]      Grow/shrink from left\n{/}      Grow/shrink from top\ne        Edit label\nm/r      Prompt move/resize\nx        Cycle style/arrow\ns        Prompt style\nh/j/k/l  Navigate interior\nESC      Exit to MOVE\n?        Quick actions\no        Outline tree\nz        Center diagram\nG h/v    Add row/column guide via menu\nu        Toggle sidebar\n")
            (_ ""))))

(defun shape-artist-ui-show-outline ()
  "Show the full outline/tree buffer for the current artist buffer."
  (interactive)
  (require 'shape-artist-diagram-outline)
  (shape-artist-diagram-outline-show))

(defun shape-artist--ui-quick-actions ()
  "Return mode-sensitive quick actions for the command menu."
  (let ((mode shape-artist--mode))
    (cl-remove-if-not
     (lambda (action)
       (let ((modes (nth 0 action)))
         (or (memq 'all modes) (memq mode modes))))
     '(((all) "a" "Show sidebar" shape-artist-ui-show)
       ((all) "u" "Toggle sidebar" shape-artist-ui-toggle)
       ((all) "o" "Show outline tree" shape-artist-ui-show-outline)
       ((all) "z" "Center diagram" shape-artist-ui-center-diagram)
       ((all) "G h" "Create horizontal guide" shape-artist-guide-create-horizontal)
       ((all) "G v" "Create vertical guide" shape-artist-guide-create-vertical)
       ((all) "G d" "Delete nearest guide" shape-artist-guide-delete-nearest)
       ((all) "G c" "Clear all guides" shape-artist-guide-clear-all)
       ((all) "G s" "Toggle guide snapping" shape-artist-snap-toggle)
       ((all) "G S" "Toggle grid snapping" shape-artist-grid-snap-toggle)
       ((all) "G V" "Toggle guide visibility" shape-artist-guide-toggle-visibility)
       ((all) "G g" "Toggle grid overlay" shape-artist-grid-toggle)
       ((all) "G a" "Adopt selection into nearest guide" shape-artist-guide-adopt-selection-nearest)
       ((all) "G A" "Clear guide adoptions" shape-artist-guide-clear-adoptions)
       ((all) "G e" "Edit guide properties" shape-artist-guide-edit-properties)
       ((all) "G L" "Set guide label" shape-artist-guide-set-label)
       ((all) "G p" "Set guide snap distance" shape-artist-guide-set-snap-distance)
       ((all) "G x" "Toggle nearest guide visibility" shape-artist-guide-toggle-nearest-visibility)
       ((all) "G w" "Save guides" shape-artist-guides-save)
       ((all) "G r" "Load guides" shape-artist-guides-load)
       ((all) "G u" "Undo guide change" shape-artist-guides-undo)
       ((all) "G R" "Redo guide change" shape-artist-guides-redo)
       ((all) "M w" "Save metadata IDs" shape-artist-metadata-save)
       ((all) "M r" "Load metadata IDs" shape-artist-metadata-load)
       ((all) "M d" "Delete saved metadata IDs" shape-artist-metadata-delete-saved)
       ((all) "g" "Parse / refresh shapes" shape-artist-shapes-parse)
       ((move) "SPC" "Start drawing from cursor" shape-artist-enter-draw-and-anchor)
       ((move) "d" "Enter draw mode" shape-artist-enter-draw)
       ((move) "C-n" "Select shapes" shape-artist-select-start)
       ((draw) "RET" "Commit shape and exit draw" shape-artist-commit-and-exit)
       ((draw) "C-RET" "Commit shape and keep drawing" shape-artist-commit-stay)
       ((draw) "p" "Pick draw tool" shape-artist-pick-tool)
       ((draw) "u" "Undo draw step" shape-artist-undo)
       ((select) "n" "Next candidate" shape-artist-select-next)
       ((select) "p" "Previous candidate" shape-artist-select-prev)
       ((select) "t" "Toggle current candidate" shape-artist-select-toggle-current)
       ((select) "T" "Select all candidates" shape-artist-select-all)
       ((select) "c" "Clear selected candidates" shape-artist-select-clear-selected)
       ((select) "RET" "Lock/edit selection" shape-artist-select-confirm)
       ((shape-edit) "H/J/K/L" "Move locked shape or group" shape-artist-shape-move)
       ((shape-edit) "e" "Edit label" shape-artist-shape-edit-label)
       ((shape-edit) "m" "Prompt move" shape-artist-shape-move)
       ((shape-edit) "r" "Prompt resize" shape-artist-shape-resize)
       ((shape-edit) "x" "Cycle style or arrow" shape-artist-shape-cycle-style)
       ((shape-edit) "s" "Prompt style" shape-artist-shape-set-style)
       ((shape-edit) "ESC" "Exit shape edit" shape-artist-shape-edit-exit)))))

(defun shape-artist--ui-quick-action-label (action)
  "Return display label for quick ACTION."
  (format "%-8s %s" (nth 1 action) (nth 2 action)))

(defun shape-artist-ui-completing-read-menu ()
  "Show a mode-sensitive completing-read command menu."
  (interactive)
  (let* ((actions (shape-artist--ui-quick-actions))
         (choices (mapcar (lambda (action)
                            (cons (shape-artist--ui-quick-action-label action)
                                  (nth 3 action)))
                          actions))
         (choice (completing-read "Shape Artist action: " choices nil t)))
    (call-interactively (cdr (assoc choice choices)))))

(defun shape-artist-ui-command-menu ()
  "Show the dependency-free, mode-sensitive Shape Artist command menu."
  (interactive)
  (shape-artist-ui-completing-read-menu))

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
                         (shape-artist--ui-layout-section)
                         "\n"
                         (shape-artist-guides-section-text)
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
