;;; shape-artist-guides.el --- Guides, grid overlays, and snapping -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Shape Artist Contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Buffer-local guide and grid support for Shape Artist.
;;
;; The MVP is intentionally ephemeral: guides live in buffer-local variables and
;; never alter diagram text.  Future metadata/history work can persist the same
;; command-level semantics without changing the public command names.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'shape-artist-core)
(require 'shape-artist-diagram-parse)

(defgroup shape-artist-guides nil
  "Guide and grid snapping support for Shape Artist."
  :group 'shape-artist)

(defcustom shape-artist-guide-snap-distance 2
  "Maximum distance, in cells, at which guides attract moving shapes."
  :type 'integer
  :group 'shape-artist-guides)

(defcustom shape-artist-guide-visible-snaps-only nil
  "When non-nil, hidden guides do not participate in snapping."
  :type 'boolean
  :group 'shape-artist-guides)

(defcustom shape-artist-grid-size 4
  "Grid spacing, in cells, for guide grid overlays and optional grid snapping."
  :type 'integer
  :group 'shape-artist-guides)

(defface shape-artist-guide-face
  '((t :inherit shadow :underline t))
  "Face used for guide overlays."
  :group 'shape-artist-guides)

(defface shape-artist-grid-face
  '((t :inherit shadow))
  "Face used for grid point overlays."
  :group 'shape-artist-guides)

(cl-defstruct (shape-artist-guide
               (:constructor shape-artist-guides--make-guide))
  "A buffer-local Shape Artist guide."
  id
  orientation
  position
  label
  visible
  snap-distance
  adopted-shape-signatures
  source)

(defvar-local shape-artist-guides--guides nil
  "Buffer-local list of `shape-artist-guide' records.")

(defvar-local shape-artist-guides--next-id 1
  "Next buffer-local guide id.")

(defvar-local shape-artist-guides--overlays nil
  "Guide display overlays.")

(defvar-local shape-artist-guides--grid-overlays nil
  "Grid display overlays.")

(defvar-local shape-artist-guides-visible t
  "Non-nil when guide overlays should be visible.")

(defvar-local shape-artist-guides-snap-enabled t
  "Non-nil when guide snapping is enabled.")

(defvar-local shape-artist-grid-visible nil
  "Non-nil when the grid overlay is visible.")

(defvar-local shape-artist-grid-snap-enabled nil
  "Non-nil when shape moves may snap to the configured grid.")

(defvar-local shape-artist-guides--last-snap-report nil
  "Plist describing the latest guide/grid snap decision.")

(defun shape-artist-guides--make-id ()
  "Return the next buffer-local guide id."
  (prog1 shape-artist-guides--next-id
    (cl-incf shape-artist-guides--next-id)))

(defun shape-artist-guides--normalize-orientation (orientation)
  "Return normalized ORIENTATION or signal a user error."
  (pcase orientation
    ((or 'horizontal "horizontal") 'horizontal)
    ((or 'vertical "vertical") 'vertical)
    (_ (user-error "Guide orientation must be horizontal or vertical"))))

(defun shape-artist-guides--active-p (guide)
  "Return non-nil when GUIDE should participate in snapping."
  (and guide
       (or (shape-artist-guide-visible guide)
           (not shape-artist-guide-visible-snaps-only))))

(defun shape-artist-guides--distance (a b)
  "Return absolute distance between numbers A and B."
  (abs (- a b)))

(defun shape-artist-guides--guide-distance-limit (guide)
  "Return GUIDE-specific or default snap distance for GUIDE."
  (or (shape-artist-guide-snap-distance guide)
      shape-artist-guide-snap-distance))

(defun shape-artist-guides--guides-for (orientation &optional active-only)
  "Return guides matching ORIENTATION.

When ACTIVE-ONLY is non-nil, hidden guides may be filtered depending on
`shape-artist-guide-visible-snaps-only'."
  (let ((orientation (shape-artist-guides--normalize-orientation orientation)))
    (cl-remove-if-not
     (lambda (guide)
       (and (eq (shape-artist-guide-orientation guide) orientation)
            (or (not active-only)
                (shape-artist-guides--active-p guide))))
     shape-artist-guides--guides)))

(defun shape-artist-guides-add (orientation position &optional label source)
  "Add a guide at POSITION with ORIENTATION.

ORIENTATION is `horizontal' for a row guide or `vertical' for a column guide.
LABEL is optional display text.  SOURCE records whether the guide is manual,
selection-derived, imported, etc."
  (let ((guide (shape-artist-guides--make-guide
                :id (shape-artist-guides--make-id)
                :orientation (shape-artist-guides--normalize-orientation orientation)
                :position (max 0 position)
                :label label
                :visible t
                :snap-distance nil
                :adopted-shape-signatures nil
                :source (or source 'manual))))
    (push guide shape-artist-guides--guides)
    (shape-artist-guides-refresh-overlays)
    (shape-artist--maybe-update-sidebar)
    guide))

(defun shape-artist-guides--guide-at-id (id)
  "Return guide with ID, or nil."
  (cl-find id shape-artist-guides--guides :key #'shape-artist-guide-id :test #'=))

(defun shape-artist-guides-delete-by-id (id)
  "Delete guide with ID and return non-nil if it existed."
  (let ((before (length shape-artist-guides--guides)))
    (setq shape-artist-guides--guides
          (cl-remove id shape-artist-guides--guides
                     :key #'shape-artist-guide-id :test #'=))
    (shape-artist-guides-refresh-overlays)
    (shape-artist--maybe-update-sidebar)
    (/= before (length shape-artist-guides--guides))))

(defun shape-artist-guides-find-nearest (orientation position &optional active-only)
  "Return nearest guide with ORIENTATION to POSITION.

When ACTIVE-ONLY is non-nil, ignore guides that should not snap."
  (let ((best nil)
        (best-dist nil))
    (dolist (guide (shape-artist-guides--guides-for orientation active-only))
      (let ((dist (shape-artist-guides--distance
                   (shape-artist-guide-position guide) position)))
        (when (or (null best)
                  (< dist best-dist)
                  (and (= dist best-dist)
                       (< (shape-artist-guide-id guide)
                          (shape-artist-guide-id best))))
          (setq best guide)
          (setq best-dist dist))))
    best))

(defun shape-artist-guides--best-correction (orientation candidates)
  "Return best correction plist for ORIENTATION and CANDIDATES.

CANDIDATES is a list of (POSITION EDGE-SYMBOL) items where POSITION is the
shape side after the requested operation."
  (let ((best nil))
    (dolist (candidate candidates)
      (let* ((position (car candidate))
             (edge (cadr candidate))
             (guide (shape-artist-guides-find-nearest orientation position t)))
        (when guide
          (let* ((dist (shape-artist-guides--distance
                        (shape-artist-guide-position guide) position))
                 (limit (shape-artist-guides--guide-distance-limit guide))
                 (corr (- (shape-artist-guide-position guide) position)))
            (when (and (<= dist limit)
                       (or (null best)
                           (< dist (plist-get best :distance))
                           (and (= dist (plist-get best :distance))
                                (< (shape-artist-guide-id guide)
                                   (shape-artist-guide-id
                                    (plist-get best :guide))))))
              (setq best (list :orientation orientation
                               :edge edge
                               :guide guide
                               :distance dist
                               :correction corr)))))))
    best))

(defun shape-artist-guides--shape-bounds (shape)
  "Return SHAPE bounds as (LEFT TOP RIGHT BOTTOM), or nil."
  (cond
   ((shape-artist-diagram-rectangle-p shape)
    (list (oref shape x)
          (oref shape y)
          (+ (oref shape x) (oref shape width))
          (+ (oref shape y) (oref shape height))))
   ((shape-artist-diagram-edge-p shape)
    (list (min (oref shape x) (oref shape x2))
          (min (oref shape y) (oref shape y2))
          (max (oref shape x) (oref shape x2))
          (max (oref shape y) (oref shape y2))))
   (t nil)))

(defun shape-artist-guides--bounds-union (shapes)
  "Return bounds union for SHAPES as (LEFT TOP RIGHT BOTTOM), or nil."
  (let ((left nil) (top nil) (right nil) (bottom nil))
    (dolist (shape shapes)
      (pcase-let ((`(,l ,t0 ,r ,b) (shape-artist-guides--shape-bounds shape)))
        (when l
          (setq left (if left (min left l) l))
          (setq top (if top (min top t0) t0))
          (setq right (if right (max right r) r))
          (setq bottom (if bottom (max bottom b) b)))))
    (when left (list left top right bottom))))

(defun shape-artist-guides--snap-bounds-move (left top right bottom dx dy &optional source)
  "Snap bounds LEFT TOP RIGHT BOTTOM moving by DX/DY.

SOURCE is stored in the snap report.  Return (SNAPPED-DX SNAPPED-DY REPORT)."
  (let* ((target-left (+ left dx))
         (target-right (+ right dx))
         (target-top (+ top dy))
         (target-bottom (+ bottom dy))
         (x-snap (and shape-artist-guides-snap-enabled
                      (shape-artist-guides--best-correction
                       'vertical
                       `((,target-left left) (,target-right right)))))
         (y-snap (and shape-artist-guides-snap-enabled
                      (shape-artist-guides--best-correction
                       'horizontal
                       `((,target-top top) (,target-bottom bottom)))))
         (snapped-dx (+ dx (or (plist-get x-snap :correction) 0)))
         (snapped-dy (+ dy (or (plist-get y-snap :correction) 0)))
         (report (list :source source
                       :requested (list dx dy)
                       :snapped (list snapped-dx snapped-dy)
                       :x x-snap
                       :y y-snap)))
    (when (or x-snap y-snap)
      (setq shape-artist-guides--last-snap-report report))
    (list snapped-dx snapped-dy report)))

(defun shape-artist-guides-snap-rectangle-move (shape dx dy)
  "Return snapped (DX DY REPORT) for moving rectangle SHAPE."
  (if (not (and shape shape-artist-guides-snap-enabled
                (shape-artist-diagram-rectangle-p shape)))
      (list dx dy nil)
    (pcase-let ((`(,left ,top ,right ,bottom)
                 (shape-artist-guides--shape-bounds shape)))
      (shape-artist-guides--snap-bounds-move
       left top right bottom dx dy 'rectangle-move))))

(defun shape-artist-guides-snap-group-move (shapes dx dy)
  "Return snapped (DX DY REPORT) for moving SHAPES as one group."
  (let ((bounds (and shape-artist-guides-snap-enabled
                     (shape-artist-guides--bounds-union shapes))))
    (if (not bounds)
        (list dx dy nil)
      (pcase-let ((`(,left ,top ,right ,bottom) bounds))
        (shape-artist-guides--snap-bounds-move
         left top right bottom dx dy 'group-move)))))

(defun shape-artist-guides-snap-rectangle-resize (shape x-anchor dx y-anchor dy)
  "Return snapped (DX DY REPORT) for resizing SHAPE from X-ANCHOR/Y-ANCHOR."
  (if (not (and shape shape-artist-guides-snap-enabled
                (shape-artist-diagram-rectangle-p shape)))
      (list dx dy nil)
    (let* ((old-x (oref shape x))
           (old-y (oref shape y))
           (old-w (oref shape width))
           (old-h (oref shape height))
           (moving-left (eq x-anchor 'left))
           (moving-top (eq y-anchor 'top))
           (target-left (if moving-left (- old-x dx) old-x))
           (target-right (if moving-left (+ old-x old-w) (+ old-x old-w dx)))
           (target-top (if moving-top (- old-y dy) old-y))
           (target-bottom (if moving-top (+ old-y old-h) (+ old-y old-h dy)))
           (x-snap (shape-artist-guides--best-correction
                    'vertical
                    (if moving-left
                        `((,target-left left))
                      `((,target-right right)))))
           (y-snap (shape-artist-guides--best-correction
                    'horizontal
                    (if moving-top
                        `((,target-top top))
                      `((,target-bottom bottom)))))
           (snapped-dx dx)
           (snapped-dy dy))
      (when x-snap
        (setq snapped-dx
              (if moving-left
                  (- old-x (shape-artist-guide-position (plist-get x-snap :guide)))
                (- (shape-artist-guide-position (plist-get x-snap :guide))
                   old-x old-w))))
      (when y-snap
        (setq snapped-dy
              (if moving-top
                  (- old-y (shape-artist-guide-position (plist-get y-snap :guide)))
                (- (shape-artist-guide-position (plist-get y-snap :guide))
                   old-y old-h))))
      ;; Do not clamp here: the existing resize implementation owns the
      ;; minimum-size user error and should keep signalling it unchanged.
      (let ((report (list :source 'rectangle-resize
                          :requested (list dx dy)
                          :snapped (list snapped-dx snapped-dy)
                          :x x-snap
                          :y y-snap)))
        (when (or x-snap y-snap)
          (setq shape-artist-guides--last-snap-report report))
        (list snapped-dx snapped-dy report)))))

(defun shape-artist-guides--grid-snap-delta (value delta)
  "Return adjusted DELTA so VALUE+DELTA lands on nearest grid line."
  (if (or (not shape-artist-grid-snap-enabled)
          (<= shape-artist-grid-size 0))
      delta
    (let* ((target (+ value delta))
           (grid shape-artist-grid-size)
           (rounded (* grid (round (/ (float target) grid)))))
      (+ delta (- rounded target)))))

(defun shape-artist-guides-clear-overlays ()
  "Delete all guide overlays in the current buffer."
  (mapc #'delete-overlay shape-artist-guides--overlays)
  (setq shape-artist-guides--overlays nil)
  (shape-artist-guides-clear-grid-overlays))

(defun shape-artist-guides-clear-grid-overlays ()
  "Delete all grid overlays in the current buffer."
  (mapc #'delete-overlay shape-artist-guides--grid-overlays)
  (setq shape-artist-guides--grid-overlays nil))

(defun shape-artist-guides--point-if-existing (row col)
  "Return buffer position at ROW/COL without mutating text, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (zerop (forward-line row))
      (let ((len (- (line-end-position) (line-beginning-position))))
        (when (< col len)
          (move-to-column col)
          (point))))))

(defun shape-artist-guides--overlay-cell (row col face target-list-var)
  "Overlay ROW/COL with FACE, pushing to TARGET-LIST-VAR."
  (let ((pos (shape-artist-guides--point-if-existing row col)))
    (when pos
      (let ((ov (make-overlay pos (min (point-max) (1+ pos)))))
        (overlay-put ov 'face face)
        (overlay-put ov 'priority 20)
        (push ov (symbol-value target-list-var))))))

(defun shape-artist-guides--line-length-at (row)
  "Return length of ROW without mutating buffer, or nil when absent."
  (save-excursion
    (goto-char (point-min))
    (when (zerop (forward-line row))
      (- (line-end-position) (line-beginning-position)))))

(defun shape-artist-guides-refresh-overlays ()
  "Refresh guide and grid overlays for the current buffer."
  (mapc #'delete-overlay shape-artist-guides--overlays)
  (setq shape-artist-guides--overlays nil)
  (when shape-artist-guides-visible
    (dolist (guide shape-artist-guides--guides)
      (when (shape-artist-guide-visible guide)
        (pcase (shape-artist-guide-orientation guide)
          ('horizontal
           (let* ((row (shape-artist-guide-position guide))
                  (len (or (shape-artist-guides--line-length-at row) 0)))
             (dotimes (col len)
               (shape-artist-guides--overlay-cell
                row col 'shape-artist-guide-face
                'shape-artist-guides--overlays))))
          ('vertical
           (let ((col (shape-artist-guide-position guide)))
             (dotimes (row (shape-artist--line-count))
               (shape-artist-guides--overlay-cell
                row col 'shape-artist-guide-face
                'shape-artist-guides--overlays))))))))
  (shape-artist-guides-refresh-grid-overlays))

(defun shape-artist-guides-refresh-grid-overlays ()
  "Refresh grid overlays for the current buffer."
  (shape-artist-guides-clear-grid-overlays)
  (when (and shape-artist-grid-visible
             (> shape-artist-grid-size 0))
    (let ((rows (shape-artist--line-count))
          (cols (shape-artist--max-line-length)))
      (cl-loop for row from 0 below rows by shape-artist-grid-size do
               (cl-loop for col from 0 below cols by shape-artist-grid-size do
                        (shape-artist-guides--overlay-cell
                         row col 'shape-artist-grid-face
                         'shape-artist-guides--grid-overlays))))))

(defun shape-artist-guide-create-horizontal (&optional position label)
  "Create a horizontal guide at POSITION or current row.

When LABEL is non-nil, store it as the guide display label."
  (interactive
   (list (when current-prefix-arg
           (read-number "Horizontal guide row: " (car (shape-artist--current-row-col))))
         nil))
  (let ((row (or position (car (shape-artist--current-row-col)))))
    (shape-artist-guides-add 'horizontal row label 'manual)
    (message "Horizontal guide at row %d" row)))

(defun shape-artist-guide-create-vertical (&optional position label)
  "Create a vertical guide at POSITION or current column.

When LABEL is non-nil, store it as the guide display label."
  (interactive
   (list (when current-prefix-arg
           (read-number "Vertical guide column: " (cdr (shape-artist--current-row-col))))
         nil))
  (let ((col (or position (cdr (shape-artist--current-row-col)))))
    (shape-artist-guides-add 'vertical col label 'manual)
    (message "Vertical guide at column %d" col)))

(defun shape-artist-guide-delete-nearest ()
  "Delete the nearest guide to point."
  (interactive)
  (let* ((pos (shape-artist--current-row-col))
         (h (shape-artist-guides-find-nearest 'horizontal (car pos)))
         (v (shape-artist-guides-find-nearest 'vertical (cdr pos)))
         (h-dist (and h (shape-artist-guides--distance
                         (shape-artist-guide-position h) (car pos))))
         (v-dist (and v (shape-artist-guides--distance
                         (shape-artist-guide-position v) (cdr pos))))
         (guide (cond
                 ((and h v) (if (<= h-dist v-dist) h v))
                 (h h)
                 (v v))))
    (if guide
        (progn
          (shape-artist-guides-delete-by-id (shape-artist-guide-id guide))
          (message "Deleted guide %d" (shape-artist-guide-id guide)))
      (message "No guides to delete"))))

(defun shape-artist-guide-clear-all ()
  "Clear all guides and guide overlays in the current buffer."
  (interactive)
  (setq shape-artist-guides--guides nil)
  (setq shape-artist-guides--last-snap-report nil)
  (shape-artist-guides-clear-overlays)
  (shape-artist--maybe-update-sidebar)
  (message "Cleared Shape Artist guides"))

(defun shape-artist-guide-toggle-visibility ()
  "Toggle guide overlay visibility."
  (interactive)
  (setq shape-artist-guides-visible (not shape-artist-guides-visible))
  (shape-artist-guides-refresh-overlays)
  (shape-artist--maybe-update-sidebar)
  (message "Guide overlays %s" (if shape-artist-guides-visible "visible" "hidden")))

(defun shape-artist-guide-move (&optional id new-position)
  "Move guide ID to NEW-POSITION."
  (interactive)
  (let* ((guide (or (and id (shape-artist-guides--guide-at-id id))
                    (let* ((choices (mapcar
                                     (lambda (g)
                                       (cons (format "%d %s %d %s"
                                                     (shape-artist-guide-id g)
                                                     (shape-artist-guide-orientation g)
                                                     (shape-artist-guide-position g)
                                                     (or (shape-artist-guide-label g) ""))
                                             g))
                                     shape-artist-guides--guides))
                           (choice (completing-read "Guide: " choices nil t)))
                      (cdr (assoc choice choices))))))
    (if (not guide)
        (message "No guide selected")
      (let ((pos (or new-position
                     (read-number "New guide position: "
                                  (shape-artist-guide-position guide)))))
        (setf (shape-artist-guide-position guide) (max 0 pos))
        (shape-artist-guides-refresh-overlays)
        (shape-artist--maybe-update-sidebar)
        (message "Moved guide %d to %d" (shape-artist-guide-id guide) pos)))))

(defun shape-artist-guide-selection-to-guide ()
  "Create guides from current shape selection or locked shape bounds."
  (interactive)
  (let* ((shapes (cond
                  ((and (boundp 'shape-artist--selected-shape-signatures)
                        shape-artist--selected-shape-signatures
                        (fboundp 'shape-artist-shapes-selected))
                   (shape-artist-shapes-selected))
                  ((and (boundp 'shape-artist--locked-shape) shape-artist--locked-shape)
                   (list shape-artist--locked-shape))
                  (t nil)))
         (bounds (shape-artist-guides--bounds-union shapes)))
    (if (not bounds)
        (message "No selected or locked shape to convert to guides")
      (pcase-let ((`(,left ,top ,right ,bottom) bounds))
        (shape-artist-guides-add 'vertical left "selection-left" 'selection)
        (shape-artist-guides-add 'vertical right "selection-right" 'selection)
        (shape-artist-guides-add 'horizontal top "selection-top" 'selection)
        (shape-artist-guides-add 'horizontal bottom "selection-bottom" 'selection)
        (message "Created guides from selection bounds (%d,%d)-(%d,%d)"
                 left top right bottom)))))

(defun shape-artist-snap-toggle ()
  "Toggle guide snapping for the current buffer."
  (interactive)
  (setq shape-artist-guides-snap-enabled (not shape-artist-guides-snap-enabled))
  (shape-artist--maybe-update-sidebar)
  (message "Guide snapping %s" (if shape-artist-guides-snap-enabled "enabled" "disabled")))

(defun shape-artist-grid-toggle ()
  "Toggle grid overlay visibility for the current buffer."
  (interactive)
  (setq shape-artist-grid-visible (not shape-artist-grid-visible))
  (shape-artist-guides-refresh-grid-overlays)
  (shape-artist--maybe-update-sidebar)
  (message "Shape Artist grid %s" (if shape-artist-grid-visible "visible" "hidden")))

(defun shape-artist-grid-set-size (&optional size)
  "Set grid SIZE for overlays and optional grid snapping."
  (interactive (list (read-number "Grid size: " shape-artist-grid-size)))
  (when (< size 1)
    (user-error "Grid size must be at least 1"))
  (setq shape-artist-grid-size size)
  (shape-artist-guides-refresh-grid-overlays)
  (shape-artist--maybe-update-sidebar)
  (message "Shape Artist grid size set to %d" size))

(defun shape-artist-grid-snap-toggle ()
  "Toggle snap-to-grid for the current buffer."
  (interactive)
  (setq shape-artist-grid-snap-enabled (not shape-artist-grid-snap-enabled))
  (shape-artist--maybe-update-sidebar)
  (message "Grid snapping %s" (if shape-artist-grid-snap-enabled "enabled" "disabled")))

(defun shape-artist-guides--snap-report-line ()
  "Return one-line text describing the latest snap, or nil."
  (when shape-artist-guides--last-snap-report
    (pcase-let* ((report shape-artist-guides--last-snap-report)
                 (`(,rx ,ry) (plist-get report :requested))
                 (`(,sx ,sy) (plist-get report :snapped)))
      (format "Last snap: %S (%d,%d) -> (%d,%d)\n"
              (plist-get report :source) rx ry sx sy))))

(defun shape-artist-guides-section-text ()
  "Return sidebar text for guides and grid state."
  (let ((text (format "=== Guides ===\nSnap: %s  Visible: %s  Grid: %s/%s size=%d\n"
                      (if shape-artist-guides-snap-enabled "on" "off")
                      (if shape-artist-guides-visible "on" "off")
                      (if shape-artist-grid-visible "visible" "hidden")
                      (if shape-artist-grid-snap-enabled "snap" "nosnap")
                      shape-artist-grid-size)))
    (if shape-artist-guides--guides
        (dolist (guide (sort (copy-sequence shape-artist-guides--guides)
                             (lambda (a b)
                               (if (eq (shape-artist-guide-orientation a)
                                       (shape-artist-guide-orientation b))
                                   (< (shape-artist-guide-position a)
                                      (shape-artist-guide-position b))
                                 (string< (symbol-name (shape-artist-guide-orientation a))
                                          (symbol-name (shape-artist-guide-orientation b)))))))
          (setq text
                (concat text
                        (format "  #%d %-10s %d %s%s\n"
                                (shape-artist-guide-id guide)
                                (shape-artist-guide-orientation guide)
                                (shape-artist-guide-position guide)
                                (if (shape-artist-guide-visible guide) "" "hidden ")
                                (or (shape-artist-guide-label guide) "")))))
      (setq text (concat text "  (none)\n")))
    (concat text (or (shape-artist-guides--snap-report-line) ""))))

(provide 'shape-artist-guides)
;;; shape-artist-guides.el ends here
