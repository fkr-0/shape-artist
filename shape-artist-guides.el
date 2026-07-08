;;; shape-artist-guides.el --- Guides, grid overlays, and snapping -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Shape Artist Contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Buffer-local guide and grid support for Shape Artist.
;;
;; Guides live in buffer-local variables, never alter diagram text, and can be
;; saved to a project-local sidecar file.  Guide state changes are recorded in a
;; small guide-specific undo/redo stack so non-text overlay metadata can be
;; rolled back without depending on text edits alone.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
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

(defcustom shape-artist-guides-persistence-file ".shape-artist/guides.el"
  "Project-local sidecar file used by `shape-artist-guides-save'."
  :type 'file
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

(defvar-local shape-artist-guides--suppress-snapping nil
  "Non-nil while guide-originated operations should bypass snapping.")

(defvar-local shape-artist-guides--undo-stack nil
  "Stack of guide-state snapshots for `shape-artist-guides-undo'.")

(defvar-local shape-artist-guides--redo-stack nil
  "Stack of guide-state snapshots for `shape-artist-guides-redo'.")

(defvar-local shape-artist-guides--history-inhibit nil
  "Non-nil while guide mutations should not create history entries.")

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

(defun shape-artist-guides--guide-to-plist (guide)
  "Return a serializable plist for GUIDE."
  (list :id (shape-artist-guide-id guide)
        :orientation (shape-artist-guide-orientation guide)
        :position (shape-artist-guide-position guide)
        :label (shape-artist-guide-label guide)
        :visible (shape-artist-guide-visible guide)
        :snap-distance (shape-artist-guide-snap-distance guide)
        :adopted-shape-signatures
        (shape-artist-guide-adopted-shape-signatures guide)
        :source (shape-artist-guide-source guide)))

(defun shape-artist-guides--plist-to-guide (plist)
  "Return a guide record from serialized PLIST."
  (shape-artist-guides--make-guide
   :id (or (plist-get plist :id) (shape-artist-guides--make-id))
   :orientation (shape-artist-guides--normalize-orientation
                 (plist-get plist :orientation))
   :position (max 0 (or (plist-get plist :position) 0))
   :label (plist-get plist :label)
   :visible (if (plist-member plist :visible)
                (plist-get plist :visible)
              t)
   :snap-distance (plist-get plist :snap-distance)
   :adopted-shape-signatures (plist-get plist :adopted-shape-signatures)
   :source (or (plist-get plist :source) 'manual)))

(defun shape-artist-guides--snapshot ()
  "Return a complete serializable snapshot of current guide state."
  (list :version 1
        :next-id shape-artist-guides--next-id
        :guides (mapcar #'shape-artist-guides--guide-to-plist
                        shape-artist-guides--guides)
        :guides-visible shape-artist-guides-visible
        :guides-snap-enabled shape-artist-guides-snap-enabled
        :grid-visible shape-artist-grid-visible
        :grid-snap-enabled shape-artist-grid-snap-enabled
        :grid-size shape-artist-grid-size
        :last-snap-report shape-artist-guides--last-snap-report))

(defun shape-artist-guides--move-adopted-for-state (state)
  "Move adopted shapes when restoring guide positions from STATE."
  (dolist (plist (plist-get state :guides))
    (let* ((id (plist-get plist :id))
           (target-position (plist-get plist :position))
           (guide (and id (shape-artist-guides--guide-at-id id))))
      (when (and guide target-position)
        (shape-artist-guides--move-adopted-shapes
         guide (- target-position (shape-artist-guide-position guide)))))))

(defun shape-artist-guides--apply-state (state &optional move-adopted)
  "Apply serialized guide STATE.

When MOVE-ADOPTED is non-nil, adopted shapes are moved along with guide
position changes before the restored signatures are installed."
  (when move-adopted
    (shape-artist-guides--move-adopted-for-state state))
  (setq shape-artist-guides--next-id (or (plist-get state :next-id) 1))
  (setq shape-artist-guides--guides
        (mapcar #'shape-artist-guides--plist-to-guide
                (plist-get state :guides)))
  (setq shape-artist-guides-visible
        (if (plist-member state :guides-visible)
            (plist-get state :guides-visible)
          t))
  (setq shape-artist-guides-snap-enabled
        (if (plist-member state :guides-snap-enabled)
            (plist-get state :guides-snap-enabled)
          t))
  (setq shape-artist-grid-visible (plist-get state :grid-visible))
  (setq shape-artist-grid-snap-enabled (plist-get state :grid-snap-enabled))
  (setq shape-artist-grid-size (or (plist-get state :grid-size)
                                  shape-artist-grid-size))
  (setq shape-artist-guides--last-snap-report
        (plist-get state :last-snap-report))
  (shape-artist-guides-refresh-overlays)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist-guides--record-history ()
  "Record current guide state for guide undo/redo."
  (unless shape-artist-guides--history-inhibit
    (push (shape-artist-guides--snapshot) shape-artist-guides--undo-stack)
    (setq shape-artist-guides--redo-stack nil)))

(defun shape-artist-guides-undo ()
  "Undo the latest guide-state change in the current buffer."
  (interactive)
  (if (not shape-artist-guides--undo-stack)
      (message "No guide change to undo")
    (let ((state (pop shape-artist-guides--undo-stack))
          (current (shape-artist-guides--snapshot))
          (shape-artist-guides--history-inhibit t))
      (push current shape-artist-guides--redo-stack)
      (shape-artist-guides--apply-state state t)
      (message "Undid guide change"))))

(defun shape-artist-guides-redo ()
  "Redo the latest guide-state change in the current buffer."
  (interactive)
  (if (not shape-artist-guides--redo-stack)
      (message "No guide change to redo")
    (let ((state (pop shape-artist-guides--redo-stack))
          (current (shape-artist-guides--snapshot))
          (shape-artist-guides--history-inhibit t))
      (push current shape-artist-guides--undo-stack)
      (shape-artist-guides--apply-state state t)
      (message "Redid guide change"))))

(defun shape-artist-guides--project-root ()
  "Return the current project root for guide persistence."
  (file-name-as-directory
   (or (locate-dominating-file default-directory ".git")
       default-directory)))

(defun shape-artist-guides--buffer-key ()
  "Return stable persistence key for the current buffer."
  (unless buffer-file-name
    (user-error "Guide persistence requires a file-backed buffer"))
  (file-relative-name (expand-file-name buffer-file-name)
                      (shape-artist-guides--project-root)))

(defun shape-artist-guides--persistence-path ()
  "Return absolute path of the guide persistence sidecar file."
  (expand-file-name shape-artist-guides-persistence-file
                    (shape-artist-guides--project-root)))

(defun shape-artist-guides--read-db (file)
  "Read guide persistence database from FILE."
  (if (not (file-exists-p file))
      (list :version 1 :buffers nil)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((data (read (current-buffer))))
        (unless (and (listp data) (plist-member data :buffers))
          (user-error "Invalid Shape Artist guide persistence file: %s" file))
        data))))

(defun shape-artist-guides--write-db (file db)
  "Write guide persistence database DB to FILE."
  (make-directory (file-name-directory file) t)
  (let ((print-length nil)
        (print-level nil))
    (with-temp-file file
      (insert ";;; Shape Artist guide state.  Generated file; edit with care.\n")
      (prin1 db (current-buffer))
      (insert "\n"))))

(defun shape-artist-guides-save ()
  "Save current buffer guide state to the project sidecar file."
  (interactive)
  (let* ((file (shape-artist-guides--persistence-path))
         (key (shape-artist-guides--buffer-key))
         (db (shape-artist-guides--read-db file))
         (buffers (plist-get db :buffers))
         (state (shape-artist-guides--snapshot))
         (cell (assoc key buffers)))
    (if cell
        (setcdr cell state)
      (push (cons key state) buffers))
    (setq db (plist-put db :buffers buffers))
    (shape-artist-guides--write-db file db)
    (message "Saved Shape Artist guides for %s" key)))

(defun shape-artist-guides-load ()
  "Load current buffer guide state from the project sidecar file."
  (interactive)
  (let* ((file (shape-artist-guides--persistence-path))
         (key (shape-artist-guides--buffer-key))
         (db (shape-artist-guides--read-db file))
         (state (cdr (assoc key (plist-get db :buffers)))))
    (if (not state)
        (message "No saved Shape Artist guides for %s" key)
      (shape-artist-guides--record-history)
      (let ((shape-artist-guides--history-inhibit t))
        (shape-artist-guides--apply-state state nil))
      (message "Loaded Shape Artist guides for %s" key))))

(defun shape-artist-guides-delete-saved ()
  "Delete current buffer guide state from the project sidecar file."
  (interactive)
  (let* ((file (shape-artist-guides--persistence-path))
         (key (shape-artist-guides--buffer-key))
         (db (shape-artist-guides--read-db file))
         (buffers (cl-remove key (plist-get db :buffers)
                             :key #'car :test #'equal)))
    (setq db (plist-put db :buffers buffers))
    (shape-artist-guides--write-db file db)
    (message "Deleted saved Shape Artist guides for %s" key)))

(defun shape-artist-guides-add (orientation position &optional label source)
  "Add a guide at POSITION with ORIENTATION.

ORIENTATION is `horizontal' for a row guide or `vertical' for a column guide.
LABEL is optional display text.  SOURCE records whether the guide is manual,
selection-derived, imported, etc."
  (shape-artist-guides--record-history)
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
  (when (shape-artist-guides--guide-at-id id)
    (shape-artist-guides--record-history))
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
  (if shape-artist-guides--suppress-snapping
      (list dx dy nil)
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
           ;; Guides have priority over grid snapping.  Grid snapping uses the
           ;; top-left corner so moving a group preserves its internal spacing.
           (grid-x (and (not x-snap)
                        (shape-artist-guides--grid-correction
                         target-left 'vertical 'grid-left)))
           (grid-y (and (not y-snap)
                        (shape-artist-guides--grid-correction
                         target-top 'horizontal 'grid-top)))
           (x-correction (or (plist-get x-snap :correction)
                             (plist-get grid-x :correction)
                             0))
           (y-correction (or (plist-get y-snap :correction)
                             (plist-get grid-y :correction)
                             0))
           (snapped-dx (+ dx x-correction))
           (snapped-dy (+ dy y-correction))
           (report (list :source source
                         :requested (list dx dy)
                         :snapped (list snapped-dx snapped-dy)
                         :x x-snap
                         :y y-snap
                         :grid-x grid-x
                         :grid-y grid-y)))
      (when (or x-snap y-snap grid-x grid-y)
        (setq shape-artist-guides--last-snap-report report))
      (list snapped-dx snapped-dy report))))

(defun shape-artist-guides-snap-rectangle-move (shape dx dy)
  "Return snapped (DX DY REPORT) for moving rectangle SHAPE."
  (if (not (and shape (shape-artist-diagram-rectangle-p shape)))
      (list dx dy nil)
    (pcase-let ((`(,left ,top ,right ,bottom)
                 (shape-artist-guides--shape-bounds shape)))
      (shape-artist-guides--snap-bounds-move
       left top right bottom dx dy 'rectangle-move))))

(defun shape-artist-guides-snap-edge-move (shape dx dy)
  "Return snapped (DX DY REPORT) for moving edge SHAPE."
  (if (not (and shape (shape-artist-diagram-edge-p shape)))
      (list dx dy nil)
    (pcase-let ((`(,left ,top ,right ,bottom)
                 (shape-artist-guides--shape-bounds shape)))
      (shape-artist-guides--snap-bounds-move
       left top right bottom dx dy 'edge-move))))

(defun shape-artist-guides-snap-group-move (shapes dx dy)
  "Return snapped (DX DY REPORT) for moving SHAPES as one group."
  (let ((bounds (shape-artist-guides--bounds-union shapes)))
    (if (not bounds)
        (list dx dy nil)
      (pcase-let ((`(,left ,top ,right ,bottom) bounds))
        (shape-artist-guides--snap-bounds-move
         left top right bottom dx dy 'group-move)))))

(defun shape-artist-guides-snap-rectangle-resize (shape x-anchor dx y-anchor dy)
  "Return snapped (DX DY REPORT) for resizing SHAPE from X-ANCHOR/Y-ANCHOR."
  (if (or shape-artist-guides--suppress-snapping
          (not (and shape (shape-artist-diagram-rectangle-p shape))))
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
           (x-snap (and (/= dx 0)
                        shape-artist-guides-snap-enabled
                        (shape-artist-guides--best-correction
                         'vertical
                         (if moving-left
                             `((,target-left left))
                           `((,target-right right))))))
           (y-snap (and (/= dy 0)
                        shape-artist-guides-snap-enabled
                        (shape-artist-guides--best-correction
                         'horizontal
                         (if moving-top
                             `((,target-top top))
                           `((,target-bottom bottom))))))
           (grid-x (and (/= dx 0)
                        (not x-snap)
                        (shape-artist-guides--grid-correction
                         (if moving-left target-left target-right)
                         'vertical
                         (if moving-left 'grid-left 'grid-right))))
           (grid-y (and (/= dy 0)
                        (not y-snap)
                        (shape-artist-guides--grid-correction
                         (if moving-top target-top target-bottom)
                         'horizontal
                         (if moving-top 'grid-top 'grid-bottom))))
           (snapped-dx dx)
           (snapped-dy dy))
      (when (or x-snap grid-x)
        (let ((position (or (and x-snap
                                 (shape-artist-guide-position
                                  (plist-get x-snap :guide)))
                            (plist-get grid-x :position))))
          (setq snapped-dx
                (if moving-left
                    (- old-x position)
                  (- position old-x old-w)))))
      (when (or y-snap grid-y)
        (let ((position (or (and y-snap
                                 (shape-artist-guide-position
                                  (plist-get y-snap :guide)))
                            (plist-get grid-y :position))))
          (setq snapped-dy
                (if moving-top
                    (- old-y position)
                  (- position old-y old-h)))))
      ;; Do not clamp here: the existing resize implementation owns the
      ;; minimum-size user error and should keep signalling it unchanged.
      (let ((report (list :source 'rectangle-resize
                          :requested (list dx dy)
                          :snapped (list snapped-dx snapped-dy)
                          :x x-snap
                          :y y-snap
                          :grid-x grid-x
                          :grid-y grid-y)))
        (when (or x-snap y-snap grid-x grid-y)
          (setq shape-artist-guides--last-snap-report report))
        (list snapped-dx snapped-dy report)))))

(defun shape-artist-guides--grid-active-p ()
  "Return non-nil when grid snapping may affect operations."
  (and shape-artist-grid-snap-enabled
       (not shape-artist-guides--suppress-snapping)
       (> shape-artist-grid-size 0)))

(defun shape-artist-guides--grid-rounded-position (position)
  "Return POSITION rounded to the nearest configured grid line."
  (* shape-artist-grid-size
     (round (/ (float position) shape-artist-grid-size))))

(defun shape-artist-guides--grid-correction (position orientation edge)
  "Return grid correction plist for POSITION, ORIENTATION, and EDGE."
  (when (shape-artist-guides--grid-active-p)
    (let* ((rounded (shape-artist-guides--grid-rounded-position position))
           (correction (- rounded position)))
      (unless (= correction 0)
        (list :orientation orientation
              :edge edge
              :grid t
              :position rounded
              :distance (abs correction)
              :correction correction)))))

(defun shape-artist-guides--grid-snap-delta (value delta)
  "Return adjusted DELTA so VALUE+DELTA lands on nearest grid line."
  (if (not (shape-artist-guides--grid-active-p))
      delta
    (let* ((target (+ value delta))
           (rounded (shape-artist-guides--grid-rounded-position target)))
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
  (when shape-artist-guides--guides
    (shape-artist-guides--record-history))
  (setq shape-artist-guides--guides nil)
  (setq shape-artist-guides--last-snap-report nil)
  (shape-artist-guides-clear-overlays)
  (shape-artist--maybe-update-sidebar)
  (message "Cleared Shape Artist guides"))

(defun shape-artist-guide-toggle-visibility ()
  "Toggle guide overlay visibility."
  (interactive)
  (shape-artist-guides--record-history)
  (setq shape-artist-guides-visible (not shape-artist-guides-visible))
  (shape-artist-guides-refresh-overlays)
  (shape-artist--maybe-update-sidebar)
  (message "Guide overlays %s" (if shape-artist-guides-visible "visible" "hidden")))

(defun shape-artist-guides--current-selection ()
  "Return selected or locked shapes, using shape helpers when available."
  (cond
   ((and (boundp 'shape-artist--selected-shape-signatures)
         shape-artist--selected-shape-signatures
         (fboundp 'shape-artist-shapes-selected))
    (shape-artist-shapes-selected))
   ((and (boundp 'shape-artist--locked-shape) shape-artist--locked-shape)
    (list shape-artist--locked-shape))
   (t nil)))

(defun shape-artist-guides--shape-signatures (shapes)
  "Return parse signatures for SHAPES when signature helpers are loaded."
  (when (fboundp 'shape-artist-shapes-signature)
    (cl-remove nil (mapcar #'shape-artist-shapes-signature shapes))))

(defun shape-artist-guides--adopt-signatures (guide signatures)
  "Append SIGNATURES to GUIDE adoption list, avoiding duplicates."
  (let ((existing (shape-artist-guide-adopted-shape-signatures guide)))
    (dolist (signature signatures)
      (cl-pushnew signature existing :test #'equal))
    (setf (shape-artist-guide-adopted-shape-signatures guide) existing)))

(defun shape-artist-guides--resolve-adopted-shapes (guide)
  "Resolve adopted shapes of GUIDE from the latest parse results."
  (when (fboundp 'shape-artist-shapes-find-by-signature)
    (cl-remove nil
               (mapcar #'shape-artist-shapes-find-by-signature
                       (shape-artist-guide-adopted-shape-signatures guide)))))

(defun shape-artist-guides--move-adopted-shapes (guide delta)
  "Move shapes adopted by GUIDE by DELTA along the guide axis."
  (when (and (/= delta 0)
             (fboundp 'shape-artist-shapes-move-group))
    (let ((shapes (shape-artist-guides--resolve-adopted-shapes guide)))
      (when shapes
        (let* ((dx (if (eq (shape-artist-guide-orientation guide) 'vertical)
                       delta 0))
               (dy (if (eq (shape-artist-guide-orientation guide) 'horizontal)
                       delta 0))
               (shape-artist-guides--suppress-snapping t)
               (moved (shape-artist-shapes-move-group shapes dx dy))
               (signatures (shape-artist-guides--shape-signatures moved)))
          (when signatures
            (setf (shape-artist-guide-adopted-shape-signatures guide)
                  signatures))
          moved)))))

(defun shape-artist-guides--nearest-guide-at-point ()
  "Return the guide nearest to point, preferring horizontal ties."
  (let* ((pos (shape-artist--current-row-col))
         (h (shape-artist-guides-find-nearest 'horizontal (car pos)))
         (v (shape-artist-guides-find-nearest 'vertical (cdr pos)))
         (h-dist (and h (shape-artist-guides--distance
                         (shape-artist-guide-position h) (car pos))))
         (v-dist (and v (shape-artist-guides--distance
                         (shape-artist-guide-position v) (cdr pos)))))
    (cond
     ((and h v) (if (<= h-dist v-dist) h v))
     (h h)
     (v v))))

(defun shape-artist-guide-adopt-selection-nearest (&optional id)
  "Adopt the current selected or locked shapes into nearest guide or guide ID."
  (interactive)
  (let* ((guide (or (and id (shape-artist-guides--guide-at-id id))
                    (shape-artist-guides--nearest-guide-at-point)))
         (shapes (shape-artist-guides--current-selection))
         (signatures (shape-artist-guides--shape-signatures shapes)))
    (cond
     ((not guide)
      (message "No guide to adopt into"))
     ((not signatures)
      (message "No selected or locked shapes to adopt"))
     (t
      (shape-artist-guides--record-history)
      (shape-artist-guides--adopt-signatures guide signatures)
      (shape-artist--maybe-update-sidebar)
      (message "Guide %d adopted %d shape(s)"
               (shape-artist-guide-id guide)
               (length signatures))))))

(defun shape-artist-guide-clear-adoptions (&optional id)
  "Clear adopted shape signatures from guide ID, or from all guides."
  (interactive)
  (shape-artist-guides--record-history)
  (if id
      (let ((guide (shape-artist-guides--guide-at-id id)))
        (when guide
          (setf (shape-artist-guide-adopted-shape-signatures guide) nil)))
    (dolist (guide shape-artist-guides--guides)
      (setf (shape-artist-guide-adopted-shape-signatures guide) nil)))
  (shape-artist--maybe-update-sidebar)
  (message "Cleared guide adoptions"))

(defun shape-artist-guides--select-guide (&optional id prompt)
  "Return guide ID or prompt with PROMPT."
  (or (and id (shape-artist-guides--guide-at-id id))
      (let* ((choices (mapcar
                       (lambda (g)
                         (cons (format "%d %s %d %s"
                                       (shape-artist-guide-id g)
                                       (shape-artist-guide-orientation g)
                                       (shape-artist-guide-position g)
                                       (or (shape-artist-guide-label g) ""))
                               g))
                       shape-artist-guides--guides))
             (choice (and choices
                          (completing-read (or prompt "Guide: ")
                                           choices nil t))))
        (cdr (assoc choice choices)))))

(defun shape-artist-guide-set-label (&optional id label)
  "Set guide ID display LABEL.  Empty LABEL clears the guide label."
  (interactive)
  (let ((guide (shape-artist-guides--select-guide id "Guide label for: ")))
    (if (not guide)
        (message "No guide selected")
      (let ((new-label (or label
                           (read-string "Guide label: "
                                        (or (shape-artist-guide-label guide)
                                            "")))))
        (shape-artist-guides--record-history)
        (setf (shape-artist-guide-label guide)
              (unless (string-empty-p new-label) new-label))
        (shape-artist-guides-refresh-overlays)
        (shape-artist--maybe-update-sidebar)
        (message "Guide %d label %s"
                 (shape-artist-guide-id guide)
                 (or (shape-artist-guide-label guide) "cleared"))))))

(defun shape-artist-guide-set-snap-distance (&optional id snap-distance-value)
  "Set guide ID snap SNAP-DISTANCE-VALUE.  Nil SNAP-DISTANCE-VALUE clears the per-guide override."
  (interactive)
  (let ((guide (shape-artist-guides--select-guide id "Guide snap distance for: ")))
    (if (not guide)
        (message "No guide selected")
      (let* ((raw (unless snap-distance-value
                    (if current-prefix-arg
                        ""
                      (read-string
                       "Snap distance (empty = default): "
                       (when (shape-artist-guide-snap-distance guide)
                         (number-to-string
                          (shape-artist-guide-snap-distance guide)))))))
             (new-distance (cond
                            (snap-distance-value snap-distance-value)
                            ((string-empty-p raw) nil)
                            (t (string-to-number raw)))))
        (when (and new-distance (< new-distance 0))
          (user-error "Snap distance must be non-negative"))
        (shape-artist-guides--record-history)
        (setf (shape-artist-guide-snap-distance guide) new-distance)
        (shape-artist--maybe-update-sidebar)
        (message "Guide %d snap distance %s"
                 (shape-artist-guide-id guide)
                 (if new-distance
                     (number-to-string new-distance)
                   "default"))))))

(defun shape-artist-guide-toggle-nearest-visibility (&optional id)
  "Toggle visibility of guide ID, or the guide nearest to point."
  (interactive)
  (let ((guide (or (and id (shape-artist-guides--guide-at-id id))
                   (shape-artist-guides--nearest-guide-at-point))))
    (if (not guide)
        (message "No guide selected")
      (shape-artist-guides--record-history)
      (setf (shape-artist-guide-visible guide)
            (not (shape-artist-guide-visible guide)))
      (shape-artist-guides-refresh-overlays)
      (shape-artist--maybe-update-sidebar)
      (message "Guide %d %s"
               (shape-artist-guide-id guide)
               (if (shape-artist-guide-visible guide) "visible" "hidden")))))

(defun shape-artist-guide-edit-properties (&optional id)
  "Edit position, label, visibility, and snap distance of guide ID."
  (interactive)
  (let ((guide (shape-artist-guides--select-guide id "Edit guide: ")))
    (if (not guide)
        (message "No guide selected")
      (let* ((position (read-number "Position: "
                                    (shape-artist-guide-position guide)))
             (label (read-string "Label: "
                                 (or (shape-artist-guide-label guide) "")))
             (visible (y-or-n-p "Visible? "))
             (snap-raw (read-string
                        "Snap distance override (empty = default): "
                        (when (shape-artist-guide-snap-distance guide)
                          (number-to-string
                           (shape-artist-guide-snap-distance guide)))))
             (snap-distance (unless (string-empty-p snap-raw)
                              (string-to-number snap-raw))))
        (when (and snap-distance (< snap-distance 0))
          (user-error "Snap distance must be non-negative"))
        (shape-artist-guides--record-history)
        (setf (shape-artist-guide-position guide) (max 0 position))
        (setf (shape-artist-guide-label guide)
              (unless (string-empty-p label) label))
        (setf (shape-artist-guide-visible guide) visible)
        (setf (shape-artist-guide-snap-distance guide) snap-distance)
        (shape-artist-guides-refresh-overlays)
        (shape-artist--maybe-update-sidebar)
        (message "Edited guide %d" (shape-artist-guide-id guide))))))

(defun shape-artist-guide-move (&optional id new-position)
  "Move guide ID to NEW-POSITION and drag adopted shapes along its axis."
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
      (let* ((old-position (shape-artist-guide-position guide))
             (pos (max 0 (or new-position
                             (read-number "New guide position: "
                                          old-position))))
             (delta (- pos old-position)))
        (shape-artist-guides--record-history)
        (let ((moved (shape-artist-guides--move-adopted-shapes guide delta)))
        (setf (shape-artist-guide-position guide) pos)
        (shape-artist-guides-refresh-overlays)
        (shape-artist--maybe-update-sidebar)
        (message "Moved guide %d to %d%s"
                 (shape-artist-guide-id guide)
                 pos
                 (if moved
                     (format ", moved %d adopted shape(s)" (length moved))
                   "")))))))

(defun shape-artist-guide-selection-to-guide ()
  "Create guides from current shape selection or locked shape bounds."
  (interactive)
  (let* ((shapes (shape-artist-guides--current-selection))
         (signatures (shape-artist-guides--shape-signatures shapes))
         (bounds (shape-artist-guides--bounds-union shapes)))
    (if (not bounds)
        (message "No selected or locked shape to convert to guides")
      (shape-artist-guides--record-history)
      (pcase-let* ((`(,left ,top ,right ,bottom) bounds)
                   (created (let ((shape-artist-guides--history-inhibit t))
                              (list
                               (shape-artist-guides-add
                                'vertical left "selection-left" 'selection)
                               (shape-artist-guides-add
                                'vertical right "selection-right" 'selection)
                               (shape-artist-guides-add
                                'horizontal top "selection-top" 'selection)
                               (shape-artist-guides-add
                                'horizontal bottom "selection-bottom" 'selection)))))
        (dolist (guide created)
          (shape-artist-guides--adopt-signatures guide signatures))
        (shape-artist-guides-refresh-overlays)
        (shape-artist--maybe-update-sidebar)
        (message "Created guides from selection bounds (%d,%d)-(%d,%d)"
                 left top right bottom)))))

(defun shape-artist-snap-toggle ()
  "Toggle guide snapping for the current buffer."
  (interactive)
  (shape-artist-guides--record-history)
  (setq shape-artist-guides-snap-enabled (not shape-artist-guides-snap-enabled))
  (shape-artist--maybe-update-sidebar)
  (message "Guide snapping %s" (if shape-artist-guides-snap-enabled "enabled" "disabled")))

(defun shape-artist-grid-toggle ()
  "Toggle grid overlay visibility for the current buffer."
  (interactive)
  (shape-artist-guides--record-history)
  (setq shape-artist-grid-visible (not shape-artist-grid-visible))
  (shape-artist-guides-refresh-grid-overlays)
  (shape-artist--maybe-update-sidebar)
  (message "Shape Artist grid %s" (if shape-artist-grid-visible "visible" "hidden")))

(defun shape-artist-grid-set-size (&optional size)
  "Set grid SIZE for overlays and optional grid snapping."
  (interactive (list (read-number "Grid size: " shape-artist-grid-size)))
  (when (< size 1)
    (user-error "Grid size must be at least 1"))
  (shape-artist-guides--record-history)
  (setq shape-artist-grid-size size)
  (shape-artist-guides-refresh-grid-overlays)
  (shape-artist--maybe-update-sidebar)
  (message "Shape Artist grid size set to %d" size))

(defun shape-artist-grid-snap-toggle ()
  "Toggle snap-to-grid for the current buffer."
  (interactive)
  (shape-artist-guides--record-history)
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
                        (format "  #%d %-10s %d %s%s%s\n"
                                (shape-artist-guide-id guide)
                                (shape-artist-guide-orientation guide)
                                (shape-artist-guide-position guide)
                                (if (shape-artist-guide-visible guide) "" "hidden ")
                                (or (shape-artist-guide-label guide) "")
                                (let ((n (length
                                          (shape-artist-guide-adopted-shape-signatures
                                           guide))))
                                  (if (> n 0)
                                      (format " adopted=%d" n)
                                    ""))))))
      (setq text (concat text "  (none)\n")))
    (concat text (or (shape-artist-guides--snap-report-line) ""))))

(provide 'shape-artist-guides)
;;; shape-artist-guides.el ends here
