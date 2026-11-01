;;; shape-artist-core.el --- Core grid helpers, movement, anchor, preview, commit, draw mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; This module provides core functionality for ASCII diagram editing,
;;; including grid helpers, canvas expansion, movement commands,
;;; drawing with preview, and shape commit operations.

;;; Code:

(require 'cl-lib)
(require 'picture)
(require 'whitespace)
(require 'artist)

(defgroup shape-artist nil
  "ASCII diagram artist mode."
  :group 'convenience
  :prefix "shape-artist-")

(defcustom shape-artist-default-padding '(1 1 1 1)
  "Default padding list (TOP RIGHT BOTTOM LEFT) for normalization."
  :type '(list integer integer integer integer)
  :group 'shape-artist)

(declare-function evil-define-state "evil")
(declare-function evil-normal-state "evil")
(declare-function evil-change-state "evil")
(declare-function shape-artist-select-start "shape-artist-select")
(declare-function shape-artist--setup-evil-keymaps "shape-artist-keys")

;;; --- Evil State Definitions ---

(defvar shape-artist--evil-states-initialized nil
  "Non-nil once custom Evil states for shape-artist are defined.")

(defun shape-artist--maybe-define-evil-states ()
  "Define custom Evil states once when Evil is available.

`evil-define-state' is a macro.  Keep the forms behind `eval' so this
package still byte-compiles cleanly when Evil is not loaded during build."
  (when (and (not shape-artist--evil-states-initialized)
             (fboundp 'evil-define-state))
    (eval
     '(evil-define-state artist
        "Artist draw state."
        :tag " <ART-DRAW> "
        :cursor (hbar . 4)
        :suppress-keymap t
        :message "-- ARTIST DRAW --"
        :entry-hook nil
        :exit-hook nil))

    (eval
     '(evil-define-state select
        "Artist select state."
        :tag " <ART-SELECT> "
        :cursor (hollow . 4)
        :suppress-keymap t
        :message "-- ARTIST SELECT --"
        :entry-hook nil
        :exit-hook nil))

    (eval
     '(evil-define-state shape-edit
        "Artist shape-edit state."
        :tag " <ART-EDIT> "
        :cursor (box . 4)
        :suppress-keymap t
        :message "-- ARTIST SHAPE-EDIT --"
        :entry-hook nil
        :exit-hook nil))

    (setq shape-artist--evil-states-initialized t)))

;;; --- Variables ---

(defvar-local shape-artist--mode 'move
  "Current artist mode: \\='move, \\='draw, \\='select, or \\='shape-edit.")

(defvar-local shape-artist--anchor nil
  "Anchor point as \\='(ROW . COL) or nil.")

(defvar-local shape-artist--preview-overlays nil
  "List of overlays used for shape preview.")

(defvar-local shape-artist--draw-char ?\s
  "Current draw character.")

(defvar-local shape-artist--previous-cursor-pos nil
  "Previous cursor position as \\='(ROW . COL) for draw mode.")

(defvar-local shape-artist--pre-select-pos nil
  "Cursor position before entering SELECT mode.")

(defvar-local shape-artist--last-root nil
  "Last detected shape tree root.")

(defvar-local shape-artist--last-shapes-list nil
  "Flat list of detected shapes.")

(defvar-local shape-artist--select-index 0
  "Current index in shape candidate list for SELECT mode.")

(defvar-local shape-artist--locked-shape nil
  "Currently locked-in shape for SHAPE-EDIT mode.")

(defvar-local shape-artist--selected-shape-signatures nil
  "Stable signatures of selected shapes for grouped SHAPE-EDIT operations.")

(defconst shape-artist-border-styles
  '(standard heavy light rounded unicode-single unicode-rounded unicode-double)
  "Rectangle border styles Shape Artist can both detect and draw.
Kept in step with the families the parser recognises; a test enforces that.")

(defconst shape-artist-edge-styles '(plain arrow-end arrow-both)
  "Connector styles a straight edge can be given.")

(defcustom shape-artist-default-style 'standard
  "Border style DRAW mode uses for newly drawn rectangles.
Straight lines are always drawn with plain ASCII `-' and `|' so the parser
keeps recognising them as connectors."
  :type '(choice (const :tag "ASCII + - |" standard)
                 (const :tag "ASCII heavy +=+" heavy)
                 (const :tag "ASCII light +.+" light)
                 (const :tag "ASCII rounded /-\\" rounded)
                 (const :tag "Unicode single" unicode-single)
                 (const :tag "Unicode rounded" unicode-rounded)
                 (const :tag "Unicode double" unicode-double))
  :group 'shape-artist)

(defface shape-artist-preview-face
  '((t :inherit highlight :extend t))
  "Face for shape preview overlays."
  :group 'shape-artist)

(defface shape-artist-mode-move-face
  '((t :inherit success :weight bold))
  "Face for MOVE mode indicators."
  :group 'shape-artist)

(defface shape-artist-mode-draw-face
  '((t :inherit error :weight bold))
  "Face for DRAW mode indicators."
  :group 'shape-artist)

(defface shape-artist-mode-select-face
  '((t :inherit warning :weight bold))
  "Face for SELECT mode indicators."
  :group 'shape-artist)

(defface shape-artist-mode-edit-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for SHAPE-EDIT mode indicators."
  :group 'shape-artist)

(defface shape-artist-select-current-face
  '((t :inherit highlight :weight bold :extend t))
  "Face for the current SELECT candidate."
  :group 'shape-artist)

(defface shape-artist-select-marked-face
  '((t :inherit secondary-selection :weight semi-bold :extend t))
  "Face for shapes retained in a multi-selection."
  :group 'shape-artist)

(defface shape-artist-edit-target-face
  '((t :inherit region :weight bold :inverse-video t :extend t))
  "Face for the active SHAPE-EDIT target or group."
  :group 'shape-artist)

(defface shape-artist-handle-face
  '((t :inherit highlight :inverse-video t :weight bold))
  "Face for the corner handles of the rectangle being edited."
  :group 'shape-artist)

(defface shape-artist-ghost-face
  '((t :inherit secondary-selection :extend t))
  "Face for the preview outline shown while dragging a shape."
  :group 'shape-artist)

(defconst shape-artist--mode-specs
  '((move
     :label "MOVE"
     :activity "NAVIGATE"
     :face shape-artist-mode-move-face
     :cursor box
     :hint "h/j/k/l move · SPC draw · C-n select · ? actions")
    (draw
     :label "DRAW"
     :activity "CREATE"
     :face shape-artist-mode-draw-face
     :cursor (hbar . 3)
     :hint "h/j/k/l draw · SPC anchor · RET commit · ESC cancel")
    (select
     :label "SELECT"
     :activity "PICK"
     :face shape-artist-mode-select-face
     :cursor hollow
     :hint "n/p candidate · t mark · RET edit · ESC cancel")
    (shape-edit
     :label "EDIT"
     :activity "TRANSFORM"
     :face shape-artist-mode-edit-face
     :cursor box
     :hint "H/J/K/L move · </> +/- resize · x style · ESC finish"))
  "Single source of truth for Shape Artist modal presentation.")

(defvar shape-artist-mode-changed-hook nil
  "Hook run after a Shape Artist mode transition.

Hook functions receive OLD-MODE and NEW-MODE as arguments.")

(defvar-local shape-artist--mode-line-segment ""
  "Mode-line segment string.")

(defconst shape-artist--mode-line-form
  '(:eval (shape-artist--mode-line-segment-fn))
  "Mode-line form appended while `shape-artist-core-mode' is active.")

(defconst shape-artist--header-line-form
  '(:eval (shape-artist--header-line-segment-fn))
  "Header-line form displayed while `shape-artist-core-mode' is active.")

(defun shape-artist--mode-spec (&optional mode)
  "Return presentation specification for MODE or the current mode."
  (cdr (assq (or mode shape-artist--mode) shape-artist--mode-specs)))

(defun shape-artist--mode-property (property &optional mode)
  "Return PROPERTY from MODE's presentation specification."
  (plist-get (shape-artist--mode-spec mode) property))

(defvar evil-local-mode)

(defun shape-artist--sync-evil-state (mode)
  "Synchronize optional Evil state with Shape Artist MODE.
Only when Evil is active in this buffer: merely having Evil loaded, with
`evil-mode' off, must not start driving its state machine."
  (cond
   ((not (bound-and-true-p evil-local-mode)) nil)
   ((eq mode 'move)
    (when (fboundp 'evil-normal-state)
      (evil-normal-state)))
   ((fboundp 'evil-change-state)
    (evil-change-state
     (pcase mode
       ('draw 'artist)
       ('select 'select)
       ('shape-edit 'shape-edit))))))

(defun shape-artist--enter-mode (mode &optional skip-evil)
  "Enter MODE and synchronize all visual state.

When SKIP-EVIL is non-nil, do not request an Evil state transition."
  (unless (assq mode shape-artist--mode-specs)
    (user-error "Unknown Shape Artist mode: %S" mode))
  (let ((old-mode shape-artist--mode))
    (setq shape-artist--mode mode)
    ;; Let optional Evil update its state first, then re-assert the Shape Artist
    ;; presentation contract so Evil cursor defaults cannot override mode cues.
    (unless skip-evil
      (shape-artist--sync-evil-state mode))
    (setq-local cursor-type (shape-artist--mode-property :cursor mode))
    (shape-artist--update-mode-line)
    (force-mode-line-update t)
    (shape-artist--maybe-update-sidebar)
    (unless (eq old-mode mode)
      (run-hook-with-args 'shape-artist-mode-changed-hook old-mode mode))))

;;; --- Grid Helpers ---

(defun shape-artist--line-count ()
  "Return the number of lines in the current buffer."
  (if (= (point-min) (point-max))
      0
    (count-lines (point-min) (point-max))))

(defun shape-artist--max-line-length ()
  "Return the maximum line length in the current buffer."
  (save-excursion
    (let ((max-len 0))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((len (- (line-end-position) (line-beginning-position))))
          (when (> len max-len)
            (setq max-len len)))
        (forward-line 1))
      max-len)))

(defun shape-artist--current-row-col ()
  "Return current cursor position as (row . col), 0-based."
  (let ((row (1- (line-number-at-pos)))
        (col (current-column)))
    (cons row col)))

(defun shape-artist--point-at-row-col (row col)
  "Return the buffer position at ROW and COL."
  (save-excursion
    (goto-char (point-min))
    (forward-line row)
    (move-to-column col t)
    (point)))

(defun shape-artist--goto-row-col (row col)
  "Move point to ROW and COL, padding line if necessary."
  (let ((line-count (shape-artist--line-count)))
    ;; Ensure we have enough lines
    (when (>= row line-count)
      (save-excursion
        (goto-char (point-max))
        (dotimes (_ (- row line-count -1))
          (insert "\n"))))
    ;; Go to the line
    (goto-char (point-min))
    (forward-line row)
    ;; Pad line if necessary (need col+1 chars for 0-based column)
    (let ((line-len (- (line-end-position) (line-beginning-position))))
      (when (< line-len (1+ col))
        (end-of-line)
        (insert (make-string (- (1+ col) line-len) ?\s))))
    (move-to-column col)))

;;; --- Canvas Expansion ---

(defun shape-artist--expand-bottom ()
  "Insert a blank line at the bottom if on the last line."
  (let ((max-len (shape-artist--max-line-length)))
    (save-excursion
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (make-string max-len ?\s) "\n"))))

(defun shape-artist--expand-top ()
  "Insert a new blank line at the top, shifting content down."
  (let ((max-len (shape-artist--max-line-length)))
    (save-excursion
      (goto-char (point-min))
      (insert (make-string max-len ?\s) "\n"))))

(defun shape-artist--expand-right ()
  "Insert a trailing space on every line, maintaining rectangular invariant."
  (shape-artist--ensure-grid-rectangular)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (insert ?\s)
      (forward-line 1))))

(defun shape-artist--expand-left ()
  "Insert a leading space on every line."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (insert ?\s)
      (forward-line 1))))

(defun shape-artist--ensure-grid-rectangular ()
  "Ensure all lines have equal length (rectangular grid invariant)."
  (let ((max-len (shape-artist--max-line-length)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-len (- (line-end-position) (line-beginning-position))))
          (when (< line-len max-len)
            (end-of-line)
            (insert (make-string (- max-len line-len) ?\s))))
        (forward-line 1)))))

;;; --- Movement (MOVE mode) ---

(defun shape-artist-move-left ()
  "Move cursor left, expanding canvas if at column 0."
  (interactive)
  (let ((pos (shape-artist--current-row-col)))
    (if (<= (cdr pos) 0)
        (progn
          (shape-artist--expand-left)
          (shape-artist--goto-row-col (car pos) 0))
      (shape-artist--goto-row-col (car pos) (1- (cdr pos)))))
  (shape-artist--update-mode-line)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist-move-right ()
  "Move cursor right, expanding canvas if at right edge."
  (interactive)
  (let* ((pos (shape-artist--current-row-col))
         (max-len (shape-artist--max-line-length)))
    (if (>= (cdr pos) (1- max-len))
        (progn
          (shape-artist--expand-right)
          (shape-artist--goto-row-col (car pos) (1+ (cdr pos))))
      (shape-artist--goto-row-col (car pos) (1+ (cdr pos)))))
  (shape-artist--update-mode-line)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist-move-up ()
  "Move cursor up, expanding canvas if at row 0."
  (interactive)
  (let ((pos (shape-artist--current-row-col)))
    (if (<= (car pos) 0)
        (progn
          (shape-artist--expand-top)
          (shape-artist--goto-row-col 0 (cdr pos)))
      (shape-artist--goto-row-col (1- (car pos)) (cdr pos))))
  (shape-artist--update-mode-line)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist-move-down ()
  "Move cursor down, expanding canvas if at bottom."
  (interactive)
  (let* ((pos (shape-artist--current-row-col))
         (line-count (shape-artist--line-count)))
    (if (>= (car pos) (1- line-count))
        (progn
          (shape-artist--expand-bottom)
          (shape-artist--goto-row-col (1+ (car pos)) (cdr pos)))
      (shape-artist--goto-row-col (1+ (car pos)) (cdr pos))))
  (shape-artist--update-mode-line)
  (shape-artist--maybe-update-sidebar))

;;; --- Movement (DRAW mode) ---

(defun shape-artist--draw-char-for-direction (dr dc)
  "Return the appropriate draw character for a movement of DR, DC."
  (cond
   ((and (= dr 0) (/= dc 0)) ?-)   ; horizontal
   ((and (/= dr 0) (= dc 0)) ?|)   ; vertical
   (t ?+)))                         ; diagonal

(defun shape-artist-draw-move-left ()
  "Move left in DRAW mode, printing draw char at previous position."
  (interactive)
  (let* ((pos (shape-artist--current-row-col))
         (row (car pos))
         (col (cdr pos)))
    ;; Place character at current position
    (shape-artist--place-draw-char row col 0 -1)
    ;; Move
    (if (<= col 0)
        (progn
          (shape-artist--expand-left)
          (shape-artist--goto-row-col row 0))
      (shape-artist--goto-row-col row (1- col))))
  (shape-artist--update-mode-line)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist-draw-move-right ()
  "Move right in DRAW mode, printing draw char at previous position."
  (interactive)
  (let* ((pos (shape-artist--current-row-col))
         (row (car pos))
         (col (cdr pos))
         (max-len (shape-artist--max-line-length)))
    ;; Place character at current position
    (shape-artist--place-draw-char row col 0 1)
    ;; Move
    (if (>= col (1- max-len))
        (progn
          (shape-artist--expand-right)
          (shape-artist--goto-row-col row (1+ col)))
      (shape-artist--goto-row-col row (1+ col))))
  (shape-artist--update-mode-line)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist-draw-move-up ()
  "Move up in DRAW mode, printing draw char at previous position."
  (interactive)
  (let* ((pos (shape-artist--current-row-col))
         (row (car pos))
         (col (cdr pos)))
    ;; Place character at current position
    (shape-artist--place-draw-char row col -1 0)
    ;; Move
    (if (<= row 0)
        (progn
          (shape-artist--expand-top)
          (shape-artist--goto-row-col 0 col))
      (shape-artist--goto-row-col (1- row) col)))
  (shape-artist--update-mode-line)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist-draw-move-down ()
  "Move down in DRAW mode, printing draw char at previous position."
  (interactive)
  (let* ((pos (shape-artist--current-row-col))
         (row (car pos))
         (col (cdr pos))
         (line-count (shape-artist--line-count)))
    ;; Place character at current position
    (shape-artist--place-draw-char row col 1 0)
    ;; Move
    (if (>= row (1- line-count))
        (progn
          (shape-artist--expand-bottom)
          (shape-artist--goto-row-col (1+ row) col))
      (shape-artist--goto-row-col (1+ row) col)))
  (shape-artist--update-mode-line)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist--place-draw-char (row col dr dc)
  "Place the appropriate draw char at ROW,COL based on direction DR,DC."
  (let ((ch (shape-artist--draw-char-for-direction dr dc)))
    (save-excursion
      (shape-artist--goto-row-col row col)
      (when (not (eolp))
        (delete-char 1)
        (insert ch)))))

;;; --- Anchor & Preview ---

(defun shape-artist-set-anchor-here ()
  "Set anchor at current position."
  (interactive)
  (setq shape-artist--anchor (shape-artist--current-row-col))
  (message "Anchor set at (%d, %d)" (car shape-artist--anchor) (cdr shape-artist--anchor))
  (shape-artist--update-mode-line)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist-enter-draw-and-anchor ()
  "Enter DRAW mode and set anchor at current position."
  (interactive)
  (setq shape-artist--anchor (shape-artist--current-row-col))
  (message "Anchor set at (%d, %d)" (car shape-artist--anchor) (cdr shape-artist--anchor))
  (shape-artist-enter-draw))

(defun shape-artist-enter-draw ()
  "Enter DRAW mode (no anchor)."
  (interactive)
  (shape-artist--enter-mode 'draw))

(defun shape-artist--clear-preview-state ()
  "Clear all preview overlays and anchor."
  (mapc #'delete-overlay shape-artist--preview-overlays)
  (setq shape-artist--preview-overlays nil)
  (setq shape-artist--anchor nil))

(defun shape-artist-update-preview ()
  "Update preview overlays from anchor to current position."
  (when shape-artist--anchor
    ;; Clear existing overlays
    (mapc #'delete-overlay shape-artist--preview-overlays)
    (setq shape-artist--preview-overlays nil)
    (let* ((anchor shape-artist--anchor)
           (current (shape-artist--current-row-col))
           (r1 (min (car anchor) (car current)))
           (c1 (min (cdr anchor) (cdr current)))
           (r2 (max (car anchor) (car current)))
           (c2 (max (cdr anchor) (cdr current))))
      ;; Create overlay for the rectangular region
      (dotimes (r (- r2 r1 -1))
        (let* ((row (+ r1 r))
               (start (shape-artist--point-at-row-col row c1))
               (end (shape-artist--point-at-row-col row (1+ c2))))
          (when (and start end (<= start end))
            (let ((ov (make-overlay start end)))
              (overlay-put ov 'face 'shape-artist-preview-face)
              (overlay-put ov 'priority 100)
              (push ov shape-artist--preview-overlays))))))))

;;; --- Commit ---

(defun shape-artist--rectangle-style-chars (style)
  "Return border character plist for STYLE or legacy horizontal char."
  (if (characterp style)
      (list :tl ?+ :tr ?+ :bl ?+ :br ?+ :h style :v ?|)
    (pcase style
      ('rounded
       (list :tl ?/ :tr ?\\ :bl ?\\ :br ?/ :h ?- :v ?|))
      ('unicode-single
       (list :tl ?┌ :tr ?┐ :bl ?└ :br ?┘ :h ?─ :v ?│))
      ('unicode-rounded
       (list :tl ?╭ :tr ?╮ :bl ?╰ :br ?╯ :h ?─ :v ?│))
      ('unicode-double
       (list :tl ?╔ :tr ?╗ :bl ?╚ :br ?╝ :h ?═ :v ?║))
      ('heavy
       (list :tl ?+ :tr ?+ :bl ?+ :br ?+ :h ?= :v ?|))
      ('light
       (list :tl ?+ :tr ?+ :bl ?+ :br ?+ :h ?. :v ?|))
      (_
       (list :tl ?+ :tr ?+ :bl ?+ :br ?+ :h ?- :v ?|)))))

(defun shape-artist--commit-shape ()
  "Commit the previewed shape by drawing actual characters into buffer."
  (when shape-artist--anchor
    (let* ((anchor shape-artist--anchor)
           (current (shape-artist--current-row-col))
           (r1 (min (car anchor) (car current)))
           (c1 (min (cdr anchor) (cdr current)))
           (r2 (max (car anchor) (car current)))
           (c2 (max (cdr anchor) (cdr current)))
           (height (- r2 r1))
           (width (- c2 c1)))
      (save-excursion
        (cond
         ;; Single point - do nothing
         ((and (= r1 r2) (= c1 c2))
          nil)
         ;; Horizontal line
         ((= r1 r2)
          (shape-artist--goto-row-col r1 c1)
          (delete-char (1+ width))
          (insert (make-string (1+ width) ?-)))
         ;; Vertical line
         ((= c1 c2)
          (dotimes (r (1+ height))
            (shape-artist--goto-row-col (+ r1 r) c1)
            (delete-char 1)
            (insert ?|)))
         ;; Rectangle
         (t
          (let* ((chars (shape-artist--rectangle-style-chars
                         shape-artist-default-style))
                 (tl (plist-get chars :tl)) (tr (plist-get chars :tr))
                 (bl (plist-get chars :bl)) (br (plist-get chars :br))
                 (hc (plist-get chars :h)) (vc (plist-get chars :v)))
            ;; Top border
            (shape-artist--goto-row-col r1 c1)
            (delete-char (1+ width))
            (insert tl (make-string (1- width) hc) tr)
            ;; Middle rows
            (dotimes (r (1- height))
              (shape-artist--goto-row-col (+ r1 1 r) c1)
              (delete-char 1)
              (insert vc)
              (shape-artist--goto-row-col (+ r1 1 r) c2)
              (delete-char 1)
              (insert vc))
            ;; Bottom border
            (shape-artist--goto-row-col r2 c1)
            (delete-char (1+ width))
            (insert bl (make-string (1- width) hc) br))))))
    (shape-artist--clear-preview-state)
    (shape-artist--ensure-grid-rectangular)
    ;; Re-run shape detection
    (when (fboundp 'shape-artist-shapes-parse)
      (shape-artist-shapes-parse))))

(defun shape-artist-commit-and-exit ()
  "Commit shape and return to MOVE mode."
  (interactive)
  (shape-artist--commit-shape)
  (shape-artist-enter-move))

(defun shape-artist-commit-stay ()
  "Commit shape and stay in current mode."
  (interactive)
  (shape-artist--commit-shape)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist-cancel-and-exit ()
  "Cancel preview and return to MOVE mode."
  (interactive)
  (shape-artist--clear-preview-state)
  (shape-artist-enter-move))

(defun shape-artist-pick-tool ()
  "Pick a drawing tool (character)."
  (interactive)
  (let ((ch (read-char "Draw char: ")))
    (setq shape-artist--draw-char ch)
    (message "Draw char set to: %c" ch)))

(defun shape-artist-undo ()
  "Undo last change."
  (interactive)
  (undo))

(defun shape-artist-commit-preview ()
  "Commit previewed shape in MOVE mode."
  (interactive)
  (shape-artist--commit-shape)
  (shape-artist--maybe-update-sidebar))

;;; --- Mode Transitions ---

(defun shape-artist-enter-move ()
  "Enter MOVE mode."
  (interactive)
  (shape-artist--enter-mode 'move))

;;; --- Normalization ---

(defun shape-artist-normalize (&optional padding title)
  "Normalize the diagram: trim, pad, and optionally box with TITLE.

PADDING is a list (TOP RIGHT BOTTOM LEFT) or nil for defaults.
TITLE is a string or nil.  When nil, skip the box."
  (interactive
   (let* ((pad-str (read-string "Padding (top right bottom left): " "1 1 1 1"))
          (pad (mapcar #'string-to-number (split-string pad-str)))
          (title (read-string "Title (empty to skip): " nil)))
     (list pad (unless (string-empty-p title) title))))
  (let ((pad (or padding shape-artist-default-padding)))
    ;; Step 1: Shrink - remove all-blank rows/columns
    (shape-artist--shrink)
    ;; Step 2: Add padding
    (shape-artist--add-padding (nth 0 pad) (nth 1 pad) (nth 2 pad) (nth 3 pad))
    ;; Step 3: Box if title given
    (when title
      (shape-artist--box-with-title title))
    (shape-artist--ensure-grid-rectangular)))

(defun shape-artist--shrink ()
  "Remove all-blank rows above/below content and all-blank columns left/right."
  (let ((lines (shape-artist--get-lines)))
    ;; Remove blank rows from top
    (while (and lines (shape-artist--line-blank-p (car lines)))
      (setq lines (cdr lines)))
    ;; Remove blank rows from bottom
    (setq lines (nreverse lines))
    (while (and lines (shape-artist--line-blank-p (car lines)))
      (setq lines (cdr lines)))
    (setq lines (nreverse lines))
    ;; Find leftmost and rightmost non-blank columns
    (when lines
      (let ((min-col most-positive-fixnum)
            (max-col 0))
        (dolist (line lines)
          (dotimes (i (length line))
            (unless (eq (aref line i) ?\s)
              (setq min-col (min min-col i))
              (setq max-col (max max-col i)))))
        (when (< min-col most-positive-fixnum)
          (setq lines (mapcar (lambda (line)
                                (substring line min-col (1+ max-col)))
                              lines)))))
    ;; Always write back — clears buffer when all lines were blank.
    (shape-artist--set-lines lines)))

(defun shape-artist--line-blank-p (line)
  "Return t if LINE is all spaces."
  (string-match-p "\\`[[:space:]]*\\'" line))

(defun shape-artist--get-lines ()
  "Return buffer content as a list of strings."
  (save-excursion
    (goto-char (point-min))
    (let ((lines nil))
      (while (not (eobp))
        (push (buffer-substring-no-properties (line-beginning-position) (line-end-position))
              lines)
        (forward-line 1))
      (nreverse lines))))

(defun shape-artist--set-lines (lines)
  "Replace buffer content with LINES."
  (erase-buffer)
  (dolist (line lines)
    (insert line "\n"))
  ;; Remove trailing newline
  (when (and (> (buffer-size) 0)
             (eq (char-before (point-max)) ?\n))
    (delete-char -1)))

(defun shape-artist--add-padding (top right bottom left)
  "Add padding: TOP lines, RIGHT cols, BOTTOM lines, LEFT cols."
  (let* ((lines (shape-artist--get-lines))
         (max-len (shape-artist--max-line-length))
         (padded-width (+ left max-len right)))
    ;; Add left and right in a single pass so max-len refers to original lengths.
    (setq lines (mapcar (lambda (line)
                          (concat (make-string left ?\s)
                                  line
                                  (make-string (+ right (- max-len (length line))) ?\s)))
                        lines))
    ;; Add top padding
    (dotimes (_ top)
      (push (make-string padded-width ?\s) lines))
    ;; Add bottom padding
    (dotimes (_ bottom)
      (setq lines (append lines (list (make-string padded-width ?\s)))))
    (shape-artist--set-lines lines)))

(defun shape-artist--box-with-title (title)
  "Draw a box around content with TITLE in top border."
  (let* ((lines (shape-artist--get-lines))
         (max-len 0))
    (dolist (line lines)
      (setq max-len (max max-len (length line))))
    ;; Ensure all lines same length
    (setq lines (mapcar (lambda (line)
                          (concat line (make-string (- max-len (length line)) ?\s)))
                        lines))
    ;; Build top border with title.
    ;; Side lines are "| " + content + "|" = max-len + 3 chars wide.
    ;; Top: "+" + "-" + title + dashes + "+" must also be max-len + 3.
    ;; => dashes = max-len - title-len.
    (let* ((title-len (length title))
           (top-border (concat "+-" title (make-string (max 0 (- max-len title-len)) ?-) "+"))
           (bottom-border (concat "+" (make-string (1+ max-len) ?-) "+"))
           (boxed-lines (list top-border)))
      ;; Add side borders
      (dolist (line lines)
        (push (concat "| " line "|") boxed-lines))
      (push bottom-border boxed-lines)
      (shape-artist--set-lines (nreverse boxed-lines)))))

;;; --- Mode Keymap Dispatch ---

(defun shape-artist--dispatch-select-start ()
  "Enter SELECT mode (available in MOVE mode)."
  (interactive)
  (when (eq shape-artist--mode 'move)
    (shape-artist-select-start)))

(defun shape-artist--dispatch-commit-stay ()
  "Commit and stay (available in DRAW mode)."
  (interactive)
  (when (eq shape-artist--mode 'draw)
    (shape-artist-commit-stay)))

(defun shape-artist--dispatch-undo ()
  "Undo (available in DRAW mode)."
  (interactive)
  (when (eq shape-artist--mode 'draw)
    (shape-artist-undo)))

;;; --- Mode-Line ---

(defun shape-artist--mode-line-segment-fn ()
  "Return the mode-line segment string based on current mode."
  (let ((label (shape-artist--mode-property :label))
        (face (shape-artist--mode-property :face)))
    (if label
        (propertize (format " SA:%s " label) 'face face)
      "")))

(defun shape-artist--header-line-segment-fn ()
  "Return a prominent mode badge and concise context hint."
  (let ((label (shape-artist--mode-property :label))
        (activity (shape-artist--mode-property :activity))
        (face (shape-artist--mode-property :face))
        (hint (shape-artist--mode-property :hint)))
    (if label
        (concat
         (propertize (format " %s · %s " label activity) 'face face)
         (propertize (format "  %s" hint) 'face 'shadow))
      "")))

(defun shape-artist--update-mode-line ()
  "Update the mode-line segment based on current mode."
  (setq shape-artist--mode-line-segment (shape-artist--mode-line-segment-fn))
  (force-mode-line-update))

;;; --- Sidebar Update Hook ---

(defvar shape-artist--update-sidebar-fn nil
  "Function to call to update sidebar, set by shape-artist-ui.el.")

(defun shape-artist--maybe-update-sidebar ()
  "Call sidebar update function if set."
  (when shape-artist--update-sidebar-fn
    (funcall shape-artist--update-sidebar-fn)))

;;; --- Post-Command Hook ---

(defvar-local shape-artist--sidebar-timer nil
  "Idle timer for sidebar updates.")

(defun shape-artist--post-command ()
  "Run after every command in artist mode."
  (when shape-artist--anchor
    (shape-artist-update-preview))
  (shape-artist--update-mode-line)
  ;; Debounce sidebar updates using idle timer.
  ;; Capture the buffer at schedule time so the timer fires in the right context.
  (when shape-artist--update-sidebar-fn
    (when shape-artist--sidebar-timer
      (cancel-timer shape-artist--sidebar-timer))
    (let ((buf (current-buffer)))
      (setq shape-artist--sidebar-timer
            (run-with-idle-timer 0.1 nil
                                 (lambda ()
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (shape-artist--maybe-update-sidebar)))))))))

;;; --- Lifecycle ---

(defvar-local shape-artist--saved-cursor-type nil
  "Saved cursor type before entering artist mode.")

(defvar-local shape-artist--saved-mode-line-format nil
  "Saved mode-line format before entering artist mode.")

(defvar-local shape-artist--saved-header-line-format nil
  "Saved header-line format before entering artist mode.")

(defvar-local shape-artist--saved-whitespace-style nil
  "Saved whitespace style before entering artist mode.")

(defvar-local shape-artist--enabled nil
  "Non-nil while `shape-artist--enable' has run and not yet been undone.
Makes enabling idempotent and disabling safe on never-enabled buffers.")

(defvar-local shape-artist--entered-picture-mode nil
  "Non-nil when artist mode switched this buffer into `picture-mode'.
Only then does disabling leave `picture-mode' again, restoring the buffer's
previous major mode, local map, and mode name.")

;;;###autoload
(define-minor-mode shape-artist-core-mode
  "Minor mode for ASCII diagram editing on top of `picture-mode'."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (set-keymap-parent map (when (boundp 'shape-artist-move-map)
                                      shape-artist-move-map))
            ;; Mode-specific dispatch bindings
            (define-key map (kbd "C-n") #'shape-artist--dispatch-select-start)
            (define-key map (kbd "C-RET") #'shape-artist--dispatch-commit-stay)
            (define-key map (kbd "C-<return>") #'shape-artist--dispatch-commit-stay)
            map)
  (if shape-artist-core-mode
      (shape-artist--enable)
    (shape-artist--disable)))

(defun shape-artist--enable ()
  "Enable artist mode setup.
Calling this while artist mode is already set up is a harmless no-op."
  (unless shape-artist--enabled
    (shape-artist--enable-1)))

(defun shape-artist--enable-1 ()
  "Perform the one-time setup for `shape-artist--enable'."
  (shape-artist--maybe-define-evil-states)
  (when (fboundp 'shape-artist--setup-evil-keymaps)
    (shape-artist--setup-evil-keymaps))
  ;; Picture mode supplies the quarter-plane editing model.  Enter it only when
  ;; the buffer is not already in it, and remember that we did so, so disabling
  ;; can restore the previous major mode (and never touches a picture mode the
  ;; user chose themselves).
  (unless (eq major-mode 'picture-mode)
    (picture-mode)
    (setq shape-artist--entered-picture-mode t))
  ;; Save state (after picture-mode has run)
  (setq shape-artist--saved-cursor-type cursor-type)
  (setq shape-artist--saved-mode-line-format mode-line-format)
  (setq shape-artist--saved-header-line-format header-line-format)
  (setq shape-artist--saved-whitespace-style
        (and (boundp 'whitespace-style) whitespace-style))
  ;; Enable whitespace visualization using the standard face-driven renderer.
  (setq-local whitespace-style '(spaces space-mark))
  (whitespace-mode 1)
  ;; Add mode-line segment
  (unless (member shape-artist--mode-line-form mode-line-format)
    (setq mode-line-format
          (append mode-line-format (list shape-artist--mode-line-form))))
  ;; Give the active editing context a stable, always-visible visual anchor.
  (setq-local header-line-format
              (append (list shape-artist--header-line-form)
                      (when shape-artist--saved-header-line-format
                        (list "  " shape-artist--saved-header-line-format))))
  ;; Set initial mode through the central transition contract.
  (shape-artist--enter-mode 'move)
  ;; Add post-command hook (buffer-local)
  (add-hook 'post-command-hook #'shape-artist--post-command nil t)
  ;; Add picture-mode exit hook for cleanup (US-13.3)
  (add-hook 'picture-mode-exit-hook #'shape-artist--disable nil t)
  ;; Ensure grid is rectangular
  (shape-artist--ensure-grid-rectangular)
  (setq shape-artist--enabled t))

(defun shape-artist--disable ()
  "Disable artist mode and clean up.
Safe to call when artist mode is not set up, and when it is called as
`picture-mode-exit-hook' because the user left picture mode directly."
  (when shape-artist--enabled
    (shape-artist--disable-1))
  ;; The hook path reaches here with the minor mode variable still set.
  (setq shape-artist-core-mode nil))

(defun shape-artist--disable-1 ()
  "Undo everything `shape-artist--enable-1' set up."
  ;; Remove hooks
  (remove-hook 'post-command-hook #'shape-artist--post-command t)
  (remove-hook 'picture-mode-exit-hook #'shape-artist--disable t)
  ;; Cancel idle timer
  (when shape-artist--sidebar-timer
    (cancel-timer shape-artist--sidebar-timer)
    (setq shape-artist--sidebar-timer nil))
  ;; Clear preview and optional guide/grid overlays.
  (shape-artist--clear-preview-state)
  (when (fboundp 'shape-artist-guides-clear-overlays)
    (shape-artist-guides-clear-overlays))
  (when (fboundp 'shape-artist-shape-edit-clear-overlays)
    (shape-artist-shape-edit-clear-overlays))
  ;; Restore cursor
  (setq cursor-type shape-artist--saved-cursor-type)
  ;; Restore presentation exactly as it was after entering picture-mode.
  (setq mode-line-format shape-artist--saved-mode-line-format)
  (setq header-line-format shape-artist--saved-header-line-format)
  ;; Disable whitespace
  (whitespace-mode -1)
  (setq whitespace-style shape-artist--saved-whitespace-style)
  ;; Reset mode
  (setq shape-artist--mode nil)
  (setq shape-artist--enabled nil)
  ;; Leave picture mode only if we entered it.  NOSTRIP keeps trailing
  ;; whitespace: the diagram text must never change just because we left.
  (when (and shape-artist--entered-picture-mode
             (eq major-mode 'picture-mode))
    (setq shape-artist--entered-picture-mode nil)
    (picture-mode-exit t))
  (setq shape-artist--entered-picture-mode nil)
  (force-mode-line-update))

(defun shape-artist-quit ()
  "Exit artist mode and return to the buffer's previous major mode."
  (interactive)
  (shape-artist-core-mode -1))

;;; --- Provide ---

(provide 'shape-artist-core)
;;; shape-artist-core.el ends here
