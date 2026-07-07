;;; shape-artist-shapes.el --- Shape operations: label editing, move, resize, replace -*- lexical-binding: t; -*-

;;; Commentary:

;;; Integrates shape parsing and provides concrete shape operations:
;;; label editing, move, resize, and border-style replacement.

;;; Code:

(require 'cl-lib)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-core)

;;; --- Shape Parsing Integration ---

(defun shape-artist-shapes-parse ()
  "Re-parse shapes in current buffer and update buffer-local state."
  (interactive)
  (let ((result (shape-artist-diagram-parse-buffer)))
    (setq shape-artist--last-root (car result))
    (setq shape-artist--last-shapes-list (cdr result))
    (message "Detected %d shapes" (length shape-artist--last-shapes-list))
    (shape-artist--maybe-update-sidebar)))

;;; --- Shape Stepping ---

(defun shape-artist-shapes-current ()
  "Return the current shape candidate or nil."
  (when (and shape-artist--last-shapes-list
             (< shape-artist--select-index (length shape-artist--last-shapes-list)))
    (nth shape-artist--select-index shape-artist--last-shapes-list)))

(defun shape-artist-shapes-count ()
  "Return the number of detected shapes."
  (length shape-artist--last-shapes-list))

;;; --- Rectangle Buffer Primitives ---

(defun shape-artist--read-rect-interior (x y w h)
  "Return interior text of rect at (X,Y) size W×H.
Interior has (1- H) rows by (1- W) columns.  Trailing whitespace is
trimmed."
  (let ((irows (max 0 (1- h)))
        (icols (max 0 (1- w)))
        (lines '()))
    (save-excursion
      (dotimes (dr irows)
        (let ((row (+ y 1 dr))
              (line ""))
          (shape-artist--goto-row-col row (+ x w))
          (shape-artist--goto-row-col row (+ x 1))
          (dotimes (_ icols)
            (setq line (concat line (string (or (char-after) ?\s))))
            (forward-char 1))
          (push (string-trim-right line) lines))))
    (nreverse lines)))

(defun shape-artist--erase-rect (x y w h)
  "Overwrite every cell of the rect at (X,Y) size W×H with spaces."
  (save-excursion
    (dotimes (dr (1+ h))
      (let ((row (+ y dr)))
        (shape-artist--goto-row-col row (+ x w))
        (shape-artist--goto-row-col row x)
        (delete-char (1+ w))
        (insert (make-string (1+ w) ?\s))))))

(defun shape-artist--draw-rect-border (x y w h &optional h-char)
  "Draw rectangle border at (X,Y) size W×H.
H-CHAR is the horizontal fill character (default ?-)."
  (let ((hc (or h-char ?-)))
    (save-excursion
      ;; Top border
      (shape-artist--goto-row-col y (+ x w))
      (shape-artist--goto-row-col y x)
      (delete-char (1+ w))
      (insert ?+ (make-string (1- w) hc) ?+)
      ;; Middle rows: only touch the two border columns
      (dotimes (dr (1- h))
        (let ((row (+ y 1 dr)))
          (shape-artist--goto-row-col row (+ x w))
          (shape-artist--goto-row-col row x)
          (delete-char 1)
          (insert ?|)
          (shape-artist--goto-row-col row (+ x w))
          (delete-char 1)
          (insert ?|)))
      ;; Bottom border
      (shape-artist--goto-row-col (+ y h) (+ x w))
      (shape-artist--goto-row-col (+ y h) x)
      (delete-char (1+ w))
      (insert ?+ (make-string (1- w) hc) ?+))))

(defun shape-artist--write-rect-interior (x y w h lines)
  "Write LINES into interior of rect at (X,Y) size W×H.
Text is truncated or padded with spaces to fit the interior dimensions."
  (let ((irows (max 0 (1- h)))
        (icols (max 0 (1- w))))
    (save-excursion
      (dotimes (dr irows)
        (let* ((raw (or (nth dr lines) ""))
               (text (if (> (length raw) icols)
                         (substring raw 0 icols)
                       (concat raw (make-string (- icols (length raw)) ?\s)))))
          (shape-artist--goto-row-col (+ y 1 dr) (+ x w))
          (shape-artist--goto-row-col (+ y 1 dr) (+ x 1))
          (delete-char icols)
          (insert text))))))

(defun shape-artist--shift-all-shapes (dcol drow)
  "Shift every known shape's EIEIO position by DCOL columns and DROW rows."
  (when (or (/= dcol 0) (/= drow 0))
    (dolist (s shape-artist--last-shapes-list)
      (oset s x (+ (oref s x) dcol))
      (oset s y (+ (oref s y) drow))
      (when (shape-artist-diagram-edge-p s)
        (oset s x2 (+ (oref s x2) dcol))
        (oset s y2 (+ (oref s y2) drow))))))

(defun shape-artist--shapes-commit-pass (primary-shape &optional h-char)
  "Redraw all known rectangles from EIEIO, PRIMARY-SHAPE drawn last.
Drawing PRIMARY-SHAPE last ensures it wins any overlap with other shapes.
H-CHAR is the horizontal border char for PRIMARY-SHAPE (default ?-)."
  (dolist (s shape-artist--last-shapes-list)
    (when (and (shape-artist-diagram-rectangle-p s)
               (not (eq s primary-shape)))
      (shape-artist--draw-rect-border
       (oref s x) (oref s y) (oref s width) (oref s height))))
  (when (and primary-shape (shape-artist-diagram-rectangle-p primary-shape))
    (shape-artist--draw-rect-border
     (oref primary-shape x) (oref primary-shape y)
     (oref primary-shape width) (oref primary-shape height)
     h-char))
  (shape-artist--ensure-grid-rectangular))

;;; --- Label Editing ---

(defun shape-artist-shapes-edit-label (shape &optional align)
  "Edit the label of SHAPE.  ALIGN is \\='left, \\='center, or \\='right (default: \\='center)."
  (let* ((align (or align 'center))
         (rect shape)
         (x (oref rect x))
         (y (oref rect y))
         (w (oref rect width))
         (h (oref rect height))
         (interior-width  (max 0 (1- w)))
         (interior-height (max 0 (1- h)))
         (labels (cl-remove-if-not
                  (lambda (s)
                    (and (shape-artist-diagram-label-p s)
                         (>= (oref s x) (1+ x))
                         (<= (oref s x) (+ x w -1))
                         (>= (oref s y) (1+ y))
                         (<= (oref s y) (+ y h -1))))
                  shape-artist--last-shapes-list))
         (prompt-text (if labels (oref (car labels) text) "")))
    (let ((new-text (read-string "Label text: " prompt-text)))
      (when new-text
        ;; Clear all interior rows
        (save-excursion
          (dotimes (dr interior-height)
            (shape-artist--goto-row-col (+ y 1 dr) (+ x 1))
            (delete-char interior-width)
            (insert (make-string interior-width ?\s))))
        ;; Insert text (first interior row only)
        (let ((text-len (length new-text)))
          (if (<= text-len interior-width)
              (let ((start-col (pcase align
                                 ('left  (+ x 1))
                                 ('right (+ x 1 (- interior-width text-len)))
                                 (_      (+ x 1 (/ (- interior-width text-len) 2))))))
                (save-excursion
                  (shape-artist--goto-row-col (+ y 1) start-col)
                  (delete-char text-len)
                  (insert new-text)))
            ;; Text wider than interior — auto-expand right
            (let ((needed-width (- text-len interior-width)))
              (shape-artist--expand-rect-right rect needed-width)
              (save-excursion
                (shape-artist--goto-row-col (+ y 1) (+ x 1))
                (delete-char text-len)
                (insert new-text))))))
      (shape-artist-shapes-parse))))

(defun shape-artist--expand-rect-right (rect amount)
  "Expand RECT to the right by AMOUNT columns."
  (let* ((x (oref rect x))
         (y (oref rect y))
         (w (oref rect width))
         (h (oref rect height)))
    (dotimes (_ amount)
      (shape-artist--expand-right))
    (save-excursion
      ;; Top border
      (shape-artist--goto-row-col y x)
      (delete-char (1+ w))
      (insert ?+ (make-string (+ w amount -1) ?-) ?+)
      ;; Middle rows
      (dotimes (dr (1- h))
        (shape-artist--goto-row-col (+ y 1 dr) x)
        (delete-char 1)
        (insert ?|)
        (shape-artist--goto-row-col (+ y 1 dr) (+ x w))
        (delete-char 1)
        (insert (make-string amount ?\s) ?|))
      ;; Bottom border
      (shape-artist--goto-row-col (+ y h) x)
      (delete-char (1+ w))
      (insert ?+ (make-string (+ w amount -1) ?-) ?+))
    (oset rect width (+ w amount))
    (shape-artist--ensure-grid-rectangular)))

;;; --- Move ---

(defun shape-artist-shapes-move (shape dx dy)
  "Move SHAPE by DX columns and DY rows on the canvas.
Interior text is carried to the new position.  Other shapes are redrawn
from their EIEIO data to restore any borders the move erases."
  (interactive
   (list (shape-artist-shapes-current)
         (read-number "dx: " 0)
         (read-number "dy: " 0)))
  (when (and shape (shape-artist-diagram-rectangle-p shape))
    (let* ((w (oref shape width))
           (h (oref shape height))
           (interior (shape-artist--read-rect-interior
                      (oref shape x) (oref shape y) w h))
           (need-left (max 0 (- (+ (oref shape x) dx))))
           (need-top  (max 0 (- (+ (oref shape y) dy)))))
      ;; Expand canvas and shift all EIEIO positions to keep them valid
      (when (> need-left 0)
        (dotimes (_ need-left) (shape-artist--expand-left))
        (shape-artist--shift-all-shapes need-left 0))
      (when (> need-top 0)
        (dotimes (_ need-top) (shape-artist--expand-top))
        (shape-artist--shift-all-shapes 0 need-top))
      ;; Erase old rect (EIEIO positions now reflect any canvas expansion)
      (shape-artist--erase-rect (oref shape x) (oref shape y) w h)
      ;; Advance EIEIO to new position
      (oset shape x (+ (oref shape x) dx))
      (oset shape y (+ (oref shape y) dy))
      ;; Redraw all shapes; moved shape drawn last so it wins any overlap
      (shape-artist--shapes-commit-pass shape)
      ;; Restore label content at new position
      (shape-artist--write-rect-interior
       (oref shape x) (oref shape y) w h interior)
      (shape-artist-shapes-parse))))

;;; --- Resize ---

(defun shape-artist-shapes-resize (shape new-width new-height)
  "Resize SHAPE to NEW-WIDTH × NEW-HEIGHT.
Interior text is preserved, truncated, or padded to fit new dimensions.
Minimum allowed size is 2×2."
  (interactive
   (list (shape-artist-shapes-current)
         (read-number "New width: ")
         (read-number "New height: ")))
  (when (and shape (shape-artist-diagram-rectangle-p shape))
    (when (or (< new-width 2) (< new-height 2))
      (user-error "Rectangle must be at least 2×2"))
    (let* ((x (oref shape x))
           (y (oref shape y))
           (interior (shape-artist--read-rect-interior
                      x y (oref shape width) (oref shape height))))
      (shape-artist--erase-rect x y (oref shape width) (oref shape height))
      (oset shape width  new-width)
      (oset shape height new-height)
      (shape-artist--shapes-commit-pass shape)
      (shape-artist--write-rect-interior x y new-width new-height interior)
      (shape-artist-shapes-parse))))

;;; --- Replace Border Style ---

(defun shape-artist-shapes-replace (shape style)
  "Change SHAPE's horizontal border character.
STYLE is one of: \\='standard (+---+), \\='heavy (+=====+), or
\\='light (+....+)."
  (interactive
   (list (shape-artist-shapes-current)
         (intern (completing-read "Border style: "
                                  '("standard" "heavy" "light") nil t))))
  (when (and shape (shape-artist-diagram-rectangle-p shape))
    (let ((h-char (pcase style
                    ('heavy ?=)
                    ('light ?.)
                    (_      ?-))))
      (shape-artist--draw-rect-border
       (oref shape x) (oref shape y)
       (oref shape width) (oref shape height)
       h-char)
      (shape-artist--ensure-grid-rectangular)
      (shape-artist-shapes-parse))))

;;; --- Provide ---

(provide 'shape-artist-shapes)
;;; shape-artist-shapes.el ends here
