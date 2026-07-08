;;; shape-artist-shapes.el --- Shape operations: label, move, resize, style -*- lexical-binding: t; -*-

;;; Commentary:

;;; Concrete shape operations for parsed rectangles and straight edges.

;;; Code:

(require 'cl-lib)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-core)

(defvar shape-artist--selected-shape-signatures nil)

(defun shape-artist-shapes-parse ()
  "Re-parse shapes in current buffer and update buffer-local state."
  (interactive)
  (let ((result (shape-artist-diagram-parse-buffer)))
    (setq shape-artist--last-root (car result))
    (setq shape-artist--last-shapes-list (cdr result))
    (message "Detected %d shapes" (length shape-artist--last-shapes-list))
    (shape-artist--maybe-update-sidebar)
    result))

(defun shape-artist-shapes-current ()
  "Return the current shape candidate or nil."
  (when (and shape-artist--last-shapes-list
             (>= shape-artist--select-index 0)
             (< shape-artist--select-index (length shape-artist--last-shapes-list)))
    (nth shape-artist--select-index shape-artist--last-shapes-list)))

(defun shape-artist-shapes-count ()
  "Return the number of detected shapes."
  (length shape-artist--last-shapes-list))

(defun shape-artist-shapes-signature (shape)
  "Return a stable parse signature for SHAPE."
  (cond
   ((shape-artist-diagram-rectangle-p shape)
    (list 'rectangle (oref shape x) (oref shape y)
          (oref shape width) (oref shape height)))
   ((shape-artist-diagram-edge-p shape)
    (list 'edge (oref shape x) (oref shape y)
          (oref shape x2) (oref shape y2)
          (and (slot-boundp shape 'arrow-start) (oref shape arrow-start))
          (and (slot-boundp shape 'arrow-end) (oref shape arrow-end))))
   ((shape-artist-diagram-label-p shape)
    (list 'label (oref shape x) (oref shape y) (oref shape text)))
   (shape
    (list 'node (oref shape x) (oref shape y)
          (oref shape width) (oref shape height)))))

(defun shape-artist-shapes-find-by-signature (signature)
  "Return parsed shape matching SIGNATURE, or nil."
  (cl-find-if
   (lambda (shape) (equal (shape-artist-shapes-signature shape) signature))
   shape-artist--last-shapes-list))

(defun shape-artist-shapes-selected ()
  "Return currently selected shapes resolved from signatures."
  (cl-remove nil
             (mapcar #'shape-artist-shapes-find-by-signature
                     shape-artist--selected-shape-signatures)))

(defun shape-artist-shapes--find-rectangle (x y width height)
  "Return parsed rectangle matching X, Y, WIDTH, and HEIGHT."
  (cl-find-if
   (lambda (shape)
     (and (shape-artist-diagram-rectangle-p shape)
          (= (oref shape x) x)
          (= (oref shape y) y)
          (= (oref shape width) width)
          (= (oref shape height) height)))
   shape-artist--last-shapes-list))

(defun shape-artist-shapes--find-edge (x y x2 y2)
  "Return parsed edge matching X, Y, X2, and Y2."
  (cl-find-if
   (lambda (shape)
     (and (shape-artist-diagram-edge-p shape)
          (= (oref shape x) x)
          (= (oref shape y) y)
          (= (oref shape x2) x2)
          (= (oref shape y2) y2)))
   shape-artist--last-shapes-list))

(defun shape-artist-shapes-border-style (shape)
  "Return SHAPE border style."
  (when (and shape (shape-artist-diagram-rectangle-p shape))
    (save-excursion
      (shape-artist--goto-row-col (oref shape y) (1+ (oref shape x)))
      (pcase (char-after)
        (?= 'heavy)
        (?. 'light)
        (_ 'standard)))))

(defun shape-artist-shapes-edge-style (edge)
  "Return EDGE connector style."
  (when (and edge (shape-artist-diagram-edge-p edge))
    (cond
     ((and (slot-boundp edge 'arrow-start) (oref edge arrow-start)
           (slot-boundp edge 'arrow-end) (oref edge arrow-end))
      'arrow-both)
     ((and (slot-boundp edge 'arrow-end) (oref edge arrow-end))
      'arrow-end)
     (t 'plain))))

(defun shape-artist--space-string (n)
  "Return a string of N spaces."
  (make-string n 32))

(defun shape-artist--set-cell (row col char)
  "Set cell at ROW and COL to CHAR."
  (save-excursion
    (shape-artist--goto-row-col row col)
    (delete-char 1)
    (insert char)))

(defun shape-artist--read-rect-interior (x y w h)
  "Return interior text of rect at X Y W H."
  (let ((irows (max 0 (1- h)))
        (icols (max 0 (1- w)))
        (lines nil))
    (save-excursion
      (dotimes (dr irows)
        (let ((line ""))
          (shape-artist--goto-row-col (+ y 1 dr) (+ x w))
          (shape-artist--goto-row-col (+ y 1 dr) (+ x 1))
          (dotimes (_ icols)
            (setq line (concat line (string (or (char-after) 32))))
            (forward-char 1))
          (push (string-trim-right line) lines))))
    (nreverse lines)))

(defun shape-artist--erase-rect (x y w h)
  "Erase rect at X Y W H."
  (save-excursion
    (dotimes (dr (1+ h))
      (shape-artist--goto-row-col (+ y dr) (+ x w))
      (shape-artist--goto-row-col (+ y dr) x)
      (delete-char (1+ w))
      (insert (shape-artist--space-string (1+ w))))))

(defun shape-artist--draw-rect-border (x y w h &optional h-char)
  "Draw rectangle border at X Y W H with optional H-CHAR."
  (let ((hc (or h-char ?-)))
    (save-excursion
      (shape-artist--goto-row-col y (+ x w))
      (shape-artist--goto-row-col y x)
      (delete-char (1+ w))
      (insert ?+ (make-string (1- w) hc) ?+)
      (dotimes (dr (1- h))
        (shape-artist--goto-row-col (+ y 1 dr) (+ x w))
        (shape-artist--goto-row-col (+ y 1 dr) x)
        (delete-char 1)
        (insert ?|)
        (shape-artist--goto-row-col (+ y 1 dr) (+ x w))
        (delete-char 1)
        (insert ?|))
      (shape-artist--goto-row-col (+ y h) (+ x w))
      (shape-artist--goto-row-col (+ y h) x)
      (delete-char (1+ w))
      (insert ?+ (make-string (1- w) hc) ?+))))

(defun shape-artist--write-rect-interior (x y w h lines)
  "Write LINES into rect interior at X Y W H."
  (let ((irows (max 0 (1- h)))
        (icols (max 0 (1- w))))
    (save-excursion
      (dotimes (dr irows)
        (let* ((raw (or (nth dr lines) ""))
               (text (if (> (length raw) icols)
                         (substring raw 0 icols)
                       (concat raw (shape-artist--space-string (- icols (length raw)))))))
          (shape-artist--goto-row-col (+ y 1 dr) (+ x w))
          (shape-artist--goto-row-col (+ y 1 dr) (+ x 1))
          (delete-char icols)
          (insert text))))))

(defun shape-artist--shift-all-shapes (dcol drow)
  "Shift known shape objects by DCOL and DROW."
  (when (or (/= dcol 0) (/= drow 0))
    (dolist (s shape-artist--last-shapes-list)
      (oset s x (+ (oref s x) dcol))
      (oset s y (+ (oref s y) drow))
      (when (shape-artist-diagram-edge-p s)
        (oset s x2 (+ (oref s x2) dcol))
        (oset s y2 (+ (oref s y2) drow))))))

(defun shape-artist--edge-horizontal-p (edge)
  "Return non-nil when EDGE is horizontal."
  (= (oref edge y) (oref edge y2)))

(defun shape-artist--edge-vertical-p (edge)
  "Return non-nil when EDGE is vertical."
  (= (oref edge x) (oref edge x2)))

(defun shape-artist--draw-edge (x y x2 y2 &optional style)
  "Draw a straight edge from X Y to X2 Y2 using STYLE."
  (let ((style (or style 'plain)))
    (cond
     ((= y y2)
      (let ((left (min x x2))
            (right (max x x2)))
        (dotimes (i (1+ (- right left)))
          (shape-artist--set-cell y (+ left i) ?-))
        (pcase style
          ('arrow-end (shape-artist--set-cell y right ?>))
          ('arrow-both
           (shape-artist--set-cell y left ?<)
           (shape-artist--set-cell y right ?>)))))
     ((= x x2)
      (let ((top (min y y2))
            (bottom (max y y2)))
        (dotimes (i (1+ (- bottom top)))
          (shape-artist--set-cell (+ top i) x ?|))
        (pcase style
          ('arrow-end (shape-artist--set-cell bottom x ?v))
          ('arrow-both
           (shape-artist--set-cell top x ?^)
           (shape-artist--set-cell bottom x ?v)))))))
  (shape-artist--ensure-grid-rectangular))

(defun shape-artist--erase-edge (x y x2 y2)
  "Erase a straight edge from X Y to X2 Y2."
  (cond
   ((= y y2)
    (dotimes (i (1+ (- (max x x2) (min x x2))))
      (shape-artist--set-cell y (+ (min x x2) i) 32)))
   ((= x x2)
    (dotimes (i (1+ (- (max y y2) (min y y2))))
      (shape-artist--set-cell (+ (min y y2) i) x 32)))))

(defun shape-artist--style-char (style)
  "Return horizontal char for border STYLE."
  (pcase style
    ('heavy ?=)
    ('light ?.)
    (_ ?-)))

(defun shape-artist--redraw-other-rectangles (primary)
  "Redraw rectangles other than PRIMARY from current parse data."
  (dolist (s shape-artist--last-shapes-list)
    (when (and (shape-artist-diagram-rectangle-p s) (not (eq s primary)))
      (shape-artist--draw-rect-border
       (oref s x) (oref s y) (oref s width) (oref s height)
       (shape-artist--style-char (shape-artist-shapes-border-style s))))))

(defun shape-artist--reparse-and-find (shape)
  "Reparse and find SHAPE by its current geometry."
  (let ((sig (shape-artist-shapes-signature shape)))
    (shape-artist-shapes-parse)
    (or (shape-artist-shapes-find-by-signature sig) shape)))

(defun shape-artist-shapes-edit-label (shape &optional align)
  "Edit label inside rectangle SHAPE using ALIGN."
  (when (and shape (shape-artist-diagram-rectangle-p shape))
    (let* ((align (or align 'center))
           (x (oref shape x))
           (y (oref shape y))
           (w (oref shape width))
           (h (oref shape height))
           (interior-width (max 0 (1- w)))
           (interior-height (max 0 (1- h)))
           (new-text (read-string "Label text: " "")))
      (save-excursion
        (dotimes (dr interior-height)
          (shape-artist--goto-row-col (+ y 1 dr) (+ x 1))
          (delete-char interior-width)
          (insert (shape-artist--space-string interior-width))))
      (when (> (length new-text) interior-width)
        (setq shape (shape-artist-shapes-resize shape (1+ (length new-text)) h))
        (setq x (oref shape x) y (oref shape y) w (oref shape width)
              interior-width (max 0 (1- w))))
      (let ((start-col (pcase align
                         ('left (+ x 1))
                         ('right (+ x 1 (- interior-width (length new-text))))
                         (_ (+ x 1 (/ (- interior-width (length new-text)) 2))))))
        (save-excursion
          (shape-artist--goto-row-col (+ y 1) start-col)
          (delete-char (length new-text))
          (insert new-text)))
      (shape-artist--reparse-and-find shape))))

(defun shape-artist-shapes-move (shape dx dy)
  "Move SHAPE by DX columns and DY rows."
  (interactive
   (list (shape-artist-shapes-current)
         (read-number "dx: " 0)
         (read-number "dy: " 0)))
  (cond
   ((shape-artist-diagram-rectangle-p shape)
    (shape-artist-shapes--move-rectangle shape dx dy))
   ((shape-artist-diagram-edge-p shape)
    (shape-artist-shapes--move-edge shape dx dy))))

(defun shape-artist-shapes--move-rectangle (shape dx dy)
  "Move rectangle SHAPE by DX and DY."
  (let* ((w (oref shape width))
         (h (oref shape height))
         (style (shape-artist-shapes-border-style shape))
         (interior (shape-artist--read-rect-interior (oref shape x) (oref shape y) w h))
         (need-left (max 0 (- (+ (oref shape x) dx))))
         (need-top (max 0 (- (+ (oref shape y) dy)))))
    (when (> need-left 0)
      (dotimes (_ need-left) (shape-artist--expand-left))
      (shape-artist--shift-all-shapes need-left 0))
    (when (> need-top 0)
      (dotimes (_ need-top) (shape-artist--expand-top))
      (shape-artist--shift-all-shapes 0 need-top))
    (shape-artist--erase-rect (oref shape x) (oref shape y) w h)
    (oset shape x (+ (oref shape x) dx))
    (oset shape y (+ (oref shape y) dy))
    (shape-artist--redraw-other-rectangles shape)
    (shape-artist--draw-rect-border (oref shape x) (oref shape y) w h
                                    (shape-artist--style-char style))
    (shape-artist--write-rect-interior (oref shape x) (oref shape y) w h interior)
    (shape-artist--reparse-and-find shape)))

(defun shape-artist-shapes--move-edge (edge dx dy)
  "Move EDGE by DX and DY."
  (let* ((style (shape-artist-shapes-edge-style edge))
         (need-left (max 0 (- (min (+ (oref edge x) dx) (+ (oref edge x2) dx)))))
         (need-top (max 0 (- (min (+ (oref edge y) dy) (+ (oref edge y2) dy))))))
    (when (> need-left 0)
      (dotimes (_ need-left) (shape-artist--expand-left))
      (shape-artist--shift-all-shapes need-left 0))
    (when (> need-top 0)
      (dotimes (_ need-top) (shape-artist--expand-top))
      (shape-artist--shift-all-shapes 0 need-top))
    (shape-artist--erase-edge (oref edge x) (oref edge y) (oref edge x2) (oref edge y2))
    (oset edge x (+ (oref edge x) dx))
    (oset edge y (+ (oref edge y) dy))
    (oset edge x2 (+ (oref edge x2) dx))
    (oset edge y2 (+ (oref edge y2) dy))
    (shape-artist--draw-edge (oref edge x) (oref edge y) (oref edge x2) (oref edge y2) style)
    (shape-artist-shapes-parse)
    (or (shape-artist-shapes--find-edge
         (oref edge x) (oref edge y) (oref edge x2) (oref edge y2))
        edge)))

(defun shape-artist-shapes-resize (shape new-width new-height)
  "Resize rectangle SHAPE to NEW-WIDTH and NEW-HEIGHT."
  (interactive
   (list (shape-artist-shapes-current)
         (read-number "New width: ")
         (read-number "New height: ")))
  (when (and shape (shape-artist-diagram-rectangle-p shape))
    (shape-artist-shapes-resize-anchored
     shape 'right (- new-width (oref shape width))
     'bottom (- new-height (oref shape height)))))

(defun shape-artist-shapes-resize-anchored (shape x-anchor dx y-anchor dy)
  "Resize rectangle SHAPE by DX and DY from X-ANCHOR and Y-ANCHOR."
  (when (and shape (shape-artist-diagram-rectangle-p shape))
    (let* ((old-x (oref shape x))
           (old-y (oref shape y))
           (old-w (oref shape width))
           (old-h (oref shape height))
           (style (shape-artist-shapes-border-style shape))
           (new-x (if (eq x-anchor 'left) (- old-x dx) old-x))
           (new-y (if (eq y-anchor 'top) (- old-y dy) old-y))
           (new-w (+ old-w dx))
           (new-h (+ old-h dy))
           (need-left (max 0 (- new-x)))
           (need-top (max 0 (- new-y))))
      (when (or (< new-w 2) (< new-h 2))
        (user-error "Rectangle must be at least 2x2"))
      (when (> need-left 0)
        (dotimes (_ need-left) (shape-artist--expand-left))
        (shape-artist--shift-all-shapes need-left 0)
        (setq old-x (+ old-x need-left) new-x (+ new-x need-left)))
      (when (> need-top 0)
        (dotimes (_ need-top) (shape-artist--expand-top))
        (shape-artist--shift-all-shapes 0 need-top)
        (setq old-y (+ old-y need-top) new-y (+ new-y need-top)))
      (let ((interior (shape-artist--read-rect-interior old-x old-y old-w old-h)))
        (shape-artist--erase-rect old-x old-y old-w old-h)
        (oset shape x new-x)
        (oset shape y new-y)
        (oset shape width new-w)
        (oset shape height new-h)
        (shape-artist--redraw-other-rectangles shape)
        (shape-artist--draw-rect-border new-x new-y new-w new-h (shape-artist--style-char style))
        (shape-artist--write-rect-interior new-x new-y new-w new-h interior)
        (shape-artist--reparse-and-find shape)))))

(defun shape-artist-shapes-edge-extend (edge delta)
  "Extend EDGE end point by DELTA cells."
  (when (and edge (shape-artist-diagram-edge-p edge))
    (let ((style (shape-artist-shapes-edge-style edge))
          (x (oref edge x))
          (y (oref edge y))
          (x2 (oref edge x2))
          (y2 (oref edge y2)))
      (cond
       ((= y y2) (setq x2 (max (1+ x) (+ x2 delta))))
       ((= x x2) (setq y2 (max (1+ y) (+ y2 delta)))))
      (shape-artist--erase-edge (oref edge x) (oref edge y) (oref edge x2) (oref edge y2))
      (oset edge x2 x2)
      (oset edge y2 y2)
      (shape-artist--draw-edge x y x2 y2 style)
      (shape-artist--reparse-and-find edge))))

(defun shape-artist-shapes-replace (shape style)
  "Change rectangle SHAPE border STYLE."
  (interactive
   (list (shape-artist-shapes-current)
         (intern (completing-read "Border style: " '("standard" "heavy" "light") nil t))))
  (when (and shape (shape-artist-diagram-rectangle-p shape))
    (unless (memq style '(standard heavy light))
      (user-error "Unsupported border style: %s" style))
    (shape-artist--draw-rect-border
     (oref shape x) (oref shape y) (oref shape width) (oref shape height)
     (shape-artist--style-char style))
    (shape-artist--reparse-and-find shape)))

(defun shape-artist-shapes-set-edge-style (edge style)
  "Change EDGE connector STYLE."
  (when (and edge (shape-artist-diagram-edge-p edge))
    (unless (memq style '(plain arrow-end arrow-both))
      (user-error "Unsupported edge style: %s" style))
    (shape-artist--draw-edge (oref edge x) (oref edge y) (oref edge x2) (oref edge y2) style)
    (shape-artist-shapes-parse)
    (or (shape-artist-shapes--find-edge
         (oref edge x) (oref edge y) (oref edge x2) (oref edge y2))
        edge)))

(provide 'shape-artist-shapes)
;;; shape-artist-shapes.el ends here
