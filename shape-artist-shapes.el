;;; shape-artist-shapes.el --- Shape operations: label, move, resize, style -*- lexical-binding: t; -*-

;;; Commentary:

;;; Concrete shape operations for parsed rectangles and straight edges.

;;; Code:

(require 'cl-lib)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-core)
(require 'shape-artist-guides)

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
    (or (ignore-errors (oref shape detected-style))
        (save-excursion
          (shape-artist--goto-row-col (oref shape y) (1+ (oref shape x)))
          (pcase (char-after)
            (?= 'heavy)
            (?. 'light)
            (?─ 'unicode-single)
            (?═ 'unicode-double)
            (_ 'standard))))))

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

(defun shape-artist--draw-rect-border (x y w h &optional style)
  "Draw rectangle border at X Y W H using STYLE.

STYLE may be a semantic style symbol or a legacy horizontal character."
  (let* ((chars (shape-artist--rectangle-style-chars (or style 'standard)))
         (tl (plist-get chars :tl))
         (tr (plist-get chars :tr))
         (bl (plist-get chars :bl))
         (br (plist-get chars :br))
         (hc (plist-get chars :h))
         (vc (plist-get chars :v)))
    (save-excursion
      (shape-artist--goto-row-col y (+ x w))
      (shape-artist--goto-row-col y x)
      (delete-char (1+ w))
      (insert tl (make-string (1- w) hc) tr)
      (dotimes (dr (1- h))
        (shape-artist--goto-row-col (+ y 1 dr) (+ x w))
        (shape-artist--goto-row-col (+ y 1 dr) x)
        (delete-char 1)
        (insert vc)
        (shape-artist--goto-row-col (+ y 1 dr) (+ x w))
        (delete-char 1)
        (insert vc))
      (shape-artist--goto-row-col (+ y h) (+ x w))
      (shape-artist--goto-row-col (+ y h) x)
      (delete-char (1+ w))
      (insert bl (make-string (1- w) hc) br))))

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
  "Return legacy horizontal char or semantic STYLE for border redraw."
  (pcase style
    ('heavy ?=)
    ('light ?.)
    ((or 'rounded 'unicode-single 'unicode-rounded 'unicode-double) style)
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

(defun shape-artist-shapes--rect-attachment-signature (shape)
  "Return the attachment signature for rectangle SHAPE."
  (list (oref shape x) (oref shape y) (oref shape width) (oref shape height)))

(defun shape-artist-shapes--attachment-on-rect-p (attachment rect-sig)
  "Return non-nil when ATTACHMENT points at RECT-SIG."
  (and attachment (equal (plist-get attachment :rect) rect-sig)))

(defun shape-artist-shapes--attachment-on-any-rect-p (attachment rect-sigs)
  "Return non-nil when ATTACHMENT points at one of RECT-SIGS."
  (and attachment (member (plist-get attachment :rect) rect-sigs)))

(defun shape-artist-shapes--edge-extent-with-minimum (lo hi lo-stuck hi-stuck)
  "Return (LO . HI) widened so the span covers at least two cells.
LO-STUCK and HI-STUCK say which ends are attached to a moving rectangle.  An
attached end stays where the rectangle put it; the free end gives way.  When
both ends are attached, or neither, LO stays and HI moves."
  (cond ((> hi lo) (cons lo hi))
        ((and hi-stuck (not lo-stuck)) (cons (1- hi) hi))
        (t (cons lo (1+ lo)))))

(defun shape-artist-shapes--finish-edge-plan (edge x y x2 y2 start-stuck end-stuck)
  "Return the redraw plan for EDGE at X Y X2 Y2, never shorter than two cells.
START-STUCK and END-STUCK say which ends follow a moving rectangle.  Without
this rule a rectangle moving into a short connector would collapse it to a
single glyph, or reverse it and corrupt the rectangle on redraw."
  (if (shape-artist--edge-horizontal-p edge)
      (pcase-let ((`(,lo . ,hi) (shape-artist-shapes--edge-extent-with-minimum
                                 x x2 start-stuck end-stuck)))
        (setq x lo x2 hi))
    (pcase-let ((`(,lo . ,hi) (shape-artist-shapes--edge-extent-with-minimum
                               y y2 start-stuck end-stuck)))
      (setq y lo y2 hi)))
  (list :edge edge :style (shape-artist-shapes-edge-style edge)
        :x x :y y :x2 x2 :y2 y2))

(defun shape-artist-shapes--edge-move-plan-for-rect (edge rect dx dy)
  "Return a straight-edge redraw plan for EDGE stuck to RECT by DX/DY.

This is the first routing slice: attached endpoints move with the rectangle.
If that would make a straight edge non-straight, the free axis of the edge is
translated so the connector remains parseable as one straight edge."
  (let* ((rect-sig (shape-artist-shapes--rect-attachment-signature rect))
         (start-attachment (and (slot-boundp edge 'start-attachment)
                                (oref edge start-attachment)))
         (end-attachment (and (slot-boundp edge 'end-attachment)
                              (oref edge end-attachment)))
         (start-stuck (shape-artist-shapes--attachment-on-rect-p
                       start-attachment rect-sig))
         (end-stuck (shape-artist-shapes--attachment-on-rect-p
                     end-attachment rect-sig)))
    (when (or start-stuck end-stuck)
      (let ((x (oref edge x))
            (y (oref edge y))
            (x2 (oref edge x2))
            (y2 (oref edge y2)))
        (cond
         ((shape-artist--edge-horizontal-p edge)
          (when (/= dy 0)
            (setq y (+ y dy)
                  y2 (+ y2 dy)))
          (when start-stuck
            (setq x (+ x dx)))
          (when end-stuck
            (setq x2 (+ x2 dx))))
         ((shape-artist--edge-vertical-p edge)
          (when (/= dx 0)
            (setq x (+ x dx)
                  x2 (+ x2 dx)))
          (when start-stuck
            (setq y (+ y dy)))
          (when end-stuck
            (setq y2 (+ y2 dy)))))
        (shape-artist-shapes--finish-edge-plan
         edge x y x2 y2 start-stuck end-stuck)))))

(defun shape-artist-shapes--attached-edge-plans (rect dx dy)
  "Return redraw plans for edges attached to RECT moving by DX/DY."
  (cl-remove nil
             (mapcar (lambda (shape)
                       (when (shape-artist-diagram-edge-p shape)
                         (shape-artist-shapes--edge-move-plan-for-rect
                          shape rect dx dy)))
                     shape-artist--last-shapes-list)))

(defun shape-artist-shapes--edge-move-plan-for-rects (edge rect-sigs dx dy)
  "Return a redraw plan for EDGE stuck to RECT-SIGS moving by DX/DY."
  (let* ((start-attachment (and (slot-boundp edge 'start-attachment)
                                (oref edge start-attachment)))
         (end-attachment (and (slot-boundp edge 'end-attachment)
                              (oref edge end-attachment)))
         (start-stuck (shape-artist-shapes--attachment-on-any-rect-p
                       start-attachment rect-sigs))
         (end-stuck (shape-artist-shapes--attachment-on-any-rect-p
                     end-attachment rect-sigs)))
    (when (or start-stuck end-stuck)
      (let ((x (oref edge x))
            (y (oref edge y))
            (x2 (oref edge x2))
            (y2 (oref edge y2)))
        (cond
         ((shape-artist--edge-horizontal-p edge)
          (when (/= dy 0)
            (setq y (+ y dy)
                  y2 (+ y2 dy)))
          (when start-stuck (setq x (+ x dx)))
          (when end-stuck (setq x2 (+ x2 dx))))
         ((shape-artist--edge-vertical-p edge)
          (when (/= dx 0)
            (setq x (+ x dx)
                  x2 (+ x2 dx)))
          (when start-stuck (setq y (+ y dy)))
          (when end-stuck (setq y2 (+ y2 dy)))))
        (shape-artist-shapes--finish-edge-plan
         edge x y x2 y2 start-stuck end-stuck)))))

(defun shape-artist-shapes--attached-edge-plans-for-rects (rects dx dy)
  "Return redraw plans for edges attached to RECTS moving by DX/DY."
  (let ((rect-sigs (mapcar #'shape-artist-shapes--rect-attachment-signature rects)))
    (cl-remove nil
               (mapcar (lambda (shape)
                         (when (shape-artist-diagram-edge-p shape)
                           (shape-artist-shapes--edge-move-plan-for-rects
                            shape rect-sigs dx dy)))
                       shape-artist--last-shapes-list))))

(defun shape-artist-shapes--plain-edge-move-plan (edge dx dy)
  "Return a full-edge move plan for EDGE by DX/DY."
  (list :edge edge :style (shape-artist-shapes-edge-style edge)
        :x (+ (oref edge x) dx) :y (+ (oref edge y) dy)
        :x2 (+ (oref edge x2) dx) :y2 (+ (oref edge y2) dy)))

(defun shape-artist-shapes--add-selected-edge-plans (plans edges dx dy)
  "Add full-move plans for selected EDGES not already present in PLANS."
  (dolist (edge edges plans)
    (unless (cl-find edge plans :key (lambda (plan) (plist-get plan :edge)))
      (push (shape-artist-shapes--plain-edge-move-plan edge dx dy) plans))))

(defun shape-artist-shapes--plan-shift (plans dcol drow)
  "Shift all edge PLANS by DCOL and DROW."
  (dolist (plan plans)
    (setf (plist-get plan :x) (+ (plist-get plan :x) dcol))
    (setf (plist-get plan :x2) (+ (plist-get plan :x2) dcol))
    (setf (plist-get plan :y) (+ (plist-get plan :y) drow))
    (setf (plist-get plan :y2) (+ (plist-get plan :y2) drow))))

(defun shape-artist-shapes--plans-min-x (plans fallback)
  "Return the minimum X coordinate among PLANS and FALLBACK."
  (let ((min-x fallback))
    (dolist (plan plans min-x)
      (setq min-x (min min-x (plist-get plan :x) (plist-get plan :x2))))))

(defun shape-artist-shapes--plans-min-y (plans fallback)
  "Return the minimum Y coordinate among PLANS and FALLBACK."
  (let ((min-y fallback))
    (dolist (plan plans min-y)
      (setq min-y (min min-y (plist-get plan :y) (plist-get plan :y2))))))

(defun shape-artist-shapes--rect-plans-min-x (plans fallback)
  "Return the minimum X coordinate among rectangle PLANS and FALLBACK."
  (let ((min-x fallback))
    (dolist (plan plans min-x)
      (setq min-x (min min-x (plist-get plan :new-x))))))

(defun shape-artist-shapes--rect-plans-min-y (plans fallback)
  "Return the minimum Y coordinate among rectangle PLANS and FALLBACK."
  (let ((min-y fallback))
    (dolist (plan plans min-y)
      (setq min-y (min min-y (plist-get plan :new-y))))))

(defun shape-artist-shapes--rect-plan-shift (plans dcol drow)
  "Shift rectangle PLANS by DCOL and DROW."
  (dolist (plan plans)
    (setf (plist-get plan :new-x) (+ (plist-get plan :new-x) dcol))
    (setf (plist-get plan :new-y) (+ (plist-get plan :new-y) drow))))

(defun shape-artist-shapes--rectangle-move-plan (shape dx dy)
  "Return a redraw plan for rectangle SHAPE moving by DX/DY."
  (list :shape shape
        :style (shape-artist-shapes-border-style shape)
        :interior (shape-artist--read-rect-interior
                   (oref shape x) (oref shape y)
                   (oref shape width) (oref shape height))
        :new-x (+ (oref shape x) dx)
        :new-y (+ (oref shape y) dy)
        :width (oref shape width)
        :height (oref shape height)))

(defun shape-artist-shapes-move-group (shapes dx dy)
  "Move SHAPES by DX and DY in one redraw pass.

Connected straight edges attached to moved rectangles are redrawn once, so a
selected box group can keep its connectors attached while moving."
  (pcase-let ((`(,snapped-dx ,snapped-dy ,_)
               (shape-artist-guides-snap-group-move shapes dx dy)))
    (setq dx snapped-dx
          dy snapped-dy))
  (let* ((rects (cl-remove-if-not #'shape-artist-diagram-rectangle-p shapes))
         (selected-edges (cl-remove-if-not #'shape-artist-diagram-edge-p shapes))
         (rect-plans (mapcar (lambda (rect)
                               (shape-artist-shapes--rectangle-move-plan rect dx dy))
                             rects))
         (edge-plans (shape-artist-shapes--add-selected-edge-plans
                      (shape-artist-shapes--attached-edge-plans-for-rects rects dx dy)
                      selected-edges dx dy))
         (need-left (max 0 (- (min (shape-artist-shapes--rect-plans-min-x
                                    rect-plans most-positive-fixnum)
                                   (shape-artist-shapes--plans-min-x
                                    edge-plans most-positive-fixnum)))))
         (need-top (max 0 (- (min (shape-artist-shapes--rect-plans-min-y
                                   rect-plans most-positive-fixnum)
                                  (shape-artist-shapes--plans-min-y
                                   edge-plans most-positive-fixnum))))))
    (when (> need-left 0)
      (dotimes (_ need-left) (shape-artist--expand-left))
      (shape-artist--shift-all-shapes need-left 0)
      (shape-artist-shapes--rect-plan-shift rect-plans need-left 0)
      (shape-artist-shapes--plan-shift edge-plans need-left 0))
    (when (> need-top 0)
      (dotimes (_ need-top) (shape-artist--expand-top))
      (shape-artist--shift-all-shapes 0 need-top)
      (shape-artist-shapes--rect-plan-shift rect-plans 0 need-top)
      (shape-artist-shapes--plan-shift edge-plans 0 need-top))
    (dolist (plan edge-plans)
      (let ((edge (plist-get plan :edge)))
        (shape-artist--erase-edge (oref edge x) (oref edge y)
                                  (oref edge x2) (oref edge y2))))
    (dolist (plan rect-plans)
      (let ((shape (plist-get plan :shape)))
        (shape-artist--erase-rect (oref shape x) (oref shape y)
                                  (oref shape width) (oref shape height))))
    (dolist (shape shape-artist--last-shapes-list)
      (when (and (shape-artist-diagram-rectangle-p shape)
                 (not (memq shape rects)))
        (shape-artist--draw-rect-border
         (oref shape x) (oref shape y) (oref shape width) (oref shape height)
         (shape-artist--style-char (shape-artist-shapes-border-style shape)))))
    (dolist (plan rect-plans)
      (let ((shape (plist-get plan :shape)))
        (oset shape x (plist-get plan :new-x))
        (oset shape y (plist-get plan :new-y))
        (shape-artist--draw-rect-border
         (oref shape x) (oref shape y)
         (plist-get plan :width) (plist-get plan :height)
         (shape-artist--style-char (plist-get plan :style)))
        (shape-artist--write-rect-interior
         (oref shape x) (oref shape y)
         (plist-get plan :width) (plist-get plan :height)
         (plist-get plan :interior))))
    (dolist (plan edge-plans)
      (let ((edge (plist-get plan :edge)))
        (oset edge x (plist-get plan :x))
        (oset edge y (plist-get plan :y))
        (oset edge x2 (plist-get plan :x2))
        (oset edge y2 (plist-get plan :y2))
        (shape-artist--draw-edge (oref edge x) (oref edge y)
                                 (oref edge x2) (oref edge y2)
                                 (plist-get plan :style))))
    (shape-artist-shapes-parse)
    (cl-remove nil
               (mapcar (lambda (shape)
                         (shape-artist-shapes-find-by-signature
                          (shape-artist-shapes-signature shape)))
                       shapes))))

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
  (pcase-let ((`(,snapped-dx ,snapped-dy ,_)
               (shape-artist-guides-snap-rectangle-move shape dx dy)))
    (setq dx snapped-dx
          dy snapped-dy))
  (let* ((w (oref shape width))
         (h (oref shape height))
         (style (shape-artist-shapes-border-style shape))
         (interior (shape-artist--read-rect-interior (oref shape x) (oref shape y) w h))
         (attached-edge-plans (shape-artist-shapes--attached-edge-plans shape dx dy))
         (new-x (+ (oref shape x) dx))
         (new-y (+ (oref shape y) dy))
         (need-left (max 0 (- (shape-artist-shapes--plans-min-x
                               attached-edge-plans new-x))))
         (need-top (max 0 (- (shape-artist-shapes--plans-min-y
                              attached-edge-plans new-y)))))
    (when (> need-left 0)
      (dotimes (_ need-left) (shape-artist--expand-left))
      (shape-artist--shift-all-shapes need-left 0)
      (shape-artist-shapes--plan-shift attached-edge-plans need-left 0)
      (setq new-x (+ new-x need-left)))
    (when (> need-top 0)
      (dotimes (_ need-top) (shape-artist--expand-top))
      (shape-artist--shift-all-shapes 0 need-top)
      (shape-artist-shapes--plan-shift attached-edge-plans 0 need-top)
      (setq new-y (+ new-y need-top)))
    (dolist (plan attached-edge-plans)
      (let ((edge (plist-get plan :edge)))
        (shape-artist--erase-edge (oref edge x) (oref edge y)
                                  (oref edge x2) (oref edge y2))))
    (shape-artist--erase-rect (oref shape x) (oref shape y) w h)
    (oset shape x new-x)
    (oset shape y new-y)
    (shape-artist--redraw-other-rectangles shape)
    (shape-artist--draw-rect-border (oref shape x) (oref shape y) w h
                                    (shape-artist--style-char style))
    (shape-artist--write-rect-interior (oref shape x) (oref shape y) w h interior)
    (dolist (plan attached-edge-plans)
      (let ((edge (plist-get plan :edge)))
        (oset edge x (plist-get plan :x))
        (oset edge y (plist-get plan :y))
        (oset edge x2 (plist-get plan :x2))
        (oset edge y2 (plist-get plan :y2))
        (shape-artist--draw-edge (oref edge x) (oref edge y)
                                 (oref edge x2) (oref edge y2)
                                 (plist-get plan :style))))
    (shape-artist--reparse-and-find shape)))

(defun shape-artist-shapes--move-edge (edge dx dy)
  "Move EDGE by DX and DY."
  (pcase-let ((`(,snapped-dx ,snapped-dy ,_)
               (shape-artist-guides-snap-edge-move edge dx dy)))
    (setq dx snapped-dx
          dy snapped-dy))
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
           (snap (shape-artist-guides-snap-rectangle-resize
                  shape x-anchor dx y-anchor dy))
           (dx (nth 0 snap))
           (dy (nth 1 snap))
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
