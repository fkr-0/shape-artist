;;; shape-artist-diagram-parse.el --- EIEIO classes, grid extraction, rectangle/edge detection -*- lexical-binding: t; -*-

;;; Commentary:

;;; This module provides EIEIO classes for diagram nodes (rectangles,
;;; edges, labels) and functions to parse ASCII art diagrams into
;;; structured shape trees.  It handles grid extraction, rectangle
;;; and edge detection, and hierarchy building.

;;; Code:

(require 'eieio)
(require 'cl-lib)

;; Forward declarations for buffer-local variables defined in shape-artist-core.el.
;; These are set by `shape-artist-diagram-parse-buffer' after parsing.
(defvar shape-artist--last-root)
(defvar shape-artist--last-shapes-list)

;;; --- EIEIO Class Hierarchy ---

(defclass shape-artist-diagram-node ()
  ((x :initarg :x :initform 0 :type integer
      :documentation "X coordinate (column) of bounding box.")
   (y :initarg :y :initform 0 :type integer
      :documentation "Y coordinate (row) of bounding box.")
   (width :initarg :width :initform 0 :type integer
         :documentation "Width of bounding box.")
   (height :initarg :height :initform 0 :type integer
          :documentation "Height of bounding box.")
   (children :initarg :children :initform nil :type list
             :documentation "List of child nodes."))
  :abstract t
  :documentation "Abstract base class for diagram nodes.")

(defclass shape-artist-diagram-rectangle (shape-artist-diagram-node)
  ((kind :initform 'rectangle :reader shape-artist-diagram-node-kind))
  :documentation "A rectangle shape in the diagram.")

(defclass shape-artist-diagram-edge (shape-artist-diagram-node)
  ((x2 :initarg :x2 :initform 0 :type integer
       :documentation "X coordinate of endpoint.")
   (y2 :initarg :y2 :initform 0 :type integer
       :documentation "Y coordinate of endpoint.")
   (edge-type :initarg :edge-type :initform 'free
              :documentation "Edge classification: \\='connected, \\='disconnected, or \\='free."))
  :documentation "An edge (line) in the diagram.")

(defclass shape-artist-diagram-label (shape-artist-diagram-node)
  ((text :initarg :text :initform "" :type string
         :documentation "The label text.")
   (align :initarg :align :initform 'left
          :documentation "Text alignment: \\='left, \\='center, or \\='right.")
   (role :initarg :role :initform 'body
          :documentation "Label role: \\='headline, \\='body, or \\='list-item."))
  :documentation "A text label in the diagram.")

(defclass shape-artist-diagram-root (shape-artist-diagram-node)
  ((kind :initform 'root :reader shape-artist-diagram-node-kind))
  :documentation "Virtual root of the shape tree.")

;;; --- Grid Extraction ---

(defun shape-artist-diagram--extract-grid (buffer)
  "Extract grid from BUFFER as a vector of strings, padded to equal width."
  (with-current-buffer buffer
    (let* ((lines '())
           (max-len 0))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (setq max-len (max max-len (length line)))
            (push line lines))
          (forward-line 1)))
      (setq lines (nreverse lines))
      ;; Pad all lines to equal width
      (vconcat (mapcar (lambda (line)
                         (if (< (length line) max-len)
                             (concat line (make-string (- max-len (length line)) ?\s))
                           line))
                       lines)))))

;;; --- Rectangle Detection ---

(defun shape-artist-diagram--detect-rectangles (grid)
  "Detect rectangles in GRID.

Return a list of \\='shape-artist-diagram-rectangle objects."
  (let ((rows (length grid))
        (cols (if (> (length grid) 0) (length (aref grid 0)) 0))
        (rectangles '())
        (visited (make-hash-table :test 'equal)))
    ;; Scan for top-left corners: '+' followed by '-'
    (dotimes (r rows)
      (dotimes (c cols)
        (when (and (not (gethash (cons r c) visited))
                   (eq (shape-artist-diagram--grid-char grid r c) ?+)
                   (< (1+ c) cols)
                   (eq (shape-artist-diagram--grid-char grid r (1+ c)) ?-))
          ;; Found potential top-left corner, try to trace rectangle
          (let ((rect (shape-artist-diagram--trace-rectangle grid r c rows cols)))
            (when rect
              ;; Mark all perimeter cells as visited
              (shape-artist-diagram--mark-rect-perimeter visited rect)
              (push rect rectangles))))))
    (nreverse rectangles)))

(defun shape-artist-diagram--grid-char (grid r c)
  "Get character at row R, col C in GRID, or nil if out of bounds."
  (when (and (>= r 0) (< r (length grid))
             (>= c 0) (< c (length (aref grid 0))))
    (aref (aref grid r) c)))

(defun shape-artist-diagram--trace-rectangle (grid r c rows cols)
  "Trace a rectangle starting from top-left corner at R,C in GRID.

Returns an \\='shape-artist-diagram-rectangle or nil if not a valid rectangle."
  ;; Find top-right corner
  (let ((c2 c))
    (while (and (< (1+ c2) cols)
                (memq (shape-artist-diagram--grid-char grid r (1+ c2)) '(?- ?+)))
      (setq c2 (1+ c2)))
    ;; Top-right must be '+'
    (when (eq (shape-artist-diagram--grid-char grid r c2) ?+)
      ;; Find bottom-left corner
      (let ((r2 r))
        (while (and (< (1+ r2) rows)
                    (memq (shape-artist-diagram--grid-char grid (1+ r2) c) '(?| ?+)))
          (setq r2 (1+ r2)))
        ;; Bottom-left must be '+'
        (when (eq (shape-artist-diagram--grid-char grid r2 c) ?+)
          ;; Verify bottom-right corner is '+'
          (when (eq (shape-artist-diagram--grid-char grid r2 c2) ?+)
            ;; Verify right side
            (let ((valid t))
              (dotimes (dr (- r2 r 1))
                (unless (memq (shape-artist-diagram--grid-char grid (+ r 1 dr) c2) '(?| ?+))
                  (setq valid nil)))
              ;; Verify bottom side
              (dotimes (dc (- c2 c 1))
                (unless (memq (shape-artist-diagram--grid-char grid r2 (+ c 1 dc)) '(?- ?+))
                  (setq valid nil)))
              (when valid
                (make-instance 'shape-artist-diagram-rectangle
                               :x c :y r
                               :width (- c2 c)
                               :height (- r2 r))))))))))

(defun shape-artist-diagram--mark-rect-perimeter (visited rect)
  "Mark all perimeter cells of RECT as visited in VISITED hash."
  (let* ((x (oref rect x))
         (y (oref rect y))
         (w (oref rect width))
         (h (oref rect height)))
    ;; Top and bottom edges
    (dotimes (dc (1+ w))
      (puthash (cons y (+ x dc)) t visited)
      (puthash (cons (+ y h) (+ x dc)) t visited))
    ;; Left and right edges
    (dotimes (dr (1- h))
      (puthash (cons (+ y 1 dr) x) t visited)
      (puthash (cons (+ y 1 dr) (+ x w)) t visited))))

;;; --- Edge Detection ---

(defun shape-artist-diagram--detect-edges (grid rectangles)
  "Detect edges in GRID that are not part of RECTANGLES.

Return a list of \\='shape-artist-diagram-edge objects."
  (let ((rows (length grid))
        (cols (if (> (length grid) 0) (length (aref grid 0)) 0))
        (edges '())
        (visited (make-hash-table :test 'equal))
        (edge-visited (make-hash-table :test 'equal)))
    ;; Mark rectangle perimeters as visited
    (dolist (rect rectangles)
      (shape-artist-diagram--mark-rect-perimeter visited rect))
    ;; Scan for horizontal edges: sequences of '-' not on rectangle perimeter
    (dotimes (r rows)
      (let ((c 0))
        (while (< c cols)
          (if (and (not (gethash (cons r c) visited))
                   (not (gethash (cons r c) edge-visited))
                   (eq (shape-artist-diagram--grid-char grid r c) ?-))
              ;; Start of potential horizontal edge
              (let ((c-start c))
                (while (and (< c cols)
                            (not (gethash (cons r c) visited))
                            (eq (shape-artist-diagram--grid-char grid r c) ?-))
                  (puthash (cons r c) t edge-visited)
                  (setq c (1+ c)))
                (let ((c-end (1- c)))
                  (when (> c-end c-start)
                    (push (make-instance 'shape-artist-diagram-edge
                                         :x c-start :y r
                                         :x2 c-end :y2 r
                                         :width (- c-end c-start)
                                         :height 0)
                          edges))))
            (setq c (1+ c))))))
    ;; Scan for vertical edges: sequences of '|' not on rectangle perimeter
    (dotimes (c cols)
      (let ((r 0))
        (while (< r rows)
          (if (and (not (gethash (cons r c) visited))
                   (not (gethash (cons r c) edge-visited))
                   (eq (shape-artist-diagram--grid-char grid r c) ?|))
              ;; Start of potential vertical edge
              (let ((r-start r))
                (while (and (< r rows)
                            (not (gethash (cons r c) visited))
                            (eq (shape-artist-diagram--grid-char grid r c) ?|))
                  (puthash (cons r c) t edge-visited)
                  (setq r (1+ r)))
                (let ((r-end (1- r)))
                  (when (> r-end r-start)
                    (push (make-instance 'shape-artist-diagram-edge
                                         :x c :y r-start
                                         :x2 c :y2 r-end
                                         :width 0
                                         :height (- r-end r-start))
                          edges))))
            (setq r (1+ r))))))
    ;; Scan for '+' connectors not on rectangle perimeter or already-visited edges
    (dotimes (r rows)
      (dotimes (c cols)
        (when (and (not (gethash (cons r c) visited))
                   (not (gethash (cons r c) edge-visited))
                   (eq (shape-artist-diagram--grid-char grid r c) ?+))
          ;; Check if adjacent to a '-' or '|' edge character
          (let ((has-horiz (or (and (> c 0) (eq (shape-artist-diagram--grid-char grid r (1- c)) ?-))
                               (and (< c (1- cols)) (eq (shape-artist-diagram--grid-char grid r (1+ c)) ?-))))
                (has-vert (or (and (> r 0) (eq (shape-artist-diagram--grid-char grid (1- r) c) ?|))
                              (and (< r (1- rows)) (eq (shape-artist-diagram--grid-char grid (1+ r) c) ?|)))))
            ;; Only count as separate edge if not connected to existing edges
            (unless (or has-horiz has-vert)
              (puthash (cons r c) t edge-visited))))))
    (nreverse edges)))

;;; --- Edge Classification ---

(defun shape-artist-diagram--classify-edges (edges rectangles)
  "Classify EDGES based on adjacency to RECTANGLES.

Sets edge-type slot: \\='connected, \\='disconnected, or \\='free."
  (dolist (edge edges)
    (let ((adjacent-count 0))
      ;; Check start point adjacency
      (when (shape-artist-diagram--point-adjacent-to-rect-p
             (oref edge x) (oref edge y) rectangles)
        (cl-incf adjacent-count))
      ;; Check end point adjacency
      (when (shape-artist-diagram--point-adjacent-to-rect-p
             (oref edge x2) (oref edge y2) rectangles)
        (cl-incf adjacent-count))
      (oset edge edge-type
            (pcase adjacent-count
              (2 'connected)
              (1 'disconnected)
              (_ 'free)))))
  edges)

(defun shape-artist-diagram--point-adjacent-to-rect-p (x y rectangles)
  "Return t if point (X,Y) is adjacent to any rectangle in RECTANGLES."
  (cl-find-if
   (lambda (rect)
     (let ((rx (oref rect x))
           (ry (oref rect y))
           (rw (oref rect width))
           (rh (oref rect height)))
       (and (>= x (1- rx)) (<= x (+ rx rw 1))
            (>= y (1- ry)) (<= y (+ ry rh 1)))))
   rectangles))

;;; --- Containment Hierarchy ---

(defun shape-artist-diagram--build-hierarchy (rectangles edges)
  "Build containment tree from RECTANGLES and EDGES.

Return an \\='shape-artist-diagram-root with children."
  (let ((root (make-instance 'shape-artist-diagram-root
                             :x 0 :y 0 :width 0 :height 0)))
    ;; Sort rectangles by area (smallest first for containment check)
    (let ((sorted-rects (sort (copy-sequence rectangles)
                              (lambda (a b)
                                (< (* (oref a width) (oref a height))
                                   (* (oref b width) (oref b height)))))))
      ;; Build parent-child relationships
      (dolist (rect sorted-rects)
        (let ((parent (shape-artist-diagram--find-parent rect sorted-rects)))
          (if parent
              (push rect (oref parent children))
            (push rect (oref root children))))))
    ;; Add edges as children of root (or specific rectangles)
    (dolist (edge edges)
      (push edge (oref root children)))
    ;; Reverse children to restore detection order (push prepends)
    (oset root children (nreverse (oref root children)))
    ;; Recursively reverse children at all levels
    (dolist (child (oref root children))
      (oset child children (nreverse (oref child children))))
    root))

(defun shape-artist-diagram--find-parent (rect all-rects)
  "Find the smallest rectangle in ALL-RECTS that contain RECT."
  (let ((parent nil)
        (parent-area most-positive-fixnum))
    (dolist (candidate all-rects)
      (when (and (not (eq candidate rect))
                 (shape-artist-diagram--contains-p candidate rect)
                 (< (* (oref candidate width) (oref candidate height)) parent-area))
        (setq parent candidate)
        (setq parent-area (* (oref candidate width) (oref candidate height)))))
    parent))

(defun shape-artist-diagram--contains-p (outer inner)
  "Return t if OUTER rectangle contain INNER rectangle."
  (and (<= (oref outer x) (oref inner x))
       (<= (oref outer y) (oref inner y))
       (>= (+ (oref outer x) (oref outer width))
           (+ (oref inner x) (oref inner width)))
       (>= (+ (oref outer y) (oref outer height))
           (+ (oref inner y) (oref inner height)))))

(defun shape-artist-diagram--reverse-children (node)
  "Reverse children lists recursively in NODE tree."
  (oset node children (nreverse (oref node children)))
  (dolist (child (oref node children))
    (shape-artist-diagram--reverse-children child)))

;;; --- Label Extraction ---

(defun shape-artist-diagram--extract-labels (grid rectangles)
  "Extract text labels from inside RECTANGLES in GRID.

Return a list of \\='shape-artist-diagram-label objects."
  (let ((labels '()))
    (dolist (rect rectangles)
      (let* ((x (oref rect x))
             (y (oref rect y))
             (w (oref rect width))
             (h (oref rect height))
             (text-lines '()))
        ;; Extract interior text (inside the rectangle borders)
        (dotimes (dr (max 0 (1- h)))
          (let ((row (+ y 1 dr))
                (line-text ""))
            (dotimes (dc (max 0 (1- w)))
              (let ((ch (shape-artist-diagram--grid-char grid row (+ x 1 dc))))
                (setq line-text (concat line-text (string (or ch ?\s))))))
            (setq line-text (string-trim-right line-text))
            (unless (string-empty-p line-text)
              (push line-text text-lines))))
        ;; Create label if we found text
        (when text-lines
          (let ((full-text (string-join (nreverse text-lines) "\n")))
            (push (make-instance 'shape-artist-diagram-label
                                 :x (+ x 1) :y (+ y 1)
                                 :width (max 0 (- w 2))
                                 :height (max 0 (- h 2))
                                 :text full-text
                                 :align 'left
                                 :role 'body)
                  labels)))))
    (nreverse labels)))

;;; --- Highlighting ---

(defun shape-artist-diagram--highlight-shape (shape &optional face)
  "Highlight SHAPE with FACE (default: \\='region)."
  (let ((face (or face 'region)))
    (let* ((x (oref shape x))
           (y (oref shape y))
           (w (oref shape width))
           (h (oref shape height))
           (overlays '()))
      (dotimes (r (1+ h))
        (let* ((row (+ y r))
               (start (save-excursion
                        (goto-char (point-min))
                        (forward-line row)
                        (move-to-column x t)
                        (point)))
               (end (save-excursion
                      (goto-char (point-min))
                      (forward-line row)
                      (move-to-column (+ x w 1) t)
                      (point))))
          (when (<= start end)
            (let ((ov (make-overlay start end)))
              (overlay-put ov 'face face)
              (overlay-put ov 'priority 50)
              (push ov overlays)))))
      overlays)))

;;; --- Main Parse Function ---

(defun shape-artist-diagram-parse-buffer (&optional buffer)
  "Parse BUFFER (default: current) and return (root . shapes-list).

Also store results in buffer-local variables.  BUFFER defaults to
current buffer."
  (let* ((buf (or buffer (current-buffer)))
         (grid (shape-artist-diagram--extract-grid buf))
         (rectangles (shape-artist-diagram--detect-rectangles grid))
         (edges (shape-artist-diagram--detect-edges grid rectangles))
         (classified-edges (shape-artist-diagram--classify-edges edges rectangles))
         (root (shape-artist-diagram--build-hierarchy rectangles classified-edges))
         (labels (shape-artist-diagram--extract-labels grid rectangles))
         (shapes-list (append (shape-artist-diagram--flatten-tree root) labels)))
    ;; Store in buffer-local variables
    (with-current-buffer buf
      (setq shape-artist--last-root root)
      (setq shape-artist--last-shapes-list shapes-list))
    (cons root shapes-list)))

(defun shape-artist-diagram--flatten-tree (node)
  "Flatten NODE tree into a depth-first ordered list, excluding root."
  (let ((result nil))
    (dolist (child (oref node children))
      (push child result)
      (dolist (descendant (shape-artist-diagram--flatten-tree child))
        (push descendant result)))
    (nreverse result)))

;;; --- Provide ---

(provide 'shape-artist-diagram-parse)
;;; shape-artist-diagram-parse.el ends here
