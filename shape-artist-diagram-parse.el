;;; shape-artist-diagram-parse.el --- EIEIO classes, grid extraction, rectangle/edge detection -*- lexical-binding: t; -*-

;;; Commentary:

;;; This module provides EIEIO classes for diagram nodes (rectangles,
;;; edges, labels) and functions to parse ASCII art diagrams into
;;; structured shape trees.  It handles grid extraction, rectangle
;;; and edge detection, and hierarchy building.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'shape-artist-metadata)

;; Forward declarations for buffer-local variables defined in shape-artist-core.el.
;; These are set by `shape-artist-diagram-parse-buffer' after parsing.
(defvar shape-artist--last-root)
(defvar shape-artist--last-shapes-list)
(defvar-local shape-artist--last-graph nil
  "Last graph analysis derived from the parsed diagram.")

;;; --- EIEIO Class Hierarchy ---

(defclass shape-artist-diagram-node ()
  ((id :initarg :id :initform nil
       :accessor shape-artist-diagram-node-id
       :documentation "Stable best-effort node identifier assigned after parsing.")
   (x :initarg :x :initform 0 :type integer
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
  ((kind :initarg :kind :initform 'rectangle :type symbol
         :reader shape-artist-diagram-node-kind
         :documentation "Rectangle kind: `rectangle' or `rounded-rectangle'.")
   (detected-style :initarg :detected-style :initform nil
                   :reader shape-artist-diagram-node-detected-style
                   :documentation "Coherent border family detected by the parser, or nil."))
  :documentation "A rectangle-like box shape in the diagram.")

(defclass shape-artist-diagram-edge (shape-artist-diagram-node)
  ((x2 :initarg :x2 :initform 0 :type integer
       :documentation "X coordinate of endpoint.")
   (y2 :initarg :y2 :initform 0 :type integer
       :documentation "Y coordinate of endpoint.")
   (edge-type :initarg :edge-type :initform 'free
              :documentation "Edge classification: connected, disconnected, or free.")
   (arrow-start :initarg :arrow-start :initform nil
                :documentation "Non-nil when the start point has an arrow head.")
   (arrow-end :initarg :arrow-end :initform nil
              :documentation "Non-nil when the end point has an arrow head.")
   (start-attachment :initarg :start-attachment :initform nil
                     :documentation "Attachment plist for the start endpoint.")
   (end-attachment :initarg :end-attachment :initform nil
                   :documentation "Attachment plist for the end endpoint."))
  :documentation "An edge (line or arrow connector) in the diagram.")

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

(cl-defstruct (shape-artist-diagram-connector
               (:constructor shape-artist-diagram--make-connector))
  "One connected orthogonal connector route assembled from edge segments."
  id segments attachments shape-ids)

(cl-defstruct (shape-artist-diagram-graph
               (:constructor shape-artist-diagram--make-graph))
  "Graph view derived from rectangles and connector routes."
  nodes connectors adjacency)

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

(defconst shape-artist-diagram--box-style-specs
  '((:name standard :tl ?+ :tr ?+ :bl ?+ :br ?+ :h ?- :v ?| :kind rectangle)
    (:name heavy :tl ?+ :tr ?+ :bl ?+ :br ?+ :h ?= :v ?| :kind rectangle)
    (:name light :tl ?+ :tr ?+ :bl ?+ :br ?+ :h ?. :v ?| :kind rectangle)
    (:name rounded :tl ?/ :tr ?\\ :bl ?\\ :br ?/ :h ?- :v ?| :kind rounded-rectangle)
    (:name unicode-single :tl ?┌ :tr ?┐ :bl ?└ :br ?┘ :h ?─ :v ?│ :kind rectangle)
    (:name unicode-rounded :tl ?╭ :tr ?╮ :bl ?╰ :br ?╯ :h ?─ :v ?│ :kind rounded-rectangle)
    (:name unicode-double :tl ?╔ :tr ?╗ :bl ?╚ :br ?╝ :h ?═ :v ?║ :kind rectangle))
  "Coherent box border families recognized by Shape Artist.")

(defun shape-artist-diagram--box-style-at (grid r c)
  "Return coherent box style plist beginning at GRID R,C, or nil."
  (let ((corner (shape-artist-diagram--grid-char grid r c))
        (next (shape-artist-diagram--grid-char grid r (1+ c))))
    (cl-find-if (lambda (spec)
                  (and (eq corner (plist-get spec :tl))
                       (eq next (plist-get spec :h))))
                shape-artist-diagram--box-style-specs)))

(defun shape-artist-diagram--horizontal-border-char-p (char)
  "Return non-nil when CHAR is a supported rectangle horizontal border."
  (memq char '(?- ?= ?. ?─ ?═)))

(defun shape-artist-diagram--box-start-corner-char-p (char)
  "Return non-nil when CHAR can start a parseable box."
  (memq char '(?+ ?/ ?┌ ?╭ ?╔)))

(defun shape-artist-diagram--box-corner-char-p (char)
  "Return non-nil when CHAR is a supported box corner."
  (memq char '(?+ ?/ ?\\ ?┌ ?┐ ?└ ?┘ ?╭ ?╮ ?╰ ?╯ ?╔ ?╗ ?╚ ?╝)))

(defun shape-artist-diagram--horizontal-or-corner-char-p (char)
  "Return non-nil when CHAR is a horizontal border or corner."
  (or (shape-artist-diagram--horizontal-border-char-p char)
      (shape-artist-diagram--box-corner-char-p char)))

(defun shape-artist-diagram--horizontal-edge-char-p (char)
  "Return non-nil when CHAR is part of a horizontal edge or arrow."
  (memq char '(?- ?< ?>)))

(defun shape-artist-diagram--vertical-edge-char-p (char)
  "Return non-nil when CHAR is part of a vertical edge or arrow."
  (memq char '(?| ?^ ?v)))

(defun shape-artist-diagram--horizontal-connector-cell-p (grid r c)
  "Return non-nil when GRID R,C participates in a horizontal connector."
  (let ((char (shape-artist-diagram--grid-char grid r c)))
    (or (shape-artist-diagram--horizontal-edge-char-p char)
        (and (eq char ?+)
             (or (shape-artist-diagram--horizontal-edge-char-p
                  (shape-artist-diagram--grid-char grid r (1- c)))
                 (shape-artist-diagram--horizontal-edge-char-p
                  (shape-artist-diagram--grid-char grid r (1+ c))))))))

(defun shape-artist-diagram--vertical-connector-cell-p (grid r c)
  "Return non-nil when GRID R,C participates in a vertical connector."
  (let ((char (shape-artist-diagram--grid-char grid r c)))
    (or (shape-artist-diagram--vertical-edge-char-p char)
        (and (eq char ?+)
             (or (shape-artist-diagram--vertical-edge-char-p
                  (shape-artist-diagram--grid-char grid (1- r) c))
                 (shape-artist-diagram--vertical-edge-char-p
                  (shape-artist-diagram--grid-char grid (1+ r) c)))))))

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
        (let ((style (and (not (gethash (cons r c) visited))
                          (< (1+ c) cols)
                          (shape-artist-diagram--box-style-at grid r c))))
          (when style
            (let ((rect (shape-artist-diagram--trace-rectangle
                         grid r c rows cols style)))
              (when rect
                (shape-artist-diagram--mark-rect-perimeter visited rect)
                (push rect rectangles)))))))
    (nreverse rectangles)))

(defun shape-artist-diagram--grid-char (grid r c)
  "Get character at row R, col C in GRID, or nil if out of bounds."
  (when (and (>= r 0) (< r (length grid))
             (>= c 0) (< c (length (aref grid 0))))
    (aref (aref grid r) c)))

(defun shape-artist-diagram--trace-rectangle (grid r c rows cols &optional style)
  "Trace a rectangle starting from top-left corner at R,C in GRID.

Returns an \\='shape-artist-diagram-rectangle or nil if not a valid rectangle."
  (let* ((style (or style (shape-artist-diagram--box-style-at grid r c)))
         (horizontal (and style (plist-get style :h)))
         (vertical (and style (plist-get style :v)))
         (top-right (and style (plist-get style :tr)))
         (bottom-left (and style (plist-get style :bl)))
         (bottom-right (and style (plist-get style :br))))
    (when style
      (let ((c2 (1+ c)))
        (while (and (< c2 cols)
                    (eq (shape-artist-diagram--grid-char grid r c2) horizontal))
          (setq c2 (1+ c2)))
        (when (and (< c2 cols)
                   (eq (shape-artist-diagram--grid-char grid r c2) top-right))
          (let ((r2 (1+ r)))
            (while (and (< r2 rows)
                        (eq (shape-artist-diagram--grid-char grid r2 c) vertical))
              (setq r2 (1+ r2)))
            (when (and (< r2 rows)
                       (eq (shape-artist-diagram--grid-char grid r2 c) bottom-left)
                       (eq (shape-artist-diagram--grid-char grid r2 c2) bottom-right))
              (let ((valid t))
                (dotimes (dr (- r2 r 1))
                  (unless (eq (shape-artist-diagram--grid-char grid (+ r 1 dr) c2)
                              vertical)
                    (setq valid nil)))
                (dotimes (dc (- c2 c 1))
                  (unless (eq (shape-artist-diagram--grid-char grid r2 (+ c 1 dc))
                              horizontal)
                    (setq valid nil)))
                (when valid
                  (make-instance 'shape-artist-diagram-rectangle
                                 :x c :y r
                                 :width (- c2 c)
                                 :height (- r2 r)
                                 :kind (plist-get style :kind)
                                 :detected-style (plist-get style :name)))))))))))

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
        (horizontal-visited (make-hash-table :test 'equal))
        (vertical-visited (make-hash-table :test 'equal)))
    ;; Mark rectangle perimeters as visited
    (dolist (rect rectangles)
      (shape-artist-diagram--mark-rect-perimeter visited rect))
    ;; Horizontal and vertical scans deliberately have independent visited sets.
    ;; This lets `+' participate in both axes at bends/junctions without turning
    ;; a connector corner into an undetected gap.
    (dotimes (r rows)
      (let ((c 0))
        (while (< c cols)
          (if (and (not (gethash (cons r c) visited))
                   (not (gethash (cons r c) horizontal-visited))
                   (shape-artist-diagram--horizontal-connector-cell-p grid r c))
              (let ((c-start c))
                (while (and (< c cols)
                            (not (gethash (cons r c) visited))
                            (shape-artist-diagram--horizontal-connector-cell-p grid r c))
                  (puthash (cons r c) t horizontal-visited)
                  (setq c (1+ c)))
                (let ((c-end (1- c)))
                  (when (> c-end c-start)
                    (push (make-instance 'shape-artist-diagram-edge
                                         :x c-start :y r
                                         :x2 c-end :y2 r
                                         :width (- c-end c-start)
                                         :height 0
                                         :arrow-start (eq (shape-artist-diagram--grid-char grid r c-start) ?<)
                                         :arrow-end (eq (shape-artist-diagram--grid-char grid r c-end) ?>))
                          edges))))
            (setq c (1+ c))))))
    (dotimes (c cols)
      (let ((r 0))
        (while (< r rows)
          (if (and (not (gethash (cons r c) visited))
                   (not (gethash (cons r c) vertical-visited))
                   (shape-artist-diagram--vertical-connector-cell-p grid r c))
              (let ((r-start r))
                (while (and (< r rows)
                            (not (gethash (cons r c) visited))
                            (shape-artist-diagram--vertical-connector-cell-p grid r c))
                  (puthash (cons r c) t vertical-visited)
                  (setq r (1+ r)))
                (let ((r-end (1- r)))
                  (when (> r-end r-start)
                    (push (make-instance 'shape-artist-diagram-edge
                                         :x c :y r-start
                                         :x2 c :y2 r-end
                                         :width 0
                                         :height (- r-end r-start)
                                         :arrow-start (eq (shape-artist-diagram--grid-char grid r-start c) ?^)
                                         :arrow-end (eq (shape-artist-diagram--grid-char grid r-end c) ?v))
                          edges))))
            (setq r (1+ r))))))
    (nreverse edges)))

;;; --- Edge Classification ---

(defun shape-artist-diagram--classify-edges (edges rectangles)
  "Classify EDGES based on adjacency to RECTANGLES.

Sets edge-type slot: \\='connected, \\='disconnected, or \\='free."
  (dolist (edge edges)
    (let ((start-attachment
           (shape-artist-diagram--point-attachment
            (oref edge x) (oref edge y) rectangles))
          (end-attachment
           (shape-artist-diagram--point-attachment
            (oref edge x2) (oref edge y2) rectangles)))
      (oset edge start-attachment start-attachment)
      (oset edge end-attachment end-attachment)
      (oset edge edge-type
            (pcase (+ (if start-attachment 1 0)
                      (if end-attachment 1 0))
              (2 'connected)
              (1 'disconnected)
              (_ 'free)))))
  edges)

(defun shape-artist-diagram--rect-attachment-signature (rect)
  "Return the stable attachment signature for RECT."
  (list (oref rect x) (oref rect y) (oref rect width) (oref rect height)))

(defun shape-artist-diagram--point-attachment (x y rectangles)
  "Return endpoint attachment metadata for point X,Y near RECTANGLES.

The return value is a plist with :rect, :side, and :offset.  :rect is a
geometry signature, :side is one of `left', `right', `top', or `bottom', and
:offset is the coordinate offset along that side."
  (cl-some
   (lambda (rect)
     (let ((rx (oref rect x))
           (ry (oref rect y))
           (rw (oref rect width))
           (rh (oref rect height)))
       (cond
        ((and (= x (1- rx)) (<= ry y) (<= y (+ ry rh)))
         (list :rect (shape-artist-diagram--rect-attachment-signature rect)
               :side 'left :offset (- y ry)))
        ((and (= x (+ rx rw 1)) (<= ry y) (<= y (+ ry rh)))
         (list :rect (shape-artist-diagram--rect-attachment-signature rect)
               :side 'right :offset (- y ry)))
        ((and (= y (1- ry)) (<= rx x) (<= x (+ rx rw)))
         (list :rect (shape-artist-diagram--rect-attachment-signature rect)
               :side 'top :offset (- x rx)))
        ((and (= y (+ ry rh 1)) (<= rx x) (<= x (+ rx rw)))
         (list :rect (shape-artist-diagram--rect-attachment-signature rect)
               :side 'bottom :offset (- x rx))))))
   rectangles))

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

;;; --- Graph Analysis ---

(defun shape-artist-diagram--edge-endpoints (edge)
  "Return EDGE endpoints as coordinate cons cells."
  (list (cons (oref edge x) (oref edge y))
        (cons (oref edge x2) (oref edge y2))))

(defun shape-artist-diagram--edge-components (edges)
  "Group EDGES into orthogonally connected route components.

Segments belong to the same component when they share an endpoint.  The parser
represents bends with `+' in both axis scans, so elbow routes naturally become
one component without changing the public edge objects used by editing code."
  (let ((by-point (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'eq))
        (components nil))
    (dolist (edge edges)
      (dolist (point (shape-artist-diagram--edge-endpoints edge))
        (puthash point (cons edge (gethash point by-point)) by-point)))
    (dolist (edge edges)
      (unless (gethash edge seen)
        (let ((queue (list edge))
              (component nil))
          (puthash edge t seen)
          (while queue
            (let ((current (pop queue)))
              (push current component)
              (dolist (point (shape-artist-diagram--edge-endpoints current))
                (dolist (neighbor (gethash point by-point))
                  (unless (gethash neighbor seen)
                    (puthash neighbor t seen)
                    (push neighbor queue))))))
          (push (nreverse component) components))))
    (nreverse components)))

(defun shape-artist-diagram--rectangle-for-attachment (attachment rectangles)
  "Return rectangle in RECTANGLES referenced by ATTACHMENT."
  (let ((signature (plist-get attachment :rect)))
    (cl-find-if
     (lambda (rect)
       (equal signature (shape-artist-diagram--rect-attachment-signature rect)))
     rectangles)))

(defun shape-artist-diagram--component-attachments (segments)
  "Return unique endpoint attachment plists used by SEGMENTS."
  (delete-dups
   (cl-remove nil
              (apply #'append
                     (mapcar
                      (lambda (edge)
                        (list (oref edge start-attachment)
                              (oref edge end-attachment)))
                      segments)))))

(defun shape-artist-diagram-build-graph (rectangles edges)
  "Build a graph analysis view from RECTANGLES and classified EDGES.

The graph leaves text editing and individual edge objects untouched.  It groups
orthogonal segments into connector routes and derives rectangle adjacency from
their endpoint attachments."
  (let ((adjacency (make-hash-table :test 'equal))
        (connectors nil)
        (connector-index 0))
    (dolist (rect rectangles)
      (puthash (shape-artist-metadata-shape-id rect) nil adjacency))
    (dolist (segments (shape-artist-diagram--edge-components edges))
      (let* ((attachments (shape-artist-diagram--component-attachments segments))
             (shape-ids
              (delete-dups
               (cl-remove
                nil
                (mapcar
                 (lambda (attachment)
                   (let ((rect (shape-artist-diagram--rectangle-for-attachment
                                attachment rectangles)))
                     (and rect (shape-artist-metadata-shape-id rect))))
                 attachments)))))
        (cl-incf connector-index)
        (push (shape-artist-diagram--make-connector
               :id (format "connector-%d" connector-index)
               :segments segments
               :attachments attachments
               :shape-ids shape-ids)
              connectors)
        (dolist (from shape-ids)
          (dolist (to shape-ids)
            (unless (equal from to)
              (cl-pushnew to (gethash from adjacency) :test #'equal))))))
    (shape-artist-diagram--make-graph
     :nodes rectangles
     :connectors (nreverse connectors)
     :adjacency adjacency)))

(defun shape-artist-diagram-graph-neighbors (graph shape-or-id)
  "Return neighbor IDs for SHAPE-OR-ID in GRAPH."
  (let ((id (if (stringp shape-or-id)
                shape-or-id
              (shape-artist-metadata-shape-id shape-or-id))))
    (copy-sequence (gethash id (shape-artist-diagram-graph-adjacency graph)))))

(defun shape-artist-diagram-graph-components (graph)
  "Return connected components of GRAPH as lists of stable shape IDs."
  (let ((seen (make-hash-table :test 'equal))
        (components nil))
    (dolist (node (shape-artist-diagram-graph-nodes graph))
      (let ((start (shape-artist-metadata-shape-id node)))
        (unless (gethash start seen)
          (let ((queue (list start))
                (component nil))
            (puthash start t seen)
            (while queue
              (let ((id (pop queue)))
                (push id component)
                (dolist (neighbor (shape-artist-diagram-graph-neighbors graph id))
                  (unless (gethash neighbor seen)
                    (puthash neighbor t seen)
                    (push neighbor queue)))))
            (push (nreverse component) components)))))
    (nreverse components)))

(defun shape-artist-diagram-graph-shortest-path (graph from to)
  "Return a breadth-first shortest ID path in GRAPH from FROM to TO.

FROM and TO may be shape objects or stable shape ID strings."
  (let* ((start (if (stringp from) from (shape-artist-metadata-shape-id from)))
         (goal (if (stringp to) to (shape-artist-metadata-shape-id to)))
         (queue (list start))
         (seen (make-hash-table :test 'equal))
         (parent (make-hash-table :test 'equal)))
    (puthash start t seen)
    (while (and queue (not (gethash goal seen)))
      (let ((current (pop queue)))
        (dolist (neighbor (shape-artist-diagram-graph-neighbors graph current))
          (unless (gethash neighbor seen)
            (puthash neighbor t seen)
            (puthash neighbor current parent)
            (setq queue (nconc queue (list neighbor)))))))
    (when (gethash goal seen)
      (let ((path (list goal))
            (current goal))
        (while (not (equal current start))
          (setq current (gethash current parent))
          (push current path))
        path))))

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
    (with-current-buffer buf
      (shape-artist-metadata-reconcile-shapes shapes-list))
    ;; Store in buffer-local variables
    (with-current-buffer buf
      (setq shape-artist--last-root root)
      (setq shape-artist--last-shapes-list shapes-list)
      (setq shape-artist--last-graph
            (shape-artist-diagram-build-graph rectangles classified-edges)))
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
