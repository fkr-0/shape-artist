;;; shape-artist-explain.el --- Explain and repair near-miss boxes -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Shape Artist Contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The diagram parser silently ignores a box whose border is almost, but not
;; quite, a rectangle: a bottom edge drawn with `=' under a `-' top edge, a
;; one-cell gap, a missing corner and so on.  This module answers "why isn't
;; this a box?".  It finds such near-misses, explains the problem in plain
;; words, highlights the box and offers a one-key repair.
;;
;; Detection works on the character grid.  Every candidate rectangle (a border
;; family plus x, y, width and height) is compared with the grid.  A candidate
;; is a near-miss when:
;;
;; - only a few perimeter cells (at most `shape-artist-explain-max-edits', and
;;   at most a quarter of the perimeter) differ from the family;
;; - every differing cell is a blank, a glyph of another border family, or a
;;   one-cell ragged corner (so prose, tables and plain connectors never
;;   qualify);
;; - applying the proposed edits makes the real parser detect exactly that
;;   rectangle without losing any rectangle it already detects.
;;
;; The border family used for the repair is the one that needs the fewest
;; edits (a majority vote over the perimeter).  Ties are broken in favour of
;; the family whose top-left corner and first horizontal glyph match what is
;; actually drawn, so a `+---+' top beats a `+===+' bottom.
;;
;; Public entry points:
;;   `shape-artist-explain-near-misses'  -- list near-misses as plists
;;   `shape-artist-explain-at-point'     -- explain and highlight one
;;   `shape-artist-repair-at-point'      -- fix the near-miss at point
;;   `shape-artist-repair-all'           -- fix every near-miss
;;
;; Positions in messages use Emacs conventions: lines are 1-based and columns
;; are 0-based (as in the mode line).  Edits use 0-based (ROW COL CHAR).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'shape-artist-core)
(require 'shape-artist-diagram-parse)

(defgroup shape-artist-explain nil
  "Explain and repair near-miss boxes in Shape Artist diagrams."
  :group 'shape-artist
  :prefix "shape-artist-explain-")

(defcustom shape-artist-explain-max-edits 3
  "Maximum number of cell edits a near-miss may need to be reported.

Smaller values make detection more conservative.  Independently of this
limit, a near-miss never needs more edits than a quarter of its perimeter."
  :type 'integer
  :group 'shape-artist-explain)

(defface shape-artist-explain-face
  '((t :inherit highlight :extend t))
  "Face used to highlight a near-miss box while it is being explained."
  :group 'shape-artist-explain)

(defvar-local shape-artist-explain--overlays nil
  "Overlays currently highlighting an explained near-miss in this buffer.")

;;; --- Glyph tables ---------------------------------------------------------

(defun shape-artist-explain--collect-glyphs (&rest keys)
  "Return the unique characters stored under KEYS in the box style specs."
  (let ((chars nil))
    (dolist (spec shape-artist-diagram--box-style-specs)
      (dolist (key keys)
        (cl-pushnew (plist-get spec key) chars)))
    (nreverse chars)))

(defconst shape-artist-explain--h-glyphs
  (shape-artist-explain--collect-glyphs :h)
  "Horizontal border glyphs of every supported box family.")

(defconst shape-artist-explain--v-glyphs
  (shape-artist-explain--collect-glyphs :v)
  "Vertical border glyphs of every supported box family.")

(defconst shape-artist-explain--corner-glyphs
  (shape-artist-explain--collect-glyphs :tl :tr :bl :br)
  "Corner glyphs of every supported box family.")

;;; --- Grid helpers ---------------------------------------------------------

(defun shape-artist-explain--normalize-grid (grid &optional margin)
  "Return GRID as a vector of equally wide strings.

GRID may be nil (use the current buffer), a string with newlines, a list of
strings or a vector of strings.  With MARGIN non-nil, add two blank columns on
the right so a border that is too short can be extended past the last
character of the longest line."
  (let* ((lines (cond ((null grid)
                       (append (shape-artist-diagram--extract-grid
                                (current-buffer))
                               nil))
                      ((stringp grid) (split-string grid "\n"))
                      ((vectorp grid) (append grid nil))
                      (t grid)))
         (width (+ (if margin 2 0) (apply #'max 0 (mapcar #'length lines)))))
    (vconcat (mapcar (lambda (line)
                       (concat line (make-string (- width (length line)) ?\s)))
                     lines))))

(defun shape-artist-explain--char (grid row col)
  "Return the character of GRID at ROW and COL, or nil outside the grid."
  (and (>= row 0) (< row (length grid))
       (>= col 0) (< col (length (aref grid row)))
       (aref (aref grid row) col)))

(defun shape-artist-explain--grid-with-edits (grid edits)
  "Return a copy of GRID with EDITS, a list of (ROW COL CHAR), applied."
  (let ((new (copy-sequence grid)))
    (dolist (edit edits)
      (let* ((row (nth 0 edit)) (col (nth 1 edit)) (line (aref new row)))
        (aset new row (concat (substring line 0 col)
                              (string (nth 2 edit))
                              (substring line (1+ col))))))
    new))

(defun shape-artist-explain--cell-key (row col)
  "Return an integer hash key for the cell at ROW and COL."
  (+ (* row 1000003) col))

(defun shape-artist-explain--perimeter-keys (x y width height)
  "Return the cell keys of the perimeter of the box at X, Y of WIDTH x HEIGHT."
  (let ((keys nil))
    (dotimes (i (1+ width))
      (push (shape-artist-explain--cell-key y (+ x i)) keys)
      (push (shape-artist-explain--cell-key (+ y height) (+ x i)) keys))
    (dotimes (i (1- height))
      (push (shape-artist-explain--cell-key (+ y 1 i) x) keys)
      (push (shape-artist-explain--cell-key (+ y 1 i) (+ x width)) keys))
    keys))

;;; --- Candidate evaluation -------------------------------------------------

(defun shape-artist-explain--cell-kind (spec role expected actual)
  "Classify ACTUAL against EXPECTED for ROLE in border family SPEC.

Return `ok' when equal, otherwise one of `gap' (a blank), `mixed' (a glyph of
another family), `corner-glyph' (a corner glyph on an edge cell), `edge-glyph'
\(the family's horizontal glyph on a corner cell), or nil when the cell cannot
be part of a near-miss."
  (cond
   ((eq actual expected) 'ok)
   ((eq actual ?\s) 'gap)
   ((memq role '(tl tr bl br))
    (cond ((memq actual shape-artist-explain--corner-glyphs) 'mixed)
          ((eq actual (plist-get spec :h)) 'edge-glyph)))
   ((memq role '(top bottom))
    (cond ((memq actual shape-artist-explain--h-glyphs) 'mixed)
          ((memq actual shape-artist-explain--corner-glyphs) 'corner-glyph)))
   (t
    (and (memq actual shape-artist-explain--v-glyphs) 'mixed))))

(defun shape-artist-explain--evaluate (grid spec x y width height limit)
  "Compare GRID with the box of family SPEC at X, Y of WIDTH x HEIGHT.

Return the symbol `reject' when a differing cell cannot belong to a near-miss
or more than LIMIT cells differ; otherwise a (possibly empty) list of mismatch
plists with keys :role :row :col :expected :actual and :kind."
  (let ((r2 (+ y height)) (c2 (+ x width)) (bad nil) (count 0) (ok t))
    (cl-flet ((check (role row col expected)
                (when ok
                  (let* ((actual (shape-artist-explain--char grid row col))
                         (kind (shape-artist-explain--cell-kind
                                spec role expected actual)))
                    (cond ((eq kind 'ok))
                          ((or (null kind) (> (setq count (1+ count)) limit))
                           (setq ok nil))
                          (t (push (list :role role :row row :col col
                                         :expected expected :actual actual
                                         :kind kind)
                                   bad)))))))
      (check 'tl y x (plist-get spec :tl))
      (check 'tr y c2 (plist-get spec :tr))
      (check 'bl r2 x (plist-get spec :bl))
      (check 'br r2 c2 (plist-get spec :br))
      (dotimes (i (1- width))
        (check 'top y (+ x 1 i) (plist-get spec :h))
        (check 'bottom r2 (+ x 1 i) (plist-get spec :h)))
      (dotimes (i (1- height))
        (check 'left (+ y 1 i) x (plist-get spec :v))
        (check 'right (+ y 1 i) c2 (plist-get spec :v))))
    (if ok (nreverse bad) 'reject)))

(defun shape-artist-explain--blank-p (grid row col)
  "Return non-nil when GRID has a blank or no cell at ROW and COL."
  (memq (shape-artist-explain--char grid row col) '(nil ?\s)))

(defun shape-artist-explain--ragged (grid spec x y width height bad)
  "Detect a border that is one cell too short or long.

GRID is the character grid.  BAD is the mismatch list of the box at X, Y of
WIDTH x HEIGHT in family SPEC.
Return (EDITS ROLE DIRECTION) when the only problem is a top or bottom border
off by one cell, where ROLE is `top' or `bottom' and DIRECTION `shorter' or
`longer'; otherwise nil."
  (let ((c2 (+ x width)) (found nil))
    (dolist (role '(top bottom))
      (unless found
        (let* ((topp (eq role 'top))
               (row (if topp y (+ y height)))
               (end-role (if topp 'tr 'br))
               (end (plist-get spec (if topp :tr :br)))
               (h (plist-get spec :h))
               (a (cl-find role bad :key (lambda (m) (plist-get m :role))))
               (b (cl-find end-role bad
                           :key (lambda (m) (plist-get m :role)))))
          (cond
           ;; Shorter: end corner one cell early, blank where it belongs.
           ((and (= (length bad) 2)
                 (eq (plist-get a :role) role)
                 (eq (plist-get a :kind) 'corner-glyph)
                 (eq (plist-get a :actual) end)
                 (eq (plist-get a :col) (1- c2))
                 (eq (plist-get b :role) end-role)
                 (eq (plist-get b :kind) 'gap))
            (setq found (list (list (list row (1- c2) h) (list row c2 end))
                              role 'shorter)))
           ;; Longer: border continues over the corner cell and ends one later.
           ((and (= (length bad) 1)
                 (eq (plist-get b :kind) 'edge-glyph)
                 (eq (shape-artist-explain--char grid row (1+ c2)) end)
                 (not (memq (shape-artist-explain--char grid row (+ c2 2))
                            shape-artist-explain--h-glyphs))
                 (shape-artist-explain--blank-p
                  grid (if topp (1+ y) (1- (+ y height))) (1+ c2)))
            (setq found (list (list (list row c2 end) (list row (1+ c2) ?\s))
                              role 'longer)))))))
    found))

;;; --- Messages -------------------------------------------------------------

(defun shape-artist-explain--role-name (role)
  "Return a plain-words name for border ROLE."
  (pcase role
    ('tl "top-left corner") ('tr "top-right corner")
    ('bl "bottom-left corner") ('br "bottom-right corner")
    ('top "top border") ('bottom "bottom border")
    ('left "left side") ('right "right side")))

(defun shape-artist-explain--opposite (role)
  "Return the border role facing ROLE."
  (pcase role ('top 'bottom) ('bottom 'top) ('left 'right) ('right 'left)))

(defun shape-artist-explain--sentence-case (text)
  "Return TEXT with only its first letter upcased."
  (concat (upcase (substring text 0 1)) (substring text 1)))

(defun shape-artist-explain--compress (numbers)
  "Return NUMBERS (sorted) as text like \"1-3, 5\"."
  (let ((parts nil))
    (while numbers
      (let ((start (car numbers)) (end (car numbers)))
        (while (and (cdr numbers) (= (cadr numbers) (1+ end)))
          (setq numbers (cdr numbers) end (car numbers)))
        (push (if (= start end) (format "%d" start) (format "%d-%d" start end))
              parts)
        (setq numbers (cdr numbers))))
    (string-join (nreverse parts) ", ")))

(defun shape-artist-explain--describe-cells (mismatches)
  "Describe where MISMATCHES are, e.g. \"line 3, columns 1-3\"."
  (let ((rows nil))
    (dolist (m mismatches)
      (let ((cell (assq (plist-get m :row) rows)))
        (if cell
            (push (plist-get m :col) (cdr cell))
          (push (list (plist-get m :row) (plist-get m :col)) rows))))
    (string-join
     (mapcar (lambda (cell)
               (let ((cols (sort (copy-sequence (cdr cell)) #'<)))
                 (format "line %d, column%s %s" (1+ (car cell))
                         (if (cdr cols) "s" "")
                         (shape-artist-explain--compress cols))))
             (sort rows (lambda (a b) (< (car a) (car b)))))
     " and ")))

(defun shape-artist-explain--quote-chars (chars)
  "Return the distinct CHARS quoted and joined, e.g. \"'=' and '.'\"."
  (string-join (mapcar (lambda (c) (format "'%c'" c))
                       (delete-dups (copy-sequence chars)))
               " and "))

(defun shape-artist-explain--group-sentence (role kind group)
  "Return a sentence about GROUP, the mismatches of ROLE with KIND."
  (let ((where (shape-artist-explain--describe-cells group))
        (expected (plist-get (car group) :expected))
        (actual (mapcar (lambda (m) (plist-get m :actual)) group)))
    (cond
     ((and (eq kind 'gap) (memq role '(tl tr bl br)))
      (format "The %s is missing at %s; expected '%c'."
              (shape-artist-explain--role-name role) where expected))
     ((eq kind 'gap)
      (if (memq role '(top bottom))
          (format "The %s has a gap at %s; expected '%c'."
                  (shape-artist-explain--role-name role) where expected)
        (format "The %s is missing '%c' at %s."
                (shape-artist-explain--role-name role) expected where)))
     ((memq role '(tl tr bl br))
      (format "The %s at %s is %s but this box style uses '%c'; make it match."
              (shape-artist-explain--role-name role) where
              (shape-artist-explain--quote-chars actual) expected))
     (t
      (let ((ref (shape-artist-explain--opposite role)))
        (format "%s uses '%c' but %s uses %s (%s); make the %s match."
                (shape-artist-explain--sentence-case
                 (shape-artist-explain--role-name ref)) expected
                (car (split-string (shape-artist-explain--role-name role)))
                (shape-artist-explain--quote-chars actual) where
                (car (split-string (shape-artist-explain--role-name role)))))))))

(defun shape-artist-explain--message (bad ragged)
  "Return a plain-words message for mismatches BAD.

RAGGED is the result of `shape-artist-explain--ragged' or nil."
  (if ragged
      (let* ((role (nth 1 ragged))
             (shorter (eq (nth 2 ragged) 'shorter))
             (edits (car ragged))
             (actual (nth (if shorter 0 1) edits))
             (wanted (nth (if shorter 1 0) edits)))
        (format "The %s is one cell %s than the %s (its corner is at line %d, \
column %d but should be at column %d); %s it."
                (shape-artist-explain--role-name role)
                (nth 2 ragged)
                (shape-artist-explain--role-name
                 (shape-artist-explain--opposite role))
                (1+ (nth 0 actual)) (nth 1 actual) (nth 1 wanted)
                (if shorter "extend" "trim")))
    (let ((groups nil))
      (dolist (m bad)
        (let* ((key (cons (plist-get m :role) (plist-get m :kind)))
               (cell (assoc key groups)))
          (if cell
              (setcdr cell (append (cdr cell) (list m)))
            (setq groups (append groups (list (list key m)))))))
      (string-join
       (mapcar (lambda (g)
                 (shape-artist-explain--group-sentence (car (car g)) (cdr (car g))
                                                       (cdr g)))
               groups)
       " "))))

;;; --- Candidate search -----------------------------------------------------

(defun shape-artist-explain--classify (grid spec x y width height bad)
  "Return (PROBLEM EDITS MESSAGE) for the mismatches BAD, or nil.

GRID, SPEC, X, Y, WIDTH and HEIGHT describe the candidate box."
  (let ((ragged (shape-artist-explain--ragged grid spec x y width height bad)))
    (cond
     (ragged
      (list 'ragged-border (car ragged) (shape-artist-explain--message bad ragged)))
     ((cl-some (lambda (m) (memq (plist-get m :kind) '(corner-glyph edge-glyph)))
               bad)
      nil)
     (t
      (let* ((roles (mapcar (lambda (m) (plist-get m :role)) bad))
             (corner-gap (cl-some (lambda (m)
                                    (and (eq (plist-get m :kind) 'gap)
                                         (memq (plist-get m :role)
                                               '(tl tr bl br))))
                                  bad))
             (side-gap (cl-some (lambda (m)
                                  (and (eq (plist-get m :kind) 'gap)
                                       (memq (plist-get m :role) '(left right))))
                                bad))
             (edge-gap (cl-some (lambda (m)
                                  (and (eq (plist-get m :kind) 'gap)
                                       (memq (plist-get m :role) '(top bottom))))
                                bad))
             (problem (cond (corner-gap 'missing-corner)
                            (side-gap 'missing-side)
                            (edge-gap 'border-gap)
                            (t 'mixed-border-family)))
             (corners-present (- 4 (cl-count-if (lambda (r) (memq r '(tl tr bl br)))
                                                roles))))
        (when (>= corners-present 2)
          (list problem
                (mapcar (lambda (m) (list (plist-get m :row) (plist-get m :col)
                                          (plist-get m :expected)))
                        bad)
                (shape-artist-explain--message bad nil))))))))

(defun shape-artist-explain--rank (grid spec x y)
  "Rank how well SPEC's top-left corner and first glyph match GRID at X, Y.

Lower is better; used to break ties between border families."
  (+ (if (eq (shape-artist-explain--char grid y x) (plist-get spec :tl)) 0 2)
     (if (eq (shape-artist-explain--char grid y (1+ x)) (plist-get spec :h)) 0 1)))

(defun shape-artist-explain--seed-p (grid row col)
  "Return non-nil when a box in GRID could have its top-left at ROW and COL."
  (let ((char (shape-artist-explain--char grid row col)))
    (or (memq char shape-artist-explain--corner-glyphs)
        (and (eq char ?\s)
             (memq (shape-artist-explain--char grid row (1+ col))
                   shape-artist-explain--h-glyphs)
             (memq (shape-artist-explain--char grid (1+ row) col)
                   shape-artist-explain--v-glyphs)))))

(defun shape-artist-explain--confidence (edit-count perimeter)
  "Return the confidence for EDIT-COUNT edits on a box with PERIMETER cells."
  (let ((score (+ edit-count (if (< perimeter 12) 1 0))))
    (cond ((<= score 1) 'high) ((<= score 2) 'medium) (t 'low))))

(defun shape-artist-explain--scan-seed (grid spec index row col limit)
  "Collect candidates in GRID for family SPEC (number INDEX) at ROW, COL.

LIMIT bounds the number of differing cells.  Return (CANDIDATES . VALID) where
VALID lists the perimeters (as key lists) of exact matches."
  (let ((rows (length grid))
        (cols (if (> (length grid) 0) (length (aref grid 0)) 0))
        (top-bad 0) (width 2) (candidates nil) (valid nil)
        (rank (shape-artist-explain--rank grid spec col row)))
    (while (and (< (+ col width) cols) (<= top-bad limit))
      (let ((kind (shape-artist-explain--cell-kind
                   spec 'top (plist-get spec :h)
                   (shape-artist-explain--char grid row (+ col width -1)))))
        (cond ((null kind) (setq top-bad (1+ limit)))
              ((eq kind 'ok))
              (t (setq top-bad (1+ top-bad)))))
      (when (<= top-bad limit)
        (let ((left-bad 0) (height 2))
          (while (and (< (+ row height) rows) (<= left-bad limit))
            (let ((kind (shape-artist-explain--cell-kind
                         spec 'left (plist-get spec :v)
                         (shape-artist-explain--char grid (+ row height -1) col))))
              (cond ((null kind) (setq left-bad (1+ limit)))
                    ((eq kind 'ok))
                    (t (setq left-bad (1+ left-bad)))))
            (when (<= left-bad limit)
              (let ((bad (shape-artist-explain--evaluate
                          grid spec col row width height limit)))
                (cond
                 ((eq bad 'reject))
                 ((null bad)
                  (push (shape-artist-explain--perimeter-keys col row width height)
                        valid))
                 (t
                  (let ((info (shape-artist-explain--classify
                               grid spec col row width height bad)))
                    (when (and info
                               (shape-artist-explain--clean-corners-p
                                grid col row width height (nth 1 info)))
                      (let* ((edits (nth 1 info))
                             (perimeter (* 2 (+ width height))))
                        (when (<= (length edits)
                                  (if (cl-every (lambda (m)
                                                  (eq (plist-get m :kind)
                                                      'mixed))
                                                bad)
                                      limit
                                    (min limit (/ perimeter 4))))
                          (push (list :x col :y row :width width :height height
                                      :problem (nth 0 info)
                                      :message (nth 2 info)
                                      :edits (sort (copy-sequence edits)
                                                   (lambda (a b)
                                                     (or (< (car a) (car b))
                                                         (and (= (car a) (car b))
                                                              (< (cadr a) (cadr b))))))
                                      :confidence (shape-artist-explain--confidence
                                                   (length edits) perimeter)
                                      :style (plist-get spec :name)
                                      :rank (list (length edits) rank index))
                                candidates)))))))))
            (setq height (1+ height)))))
      (setq width (1+ width)))
    (cons candidates valid)))

(defun shape-artist-explain--clean-corners-p (grid x y width height edits)
  "Return non-nil when no border overhangs past a corner of the box.

GRID is the original grid, EDITS the proposed (ROW COL CHAR) edits, and X, Y,
WIDTH and HEIGHT describe the box.  A corner whose border continues outward is
a junction of a bigger drawing (a bit-field table, a box with T-junctions), not
a box corner, so the candidate would be a misleading explanation."
  (let ((r2 (+ y height)) (c2 (+ x width)))
    (cl-flet ((at (row col)
                (let ((edit (cl-find-if (lambda (e) (and (= (nth 0 e) row)
                                                         (= (nth 1 e) col)))
                                        edits)))
                  (if edit (nth 2 edit)
                    (shape-artist-explain--char grid row col)))))
      (cl-flet ((hp (row col) (memq (at row col) shape-artist-explain--h-glyphs))
                (vp (row col) (memq (at row col) shape-artist-explain--v-glyphs)))
        (not (or (hp y (1- x)) (hp r2 (1- x)) (hp y (1+ c2)) (hp r2 (1+ c2))
                 (vp (1- y) x) (vp (1- y) c2) (vp (1+ r2) x) (vp (1+ r2) c2)))))))

(defun shape-artist-explain--verify (grid candidate real)
  "Return non-nil when applying CANDIDATE's edits to GRID is detected.

The parser must find CANDIDATE's rectangle and keep every REAL signature."
  (let* ((new (shape-artist-explain--grid-with-edits
               grid (plist-get candidate :edits)))
         (found (mapcar #'shape-artist-diagram--rect-attachment-signature
                        (shape-artist-diagram--detect-rectangles new))))
    (and (member (list (plist-get candidate :x) (plist-get candidate :y)
                       (plist-get candidate :width) (plist-get candidate :height))
                 found)
         (cl-every (lambda (sig) (member sig found)) real))))

(defun shape-artist-explain--candidate< (a b)
  "Return non-nil when candidate A should be preferred over candidate B."
  (let ((ra (plist-get a :rank)) (rb (plist-get b :rank)))
    (cl-loop for x in (append ra (list (plist-get a :y) (plist-get a :x)
                                       (plist-get a :width) (plist-get a :height)))
             for y in (append rb (list (plist-get b :y) (plist-get b :x)
                                       (plist-get b :width) (plist-get b :height)))
             unless (= x y) return (< x y)
             finally return nil)))

;;;###autoload
(defun shape-artist-explain-near-misses (&optional grid)
  "Return the near-miss boxes in GRID, or in the current buffer when nil.

GRID may be a vector or list of strings, or a string with newlines.  Each
result is a plist with :x :y :width :height (the intended box), :problem (one
of `mixed-border-family', `border-gap', `missing-corner', `missing-side' or
`ragged-border'), :message (plain words), :edits (a list of (ROW COL CHAR),
0-based) that make the parser detect the box, :confidence (`high', `medium' or
`low') and :style (the border family the repair uses).  Results are ordered by
row, then column."
  (let* ((grid (shape-artist-explain--normalize-grid grid t))
         (rows (length grid))
         (cols (if (> rows 0) (length (aref grid 0)) 0))
         (limit shape-artist-explain-max-edits)
         (real-rects (shape-artist-diagram--detect-rectangles grid))
         (real (mapcar #'shape-artist-diagram--rect-attachment-signature
                       real-rects))
         (taken (make-hash-table :test 'eql))
         (candidates nil)
         (result nil))
    (when (>= limit 1)
      (dolist (rect real-rects)
        (dolist (key (shape-artist-explain--perimeter-keys
                      (oref rect x) (oref rect y)
                      (oref rect width) (oref rect height)))
          (puthash key t taken)))
      (dotimes (row rows)
        (dotimes (col cols)
          (when (shape-artist-explain--seed-p grid row col)
            (let ((index 0))
              (dolist (spec shape-artist-diagram--box-style-specs)
                (let ((found (shape-artist-explain--scan-seed
                              grid spec index row col limit)))
                  (setq candidates (nconc (car found) candidates))
                  ;; An exact match under any family is a valid box, even
                  ;; when the parser skips it; never report overlapping ones.
                  (dolist (keys (cdr found))
                    (dolist (key keys) (puthash key t taken))))
                (setq index (1+ index)))))))
      (dolist (cand (sort candidates #'shape-artist-explain--candidate<))
        (let ((keys (shape-artist-explain--perimeter-keys
                     (plist-get cand :x) (plist-get cand :y)
                     (plist-get cand :width) (plist-get cand :height)))
              (edit-keys (mapcar (lambda (e)
                                   (shape-artist-explain--cell-key
                                    (nth 0 e) (nth 1 e)))
                                 (plist-get cand :edits))))
          (unless (or (cl-some (lambda (k) (gethash k taken)) keys)
                      (cl-some (lambda (k) (gethash k taken)) edit-keys)
                      (not (shape-artist-explain--verify grid cand real)))
            (dolist (k (append keys edit-keys)) (puthash k t taken))
            (push (list :x (plist-get cand :x) :y (plist-get cand :y)
                        :width (plist-get cand :width)
                        :height (plist-get cand :height)
                        :problem (plist-get cand :problem)
                        :message (plist-get cand :message)
                        :edits (plist-get cand :edits)
                        :confidence (plist-get cand :confidence)
                        :style (plist-get cand :style))
                  result)))))
    (sort result (lambda (a b)
                   (or (< (plist-get a :y) (plist-get b :y))
                       (and (= (plist-get a :y) (plist-get b :y))
                            (< (plist-get a :x) (plist-get b :x))))))))

;;; --- Point lookup and highlighting ----------------------------------------

(defun shape-artist-explain--point-cell ()
  "Return (ROW . COL) of point relative to the accessible buffer text."
  (cons (count-lines (point-min) (line-beginning-position))
        (- (point) (line-beginning-position))))

(defun shape-artist-explain--distance (miss row col)
  "Return the distance from cell ROW, COL to the box of near-miss MISS."
  (let ((x (plist-get miss :x)) (y (plist-get miss :y)))
    (+ (max 0 (- x col) (- col (+ x (plist-get miss :width))))
       (max 0 (- y row) (- row (+ y (plist-get miss :height)))))))

(defun shape-artist-explain--miss-at-point (misses)
  "Return the member of MISSES containing point, else the nearest one."
  (let ((cell (shape-artist-explain--point-cell)) (best nil) (best-key nil))
    (dolist (miss misses)
      (let ((key (list (shape-artist-explain--distance miss (car cell) (cdr cell))
                       (* (plist-get miss :width) (plist-get miss :height)))))
        (when (or (null best-key)
                  (< (car key) (car best-key))
                  (and (= (car key) (car best-key)) (< (cadr key) (cadr best-key))))
          (setq best miss best-key key))))
    best))

(defun shape-artist-explain--clear-highlight ()
  "Remove the near-miss highlight and its one-shot cleanup hook."
  (mapc #'delete-overlay shape-artist-explain--overlays)
  (setq shape-artist-explain--overlays nil)
  (remove-hook 'pre-command-hook #'shape-artist-explain--clear-highlight t))

(defun shape-artist-explain--highlight (miss)
  "Highlight the box of near-miss MISS until the next command."
  (shape-artist-explain--clear-highlight)
  (save-excursion
    (dotimes (i (1+ (plist-get miss :height)))
      (goto-char (point-min))
      (when (zerop (forward-line (+ (plist-get miss :y) i)))
        (let* ((bol (line-beginning-position))
               (len (- (line-end-position) bol))
               (from (min (plist-get miss :x) len))
               (to (min (+ (plist-get miss :x) (plist-get miss :width) 1) len)))
          (when (< from to)
            (let ((ov (make-overlay (+ bol from) (+ bol to))))
              (overlay-put ov 'face 'shape-artist-explain-face)
              (overlay-put ov 'priority 60)
              (push ov shape-artist-explain--overlays)))))))
  (add-hook 'pre-command-hook #'shape-artist-explain--clear-highlight nil t))

(defun shape-artist-explain--location (miss)
  "Return \"line L, column C\" for the top-left corner of near-miss MISS."
  (format "line %d, column %d" (1+ (plist-get miss :y)) (plist-get miss :x)))

;;;###autoload
(defun shape-artist-explain-at-point ()
  "Explain why the box at point is not detected, and highlight it.

Pick the near-miss containing point, or else the nearest one.  The highlight
disappears on the next command.  Return the near-miss plist, or nil."
  (interactive)
  (let ((miss (shape-artist-explain--miss-at-point
               (shape-artist-explain-near-misses))))
    (if (null miss)
        (progn (message "No near-miss box found here") nil)
      (shape-artist-explain--highlight miss)
      (message "%s (M-x shape-artist-repair-at-point to fix)"
               (plist-get miss :message))
      miss)))

;;; --- Repair ---------------------------------------------------------------

(defun shape-artist-explain--set-cell (row col char)
  "Replace the character at ROW and COL with CHAR, padding short lines.

Only that one character changes, so the rest of the text stays untouched."
  (goto-char (point-min))
  (unless (zerop (forward-line row))
    (error "Row %d is outside the buffer" row))
  (let* ((bol (point)) (len (- (line-end-position) bol)))
    (if (< col len)
        (progn (goto-char (+ bol col))
               (delete-char 1)
               ;; Blanking the last cell of a line trims it instead of
               ;; leaving trailing whitespace; the grid is padded anyway.
               (unless (and (eq char ?\s) (= col (1- len)))
                 (insert char)))
      (goto-char (line-end-position))
      (insert (make-string (- col len) ?\s) (string char)))))

(defun shape-artist-explain--apply-edits (edits)
  "Apply EDITS, a list of (ROW COL CHAR), as one atomic change."
  (atomic-change-group
    (save-excursion
      (dolist (edit edits)
        (shape-artist-explain--set-cell (nth 0 edit) (nth 1 edit) (nth 2 edit))))))

(defun shape-artist-explain--reparse ()
  "Re-run the diagram parser and refresh the sidebar when there is one."
  (shape-artist-diagram-parse-buffer)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist-explain-apply-near-miss (miss)
  "Apply the edits of near-miss MISS to the buffer and re-parse it.

Return MISS."
  (shape-artist-explain--apply-edits (plist-get miss :edits))
  (shape-artist-explain--reparse)
  miss)

;;;###autoload
(defun shape-artist-repair-at-point ()
  "Repair the near-miss box at point so the parser detects it.

Pick the near-miss containing point, or else the nearest one.  All edits form
one undoable change.  Signal a `user-error' when nothing is repairable."
  (interactive)
  (let ((miss (shape-artist-explain--miss-at-point
               (shape-artist-explain-near-misses))))
    (unless miss
      (user-error "No near-miss box found here; nothing to repair"))
    (shape-artist-explain--clear-highlight)
    (shape-artist-explain-apply-near-miss miss)
    (message "Repaired: the box at %s is now detected"
             (shape-artist-explain--location miss))
    miss))

;;;###autoload
(defun shape-artist-repair-all ()
  "Repair every near-miss box in the buffer; return the number repaired.

Repairs repeat until no near-miss is left, as one undoable change."
  (interactive)
  (let ((total 0) (pass 0) (misses nil))
    (shape-artist-explain--clear-highlight)
    (atomic-change-group
      (while (and (< pass 10)
                  (setq misses (shape-artist-explain-near-misses)))
        (shape-artist-explain--apply-edits
         (apply #'append (mapcar (lambda (m) (plist-get m :edits)) misses)))
        (setq total (+ total (length misses)) pass (1+ pass))))
    (when (> total 0)
      (shape-artist-explain--reparse))
    (if (zerop total)
        (message "No near-miss boxes found")
      (message "Repaired %d near-miss box%s" total (if (= total 1) "" "es")))
    total))

(provide 'shape-artist-explain)
;;; shape-artist-explain.el ends here
