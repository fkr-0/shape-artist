;;; test-shape-artist-diagram-parse.el --- ERT tests for shape detection -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'shape-artist-diagram-parse)

;;; --- Rectangle Detection Tests ---

(ert-deftest test-shape-artist-diagram-detect-simple-rectangle ()
  "Test detection of a simple rectangle."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let* ((grid (shape-artist-diagram--extract-grid (current-buffer)))
           (rects (shape-artist-diagram--detect-rectangles grid)))
      (should (= (length rects) 1))
      (let ((rect (car rects)))
        (should (eq (eieio-object-class rect) 'shape-artist-diagram-rectangle))
        (should (= (oref rect x) 0))
        (should (= (oref rect y) 0))
        (should (= (oref rect width) 4))
        (should (= (oref rect height) 2))))))

(ert-deftest test-shape-artist-diagram-detect-nested-rectangles ()
  "Test detection of nested rectangles."
  (test-artist-with-buffer
      "+-------+\n| +---+ |\n| |   | |\n| +---+ |\n+-------+"
    (let* ((grid (shape-artist-diagram--extract-grid (current-buffer)))
           (rects (shape-artist-diagram--detect-rectangles grid)))
      (should (>= (length rects) 2)))))

(ert-deftest test-shape-artist-diagram-detect-multiple-rectangles ()
  "Test detection of multiple separate rectangles."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+\n\n+---+\n|   |\n+---+"
    (let* ((grid (shape-artist-diagram--extract-grid (current-buffer)))
           (rects (shape-artist-diagram--detect-rectangles grid)))
      (should (= (length rects) 2)))))

(ert-deftest test-shape-artist-diagram-detect-empty-buffer ()
  "Test that no rectangles are detected in an empty buffer."
  (test-artist-with-buffer
      ""
    (let* ((grid (shape-artist-diagram--extract-grid (current-buffer)))
           (rects (shape-artist-diagram--detect-rectangles grid)))
      (should (= (length rects) 0)))))

(ert-deftest test-shape-artist-diagram-detect-no-rectangles ()
  "Test that no rectangles are detected in non-rectangular content."
  (test-artist-with-buffer
      "hello world\nfoo bar"
    (let* ((grid (shape-artist-diagram--extract-grid (current-buffer)))
           (rects (shape-artist-diagram--detect-rectangles grid)))
      (should (= (length rects) 0)))))

;;; --- Edge Detection Tests ---

(ert-deftest test-shape-artist-diagram-detect-horizontal-edge ()
  "Test detection of a horizontal edge."
  (test-artist-with-buffer
      "-----"
    (let* ((grid (shape-artist-diagram--extract-grid (current-buffer)))
           (rects (shape-artist-diagram--detect-rectangles grid))
           (edges (shape-artist-diagram--detect-edges grid rects)))
      (should (>= (length edges) 1)))))

(ert-deftest test-shape-artist-diagram-detect-vertical-edge ()
  "Test detection of a vertical edge."
  (test-artist-with-buffer
      "|\n|\n|"
    (let* ((grid (shape-artist-diagram--extract-grid (current-buffer)))
           (rects (shape-artist-diagram--detect-rectangles grid))
           (edges (shape-artist-diagram--detect-edges grid rects)))
      (should (>= (length edges) 1)))))

;;; --- Edge Classification Tests ---

(ert-deftest test-shape-artist-diagram-classify-connected-edge ()
  "Test classification of a connected edge (both endpoints adjacent to rectangles)."
  (test-artist-with-buffer
      "+---+-----+---+\n|   |     |   |\n+---+     +---+"
    (let* ((grid (shape-artist-diagram--extract-grid (current-buffer)))
           (rects (shape-artist-diagram--detect-rectangles grid))
           (edges (shape-artist-diagram--detect-edges grid rects))
           (classified (shape-artist-diagram--classify-edges edges rects)))
      ;; At least one edge should be detected
      (should (> (length classified) 0))
      ;; Check that edges have been classified
      (dolist (edge classified)
        (should (memq (oref edge edge-type) '(connected disconnected free)))))))

(ert-deftest test-shape-artist-diagram-classify-free-edge ()
  "Test classification of a free edge (no endpoints adjacent to rectangles)."
  (test-artist-with-buffer
      "-----\n\n+---+\n|   |\n+---+"
    (let* ((grid (shape-artist-diagram--extract-grid (current-buffer)))
           (rects (shape-artist-diagram--detect-rectangles grid))
           (edges (shape-artist-diagram--detect-edges grid rects))
           (classified (shape-artist-diagram--classify-edges edges rects)))
      ;; The horizontal edge should be free
      (let ((free-edges (cl-remove-if-not (lambda (e) (eq (oref e edge-type) 'free)) classified)))
        (should (>= (length free-edges) 1))))))

;;; --- Hierarchy Tests ---

(ert-deftest test-shape-artist-diagram-hierarchy-root ()
  "Test that hierarchy building returns a root node."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let* ((result (shape-artist-diagram-parse-buffer))
           (root (car result)))
      (should (shape-artist-diagram-root-p root))
      (should (>= (length (oref root children)) 1)))))

(ert-deftest test-shape-artist-diagram-hierarchy-nested ()
  "Test that nested rectangles produce correct parent-child relationships."
  (test-artist-with-buffer
      "+-------+\n| +---+ |\n| |   | |\n| +---+ |\n+-------+"
    (let* ((result (shape-artist-diagram-parse-buffer))
           (root (car result))
           (shapes (cdr result)))
      (should (>= (length shapes) 2))
      ;; The outer rectangle should have children
      (let ((outer (car (cl-remove-if-not #'shape-artist-diagram-rectangle-p
                                          (oref root children)))))
        (when outer
          (should (>= (length (oref outer children)) 1)))))))

;;; --- Grid Extraction Tests ---

(ert-deftest test-shape-artist-diagram-grid-rectangular ()
  "Test that grid extraction produces equal-width lines."
  (test-artist-with-buffer
      "abc\ndefgh\nij"
    (let ((grid (shape-artist-diagram--extract-grid (current-buffer))))
      ;; All lines should have the same length
      (let ((len (length (aref grid 0))))
        (dotimes (i (length grid))
          (should (= (length (aref grid i)) len)))))))

;;; --- Label Extraction Tests ---

(ert-deftest test-shape-artist-diagram-extract-label ()
  "Test label extraction from inside a rectangle."
  (test-artist-with-buffer
      "+----+\n|foo |\n+----+"
    (let* ((grid (shape-artist-diagram--extract-grid (current-buffer)))
           (rects (shape-artist-diagram--detect-rectangles grid))
           (labels (shape-artist-diagram--extract-labels grid rects)))
      (should (= (length labels) 1))
      (should (string-match-p "foo" (oref (car labels) text))))))

;;; --- Parse Buffer Tests ---

(ert-deftest test-shape-artist-diagram-parse-buffer-stores-results ()
  "Test that parse-buffer stores results in buffer-local variables."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-diagram-parse-buffer)
    (should shape-artist--last-root)
    (should shape-artist--last-shapes-list)
    (should (> (length shape-artist--last-shapes-list) 0))))

(ert-deftest test-shape-artist-diagram-parse-buffer-shapes-count ()
  "Test that parse-buffer returns correct shapes-list length."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let* ((result (shape-artist-diagram-parse-buffer))
           (shapes (cdr result)))
      ;; Should have 1 rectangle (root is excluded from flat list)
      (should (= (length shapes) 1))
      (should (shape-artist-diagram-rectangle-p (car shapes))))))

(ert-deftest test-shape-artist-diagram-parse-buffer-with-label ()
  "Test that parse-buffer includes labels in shapes-list."
  (test-artist-with-buffer
      "+----+\n|foo |\n+----+"
    (let* ((result (shape-artist-diagram-parse-buffer))
           (shapes (cdr result)))
      ;; Should have rectangle + label
      (should (>= (length shapes) 2))
      (should (cl-some #'shape-artist-diagram-label-p shapes)))))

;;; --- Flatten Tree Tests ---

(ert-deftest test-shape-artist-diagram-flatten-tree ()
  "Test that flatten-tree produces correct depth-first order, excluding root."
  (let ((root (make-instance 'shape-artist-diagram-root))
        (child1 (make-instance 'shape-artist-diagram-rectangle :x 0 :y 0 :width 5 :height 3))
        (child2 (make-instance 'shape-artist-diagram-rectangle :x 10 :y 10 :width 3 :height 2)))
    (oset root children (list child1 child2))
    (let ((flat (shape-artist-diagram--flatten-tree root)))
      ;; Root is excluded, so only 2 children
      (should (= (length flat) 2))
      (should (eq (car flat) child1))
      (should (eq (cadr flat) child2)))))

;;; --- Grid Char Bounds Tests ---

(ert-deftest test-shape-artist-diagram-grid-char-in-bounds ()
  "grid-char returns the correct character for valid coordinates."
  (let ((grid (vector "abc" "def")))
    (should (eq (shape-artist-diagram--grid-char grid 0 0) ?a))
    (should (eq (shape-artist-diagram--grid-char grid 1 2) ?f))))

(ert-deftest test-shape-artist-diagram-grid-char-out-of-bounds ()
  "grid-char returns nil for coordinates outside the grid."
  (let ((grid (vector "abc" "def")))
    (should-not (shape-artist-diagram--grid-char grid -1 0))
    (should-not (shape-artist-diagram--grid-char grid 2 0))
    (should-not (shape-artist-diagram--grid-char grid 0 3))
    (should-not (shape-artist-diagram--grid-char grid 0 -1))))

;;; --- Contains-P Tests ---

(ert-deftest test-shape-artist-diagram-contains-p-true ()
  "contains-p returns t when outer fully encloses inner."
  (let ((outer (make-instance 'shape-artist-diagram-rectangle :x 0 :y 0 :width 10 :height 8))
        (inner (make-instance 'shape-artist-diagram-rectangle :x 2 :y 2 :width 4 :height 3)))
    (should (shape-artist-diagram--contains-p outer inner))))

(ert-deftest test-shape-artist-diagram-contains-p-false ()
  "contains-p returns nil when inner extends beyond outer."
  (let ((outer (make-instance 'shape-artist-diagram-rectangle :x 0 :y 0 :width 4 :height 3))
        (inner (make-instance 'shape-artist-diagram-rectangle :x 2 :y 2 :width 6 :height 3)))
    (should-not (shape-artist-diagram--contains-p outer inner))))

;;; --- Single-Dash Not an Edge ---

(ert-deftest test-shape-artist-diagram-single-dash-not-edge ()
  "A single '-' character is not detected as an edge (minimum length is 2)."
  (test-artist-with-buffer
      "-"
    (let* ((grid (shape-artist-diagram--extract-grid (current-buffer)))
           (rects (shape-artist-diagram--detect-rectangles grid))
           (edges (shape-artist-diagram--detect-edges grid rects)))
      (should (= (length edges) 0)))))

;;; --- Disconnected Edge Classification ---

(ert-deftest test-shape-artist-diagram-classify-disconnected-edge ()
  "An edge with exactly one endpoint adjacent to a rectangle is 'disconnected."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+-----\n"
    (let* ((grid (shape-artist-diagram--extract-grid (current-buffer)))
           (rects (shape-artist-diagram--detect-rectangles grid))
           (edges (shape-artist-diagram--detect-edges grid rects))
           (classified (shape-artist-diagram--classify-edges edges rects))
           (disconnected (cl-remove-if-not
                          (lambda (e) (eq (oref e edge-type) 'disconnected))
                          classified)))
      (should (>= (length disconnected) 1)))))

;;; --- Multiline Label Extraction ---

(ert-deftest test-shape-artist-diagram-extract-multiline-label ()
  "Labels spanning multiple lines are joined with newline."
  (test-artist-with-buffer
      "+-----+\n|foo  |\n|bar  |\n+-----+"
    (let* ((grid (shape-artist-diagram--extract-grid (current-buffer)))
           (rects (shape-artist-diagram--detect-rectangles grid))
           (labels (shape-artist-diagram--extract-labels grid rects)))
      (should (= (length labels) 1))
      (should (string-match-p "foo" (oref (car labels) text)))
      (should (string-match-p "bar" (oref (car labels) text))))))

;;; --- Point Adjacent to Rect ---

(ert-deftest test-shape-artist-diagram-point-adjacent-true ()
  "point-adjacent-to-rect-p returns true for a point touching the rect boundary."
  (let ((rects (list (make-instance 'shape-artist-diagram-rectangle
                                    :x 2 :y 2 :width 4 :height 3))))
    ;; Point just outside top-left corner
    (should (shape-artist-diagram--point-adjacent-to-rect-p 1 1 rects))
    ;; Point at bottom-right corner itself
    (should (shape-artist-diagram--point-adjacent-to-rect-p 6 5 rects))))

(ert-deftest test-shape-artist-diagram-point-adjacent-false ()
  "point-adjacent-to-rect-p returns nil for a point far from all rects."
  (let ((rects (list (make-instance 'shape-artist-diagram-rectangle
                                    :x 2 :y 2 :width 4 :height 3))))
    (should-not (shape-artist-diagram--point-adjacent-to-rect-p 0 0 rects))
    (should-not (shape-artist-diagram--point-adjacent-to-rect-p 20 20 rects))))

;;; --- Flatten Tree ---

(ert-deftest test-shape-artist-diagram-flatten-tree-nested ()
  "Flatten-tree returns grandchildren in depth-first order."
  (let* ((root (make-instance 'shape-artist-diagram-root))
         (child (make-instance 'shape-artist-diagram-rectangle :x 0 :y 0 :width 10 :height 8))
         (grandchild (make-instance 'shape-artist-diagram-rectangle :x 1 :y 1 :width 3 :height 2)))
    (oset child children (list grandchild))
    (oset root children (list child))
    (let ((flat (shape-artist-diagram--flatten-tree root)))
      (should (= (length flat) 2))
      (should (eq (nth 0 flat) child))
      (should (eq (nth 1 flat) grandchild)))))

;;; test-shape-artist-diagram-parse.el ends here
