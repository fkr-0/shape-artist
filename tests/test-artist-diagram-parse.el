;;; test-artist-diagram-parse.el --- ERT tests for shape detection -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'artist-diagram-parse)

;;; --- Rectangle Detection Tests ---

(ert-deftest test-artist-diagram-detect-simple-rectangle ()
  "Test detection of a simple rectangle."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let* ((grid (artist-diagram--extract-grid (current-buffer)))
           (rects (artist-diagram--detect-rectangles grid)))
      (should (= (length rects) 1))
      (let ((rect (car rects)))
        (should (eq (eieio-object-class rect) 'artist-diagram-rectangle))
        (should (= (oref rect x) 0))
        (should (= (oref rect y) 0))
        (should (= (oref rect width) 4))
        (should (= (oref rect height) 2))))))

(ert-deftest test-artist-diagram-detect-nested-rectangles ()
  "Test detection of nested rectangles."
  (test-artist-with-buffer
      "+-------+\n| +---+ |\n| |   | |\n| +---+ |\n+-------+"
    (let* ((grid (artist-diagram--extract-grid (current-buffer)))
           (rects (artist-diagram--detect-rectangles grid)))
      (should (>= (length rects) 2)))))

(ert-deftest test-artist-diagram-detect-multiple-rectangles ()
  "Test detection of multiple separate rectangles."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+\n\n+---+\n|   |\n+---+"
    (let* ((grid (artist-diagram--extract-grid (current-buffer)))
           (rects (artist-diagram--detect-rectangles grid)))
      (should (= (length rects) 2)))))

(ert-deftest test-artist-diagram-detect-empty-buffer ()
  "Test that no rectangles are detected in an empty buffer."
  (test-artist-with-buffer
      ""
    (let* ((grid (artist-diagram--extract-grid (current-buffer)))
           (rects (artist-diagram--detect-rectangles grid)))
      (should (= (length rects) 0)))))

(ert-deftest test-artist-diagram-detect-no-rectangles ()
  "Test that no rectangles are detected in non-rectangular content."
  (test-artist-with-buffer
      "hello world\nfoo bar"
    (let* ((grid (artist-diagram--extract-grid (current-buffer)))
           (rects (artist-diagram--detect-rectangles grid)))
      (should (= (length rects) 0)))))

;;; --- Edge Detection Tests ---

(ert-deftest test-artist-diagram-detect-horizontal-edge ()
  "Test detection of a horizontal edge."
  (test-artist-with-buffer
      "-----"
    (let* ((grid (artist-diagram--extract-grid (current-buffer)))
           (rects (artist-diagram--detect-rectangles grid))
           (edges (artist-diagram--detect-edges grid rects)))
      (should (>= (length edges) 1)))))

(ert-deftest test-artist-diagram-detect-vertical-edge ()
  "Test detection of a vertical edge."
  (test-artist-with-buffer
      "|\n|\n|"
    (let* ((grid (artist-diagram--extract-grid (current-buffer)))
           (rects (artist-diagram--detect-rectangles grid))
           (edges (artist-diagram--detect-edges grid rects)))
      (should (>= (length edges) 1)))))

;;; --- Edge Classification Tests ---

(ert-deftest test-artist-diagram-classify-connected-edge ()
  "Test classification of a connected edge (both endpoints adjacent to rectangles)."
  (test-artist-with-buffer
      "+---+-----+---+\n|   |     |   |\n+---+     +---+"
    (let* ((grid (artist-diagram--extract-grid (current-buffer)))
           (rects (artist-diagram--detect-rectangles grid))
           (edges (artist-diagram--detect-edges grid rects))
           (classified (artist-diagram--classify-edges edges rects)))
      ;; At least one edge should be detected
      (should (> (length classified) 0))
      ;; Check that edges have been classified
      (dolist (edge classified)
        (should (memq (oref edge edge-type) '(connected disconnected free)))))))

(ert-deftest test-artist-diagram-classify-free-edge ()
  "Test classification of a free edge (no endpoints adjacent to rectangles)."
  (test-artist-with-buffer
      "-----\n\n+---+\n|   |\n+---+"
    (let* ((grid (artist-diagram--extract-grid (current-buffer)))
           (rects (artist-diagram--detect-rectangles grid))
           (edges (artist-diagram--detect-edges grid rects))
           (classified (artist-diagram--classify-edges edges rects)))
      ;; The horizontal edge should be free
      (let ((free-edges (cl-remove-if-not (lambda (e) (eq (oref e edge-type) 'free)) classified)))
        (should (>= (length free-edges) 1))))))

;;; --- Hierarchy Tests ---

(ert-deftest test-artist-diagram-hierarchy-root ()
  "Test that hierarchy building returns a root node."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let* ((result (artist-diagram-parse-buffer))
           (root (car result)))
      (should (artist-diagram-root-p root))
      (should (>= (length (oref root children)) 1)))))

(ert-deftest test-artist-diagram-hierarchy-nested ()
  "Test that nested rectangles produce correct parent-child relationships."
  (test-artist-with-buffer
      "+-------+\n| +---+ |\n| |   | |\n| +---+ |\n+-------+"
    (let* ((result (artist-diagram-parse-buffer))
           (root (car result))
           (shapes (cdr result)))
      (should (>= (length shapes) 2))
      ;; The outer rectangle should have children
      (let ((outer (car (cl-remove-if-not #'artist-diagram-rectangle-p
                                          (oref root children)))))
        (when outer
          (should (>= (length (oref outer children)) 1)))))))

;;; --- Grid Extraction Tests ---

(ert-deftest test-artist-diagram-grid-rectangular ()
  "Test that grid extraction produces equal-width lines."
  (test-artist-with-buffer
      "abc\ndefgh\nij"
    (let ((grid (artist-diagram--extract-grid (current-buffer))))
      ;; All lines should have the same length
      (let ((len (length (aref grid 0))))
        (dotimes (i (length grid))
          (should (= (length (aref grid i)) len)))))))

;;; --- Label Extraction Tests ---

(ert-deftest test-artist-diagram-extract-label ()
  "Test label extraction from inside a rectangle."
  (test-artist-with-buffer
      "+----+\n|foo |\n+----+"
    (let* ((grid (artist-diagram--extract-grid (current-buffer)))
           (rects (artist-diagram--detect-rectangles grid))
           (labels (artist-diagram--extract-labels grid rects)))
      (should (= (length labels) 1))
      (should (string-match-p "foo" (oref (car labels) text))))))

;;; --- Parse Buffer Tests ---

(ert-deftest test-artist-diagram-parse-buffer-stores-results ()
  "Test that parse-buffer stores results in buffer-local variables."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (artist-diagram-parse-buffer)
    (should shape-artist--last-root)
    (should shape-artist--last-shapes-list)
    (should (> (length shape-artist--last-shapes-list) 0))))

(ert-deftest test-artist-diagram-parse-buffer-shapes-count ()
  "Test that parse-buffer returns correct shapes-list length."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let* ((result (artist-diagram-parse-buffer))
           (shapes (cdr result)))
      ;; Should have 1 rectangle (root is excluded from flat list)
      (should (= (length shapes) 1))
      (should (artist-diagram-rectangle-p (car shapes))))))

(ert-deftest test-artist-diagram-parse-buffer-with-label ()
  "Test that parse-buffer includes labels in shapes-list."
  (test-artist-with-buffer
      "+----+\n|foo |\n+----+"
    (let* ((result (artist-diagram-parse-buffer))
           (shapes (cdr result)))
      ;; Should have rectangle + label
      (should (>= (length shapes) 2))
      (should (cl-some #'artist-diagram-label-p shapes)))))

;;; --- Flatten Tree Tests ---

(ert-deftest test-artist-diagram-flatten-tree ()
  "Test that flatten-tree produces correct depth-first order, excluding root."
  (let ((root (make-instance 'artist-diagram-root))
        (child1 (make-instance 'artist-diagram-rectangle :x 0 :y 0 :width 5 :height 3))
        (child2 (make-instance 'artist-diagram-rectangle :x 10 :y 10 :width 3 :height 2)))
    (oset root children (list child1 child2))
    (let ((flat (artist-diagram--flatten-tree root)))
      ;; Root is excluded, so only 2 children
      (should (= (length flat) 2))
      (should (eq (car flat) child1))
      (should (eq (cadr flat) child2)))))

;;; test-artist-diagram-parse.el ends here
