;;; test-shape-artist-guides.el --- ERT tests for guide snapping -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-core)
(require 'shape-artist-shapes)
(require 'shape-artist-guides)
(require 'shape-artist-ui)

(defmacro test-artist-with-guides-buffer (content &rest body)
  "Create a temp Shape Artist CONTENT buffer and run BODY."
  (declare (indent 1))
  `(test-artist-with-buffer
       ,content
     (let ((shape-artist--update-sidebar-fn nil)
           (shape-artist-guide-snap-distance 2)
           (shape-artist-guides--guides nil)
           (shape-artist-guides--next-id 1)
           (shape-artist-guides--overlays nil)
           (shape-artist-guides--grid-overlays nil)
           (shape-artist-guides-visible t)
           (shape-artist-guides-snap-enabled t)
           (shape-artist-grid-visible nil)
           (shape-artist-grid-snap-enabled nil)
           (shape-artist-guides--last-snap-report nil))
       (shape-artist-shapes-parse)
       ,@body)))

(defun test-shape-artist-guides--first-rect ()
  "Return first parsed rectangle in the current test buffer."
  (cl-find-if #'shape-artist-diagram-rectangle-p shape-artist--last-shapes-list))

(defun test-shape-artist-guides--rects ()
  "Return parsed rectangles in the current test buffer."
  (cl-remove-if-not #'shape-artist-diagram-rectangle-p shape-artist--last-shapes-list))

(ert-deftest test-shape-artist-guides-adds-horizontal-and-vertical ()
  "Guide creation stores orientation, position, and monotonic ids."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (let ((h (shape-artist-guides-add 'horizontal 1 "row" 'manual))
          (v (shape-artist-guides-add 'vertical 3 "col" 'manual)))
      (should (= (shape-artist-guide-id h) 1))
      (should (= (shape-artist-guide-id v) 2))
      (should (eq (shape-artist-guide-orientation h) 'horizontal))
      (should (eq (shape-artist-guide-orientation v) 'vertical))
      (should (= (shape-artist-guide-position h) 1))
      (should (= (shape-artist-guide-position v) 3)))))

(ert-deftest test-shape-artist-guides-nearest-is-deterministic ()
  "Nearest guide lookup uses distance, then lower id for ties."
  (test-artist-with-guides-buffer
      "abc"
    (let ((g1 (shape-artist-guides-add 'vertical 2 nil 'manual))
          (_g2 (shape-artist-guides-add 'vertical 4 nil 'manual)))
      (should (eq (shape-artist-guides-find-nearest 'vertical 3) g1)))))

(ert-deftest test-shape-artist-guides-hidden-policy ()
  "Hidden guides are still active unless visible-snaps-only is set."
  (test-artist-with-guides-buffer
      "abc"
    (let ((guide (shape-artist-guides-add 'vertical 2 nil 'manual)))
      (setf (shape-artist-guide-visible guide) nil)
      (let ((shape-artist-guide-visible-snaps-only nil))
        (should (shape-artist-guides-find-nearest 'vertical 2 t)))
      (let ((shape-artist-guide-visible-snaps-only t))
        (should-not (shape-artist-guides-find-nearest 'vertical 2 t))))))

(ert-deftest test-shape-artist-guides-snap-rectangle-move-left-edge ()
  "Moving near a vertical guide snaps the rectangle left edge."
  (test-artist-with-guides-buffer
      "    +---+\n    |   |\n    +---+"
    (shape-artist-guides-add 'vertical 2 nil 'manual)
    (let* ((rect (test-shape-artist-guides--first-rect))
           (snap (shape-artist-guides-snap-rectangle-move rect -1 0)))
      (should (equal (butlast snap 1) '(-2 0))))))

(ert-deftest test-shape-artist-guides-snap-rectangle-move-outside-distance ()
  "Movement outside snap distance remains unchanged."
  (test-artist-with-guides-buffer
      "    +---+\n    |   |\n    +---+"
    (shape-artist-guides-add 'vertical 0 nil 'manual)
    (let* ((rect (test-shape-artist-guides--first-rect))
           (snap (shape-artist-guides-snap-rectangle-move rect -1 0)))
      (should (equal (butlast snap 1) '(-1 0))))))

(ert-deftest test-shape-artist-guides-move-rectangle-snaps-buffer-shape ()
  "The existing rectangle move command uses snapped deltas."
  (test-artist-with-guides-buffer
      "    +---+\n    |   |\n    +---+"
    (shape-artist-guides-add 'vertical 2 nil 'manual)
    (let ((rect (test-shape-artist-guides--first-rect)))
      (shape-artist-shapes-move rect -1 0)
      (let ((moved (test-shape-artist-guides--first-rect)))
        (should (= (oref moved x) 2))))))

(ert-deftest test-shape-artist-guides-group-move-snaps-once ()
  "Group move applies one snapped delta to every selected rectangle."
  (test-artist-with-guides-buffer
      "    +---+   +---+\n    | A |---| B |\n    +---+   +---+"
    (let* ((rects (test-shape-artist-guides--rects))
           (x0 (mapcar (lambda (r) (oref r x)) rects)))
      (shape-artist-guides-add 'vertical 2 nil 'manual)
      (shape-artist-shapes-move-group rects -1 0)
      (let ((x1 (mapcar (lambda (r) (oref r x)) (test-shape-artist-guides--rects))))
        (should (equal x1 (mapcar (lambda (x) (- x 2)) x0)))))))

(ert-deftest test-shape-artist-guides-resize-snaps-right-edge ()
  "Anchored resize snaps the moving right edge to a vertical guide."
  (test-artist-with-guides-buffer
      "    +---+\n    |   |\n    +---+"
    (shape-artist-guides-add 'vertical 10 nil 'manual)
    (let* ((rect (test-shape-artist-guides--first-rect))
           (old-width (oref rect width)))
      (shape-artist-shapes-resize-anchored rect 'right 1 'bottom 0)
      (let ((resized (test-shape-artist-guides--first-rect)))
        (should (= (+ (oref resized x) (oref resized width)) 10))
        (should (= (oref resized width) (+ old-width 2)))))))

(ert-deftest test-shape-artist-guides-overlay-does-not_mutate_buffer ()
  "Guide overlay refresh must not alter buffer text."
  (test-artist-with-guides-buffer
      "abc\ndef"
    (let ((before (buffer-string)))
      (shape-artist-guides-add 'horizontal 0 nil 'manual)
      (shape-artist-guides-refresh-overlays)
      (should shape-artist-guides--overlays)
      (should (equal (buffer-string) before))
      (shape-artist-guide-clear-all)
      (should-not shape-artist-guides--overlays)
      (should (equal (buffer-string) before)))))

(ert-deftest test-shape-artist-guides-grid-overlay-does-not_mutate_buffer ()
  "Grid overlay toggling must not alter buffer text."
  (test-artist-with-guides-buffer
      "abcd\nefgh"
    (let ((before (buffer-string)))
      (shape-artist-grid-toggle)
      (should shape-artist-guides--grid-overlays)
      (should (equal (buffer-string) before))
      (shape-artist-grid-toggle)
      (should-not shape-artist-guides--grid-overlays)
      (should (equal (buffer-string) before)))))

(ert-deftest test-shape-artist-guides-sidebar-section ()
  "Guide sidebar text lists guide state and positions."
  (test-artist-with-guides-buffer
      "abc"
    (shape-artist-guides-add 'horizontal 1 "row" 'manual)
    (shape-artist-guides-add 'vertical 2 "col" 'manual)
    (let ((text (shape-artist-guides-section-text)))
      (should (string-match-p "=== Guides ===" text))
      (should (string-match-p "horizontal" text))
      (should (string-match-p "vertical" text))
      (should (string-match-p "Snap: on" text)))))

(ert-deftest test-shape-artist-guides-commands-use-current-position ()
  "Interactive guide creation commands default to current row/column."
  (test-artist-with-guides-buffer
      "abc\ndef"
    (shape-artist--goto-row-col 1 2)
    (shape-artist-guide-create-horizontal)
    (shape-artist-guide-create-vertical)
    (should (shape-artist-guides-find-nearest 'horizontal 1))
    (should (shape-artist-guides-find-nearest 'vertical 2))))

(provide 'test-shape-artist-guides)
;;; test-shape-artist-guides.el ends here
