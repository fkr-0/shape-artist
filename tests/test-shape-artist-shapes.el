;;; test-shape-artist-shapes.el --- ERT tests for shape operations -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-core)
(require 'shape-artist-shapes)

;;; --- Helper ---

(defmacro test-artist-with-shapes-buffer (content &rest body)
  "Create a temp buffer with CONTENT, disable sidebar, parse shapes, run BODY."
  (declare (indent 1))
  `(test-artist-with-buffer
       ,content
     (let ((shape-artist--update-sidebar-fn nil))
       (shape-artist-shapes-parse)
       ,@body)))

;;; --- Shape Parsing Tests ---

(ert-deftest test-shape-artist-shapes-parse-updates-state ()
  "Test that parse updates buffer-local variables."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (should shape-artist--last-root)
      (should shape-artist--last-shapes-list)
      (should (> (length shape-artist--last-shapes-list) 0)))))

(ert-deftest test-shape-artist-shapes-parse-empty-buffer ()
  "Test parse on buffer with no shapes."
  (test-artist-with-buffer
      "hello world"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (should shape-artist--last-root)
      (should (= (length shape-artist--last-shapes-list) 0)))))

;;; --- shapes-current Tests ---

(ert-deftest test-shape-artist-shapes-current-returns-shape ()
  "Test that shapes-current returns the shape at the current index."
  (test-artist-with-shapes-buffer
      "+---+\n|   |\n+---+"
    (setq shape-artist--select-index 0)
    (let ((shape (shape-artist-shapes-current)))
      (should shape)
      (should (shape-artist-diagram-rectangle-p shape)))))

(ert-deftest test-shape-artist-shapes-current-out-of-bounds ()
  "Test that shapes-current returns nil when index is out of bounds."
  (test-artist-with-shapes-buffer
      "+---+\n|   |\n+---+"
    (setq shape-artist--select-index 999)
    (should-not (shape-artist-shapes-current))))

(ert-deftest test-shape-artist-shapes-current-empty-list ()
  "Test shapes-current when no shapes detected."
  (test-artist-with-buffer
      ""
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (setq shape-artist--select-index 0)
      (should-not (shape-artist-shapes-current)))))

;;; --- shapes-count Tests ---

(ert-deftest test-shape-artist-shapes-count-one ()
  "Test shape count with a single rectangle."
  (test-artist-with-shapes-buffer
      "+---+\n|   |\n+---+"
    (should (= (shape-artist-shapes-count) 1))))

(ert-deftest test-shape-artist-shapes-count-multiple ()
  "Test shape count with multiple rectangles."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+\n\n+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (should (>= (shape-artist-shapes-count) 2)))))

(ert-deftest test-shape-artist-shapes-count-empty ()
  "Test shape count with no shapes."
  (test-artist-with-buffer
      "hello"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (should (= (shape-artist-shapes-count) 0)))))

;;; --- read-rect-interior Tests ---

(ert-deftest test-shape-artist-read-rect-interior ()
  "Test reading interior text of a rectangle."
  (test-artist-with-buffer
      "+---+\n|abc|\n|def|\n+---+"
    (let ((interior (shape-artist--read-rect-interior 0 0 4 3)))
      (should (= (length interior) 2))
      (should (string-match-p "abc" (nth 0 interior)))
      (should (string-match-p "def" (nth 1 interior))))))

(ert-deftest test-shape-artist-read-rect-interior-empty ()
  "Test reading interior of a 2x2 rectangle (no interior rows)."
  (test-artist-with-buffer
      "+--+\n+--+"
    (let ((interior (shape-artist--read-rect-interior 0 0 3 1)))
      (should (= (length interior) 0)))))

;;; --- erase-rect Tests ---

(ert-deftest test-shape-artist-erase-rect ()
  "Test that erase-rect fills rect area with spaces."
  (test-artist-with-buffer
      "+---+\n|abc|\n|def|\n+---+"
    (shape-artist--erase-rect 0 0 4 3)
    (save-excursion
      (goto-char (point-min))
      (forward-line 0)
      (move-to-column 0)
      (should (eq (char-after) ?\s))
      (move-to-column 4)
      (should (eq (char-after) ?\s))
      (forward-line 1)
      (move-to-column 0)
      (should (eq (char-after) ?\s))
      (move-to-column 4)
      (should (eq (char-after) ?\s)))))

;;; --- draw-rect-border Tests ---

(ert-deftest test-shape-artist-draw-rect-border ()
  "Test that draw-rect-border creates proper border characters.
Rect at (1,1) w=3 h=2 on 4-line buffer of spaces."
  (test-artist-with-buffer
      "         \n         \n         \n         "
    (shape-artist--draw-rect-border 1 1 3 2)
    (save-excursion
      (goto-char (point-min))
      ;; Top border (line 2, row 1): +--+
      (forward-line 1)
      (move-to-column 1)
      (should (eq (char-after) ?+))
      (move-to-column 2)
      (should (eq (char-after) ?-))
      (move-to-column 3)
      (should (eq (char-after) ?-))
      (move-to-column 4)
      (should (eq (char-after) ?+))
      ;; Middle row (line 3, row 2): |  |
      (forward-line 1)
      (move-to-column 1)
      (should (eq (char-after) ?|))
      (move-to-column 4)
      (should (eq (char-after) ?|))
      ;; Bottom border (line 4, row 3): +--+
      (forward-line 1)
      (move-to-column 1)
      (should (eq (char-after) ?+))
      (move-to-column 4)
      (should (eq (char-after) ?+)))))

(ert-deftest test-shape-artist-draw-rect-border-heavy ()
  "Test heavy border style."
  (test-artist-with-buffer
      "         \n         \n         "
    (shape-artist--draw-rect-border 0 0 2 1 ?=)
    (save-excursion
      (goto-char (point-min))
      (should (eq (char-after) ?+))
      (move-to-column 1)
      (should (eq (char-after) ?=))
      (move-to-column 2)
      (should (eq (char-after) ?+)))))

;;; --- write-rect-interior Tests ---

(ert-deftest test-shape-artist-write-rect-interior ()
  "Test writing text into rectangle interior."
  (test-artist-with-buffer
      "         \n         \n         \n         "
    (shape-artist--draw-rect-border 1 1 5 3)
    (shape-artist--write-rect-interior 1 1 5 3 '("hello"))
    (save-excursion
      (goto-char (point-min))
      ;; rect at y=1, interior row 1 is at buffer line y+1 = line 2
      ;; interior columns: x+1 to x+w-1 = 2 to 5 (4 cols)
      ;; text "hello" truncated to 4 chars: "hell"
      (forward-line 2)
      (move-to-column 2)
      (should (eq (char-after) ?h))
      (move-to-column 3)
      (should (eq (char-after) ?e))
      (move-to-column 4)
      (should (eq (char-after) ?l))
      (move-to-column 5)
      (should (eq (char-after) ?l)))))

(ert-deftest test-shape-artist-write-rect-interior-truncates ()
  "Test that write-rect-interior truncates long text."
  (test-artist-with-buffer
      "         \n         \n         "
    (shape-artist--draw-rect-border 0 0 3 2)
    (shape-artist--write-rect-interior 0 0 3 2 '("toolong"))
    (save-excursion
      (goto-char (point-min))
      ;; rect at y=0, interior row 1 is at buffer line y+1 = line 1
      ;; interior columns: x+1 to x+w-1 = 1 to 2 (2 cols)
      ;; text "toolong" truncated to 2 chars: "to"
      (forward-line 1)
      (move-to-column 1)
      (should (eq (char-after) ?t))
      (move-to-column 2)
      (should (eq (char-after) ?o)))))

(ert-deftest test-shape-artist-write-rect-interior-pads ()
  "Test that write-rect-interior pads short text with spaces."
  (test-artist-with-buffer
      "         \n         \n         "
    (shape-artist--draw-rect-border 0 0 4 2)
    (shape-artist--write-rect-interior 0 0 4 2 '("hi"))
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (move-to-column 1)
      (should (eq (char-after) ?h))
      (move-to-column 2)
      (should (eq (char-after) ?i))
      (move-to-column 3)
      (should (eq (char-after) ?\s)))))

;;; --- edit-label Tests ---

(ert-deftest test-shape-artist-edit-label-center ()
  "Test editing a label with center alignment."
  (test-artist-with-buffer
      "+------+\n|      |\n+------+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((shape (nth 0 shape-artist--last-shapes-list)))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "foo")))
          (shape-artist-shapes-edit-label shape 'center))
        (save-excursion
          (goto-char (point-min))
          (forward-line 1)
          (move-to-column 2)
          (should (eq (char-after) ?f))
          (move-to-column 3)
          (should (eq (char-after) ?o))
          (move-to-column 4)
          (should (eq (char-after) ?o)))))))

(ert-deftest test-shape-artist-edit-label-left ()
  "Test editing a label with left alignment."
  (test-artist-with-buffer
      "+------+\n|      |\n+------+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((shape (nth 0 shape-artist--last-shapes-list)))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "bar")))
          (shape-artist-shapes-edit-label shape 'left))
        (save-excursion
          (goto-char (point-min))
          (forward-line 1)
          (move-to-column 1)
          (should (eq (char-after) ?b))
          (move-to-column 2)
          (should (eq (char-after) ?a))
          (move-to-column 3)
          (should (eq (char-after) ?r)))))))

(ert-deftest test-shape-artist-edit-label-right ()
  "Test editing a label with right alignment."
  (test-artist-with-buffer
      "+------+\n|      |\n+------+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((shape (nth 0 shape-artist--last-shapes-list)))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "x")))
          (shape-artist-shapes-edit-label shape 'right))
        (save-excursion
          (goto-char (point-min))
          (forward-line 1)
          ;; interior-width=6, text "x" len 1, start = 1 + (6-1) = 6
          (move-to-column 6)
          (should (eq (char-after) ?x)))))))

;;; --- shapes-move Tests ---

(ert-deftest test-shape-artist-shapes-move ()
  "Test moving a shape by delta."
  (test-artist-with-buffer
      "         \n         \n         \n         \n         "
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist--draw-rect-border 1 1 3 2)
      (shape-artist-shapes-parse)
      (let ((shape (nth 0 shape-artist--last-shapes-list)))
        (shape-artist-shapes-move shape 2 1)
        (should (= (oref shape x) 3))
        (should (= (oref shape y) 2))))))

(ert-deftest test-shape-artist-shapes-move-no-op ()
  "Test moving shape by zero delta."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((shape (nth 0 shape-artist--last-shapes-list))
            (orig-x (oref (nth 0 shape-artist--last-shapes-list) x))
            (orig-y (oref (nth 0 shape-artist--last-shapes-list) y)))
        (shape-artist-shapes-move shape 0 0)
        (should (= (oref shape x) orig-x))
        (should (= (oref shape y) orig-y))))))

;;; --- shapes-resize Tests ---

(ert-deftest test-shape-artist-shapes-resize ()
  "Test resizing a shape."
  (test-artist-with-buffer
      "            \n            \n            \n            \n            "
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist--draw-rect-border 0 0 4 3)
      (shape-artist-shapes-parse)
      (let ((shape (nth 0 shape-artist--last-shapes-list)))
        (shape-artist-shapes-resize shape 6 4)
        (should (= (oref shape width) 6))
        (should (= (oref shape height) 4))))))

(ert-deftest test-shape-artist-shapes-resize-minimum ()
  "Test that resize rejects width < 2."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((shape (nth 0 shape-artist--last-shapes-list)))
        (should-error (shape-artist-shapes-resize shape 1 3)
                      :type 'user-error)))))

(ert-deftest test-shape-artist-shapes-resize-minimum-height ()
  "Test that resize rejects height < 2."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((shape (nth 0 shape-artist--last-shapes-list)))
        (should-error (shape-artist-shapes-resize shape 3 1)
                      :type 'user-error)))))

;;; --- shapes-replace Tests ---

(ert-deftest test-shape-artist-shapes-replace-heavy ()
  "Test replacing border style to heavy."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((shape (nth 0 shape-artist--last-shapes-list)))
        (shape-artist-shapes-replace shape 'heavy)
        (save-excursion
          (goto-char (point-min))
          (move-to-column 1)
          (should (eq (char-after) ?=)))))))

(ert-deftest test-shape-artist-shapes-replace-light ()
  "Test replacing border style to light."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((shape (nth 0 shape-artist--last-shapes-list)))
        (shape-artist-shapes-replace shape 'light)
        (save-excursion
          (goto-char (point-min))
          (move-to-column 1)
          (should (eq (char-after) ?.)))))))

(ert-deftest test-shape-artist-shapes-replace-standard ()
  "Test replacing border style back to standard from heavy."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((shape (nth 0 shape-artist--last-shapes-list)))
        ;; First change to heavy, then back to standard
        (shape-artist-shapes-replace shape 'heavy)
        (shape-artist-shapes-replace shape 'standard)
        (save-excursion
          (goto-char (point-min))
          (move-to-column 1)
          (should (eq (char-after) ?-)))))))

;;; --- Rectangle Buffer Primitives Integration ---

(ert-deftest test-shape-artist-erase-and-draw-roundtrip ()
  "Test erase then redraw produces valid border."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((shape (nth 0 shape-artist--last-shapes-list)))
        (shape-artist--erase-rect (oref shape x) (oref shape y)
                                  (oref shape width) (oref shape height))
        (shape-artist--draw-rect-border (oref shape x) (oref shape y)
                                        (oref shape width) (oref shape height))
        (save-excursion
          (goto-char (point-min))
          (should (eq (char-after) ?+))
          (move-to-column (oref shape width))
          (should (eq (char-after) ?+))
          (forward-line (oref shape height))
          (move-to-column (oref shape x))
          (should (eq (char-after) ?+)))))))

;;; test-shape-artist-shapes.el ends here
