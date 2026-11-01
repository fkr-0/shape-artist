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

(ert-deftest test-shape-artist-shapes-move-preserves-unicode-border-family ()
  "Moving a Unicode rectangle keeps its detected border family intact."
  (test-artist-with-buffer
      "        \n╔═══╗   \n║ x ║   \n╚═══╝   \n        "
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((shape (cl-find-if #'shape-artist-diagram-rectangle-p
                               shape-artist--last-shapes-list)))
        (should (eq (shape-artist-shapes-border-style shape) 'unicode-double))
        (shape-artist-shapes-move shape 1 0)
        (shape-artist-shapes-parse)
        (let ((moved (cl-find-if #'shape-artist-diagram-rectangle-p
                                 shape-artist--last-shapes-list)))
          (should moved)
          (should (eq (shape-artist-shapes-border-style moved) 'unicode-double))
          (save-excursion
            (goto-char (point-min))
            (forward-line 1)
            (move-to-column 1)
            (should (eq (char-after) ?╔))))))))

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

(ert-deftest test-shape-artist-shapes-move-rectangle-sticks-attached-edge ()
  "Moving a rectangle moves a straight edge attached to its side."
  (test-artist-with-shapes-buffer
      "+--+    \n|  |--- \n+--+    \n        "
    (let ((rect (cl-find-if #'shape-artist-diagram-rectangle-p
                            shape-artist--last-shapes-list)))
      (shape-artist-shapes-move rect 0 1)
      (shape-artist-shapes-parse)
      (let ((edge (cl-find-if #'shape-artist-diagram-edge-p
                              shape-artist--last-shapes-list)))
        (should (= (oref rect y) 1))
        (should (= (oref edge y) 2))
        (should (= (oref edge y2) 2))
        (should (equal (plist-get (oref edge start-attachment) :rect)
                       '(0 1 3 2)))
        (should (eq (plist-get (oref edge start-attachment) :side)
                    'right))))))

(ert-deftest test-shape-artist-shapes-move-group-preserves-connector ()
  "Moving connected rectangles as a group redraws their connector once."
  (test-artist-with-shapes-buffer
      "+--+   +--+\n|  |---|  |\n+--+   +--+\n           "
    (let ((rects (cl-remove-if-not #'shape-artist-diagram-rectangle-p
                                   shape-artist--last-shapes-list)))
      (shape-artist-shapes-move-group rects 0 1)
      (shape-artist-shapes-parse)
      (let ((edges (cl-remove-if-not #'shape-artist-diagram-edge-p
                                     shape-artist--last-shapes-list))
            (ys (sort (mapcar (lambda (shape) (oref shape y))
                              (cl-remove-if-not #'shape-artist-diagram-rectangle-p
                                                shape-artist--last-shapes-list))
                      #'<)))
        (should (equal ys '(1 1)))
        (should (= (length edges) 1))
        (should (= (oref (car edges) y) 2))
        (should (= (oref (car edges) y2) 2))
        (should (eq (oref (car edges) edge-type) 'connected))))))

;;; --- Identity and Style Tests ---

(defun test-shape-artist-shapes--rects ()
  "Return parsed rectangles of the current test buffer."
  (cl-remove-if-not #'shape-artist-diagram-rectangle-p shape-artist--last-shapes-list))

(defun test-shape-artist-shapes--edges ()
  "Return parsed edges of the current test buffer."
  (cl-remove-if-not #'shape-artist-diagram-edge-p shape-artist--last-shapes-list))

(defun test-shape-artist-shapes--edge-coords (edge)
  "Return (X Y X2 Y2) of EDGE."
  (list (oref edge x) (oref edge y) (oref edge x2) (oref edge y2)))

(defun test-shape-artist-shapes--rect-box (rect)
  "Return (X Y WIDTH HEIGHT) of RECT."
  (list (oref rect x) (oref rect y) (oref rect width) (oref rect height)))

(ert-deftest test-shape-artist-shapes-signature-label-and-node ()
  "Labels and bare nodes get their own signature shapes."
  (should (equal (shape-artist-shapes-signature
                  (make-instance 'shape-artist-diagram-label :text "db" :x 1 :y 2))
                 '(label 1 2 "db")))
  (should (equal (shape-artist-shapes-signature
                  (make-instance 'shape-artist-diagram-node
                                 :x 1 :y 2 :width 3 :height 4))
                 '(node 1 2 3 4))))

(ert-deftest test-shape-artist-shapes-find-rectangle-by-geometry ()
  "A rectangle is found only by its exact position and size."
  (test-artist-with-shapes-buffer
      "+---+\n|   |\n+---+"
    (should (eq (shape-artist-shapes--find-rectangle 0 0 4 2)
                (car (test-shape-artist-shapes--rects))))
    (should-not (shape-artist-shapes--find-rectangle 0 0 4 3))
    (should-not (shape-artist-shapes--find-rectangle 1 0 4 2))))

(ert-deftest test-shape-artist-shapes-border-style-falls-back-to-border-glyph ()
  "Without a detected style the border style is read from the top border."
  (dolist (case '(("+===+\n|   |\n+===+" . heavy)
                  ("+...+\n|   |\n+...+" . light)
                  ("+---+\n|   |\n+---+" . standard)
                  ("┌───┐\n│   │\n└───┘" . unicode-single)
                  ("╔═══╗\n║   ║\n╚═══╝" . unicode-double)))
    (ert-info ((format "expecting %s" (cdr case)))
      (test-artist-with-buffer (car case)
        (let ((bare (make-instance 'shape-artist-diagram-rectangle
                                   :x 0 :y 0 :width 4 :height 2)))
          (should-not (shape-artist-diagram-node-detected-style bare))
          (should (eq (shape-artist-shapes-border-style bare) (cdr case))))))))

(ert-deftest test-shape-artist-shapes-edge-style-from-arrow-flags ()
  "Edge style is derived from the arrow-start and arrow-end flags."
  (let ((mk (lambda (&rest args)
              (apply #'make-instance 'shape-artist-diagram-edge
                     :x 0 :y 0 :x2 3 :y2 0 args))))
    (should (eq (shape-artist-shapes-edge-style (funcall mk)) 'plain))
    (should (eq (shape-artist-shapes-edge-style (funcall mk :arrow-end t)) 'arrow-end))
    (should (eq (shape-artist-shapes-edge-style
                 (funcall mk :arrow-start t :arrow-end t))
                'arrow-both))
    (should-not (shape-artist-shapes-edge-style
                 (make-instance 'shape-artist-diagram-node)))))

;;; --- Drawing Primitive Tests ---

(ert-deftest test-shape-artist-shapes-draw-rect-border-glyphs-per-style ()
  "Every supported border style draws its own corner and edge glyphs."
  (dolist (case '((rounded ?/ ?\\ ?\\ ?/ ?- ?|)
                  (unicode-single ?┌ ?┐ ?└ ?┘ ?─ ?│)
                  (unicode-rounded ?╭ ?╮ ?╰ ?╯ ?─ ?│)
                  (unicode-double ?╔ ?╗ ?╚ ?╝ ?═ ?║)
                  (heavy ?+ ?+ ?+ ?+ ?= ?|)
                  (light ?+ ?+ ?+ ?+ ?. ?|)
                  (standard ?+ ?+ ?+ ?+ ?- ?|)))
    (pcase-let ((`(,style ,tl ,tr ,bl ,br ,h ,v) case))
      (ert-info ((format "style %s" style))
        (test-artist-with-buffer "      \n      \n      "
          (shape-artist--draw-rect-border 0 0 4 2 style)
          (let ((at (lambda (row col)
                      (save-excursion (shape-artist--goto-row-col row col)
                                      (char-after)))))
            (should (equal (list (funcall at 0 0) (funcall at 0 4)
                                 (funcall at 2 0) (funcall at 2 4)
                                 (funcall at 0 2) (funcall at 1 0))
                           (list tl tr bl br h v)))))))))

(ert-deftest test-shape-artist-shapes-draw-and-erase-edges-in-both-axes ()
  "Edges draw with the right glyphs and arrowheads, and erase to blanks."
  (test-artist-with-buffer "   \n   \n   "
    (shape-artist--draw-edge 1 0 1 2 'arrow-both)
    (should (equal (shape-artist--get-lines) '(" ^ " " | " " v ")))
    (shape-artist--erase-edge 1 0 1 2)
    (should (equal (shape-artist--get-lines) '("   " "   " "   ")))
    (shape-artist--draw-edge 1 0 1 2 'arrow-end)
    (should (equal (shape-artist--get-lines) '(" | " " | " " v ")))
    (shape-artist--draw-edge 1 0 1 2 'plain)
    (should (equal (shape-artist--get-lines) '(" | " " | " " | ")))
    (shape-artist--erase-edge 0 1 2 1)
    (should (equal (shape-artist--get-lines) '(" | " "   " " | "))))
  (test-artist-with-buffer "    "
    (shape-artist--draw-edge 0 0 3 0 'arrow-both)
    (should (equal (shape-artist--get-lines) '("<-->")))))

(ert-deftest test-shape-artist-shapes-shift-all-shapes-moves-rects-and-edges ()
  "Shifting translates every known shape, both ends of edges included."
  (with-temp-buffer
    (let ((rect (make-instance 'shape-artist-diagram-rectangle
                               :x 1 :y 1 :width 3 :height 2))
          (edge (make-instance 'shape-artist-diagram-edge
                               :x 5 :y 1 :x2 8 :y2 1)))
      (setq shape-artist--last-shapes-list (list rect edge))
      (shape-artist--shift-all-shapes 0 0)
      (should (equal (test-shape-artist-shapes--rect-box rect) '(1 1 3 2)))
      (shape-artist--shift-all-shapes 2 3)
      (should (equal (test-shape-artist-shapes--rect-box rect) '(3 4 3 2)))
      (should (equal (test-shape-artist-shapes--edge-coords edge) '(7 4 10 4))))))

;;; --- Canvas Growth Tests ---

(ert-deftest test-shape-artist-shapes-move-rectangle-past-origin-grows-canvas ()
  "Moving a rectangle beyond the top-left grows the canvas instead of clipping."
  (test-artist-with-shapes-buffer
      "+--+\n|  |\n+--+"
    (let ((moved (shape-artist-shapes-move (car (test-shape-artist-shapes--rects)) -2 -1)))
      (should (equal (test-shape-artist-shapes--rect-box moved) '(0 0 3 2)))
      (should (equal (shape-artist--get-lines)
                     '("+--+  " "|  |  " "+--+  " "      "))))))

(ert-deftest test-shape-artist-shapes-move-edge-past-origin-grows-canvas ()
  "Moving an edge beyond the left or top keeps it whole by growing the canvas."
  (test-artist-with-shapes-buffer "---"
    (let ((moved (shape-artist-shapes-move (car (test-shape-artist-shapes--edges)) -1 0)))
      (should (equal (test-shape-artist-shapes--edge-coords moved) '(0 0 2 0)))
      (should (equal (shape-artist--get-lines) '("--- ")))))
  (test-artist-with-shapes-buffer " |\n |\n |"
    (let ((moved (shape-artist-shapes-move (car (test-shape-artist-shapes--edges)) 0 -1)))
      (should (equal (test-shape-artist-shapes--edge-coords moved) '(1 0 1 2))))))

(ert-deftest test-shape-artist-shapes-resize-anchored-grows-past-origin ()
  "Growing a rectangle left or up from the origin extends the canvas."
  (test-artist-with-shapes-buffer "+---+\n|   |\n+---+"
    (let ((grown (shape-artist-shapes-resize-anchored
                  (car (test-shape-artist-shapes--rects)) 'left 1 'bottom 0)))
      (should (equal (test-shape-artist-shapes--rect-box grown) '(0 0 5 2)))
      (should (equal (shape-artist--get-lines) '("+----+" "|    |" "+----+")))))
  (test-artist-with-shapes-buffer "+---+\n|   |\n+---+"
    (let ((grown (shape-artist-shapes-resize-anchored
                  (car (test-shape-artist-shapes--rects)) 'right 0 'top 1)))
      (should (equal (test-shape-artist-shapes--rect-box grown) '(0 0 4 3)))
      (should (equal (shape-artist--get-lines)
                     '("+---+" "|   |" "|   |" "+---+"))))))

(ert-deftest test-shape-artist-shapes-edit-label-grows-rectangle-to-fit ()
  "A label longer than the interior widens the rectangle around it."
  (test-artist-with-shapes-buffer "+--+\n|  |\n+--+"
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "hello")))
      (let ((grown (shape-artist-shapes-edit-label
                    (car (test-shape-artist-shapes--rects)) 'center)))
        (should (equal (test-shape-artist-shapes--rect-box grown) '(0 0 6 2)))
        (should (equal (shape-artist--get-lines)
                       '("+-----+" "|hello|" "+-----+")))))))

;;; --- Connector Tracking Tests ---

(ert-deftest test-shape-artist-shapes-move-rectangle-sideways-drags-vertical-connector ()
  "A vertical connector hanging off a box follows it when the box moves sideways."
  (test-artist-with-shapes-buffer
      "+--+\n|  |\n+--+\n |  \n |  \n    "
    (let ((rect (car (test-shape-artist-shapes--rects))))
      (shape-artist-shapes-move rect 2 0)
      (let ((edge (car (test-shape-artist-shapes--edges))))
        (should (equal (test-shape-artist-shapes--rect-box
                        (car (test-shape-artist-shapes--rects)))
                       '(2 0 3 2)))
        (should (equal (test-shape-artist-shapes--edge-coords edge) '(3 3 3 4)))
        (should (eq (plist-get (oref edge start-attachment) :side) 'bottom))))))

(ert-deftest test-shape-artist-shapes-move-rectangle-down-keeps-connector-free-end-fixed ()
  "Moving a box along its vertical connector shortens it; the free end stays put."
  (test-artist-with-shapes-buffer
      "+--+\n|  |\n+--+\n |  \n |  \n |  \n    "
    (shape-artist-shapes-move (car (test-shape-artist-shapes--rects)) 0 1)
    (should (equal (test-shape-artist-shapes--edge-coords
                    (car (test-shape-artist-shapes--edges)))
                   '(1 4 1 5)))))

(ert-deftest test-shape-artist-shapes-move-past-origin-keeps-connector-attached ()
  "Growing the canvas while moving a box keeps its connector attached."
  (let ((content "    \n +--+\n |  |\n +--+\n  |  \n  |  \n     "))
    (dolist (mover (list (lambda (rects) (shape-artist-shapes-move (car rects) -2 -2))
                         (lambda (rects) (shape-artist-shapes-move-group rects -2 -2))))
      (test-artist-with-shapes-buffer content
        (funcall mover (test-shape-artist-shapes--rects))
        (should (equal (test-shape-artist-shapes--rect-box
                        (car (test-shape-artist-shapes--rects)))
                       '(0 0 3 2)))
        (should (equal (test-shape-artist-shapes--edge-coords
                        (car (test-shape-artist-shapes--edges)))
                       '(1 3 1 6)))))))

;;; --- Edge Extension and Style Tests ---

(ert-deftest test-shape-artist-shapes-edge-extend-vertical-grows-and-clamps ()
  "Vertical edges extend downward and never shrink below one cell."
  (test-artist-with-shapes-buffer " |\n |\n  "
    (let ((edge (shape-artist-shapes-edge-extend
                 (car (test-shape-artist-shapes--edges)) 2)))
      (should (equal (test-shape-artist-shapes--edge-coords edge) '(1 0 1 3)))
      (should (equal (shape-artist--get-lines) '(" |" " |" " |" " |")))))
  (test-artist-with-shapes-buffer " |\n |\n |\n |"
    (let ((edge (shape-artist-shapes-edge-extend
                 (car (test-shape-artist-shapes--edges)) -9)))
      (should (equal (test-shape-artist-shapes--edge-coords edge) '(1 0 1 1))))))

(ert-deftest test-shape-artist-shapes-set-edge-style-applies-and-validates ()
  "Edge styles redraw the connector; unknown styles are rejected."
  (test-artist-with-shapes-buffer "---"
    (let ((edge (shape-artist-shapes-set-edge-style
                 (car (test-shape-artist-shapes--edges)) 'arrow-both)))
      (should (equal (shape-artist--get-lines) '("<->")))
      (should (oref edge arrow-start))
      (should (oref edge arrow-end)))
    (should-error (shape-artist-shapes-set-edge-style
                   (car (test-shape-artist-shapes--edges)) 'bogus)
                  :type 'user-error)))

(ert-deftest test-shape-artist-shapes-replace-validates-style-and-shape ()
  "Replacing a border rejects unsupported styles and ignores non-rectangles."
  (test-artist-with-shapes-buffer "+---+\n|   |\n+---+"
    (should-error (shape-artist-shapes-replace
                   (car (test-shape-artist-shapes--rects)) 'rounded)
                  :type 'user-error)
    (should (equal (shape-artist--get-lines) '("+---+" "|   |" "+---+"))))
  (test-artist-with-shapes-buffer "---"
    (should-not (shape-artist-shapes-replace
                 (car (test-shape-artist-shapes--edges)) 'heavy))))

;;; --- Interactive Spec Tests ---

(defmacro test-shape-artist-shapes--with-answers (answers &rest body)
  "Run BODY with `read-number' returning ANSWERS in order."
  (declare (indent 1))
  `(let ((answers ,answers))
     (cl-letf (((symbol-function 'read-number) (lambda (&rest _) (pop answers))))
       ,@body)))

(ert-deftest test-shape-artist-shapes-move-interactive-prompts-for-offsets ()
  "Interactively, move acts on the current candidate and prompts for dx and dy."
  (test-artist-with-shapes-buffer "+--+     \n|  |     \n+--+     "
    (setq shape-artist--select-index 0)
    (test-shape-artist-shapes--with-answers '(2 1)
      (call-interactively #'shape-artist-shapes-move))
    (should (equal (test-shape-artist-shapes--rect-box
                    (car (test-shape-artist-shapes--rects)))
                   '(2 1 3 2)))))

(ert-deftest test-shape-artist-shapes-resize-interactive-prompts-for-size ()
  "Interactively, resize prompts for the new width and height."
  (test-artist-with-shapes-buffer "+--+     \n|  |     \n+--+     \n         "
    (setq shape-artist--select-index 0)
    (test-shape-artist-shapes--with-answers '(5 3)
      (call-interactively #'shape-artist-shapes-resize))
    (should (equal (test-shape-artist-shapes--rect-box
                    (car (test-shape-artist-shapes--rects)))
                   '(0 0 5 3)))))

(ert-deftest test-shape-artist-shapes-replace-interactive-prompts-for-style ()
  "Interactively, replace prompts for a border style and applies it."
  (test-artist-with-shapes-buffer "+---+\n|   |\n+---+"
    (setq shape-artist--select-index 0)
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "heavy")))
      (call-interactively #'shape-artist-shapes-replace))
    (should (equal (car (shape-artist--get-lines)) "+===+"))))

;;; --- Connector Minimum-Length Tests ---

(ert-deftest test-shape-artist-shapes-moving-box-never-collapses-vertical-connector ()
  "A box moving into a short connector pushes its free end instead of erasing it."
  (test-artist-with-shapes-buffer
      "+--+\n|  |\n+--+\n |  \n |  \n    "
    (shape-artist-shapes-move (car (test-shape-artist-shapes--rects)) 0 1)
    (should (equal (test-shape-artist-shapes--edge-coords
                    (car (test-shape-artist-shapes--edges)))
                   '(1 4 1 5)))
    (should (equal (shape-artist--get-lines)
                   '("    " "+--+" "|  |" "+--+" " |  " " |  ")))
    ;; The connector is still attached to the box it hangs from.
    (should (eq (plist-get (oref (car (test-shape-artist-shapes--edges))
                                 start-attachment)
                           :side)
                'bottom))))

(ert-deftest test-shape-artist-shapes-moving-box-never-collapses-horizontal-connector ()
  "The same minimum length holds for a connector on the right of a box."
  (test-artist-with-shapes-buffer
      "+--+  \n|  |--\n+--+  "
    (shape-artist-shapes-move (car (test-shape-artist-shapes--rects)) 1 0)
    (should (equal (test-shape-artist-shapes--edge-coords
                    (car (test-shape-artist-shapes--edges)))
                   '(5 1 6 1)))
    (should (equal (shape-artist--get-lines)
                   '(" +--+  " " |  |--" " +--+  ")))))

(ert-deftest test-shape-artist-shapes-moving-box-pulls-free-start-of-end-attached-connector ()
  "When the stuck end is the far one, the free start is pulled along instead."
  (test-artist-with-shapes-buffer
      "   +--+\n---|  |\n   +--+"
    (shape-artist-shapes-move (car (test-shape-artist-shapes--rects)) -3 0)
    (should (equal (test-shape-artist-shapes--edge-coords
                    (car (test-shape-artist-shapes--edges)))
                   '(0 1 1 1)))
    ;; The canvas only ever grows, so ignore trailing padding.
    (should (equal (mapcar #'string-trim-right (shape-artist--get-lines))
                   '("  +--+" "--|  |" "  +--+")))))

(ert-deftest test-shape-artist-shapes-group-move-keeps-connector-minimum-length ()
  "Moving a group applies the same minimum connector length."
  (test-artist-with-shapes-buffer
      "+--+\n|  |\n+--+\n |  \n |  \n    "
    (shape-artist-shapes-move-group (test-shape-artist-shapes--rects) 0 1)
    (should (equal (test-shape-artist-shapes--edge-coords
                    (car (test-shape-artist-shapes--edges)))
                   '(1 4 1 5)))))

;;; test-shape-artist-shapes.el ends here
