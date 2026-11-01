;;; test-shape-artist-explain.el --- ERT tests for near-miss box explanations -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-core)
(require 'shape-artist-explain)

;;; --- Helpers ---

(defun test-explain--rects (text)
  "Return the (X Y WIDTH HEIGHT) of every rectangle the parser finds in TEXT."
  (test-artist-with-buffer
      text
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-diagram-parse-buffer)
      (cl-loop for shape in shape-artist--last-shapes-list
               when (shape-artist-diagram-rectangle-p shape)
               collect (list (oref shape x) (oref shape y)
                             (oref shape width) (oref shape height))))))

(defun test-explain--apply (text edits)
  "Return TEXT with EDITS, a list of (ROW COL CHAR), applied by hand."
  (let ((lines (split-string text "\n")))
    (dolist (edit edits)
      (let* ((row (nth 0 edit)) (col (nth 1 edit)) (line (nth row lines)))
        (when (< (length line) col)
          (setq line (concat line (make-string (- col (length line)) ?\s))))
        (when (= (length line) col)
          (setq line (concat line " ")))
        (setf (nth row lines)
              (concat (substring line 0 col) (string (nth 2 edit))
                      (substring line (1+ col))))))
    (mapconcat #'identity lines "\n")))

(defun test-explain--misses (text)
  "Return the near-misses of TEXT, computed from a buffer holding it."
  (test-artist-with-buffer
      text
    (shape-artist-explain-near-misses)))

(defun test-explain--sole (text)
  "Assert TEXT has exactly one near-miss, that is not detected, and return it.

Also assert that applying its edits makes the parser detect the geometry."
  (let ((misses (test-explain--misses text)))
    (should (= (length misses) 1))
    (let* ((miss (car misses))
           (geometry (list (plist-get miss :x) (plist-get miss :y)
                           (plist-get miss :width) (plist-get miss :height))))
      (should-not (member geometry (test-explain--rects text)))
      (should (member geometry
                      (test-explain--rects
                       (test-explain--apply text (plist-get miss :edits)))))
      miss)))

(defun test-explain--should-mention (miss &rest needles)
  "Assert the message of MISS contains every string in NEEDLES."
  (dolist (needle needles)
    (should (string-match-p (regexp-quote needle) (plist-get miss :message)))))

(defmacro test-explain--with-buffer (content &rest body)
  "Run BODY in a temp buffer holding CONTENT, with the sidebar disabled."
  (declare (indent 1))
  `(test-artist-with-buffer
       ,content
     (let ((shape-artist--update-sidebar-fn nil))
       ,@body)))

;;; --- Required near-miss cases ---

(ert-deftest test-shape-artist-explain-mixed-horizontal-top-bottom ()
  "A `=' bottom under a `-' top is a mixed border family."
  (let ((miss (test-explain--sole "+---+\n|   |\n+===+")))
    (should (eq (plist-get miss :problem) 'mixed-border-family))
    (should (equal (plist-get miss :edits) '((2 1 ?-) (2 2 ?-) (2 3 ?-))))
    (should (equal (list (plist-get miss :x) (plist-get miss :y)
                         (plist-get miss :width) (plist-get miss :height))
                   '(0 0 4 2)))
    (should (eq (plist-get miss :style) 'standard))
    (test-explain--should-mention
     miss "Top border uses '-' but bottom uses '='" "make the bottom match"
     "line 3" "columns 1-3")))

(ert-deftest test-shape-artist-explain-mixed-prefers-top-family ()
  "The family comes from the top edge, so a `=' top makes the bottom `='."
  (let ((miss (test-explain--sole "+===+\n|   |\n+---+")))
    (should (eq (plist-get miss :problem) 'mixed-border-family))
    (should (eq (plist-get miss :style) 'heavy))
    (should (equal (plist-get miss :edits) '((2 1 ?=) (2 2 ?=) (2 3 ?=))))
    (test-explain--should-mention
     miss "Top border uses '=' but bottom uses '-'")))

(ert-deftest test-shape-artist-explain-mixed-single-glyph-in-bottom ()
  "One stray `=' in a `-' border is reported at its exact cell."
  (let ((miss (test-explain--sole "+----+\n|    |\n+-=--+")))
    (should (eq (plist-get miss :problem) 'mixed-border-family))
    (should (equal (plist-get miss :edits) '((2 2 ?-))))
    (test-explain--should-mention miss "'='" "'-'" "line 3, column 2")))

(ert-deftest test-shape-artist-explain-mixed-vertical-sides ()
  "A unicode `│' on one side of an ASCII `|' box is a mixed family."
  (let ((miss (test-explain--sole "+---+\n|   │\n+---+")))
    (should (eq (plist-get miss :problem) 'mixed-border-family))
    (should (equal (plist-get miss :edits) '((1 4 ?|))))
    (test-explain--should-mention
     miss "Left side uses '|' but right uses '│'" "make the right match")))

(ert-deftest test-shape-artist-explain-gap-in-top-border ()
  "A blank cell in the top border is a border gap."
  (let ((miss (test-explain--sole "+-- +\n|   |\n+---+")))
    (should (eq (plist-get miss :problem) 'border-gap))
    (should (equal (plist-get miss :edits) '((0 3 ?-))))
    (test-explain--should-mention miss "top border" "gap" "line 1, column 3" "'-'")))

(ert-deftest test-shape-artist-explain-gap-in-bottom-border ()
  "A blank cell in the bottom border is a border gap."
  (let ((miss (test-explain--sole "+----+\n|    |\n+-- -+")))
    (should (eq (plist-get miss :problem) 'border-gap))
    (should (equal (plist-get miss :edits) '((2 3 ?-))))
    (test-explain--should-mention miss "bottom border" "line 3, column 3")))

(ert-deftest test-shape-artist-explain-gap-two-cells-compressed ()
  "Adjacent gap cells are described as a column range."
  (let ((miss (test-explain--sole "+------+\n|      |\n+--  --+")))
    (should (eq (plist-get miss :problem) 'border-gap))
    (should (equal (plist-get miss :edits) '((2 3 ?-) (2 4 ?-))))
    (test-explain--should-mention miss "columns 3-4")))

(ert-deftest test-shape-artist-explain-missing-top-right-corner ()
  "A blank top-right corner is a missing corner."
  (let ((miss (test-explain--sole "+---\n|   |\n+---+")))
    (should (eq (plist-get miss :problem) 'missing-corner))
    (should (equal (plist-get miss :edits) '((0 4 ?+))))
    (test-explain--should-mention miss "top-right corner" "line 1, column 4" "'+'")))

(ert-deftest test-shape-artist-explain-missing-bottom-left-corner ()
  "A blank bottom-left corner is a missing corner."
  (let ((miss (test-explain--sole "+---+\n|   |\n ---+")))
    (should (eq (plist-get miss :problem) 'missing-corner))
    (should (equal (plist-get miss :edits) '((2 0 ?+))))
    (test-explain--should-mention miss "bottom-left corner" "line 3, column 0")))

(ert-deftest test-shape-artist-explain-missing-top-left-corner ()
  "A blank top-left corner is found through the rest of the border."
  (let ((miss (test-explain--sole " ---+\n|   |\n+---+")))
    (should (eq (plist-get miss :problem) 'missing-corner))
    (should (equal (plist-get miss :edits) '((0 0 ?+))))
    (test-explain--should-mention miss "top-left corner" "line 1, column 0")))

(ert-deftest test-shape-artist-explain-missing-two-corners ()
  "Two blank opposite corners are reported together."
  (let ((miss (test-explain--sole " ---+\n|   |\n+---  ")))
    (should (eq (plist-get miss :problem) 'missing-corner))
    (should (equal (plist-get miss :edits) '((0 0 ?+) (2 4 ?+))))
    (test-explain--should-mention miss "top-left corner" "bottom-right corner")))

(ert-deftest test-shape-artist-explain-missing-left-side-char ()
  "A blank in the left column is a missing side character."
  (let ((miss (test-explain--sole "+---+\n    |\n+---+")))
    (should (eq (plist-get miss :problem) 'missing-side))
    (should (equal (plist-get miss :edits) '((1 0 ?|))))
    (test-explain--should-mention miss "left side" "line 2, column 0" "'|'")))

(ert-deftest test-shape-artist-explain-missing-right-side-char ()
  "A blank in the right column is a missing side character."
  (let ((miss (test-explain--sole "+---+\n|   |\n|    \n+---+")))
    (should (eq (plist-get miss :problem) 'missing-side))
    (should (equal (plist-get miss :edits) '((2 4 ?|))))
    (test-explain--should-mention miss "right side" "line 3, column 4")))

(ert-deftest test-shape-artist-explain-ragged-bottom-shorter ()
  "A bottom border one cell short of the top is ragged."
  (let ((miss (test-explain--sole "+---+\n|   |\n+--+")))
    (should (eq (plist-get miss :problem) 'ragged-border))
    (should (equal (plist-get miss :edits) '((2 3 ?-) (2 4 ?+))))
    (test-explain--should-mention
     miss "bottom border" "one cell shorter" "line 3, column 3" "column 4"
     "extend")))

(ert-deftest test-shape-artist-explain-ragged-bottom-longer ()
  "A bottom border one cell longer than the top is ragged."
  (let ((miss (test-explain--sole "+---+\n|   |\n+----+")))
    (should (eq (plist-get miss :problem) 'ragged-border))
    (should (equal (plist-get miss :edits) '((2 4 ?+) (2 5 ?\s))))
    (test-explain--should-mention
     miss "bottom border" "one cell longer" "line 3, column 5" "column 4" "trim")))

(ert-deftest test-shape-artist-explain-ragged-top-shorter ()
  "The top border can be the ragged one too."
  (let ((miss (test-explain--sole "+--+\n|   |\n+---+")))
    (should (eq (plist-get miss :problem) 'ragged-border))
    (should (equal (plist-get miss :edits) '((0 3 ?-) (0 4 ?+))))
    (test-explain--should-mention miss "top border" "shorter")))

(ert-deftest test-shape-artist-explain-ragged-keeps-neighbour-text ()
  "Trimming a long border blanks only the extra corner cell."
  (let* ((text "+---+\n|   |\n+----+ x")
         (miss (test-explain--sole text)))
    (should (equal (test-explain--apply text (plist-get miss :edits))
                   "+---+\n|   |\n+---+  x"))))

;;; --- Other families and layouts ---

(ert-deftest test-shape-artist-explain-unicode-single-wrong-glyph ()
  "One wrong glyph in a unicode box is explained and fixed in that family."
  (let ((miss (test-explain--sole "┌──┐\n│  │\n└─-┘")))
    (should (eq (plist-get miss :problem) 'mixed-border-family))
    (should (eq (plist-get miss :style) 'unicode-single))
    (should (equal (plist-get miss :edits) '((2 2 ?─))))
    (test-explain--should-mention miss "'-'" "'─'" "line 3, column 2")))

(ert-deftest test-shape-artist-explain-unicode-wrong-corner ()
  "A corner glyph from another unicode family is named and replaced."
  (let ((miss (test-explain--sole "┌──┐\n│  │\n└──╝")))
    (should (eq (plist-get miss :problem) 'mixed-border-family))
    (should (equal (plist-get miss :edits) '((2 3 ?┘))))
    (test-explain--should-mention miss "bottom-right corner" "'╝'" "'┘'")))

(ert-deftest test-shape-artist-explain-unicode-double-mixed-bottom ()
  "A single-line bottom under a double-line top is mixed."
  (let ((miss (test-explain--sole "╔═══╗\n║   ║\n╚───╝")))
    (should (eq (plist-get miss :style) 'unicode-double))
    (should (equal (plist-get miss :edits) '((2 1 ?═) (2 2 ?═) (2 3 ?═))))))

(ert-deftest test-shape-artist-explain-rounded-with-square-corner ()
  "A rounded box with one square corner is repaired to rounded."
  (let ((miss (test-explain--sole "/---\\\n|   |\n\\---+")))
    (should (eq (plist-get miss :style) 'rounded))
    (should (equal (plist-get miss :edits) '((2 4 ?/))))))

(ert-deftest test-shape-artist-explain-unicode-rounded-missing-side ()
  "A rounded unicode box with a missing side is repaired."
  (let ((miss (test-explain--sole "╭──╮\n│   \n╰──╯")))
    (should (eq (plist-get miss :problem) 'missing-side))
    (should (eq (plist-get miss :style) 'unicode-rounded))
    (should (equal (plist-get miss :edits) '((1 3 ?│))))))

(ert-deftest test-shape-artist-explain-box-with-label-and-offset ()
  "A near-miss away from the origin keeps its interior text untouched."
  (let* ((text "title\n\n   +------+\n   | Name |\n   +--- --+")
         (miss (test-explain--sole text)))
    (should (equal (list (plist-get miss :x) (plist-get miss :y)
                         (plist-get miss :width) (plist-get miss :height))
                   '(3 2 7 2)))
    (should (equal (plist-get miss :edits) '((4 7 ?-))))))

(ert-deftest test-shape-artist-explain-two-near-misses-in-order ()
  "Two near-miss boxes are reported by row, then column."
  (let* ((text (concat "+---+   +===+\n"
                       "|   |   |   |\n"
                       "+-=-+   +---+\n"
                       "\n"
                       "+---    +---+\n"
                       "|   |   |   |\n"
                       "+---+   +--+"))
         (misses (test-explain--misses text)))
    (should (= (length misses) 4))
    (should (equal (mapcar (lambda (m) (list (plist-get m :y) (plist-get m :x)))
                           misses)
                   '((0 0) (0 8) (4 0) (4 8))))
    (should (equal (mapcar (lambda (m) (plist-get m :problem)) misses)
                   '(mixed-border-family mixed-border-family
                     missing-corner ragged-border)))
    (let ((fixed (test-explain--apply
                  text (apply #'append (mapcar (lambda (m) (plist-get m :edits))
                                               misses)))))
      (should (= (length (test-explain--rects fixed)) 4)))))

(ert-deftest test-shape-artist-explain-near-miss-next-to-valid-box ()
  "Only the broken box is reported when a valid box sits beside it."
  (let* ((text "+---+ +---+\n| a | | b |\n+---+ +--=+")
         (miss (test-explain--sole text)))
    (should (equal (list (plist-get miss :x) (plist-get miss :y)) '(6 0)))
    (should (equal (plist-get miss :edits) '((2 9 ?-))))
    (should (member '(0 0 4 2) (test-explain--rects text)))))

(ert-deftest test-shape-artist-explain-valid-box-unaffected-by-neighbour ()
  "The valid neighbour keeps being detected after the repair."
  (let* ((text "+---+ +---+\n| a | | b |\n+---+ +--=+")
         (miss (car (test-explain--misses text)))
         (fixed (test-explain--apply text (plist-get miss :edits))))
    (should (equal (sort (test-explain--rects fixed)
                         (lambda (a b) (< (car a) (car b))))
                   '((0 0 4 2) (6 0 4 2))))))

(ert-deftest test-shape-artist-explain-accepts-string-list-and-vector-grids ()
  "The optional grid argument may be a string, a list or a vector."
  (let ((expected (test-explain--misses "+---+\n|   |\n+===+")))
    (should (equal (shape-artist-explain-near-misses "+---+\n|   |\n+===+")
                   expected))
    (should (equal (shape-artist-explain-near-misses
                    '("+---+" "|   |" "+===+"))
                   expected))
    (should (equal (shape-artist-explain-near-misses
                    (vector "+---+" "|   |" "+===+"))
                   expected))))

(ert-deftest test-shape-artist-explain-result-has-documented-keys ()
  "Every near-miss plist carries the documented keys and types."
  (let ((miss (car (test-explain--misses "+---+\n|   |\n+===+"))))
    (dolist (key '(:x :y :width :height :problem :message :edits :confidence))
      (should (plist-member miss key)))
    (should (memq (plist-get miss :confidence) '(high medium low)))
    (should (stringp (plist-get miss :message)))
    (should (memq (plist-get miss :problem)
                  '(mixed-border-family border-gap missing-corner missing-side
                    ragged-border)))))

(ert-deftest test-shape-artist-explain-confidence-tracks-edit-count ()
  "Fewer edits mean higher confidence."
  (should (eq (plist-get (car (test-explain--misses
                               "+-----+\n|     |\n+-- --+"))
                         :confidence)
              'high))
  (should (eq (plist-get (car (test-explain--misses
                               "+-----+\n|     |\n+-- - +"))
                         :confidence)
              'medium)))

(ert-deftest test-shape-artist-explain-deterministic ()
  "Repeated runs produce identical results, as do reversed box orders."
  (let ((text "+---+ +===+\n|   | |   |\n+-=-+ +---+\n\n+---\n|   |\n+---+"))
    (should (equal (test-explain--misses text) (test-explain--misses text)))
    (should (equal (test-explain--misses text)
                   (test-artist-with-buffer text
                     (shape-artist-diagram-parse-buffer)
                     (shape-artist-explain-near-misses))))))

(ert-deftest test-shape-artist-explain-max-edits-customization ()
  "`shape-artist-explain-max-edits' limits what is reported."
  (let ((text "+---+\n|   |\n+===+"))
    (let ((shape-artist-explain-max-edits 2))
      (should-not (test-explain--misses text)))
    (let ((shape-artist-explain-max-edits 0))
      (should-not (test-explain--misses text)))
    (let ((shape-artist-explain-max-edits 6))
      (should (test-explain--misses "+-----+\n|     |\n+=====+")))
    (should-not (test-explain--misses "+-----+\n|     |\n+=====+"))))

(ert-deftest test-shape-artist-explain-all-edits-are-at-most-three ()
  "No default near-miss needs more than three edits."
  (dolist (text '("+---+\n|   |\n+===+" "+---\n|   |\n+---+" "+---+\n|   |\n+----+"
                  "+---+\n    |\n+---+" "╔═══╗\n║   ║\n╚───╝"))
    (dolist (miss (test-explain--misses text))
      (should (<= 1 (length (plist-get miss :edits)) 3)))))

(ert-deftest test-shape-artist-explain-small-boxes-need-strong-evidence ()
  "A tiny box may not need more than a quarter of its perimeter in edits."
  ;; 8-cell perimeter: two repairs are fine, three are not.
  (should (test-explain--misses "+-+\n  |\n+ +"))
  (should-not (test-explain--misses "+ +\n  |\n+ +"))
  ;; The same three gaps in a larger box are fine.
  (should (test-explain--misses "+- -+\n    |\n+ --+")))

;;; --- False positives ---

(ert-deftest test-shape-artist-explain-ignores-valid-box ()
  "A box the parser already detects is never a near-miss."
  (dolist (text '("+---+\n|   |\n+---+" "+===+\n|   |\n+===+" "+...+\n|   |\n+...+"
                  "/---\\\n|   |\n\\---/" "┌──┐\n│  │\n└──┘" "╭──╮\n│  │\n╰──╯"
                  "╔══╗\n║  ║\n╚══╝" "+-+\n| |\n+-+"))
    (should-not (test-explain--misses text))))

(ert-deftest test-shape-artist-explain-ignores-clean-diagram ()
  "A diagram with several boxes, connectors and labels reports nothing."
  (let ((text (concat "+-------+      +-------+      +-------+\n"
                      "| Start |----->|  Work |----->|  End  |\n"
                      "+-------+      +-------+      +-------+\n"
                      "    |              ^\n"
                      "    v              |\n"
                      "+-------+      +-------+\n"
                      "| Queue |<-----| Retry |\n"
                      "+-------+      +-------+\n"
                      "\n"
                      "+------------------+\n"
                      "| outer  +-----+   |\n"
                      "|        | in  |   |\n"
                      "|        +-----+   |\n"
                      "+------------------+\n")))
    (should (>= (length (test-explain--rects text)) 6))
    (should-not (test-explain--misses text))))

(ert-deftest test-shape-artist-explain-ignores-connectors-and-arrows ()
  "Lone connectors, arrows and elbows are not boxes."
  (dolist (text '("+--" "+--\n|\n|" "---->" "<----" "+---+" "|\n|\n|"
                  "+-----+\n      |\n      v" "A --> B\nB --> C"
                  "+----+\n|    |\n+----+--->\n"))
    (should-not (test-explain--misses text))))

(ert-deftest test-shape-artist-explain-ignores-prose-and-code ()
  "Ordinary text, bullet lists and code are not near-misses."
  (dolist (text '("Hello world. This is a sentence.\nAnd another one - with a dash.\n"
                  "- item one\n- item two\n  - nested | pipe\n"
                  "if (a || b) {\n  return x - y;\n}\n"
                  "a + b = c\nd - e = f\n+ g\n"
                  "== Heading ==\n| not | a table\n== Done =="
                  ""))
    (should-not (test-explain--misses text))))

(ert-deftest test-shape-artist-explain-ignores-tables ()
  "Markdown, Org and grid tables are left alone."
  (dolist (text '("| a | b |\n|---|---|\n| 1 | 2 |\n"
                  "| a | b |\n|---+---|\n| 1 | 2 |\n|---+---|\n| 3 | 4 |\n"
                  "+---+---+\n| a | b |\n+---+---+\n| c | d |\n+---+---+\n"))
    (should-not (test-explain--misses text))))

(ert-deftest test-shape-artist-explain-ignores-border-junctions ()
  "A box whose border has T-junctions is not explained by a sub-box."
  (should-not (test-explain--misses
               (concat "    |\n"
                       "+---+---+\n"
                       "| PAM   |\n"
                       "+---+---+\n"
                       "    |\n"))))

(ert-deftest test-shape-artist-explain-ignores-stacked-boxes-sharing-a-wall ()
  "Boxes stacked on a shared border row are not near-misses."
  (should-not (test-explain--misses
               "+-+\n| |\n+-+\n| |\n+-+\n"))
  (should-not (test-explain--misses
               "+---+\n| a |\n+---+\n| b |\n+---+\n")))

(ert-deftest test-shape-artist-explain-ignores-unparsed-but-valid-boxes ()
  "A valid box the parser skips (shared wall) is not a heavy-family near-miss."
  ;; The second box is not detected by the parser (its top row is the first
  ;; box's bottom row) but is drawn correctly, so nothing may be reported even
  ;; though a `heavy' reading needs only two edits.
  (should-not (test-explain--misses "+-+\n| |\n+-+\n| |\n+-+"))
  (should (equal (test-explain--rects "+-+\n| |\n+-+\n| |\n+-+")
                 '((0 0 2 2)))))

(ert-deftest test-shape-artist-explain-ignores-far-off-corners ()
  "Distant corners and stray text do not combine into a box."
  (should-not (test-explain--misses
               "+---+          +---+\n\n\n+---+          +---+"))
  (should-not (test-explain--misses "+   +\n\n+   +")))

(ert-deftest test-shape-artist-explain-empty-and-tiny-buffers ()
  "Empty buffers and single characters report nothing without error."
  (dolist (text '("" "\n" "+" "x" "+-" "-\n|"))
    (should-not (test-explain--misses text))))

(ert-deftest test-shape-artist-explain-ignores-text-in-border-position ()
  "Letters in a border row never count as gaps."
  (should-not (test-explain--misses "+-a-+\n|   |\n+---+"))
  (should-not (test-explain--misses "+---+\n|   |\nx---+")))

;;; --- At point ---

(ert-deftest test-shape-artist-explain-at-point-inside-box ()
  "Explaining inside a near-miss returns it, highlights it and messages."
  (test-explain--with-buffer "+---+\n|   |\n+===+"
    (goto-char (point-min))
    (forward-line 1)
    (forward-char 2)
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (let ((miss (shape-artist-explain-at-point)))
          (should (eq (plist-get miss :problem) 'mixed-border-family))
          (should (= (length messages) 1))
          (should (string-match-p "Top border uses '-'" (car messages)))
          (should (string-match-p
                   (regexp-quote "(M-x shape-artist-repair-at-point to fix)")
                   (car messages)))))
      (should shape-artist-explain--overlays)
      (dolist (ov shape-artist-explain--overlays)
        (should (eq (overlay-get ov 'face) 'shape-artist-explain-face)))
      ;; One overlay per box row, covering the whole box width.
      (should (= (length shape-artist-explain--overlays) 3))
      (should (= (apply #'+ (mapcar (lambda (ov) (- (overlay-end ov)
                                                    (overlay-start ov)))
                                    shape-artist-explain--overlays))
                 15)))))

(ert-deftest test-shape-artist-explain-highlight-cleared-on-next-command ()
  "The highlight and its hook vanish when the next command starts."
  (test-explain--with-buffer "+---+\n|   |\n+===+"
    (cl-letf (((symbol-function 'message) #'ignore))
      (shape-artist-explain-at-point))
    (should (memq 'shape-artist-explain--clear-highlight
                  (buffer-local-value 'pre-command-hook (current-buffer))))
    (should-not (memq 'shape-artist-explain--clear-highlight
                      (default-value 'pre-command-hook)))
    (run-hooks 'pre-command-hook)
    (should-not shape-artist-explain--overlays)
    (should-not (memq 'shape-artist-explain--clear-highlight pre-command-hook))
    (should-not (overlays-in (point-min) (point-max)))))

(ert-deftest test-shape-artist-explain-highlight-replaced-not-stacked ()
  "Explaining twice leaves a single set of overlays and a single hook."
  (test-explain--with-buffer "+---+\n|   |\n+===+"
    (cl-letf (((symbol-function 'message) #'ignore))
      (shape-artist-explain-at-point)
      (shape-artist-explain-at-point))
    (should (= (length (overlays-in (point-min) (point-max))) 3))
    (should (= (cl-count 'shape-artist-explain--clear-highlight pre-command-hook)
               1))
    (shape-artist-explain--clear-highlight)
    (should-not (overlays-in (point-min) (point-max)))))

(ert-deftest test-shape-artist-explain-at-point-nothing-found ()
  "Without near-misses, say so and return nil without highlighting."
  (test-explain--with-buffer "+---+\n|   |\n+---+"
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (should-not (shape-artist-explain-at-point)))
      (should (equal messages '("No near-miss box found here")))
      (should-not (overlays-in (point-min) (point-max)))
      (should-not (memq 'shape-artist-explain--clear-highlight pre-command-hook)))))

(ert-deftest test-shape-artist-explain-at-point-picks-nearest ()
  "Away from every box, the nearest near-miss wins."
  (test-explain--with-buffer (concat "+---+\n|   |\n+===+\n\n\n\n\n\n"
                                     "+---+\n|   |\n+--=+")
    (cl-letf (((symbol-function 'message) #'ignore))
      (goto-char (point-max))
      (should (= (plist-get (shape-artist-explain-at-point) :y) 8))
      (goto-char (point-min))
      (should (= (plist-get (shape-artist-explain-at-point) :y) 0)))))

(ert-deftest test-shape-artist-explain-at-point-prefers-containing-box ()
  "A point inside a box beats a nearer-by edge of another box."
  (test-explain--with-buffer (concat "+-----------+\n"
                                     "|  +---+    |\n"
                                     "|  |   |    |\n"
                                     "|  +===+    |\n"
                                     "+-----------+")
    (cl-letf (((symbol-function 'message) #'ignore))
      (goto-char (point-min))
      (forward-line 2)
      (forward-char 5)
      (let ((miss (shape-artist-explain-at-point)))
        (should (equal (list (plist-get miss :x) (plist-get miss :y)) '(3 1)))))))

(ert-deftest test-shape-artist-explain-face-is-theme-friendly ()
  "The face inherits from an existing face and extends to the line end."
  (should (facep 'shape-artist-explain-face))
  (should (face-attribute 'shape-artist-explain-face :inherit nil nil))
  (should (eq (face-attribute 'shape-artist-explain-face :extend nil nil) t)))

(ert-deftest test-shape-artist-explain-commands-are-interactive ()
  "The user-facing commands are interactive."
  (dolist (command '(shape-artist-explain-at-point shape-artist-repair-at-point
                     shape-artist-repair-all))
    (should (commandp command))))

;;; --- Repair ---

(ert-deftest test-shape-artist-repair-at-point-fixes-and-reparses ()
  "Repairing makes the parser detect the box and updates the shape list."
  (test-explain--with-buffer "+---+\n|   |\n+===+"
    (goto-char (point-min))
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (let ((miss (shape-artist-repair-at-point)))
          (should (eq (plist-get miss :problem) 'mixed-border-family))))
      (should (equal (buffer-string) "+---+\n|   |\n+---+"))
      (should (cl-find-if #'shape-artist-diagram-rectangle-p
                          shape-artist--last-shapes-list))
      (should (string-match-p "now detected" (car messages)))
      (should (string-match-p "line 1, column 0" (car messages))))))

(ert-deftest test-shape-artist-repair-at-point-preserves-other-text ()
  "Only the edited cells change; surrounding text and point survive."
  (let ((text "intro  line\n\n  +---+  keep\n  |   |  me\n  +--=+  too\ntail"))
    (test-explain--with-buffer text
      (goto-char (point-min))
      (search-forward "tail")
      (let ((point-before (point)))
        (cl-letf (((symbol-function 'message) #'ignore))
          (shape-artist-repair-at-point))
        (should (= (point) point-before))
        (should (equal (buffer-string)
                       "intro  line\n\n  +---+  keep\n  |   |  me\n  +---+  too\ntail"))
        (should (= (length (buffer-string)) (length text)))))))

(ert-deftest test-shape-artist-repair-at-point-pads-short-lines ()
  "A short line is padded with spaces only as far as the repair needs."
  (test-explain--with-buffer "+---+\n|\n+---+"
    (cl-letf (((symbol-function 'message) #'ignore))
      (shape-artist-repair-at-point))
    (should (equal (buffer-string) "+---+\n|   |\n+---+"))))

(ert-deftest test-shape-artist-repair-at-point-appends-at-end-of-line ()
  "A missing last character at the end of a line is appended."
  (test-explain--with-buffer "+---+\n|   |\n+---"
    (cl-letf (((symbol-function 'message) #'ignore))
      (shape-artist-repair-at-point))
    (should (equal (buffer-string) "+---+\n|   |\n+---+"))))

(ert-deftest test-shape-artist-repair-at-point-is-one-undo-step ()
  "All edits of one repair undo together."
  (test-explain--with-buffer "+---+\n|   |\n+===+"
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (let ((before (buffer-string)))
      (cl-letf (((symbol-function 'message) #'ignore))
        (shape-artist-repair-at-point))
      (should-not (equal (buffer-string) before))
      (should-not (memq nil buffer-undo-list))
      (primitive-undo 1 buffer-undo-list)
      (should (equal (buffer-string) before)))))

(ert-deftest test-shape-artist-repair-at-point-signals-user-error ()
  "With nothing to repair, signal a helpful `user-error' and change nothing."
  (test-explain--with-buffer "+---+\n|   |\n+---+"
    (let ((err (should-error (shape-artist-repair-at-point) :type 'user-error)))
      (should (string-match-p "nothing to repair" (cadr err))))
    (should (equal (buffer-string) "+---+\n|   |\n+---+")))
  (test-explain--with-buffer ""
    (should-error (shape-artist-repair-at-point) :type 'user-error)))

(ert-deftest test-shape-artist-repair-at-point-respects-read-only ()
  "A read-only buffer is left untouched and the error propagates."
  (test-explain--with-buffer "+---+\n|   |\n+===+"
    (setq buffer-read-only t)
    (should-error (shape-artist-repair-at-point))
    (should (equal (buffer-string) "+---+\n|   |\n+===+"))))

(ert-deftest test-shape-artist-repair-each-problem-kind ()
  "Every problem kind repairs to a detected, clean rectangle."
  (dolist (case '(("+---+\n|   |\n+===+" mixed-border-family)
                  ("+-- +\n|   |\n+---+" border-gap)
                  ("+---\n|   |\n+---+" missing-corner)
                  ("+---+\n    |\n+---+" missing-side)
                  ("+---+\n|   |\n+--+" ragged-border)
                  ("+---+\n|   |\n+----+" ragged-border)))
    (test-explain--with-buffer (car case)
      (should (eq (plist-get (car (shape-artist-explain-near-misses)) :problem)
                  (cadr case)))
      (cl-letf (((symbol-function 'message) #'ignore))
        (shape-artist-repair-at-point))
      (should (equal (test-explain--rects (buffer-string)) '((0 0 4 2))))
      (should-not (shape-artist-explain-near-misses))
      (should (equal (buffer-string) "+---+\n|   |\n+---+")))))

(ert-deftest test-shape-artist-repair-all-fixes-everything-and-counts ()
  "Repair-all repairs every near-miss and reports the count."
  (let ((text (concat "+---+   +===+\n|   |   |   |\n+-=-+   +---+\n\n"
                      "+---    +---+\n|   |   |   |\n+---+   +--+")))
    (test-explain--with-buffer text
      (let ((messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (should (= (shape-artist-repair-all) 4)))
        (should (equal messages '("Repaired 4 near-miss boxes")))
        (should-not (shape-artist-explain-near-misses))
        (should (= (length (test-explain--rects (buffer-string))) 4))
        (should (= (cl-count-if #'shape-artist-diagram-rectangle-p
                                shape-artist--last-shapes-list)
                   4))))))

(ert-deftest test-shape-artist-repair-all-is-idempotent ()
  "A second repair-all finds nothing and leaves the text alone."
  (test-explain--with-buffer "+---+ +===+\n|   | |   |\n+-=-+ +---+\n\n+---\n|   |\n+---+"
    (cl-letf (((symbol-function 'message) #'ignore))
      (should (= (shape-artist-repair-all) 3))
      (let ((after (buffer-string)))
        (should (= (shape-artist-repair-all) 0))
        (should (equal (buffer-string) after))
        (should-not (shape-artist-explain-near-misses))))))

(ert-deftest test-shape-artist-repair-failure-rolls-everything-back ()
  "A failure half way through a repair leaves the buffer unchanged."
  (test-explain--with-buffer "+---+\n|   |\n+===+"
    (let ((calls 0)
          (original (symbol-function 'shape-artist-explain--set-cell)))
      (cl-letf (((symbol-function 'message) #'ignore)
                ((symbol-function 'shape-artist-explain--set-cell)
                 (lambda (row col char)
                   (setq calls (1+ calls))
                   (when (= calls 2) (error "Simulated failure"))
                   (funcall original row col char))))
        (should-error (shape-artist-repair-at-point)))
      (should (= calls 2))
      (should (equal (buffer-string) "+---+\n|   |\n+===+")))))

(ert-deftest test-shape-artist-repair-all-nothing-to-do ()
  "Without near-misses repair-all returns zero and says so."
  (test-explain--with-buffer "+---+\n|   |\n+---+"
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (should (= (shape-artist-repair-all) 0)))
      (should (equal messages '("No near-miss boxes found"))))))

(ert-deftest test-shape-artist-repair-all-is-one-undo-step ()
  "Repair-all across several boxes undoes in one step."
  (test-explain--with-buffer "+---+ +===+\n|   | |   |\n+-=-+ +---+"
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (let ((before (buffer-string)))
      (cl-letf (((symbol-function 'message) #'ignore))
        (should (= (shape-artist-repair-all) 2)))
      (should-not (memq nil buffer-undo-list))
      (primitive-undo 1 buffer-undo-list)
      (should (equal (buffer-string) before)))))

(ert-deftest test-shape-artist-repair-all-keeps-valid-boxes ()
  "Valid boxes keep their exact text during repair-all."
  (let ((text "+---+ +---+\n| a | | b |\n+---+ +--=+\n"))
    (test-explain--with-buffer text
      (cl-letf (((symbol-function 'message) #'ignore))
        (shape-artist-repair-all))
      (should (equal (buffer-string)
                     "+---+ +---+\n| a | | b |\n+---+ +---+\n")))))

(ert-deftest test-shape-artist-explain-narrowed-buffer ()
  "Rows are relative to the accessible region, and repair edits inside it."
  (test-explain--with-buffer "head\n+---+\n|   |\n+===+\ntail"
    (narrow-to-region (save-excursion (goto-char (point-min)) (forward-line 1)
                                      (point))
                      (save-excursion (goto-char (point-min)) (forward-line 4)
                                      (point)))
    (should (= (plist-get (car (shape-artist-explain-near-misses)) :y) 0))
    (cl-letf (((symbol-function 'message) #'ignore))
      (shape-artist-repair-all))
    (widen)
    (should (equal (buffer-string) "head\n+---+\n|   |\n+---+\ntail"))))

;;; test-shape-artist-explain.el ends here
