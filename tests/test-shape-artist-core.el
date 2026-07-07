;;; test-shape-artist-core.el --- ERT tests for movement + canvas expansion -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'shape-artist-core)

;;; --- Grid Helper Tests ---

(ert-deftest test-artist-core-line-count ()
  "Test line count helper."
  (test-artist-with-buffer
      "abc\ndef\nghi"
    (should (= (shape-artist--line-count) 3))))

(ert-deftest test-artist-core-line-count-single ()
  "Test line count with single line."
  (test-artist-with-buffer
      "hello"
    (should (= (shape-artist--line-count) 1))))

(ert-deftest test-artist-core-max-line-length ()
  "Test max line length."
  (test-artist-with-buffer
      "abc\ndefgh\nij"
    (should (= (shape-artist--max-line-length) 5))))

(ert-deftest test-artist-core-max-line-length-equal ()
  "Test max line length with equal lines."
  (test-artist-with-buffer
      "abc\ndef\nghi"
    (should (= (shape-artist--max-line-length) 3))))

;;; --- Cursor Position Tests ---

(ert-deftest test-artist-core-current-row-col ()
  "Test current row/col tracking."
  (test-artist-with-buffer
      "abcde\nfghij\nklmno"
    (goto-char (point-min))
    (let ((pos (shape-artist--current-row-col)))
      (should (= (car pos) 0))
      (should (= (cdr pos) 0)))))

(ert-deftest test-artist-core-goto-row-col ()
  "Test goto row/col."
  (test-artist-with-buffer
      "abcde\nfghij\nklmno"
    (shape-artist--goto-row-col 1 2)
    (let ((pos (shape-artist--current-row-col)))
      (should (= (car pos) 1))
      (should (= (cdr pos) 2)))))

(ert-deftest test-artist-core-goto-row-col-pad ()
  "Test that goto-row-col pads short lines."
  (test-artist-with-buffer
      "ab\ncdefgh"
    (shape-artist--goto-row-col 0 5)
    (let ((pos (shape-artist--current-row-col)))
      (should (= (car pos) 0))
      (should (= (cdr pos) 5)))
    ;; Verify the line was padded
    (save-excursion
      (goto-char (point-min))
      (should (= (- (line-end-position) (line-beginning-position)) 6)))))

;;; --- Canvas Expansion Tests ---

(ert-deftest test-artist-core-expand-bottom ()
  "Test bottom expansion inserts a new blank line."
  (test-artist-with-buffer
      "abc\ndef"
    (shape-artist--expand-bottom)
    (should (= (shape-artist--line-count) 3))))

(ert-deftest test-artist-core-expand-top ()
  "Test top expansion inserts a new blank line at the top."
  (test-artist-with-buffer
      "abc\ndef"
    (shape-artist--expand-top)
    (should (= (shape-artist--line-count) 3))
    ;; First line should be spaces
    (save-excursion
      (goto-char (point-min))
      (should (string-match-p "^[[:space:]]*$"
                               (buffer-substring-no-properties
                                (line-beginning-position) (line-end-position)))))))

(ert-deftest test-artist-core-expand-right ()
  "Test right expansion adds trailing space."
  (test-artist-with-buffer
      "abc\ndef"
    (let ((orig-len (shape-artist--max-line-length)))
      (shape-artist--expand-right)
      (should (= (shape-artist--max-line-length) (1+ orig-len))))))

(ert-deftest test-artist-core-expand-left ()
  "Test left expansion adds leading space."
  (test-artist-with-buffer
      "abc\ndef"
    (let ((orig-len (shape-artist--max-line-length)))
      (shape-artist--expand-left)
      (should (= (shape-artist--max-line-length) (1+ orig-len))))))

(ert-deftest test-artist-core-rectangular-invariant ()
  "Test that ensure-grid-rectangular makes all lines equal length."
  (test-artist-with-buffer
      "abc\nde\nf"
    (shape-artist--ensure-grid-rectangular)
    (let ((max-len (shape-artist--max-line-length)))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (should (= (- (line-end-position) (line-beginning-position)) max-len))
          (forward-line 1))))))

;;; --- Movement Tests (MOVE mode) ---

(ert-deftest test-artist-core-move-right ()
  "Test move right."
  (test-artist-with-buffer
      "abcde\nfghij\nklmno"
    (goto-char (point-min))
    (shape-artist-move-right)
    (let ((pos (shape-artist--current-row-col)))
      (should (= (car pos) 0))
      (should (= (cdr pos) 1)))))

(ert-deftest test-artist-core-move-left-at-zero ()
  "Test move left from column 0 expands canvas."
  (test-artist-with-buffer
      "abc\ndef"
    (goto-char (point-min))
    (let ((orig-len (shape-artist--max-line-length)))
      (shape-artist-move-left)
      ;; Should still be at col 0
      (let ((pos (shape-artist--current-row-col)))
        (should (= (cdr pos) 0)))
      ;; Canvas should have expanded
      (should (= (shape-artist--max-line-length) (1+ orig-len))))))

(ert-deftest test-artist-core-move-down ()
  "Test move down."
  (test-artist-with-buffer
      "abc\ndef\nghi"
    (goto-char (point-min))
    (shape-artist-move-down)
    (let ((pos (shape-artist--current-row-col)))
      (should (= (car pos) 1))
      (should (= (cdr pos) 0)))))

(ert-deftest test-artist-core-move-up-at-zero ()
  "Test move up from row 0 expands canvas."
  (test-artist-with-buffer
      "abc\ndef"
    (goto-char (point-min))
    (let ((orig-lines (shape-artist--line-count)))
      (shape-artist-move-up)
      ;; Should still be at row 0
      (let ((pos (shape-artist--current-row-col)))
        (should (= (car pos) 0)))
      ;; Canvas should have expanded
      (should (= (shape-artist--line-count) (1+ orig-lines))))))

(ert-deftest test-artist-core-move-preserves-position ()
  "Test that movement updates current-row-col correctly."
  (test-artist-with-buffer
      "abcde\nfghij\nklmno"
    (goto-char (point-min))
    (shape-artist-move-right)
    (shape-artist-move-right)
    (shape-artist-move-down)
    (let ((pos (shape-artist--current-row-col)))
      (should (= (car pos) 1))
      (should (= (cdr pos) 2)))))

;;; --- Anchor Tests ---

(ert-deftest test-artist-core-set-anchor ()
  "Test anchor setting."
  (test-artist-with-buffer
      "abcde\nfghij"
    (goto-char (point-min))
    (shape-artist-set-anchor-here)
    (should shape-artist--anchor)
    (should (= (car shape-artist--anchor) 0))
    (should (= (cdr shape-artist--anchor) 0))))

(ert-deftest test-artist-core-clear-preview ()
  "Test that clearing preview removes anchor."
  (test-artist-with-buffer
      "abcde\nfghij"
    (goto-char (point-min))
    (shape-artist-set-anchor-here)
    (should shape-artist--anchor)
    (shape-artist--clear-preview-state)
    (should-not shape-artist--anchor)))

;;; --- Commit Tests ---

(ert-deftest test-artist-core-commit-horizontal-line ()
  "Test commit of a horizontal line."
  (test-artist-with-buffer
      "     \n     \n     "
    (goto-char (point-min))
    ;; Set anchor at (0,0)
    (shape-artist-set-anchor-here)
    ;; Move to (0,4)
    (shape-artist--goto-row-col 0 4)
    ;; Commit
    (shape-artist--commit-shape)
    ;; Check that line was drawn
    (save-excursion
      (goto-char (point-min))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (should (string-match-p "^-----" line))))))

(ert-deftest test-artist-core-commit-vertical-line ()
  "Test commit of a vertical line."
  (test-artist-with-buffer
      "   \n   \n   \n   \n   "
    (goto-char (point-min))
    ;; Set anchor at (0,1)
    (shape-artist--goto-row-col 0 1)
    (shape-artist-set-anchor-here)
    ;; Move to (3,1)
    (shape-artist--goto-row-col 3 1)
    ;; Commit
    (shape-artist--commit-shape)
    ;; Check that vertical line was drawn
    (save-excursion
      (goto-char (point-min))
      (forward-line 0)
      (move-to-column 1)
      (should (eq (char-after) ?|))
      (forward-line 1)
      (move-to-column 1)
      (should (eq (char-after) ?|))
      (forward-line 2)
      (move-to-column 1)
      (should (eq (char-after) ?|)))))

(ert-deftest test-artist-core-commit-rectangle ()
  "Test commit of a rectangle."
  (test-artist-with-buffer
      "      \n      \n      \n      "
    (goto-char (point-min))
    ;; Set anchor at (0,0)
    (shape-artist-set-anchor-here)
    ;; Move to (2,4)
    (shape-artist--goto-row-col 2 4)
    ;; Commit
    (shape-artist--commit-shape)
    ;; Check corners
    (save-excursion
      ;; Top-left
      (goto-char (point-min))
      (should (eq (char-after) ?+))
      ;; Top-right
      (move-to-column 4)
      (should (eq (char-after) ?+))
      ;; Bottom-left
      (forward-line 2)
      (move-to-column 0)
      (should (eq (char-after) ?+))
      ;; Bottom-right
      (move-to-column 4)
      (should (eq (char-after) ?+)))))

(ert-deftest test-artist-core-commit-no-anchor ()
  "Test that commit with no anchor does nothing."
  (test-artist-with-buffer
      "abc"
    (goto-char (point-min))
    (should-not shape-artist--anchor)
    (shape-artist--commit-shape)
    ;; Buffer should be unchanged
    (should (string= (buffer-string) "abc"))))

(ert-deftest test-artist-core-commit-same-point ()
  "Test that commit at same point as anchor does nothing."
  (test-artist-with-buffer
      "   "
    (goto-char (point-min))
    (shape-artist-set-anchor-here)
    (shape-artist--commit-shape)
    ;; Should not draw anything (single point)
    (should-not shape-artist--anchor)))

;;; --- Mode Transition Tests ---

(ert-deftest test-artist-core-enter-draw ()
  "Test entering draw mode."
  (test-artist-with-buffer
      "abc"
    (setq shape-artist--mode 'move)
    (shape-artist-enter-draw)
    (should (eq shape-artist--mode 'draw))))

(ert-deftest test-artist-core-enter-move ()
  "Test entering move mode."
  (test-artist-with-buffer
      "abc"
    (setq shape-artist--mode 'draw)
    (shape-artist-enter-move)
    (should (eq shape-artist--mode 'move))))

(ert-deftest test-artist-core-cancel-exits-move ()
  "Test that cancel returns to move mode."
  (test-artist-with-buffer
      "abc"
    (setq shape-artist--mode 'draw)
    (shape-artist-cancel-and-exit)
    (should (eq shape-artist--mode 'move))))

;;; --- Normalization Tests ---

(ert-deftest test-artist-core-shrink ()
  "Test that shrink removes blank rows and columns."
  (test-artist-with-buffer
      "     \n  abc  \n  def  \n     "
    (shape-artist--shrink)
    (let ((lines (shape-artist--get-lines)))
      ;; Should have removed blank rows
      (should (= (length lines) 2))
      ;; Should have trimmed blank columns
      (should (string= (car lines) "abc"))
      (should (string= (cadr lines) "def")))))

(ert-deftest test-artist-core-box-with-title ()
  "Test boxing with title."
  (test-artist-with-buffer
      "abc"
    (shape-artist--box-with-title "Test")
    (let ((lines (shape-artist--get-lines)))
      (should (= (length lines) 3))
      ;; Top border should have title
      (should (string-match-p "Test" (car lines)))
      ;; Middle should have side borders
      (should (string-match-p "^|" (cadr lines)))
      ;; Bottom border
      (should (string-match-p "^\\+" (caddr lines))))))

(ert-deftest test-artist-core-normalize ()
  "Test full normalization pipeline."
  (test-artist-with-buffer
      "     \n  +---+  \n  |   |  \n  +---+  \n     "
    (shape-artist-normalize '(1 1 1 1) "Title")
    (let ((lines (shape-artist--get-lines)))
      ;; Should have content (shrink + pad + box)
      (should (> (length lines) 0))
      ;; All lines should have equal length
      (let ((len (length (car lines))))
        (dolist (line lines)
          (should (= (length line) len)))))))

;;; --- Commit Edge Case Tests ---

(ert-deftest test-artist-core-commit-2x2-rectangle ()
  "Test commit of a minimal 2x2 rectangle."
  (test-artist-with-buffer
      "    \n    \n    "
    (goto-char (point-min))
    (shape-artist-set-anchor-here)
    (shape-artist--goto-row-col 1 2)
    (shape-artist--commit-shape)
    ;; Check corners
    (save-excursion
      (goto-char (point-min))
      (should (eq (char-after) ?+))
      (move-to-column 2)
      (should (eq (char-after) ?+))
      (forward-line 1)
      (move-to-column 0)
      (should (eq (char-after) ?+))
      (move-to-column 2)
      (should (eq (char-after) ?+)))))

;;; --- Line Count Edge Cases ---

(ert-deftest test-artist-core-line-count-empty ()
  "Test line count with empty buffer."
  (test-artist-with-buffer
      ""
    (should (= (shape-artist--line-count) 0))))

;;; --- Expand Bottom Trailing Newline ---

(ert-deftest test-artist-core-expand-bottom-has-newline ()
  "Test that expand-bottom produces a proper new line."
  (test-artist-with-buffer
      "abc"
    (shape-artist--expand-bottom)
    (should (= (shape-artist--line-count) 2))
    ;; The new line should be accessible via forward-line
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (should (= (line-number-at-pos) 2)))))

;;; --- Preview Overlay Tests ---

(ert-deftest test-artist-core-preview-creates-overlays ()
  "Test that preview creates overlays."
  (test-artist-with-buffer
      "     \n     \n     "
    (goto-char (point-min))
    (shape-artist-set-anchor-here)
    (shape-artist--goto-row-col 2 4)
    (shape-artist-update-preview)
    (should (> (length shape-artist--preview-overlays) 0))))

(ert-deftest test-artist-core-preview-cleanup ()
  "Test that clearing preview removes all overlays."
  (test-artist-with-buffer
      "     \n     \n     "
    (goto-char (point-min))
    (shape-artist-set-anchor-here)
    (shape-artist--goto-row-col 2 4)
    (shape-artist-update-preview)
    (should (> (length shape-artist--preview-overlays) 0))
    (shape-artist--clear-preview-state)
    (should (= (length shape-artist--preview-overlays) 0))))

;;; --- Expand Right Grid Invariant ---

(ert-deftest test-artist-core-expand-right-maintains-grid ()
  "Test that expand-right maintains rectangular grid invariant."
  (test-artist-with-buffer
      "abc\nde\nf"
    (shape-artist--expand-right)
    (let ((max-len (shape-artist--max-line-length)))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (should (= (- (line-end-position) (line-beginning-position)) max-len))
          (forward-line 1))))))

;;; --- Draw Char Direction Tests ---

(ert-deftest test-artist-core-draw-char-horizontal ()
  "Horizontal movement produces '-'."
  (should (eq (shape-artist--draw-char-for-direction 0 1) ?-))
  (should (eq (shape-artist--draw-char-for-direction 0 -1) ?-)))

(ert-deftest test-artist-core-draw-char-vertical ()
  "Vertical movement produces '|'."
  (should (eq (shape-artist--draw-char-for-direction 1 0) ?|))
  (should (eq (shape-artist--draw-char-for-direction -1 0) ?|)))

(ert-deftest test-artist-core-draw-char-diagonal ()
  "Diagonal movement produces '+'."
  (should (eq (shape-artist--draw-char-for-direction 1 1) ?+))
  (should (eq (shape-artist--draw-char-for-direction -1 -1) ?+)))

;;; --- Get/Set Lines Roundtrip ---

(ert-deftest test-artist-core-get-set-lines-roundtrip ()
  "Get-lines and set-lines are inverse operations."
  (test-artist-with-buffer
      "abc\ndef\nghi"
    (let ((lines (shape-artist--get-lines)))
      (should (equal lines '("abc" "def" "ghi")))
      ;; Replace with different content
      (shape-artist--set-lines '("xyz" "uvw"))
      (should (equal (shape-artist--get-lines) '("xyz" "uvw"))))))

(ert-deftest test-artist-core-get-lines-empty ()
  "Get-lines on empty buffer returns empty list."
  (test-artist-with-buffer
      ""
    (should (null (shape-artist--get-lines)))))

;;; --- Add Padding Tests ---

(ert-deftest test-artist-core-add-padding-top ()
  "Adding top padding inserts blank lines at the start."
  (test-artist-with-buffer
      "abc"
    (shape-artist--add-padding 2 0 0 0)
    (let ((lines (shape-artist--get-lines)))
      (should (= (length lines) 3))
      (should (string-match-p "^[[:space:]]*$" (nth 0 lines)))
      (should (string-match-p "^[[:space:]]*$" (nth 1 lines))))))

(ert-deftest test-artist-core-add-padding-left ()
  "Adding left padding prepends spaces to every line."
  (test-artist-with-buffer
      "abc\ndef"
    (shape-artist--add-padding 0 0 0 3)
    (let ((lines (shape-artist--get-lines)))
      (should (string-prefix-p "   abc" (nth 0 lines)))
      (should (string-prefix-p "   def" (nth 1 lines))))))

(ert-deftest test-artist-core-add-padding-bottom ()
  "Adding bottom padding appends blank lines."
  (test-artist-with-buffer
      "abc"
    (shape-artist--add-padding 0 0 2 0)
    (should (= (shape-artist--line-count) 3))))

;;; --- Box with Title Edge Cases ---

(ert-deftest test-artist-core-box-with-title-short ()
  "Box with short title pads top border with dashes."
  (test-artist-with-buffer
      "hello world"
    (shape-artist--box-with-title "T")
    (let ((lines (shape-artist--get-lines)))
      (should (string-match-p "^\\+-T-" (nth 0 lines))))))

(ert-deftest test-artist-core-box-with-title-content-width ()
  "After boxing, all lines have equal length."
  (test-artist-with-buffer
      "abc\nde\nf"
    (shape-artist--box-with-title "X")
    (let* ((lines (shape-artist--get-lines))
           (len (length (car lines))))
      (dolist (line lines)
        (should (= (length line) len))))))

;;; --- Shrink Edge Cases ---

(ert-deftest test-artist-core-shrink-already-tight ()
  "Shrink on content with no surrounding blanks is a no-op."
  (test-artist-with-buffer
      "abc\ndef"
    (shape-artist--shrink)
    (let ((lines (shape-artist--get-lines)))
      (should (equal lines '("abc" "def"))))))

(ert-deftest test-artist-core-shrink-blank-only ()
  "Shrink on all-blank buffer yields empty result."
  (test-artist-with-buffer
      "   \n   \n   "
    (shape-artist--shrink)
    (should (= (shape-artist--line-count) 0))))

;;; --- Place Draw Char ---

(ert-deftest test-artist-core-place-draw-char ()
  "place-draw-char overwrites the character at the given position."
  (test-artist-with-buffer
      "     \n     "
    (shape-artist--place-draw-char 0 2 0 1)
    (save-excursion
      (goto-char (point-min))
      (move-to-column 2)
      (should (eq (char-after) ?-)))))

;;; test-shape-artist-core.el ends here
