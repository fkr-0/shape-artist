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

(ert-deftest test-artist-core-mode-contract-drives-visual-state ()
  "Central mode transitions update cursor and presentation metadata together."
  (test-artist-with-buffer
      "abc"
    (let ((events nil))
      (let ((shape-artist-mode-changed-hook
             (list (lambda (old new) (push (list old new) events)))))
        (setq shape-artist--mode 'move)
        (shape-artist--enter-mode 'draw t)
        (should (eq shape-artist--mode 'draw))
        (should (equal cursor-type '(hbar . 3)))
        (should (equal (shape-artist--mode-property :activity) "CREATE"))
        (should (string-match-p "DRAW · CREATE"
                                (substring-no-properties
                                 (shape-artist--header-line-segment-fn))))
        (should (equal events '((move draw))))))))

(ert-deftest test-artist-core-mode-contract-has-distinct-accessible-faces ()
  "Each user mode has an explicit semantic face and activity label."
  (let ((faces (mapcar (lambda (mode)
                         (shape-artist--mode-property :face mode))
                       '(move draw select shape-edit)))
        (activities (mapcar (lambda (mode)
                              (shape-artist--mode-property :activity mode))
                            '(move draw select shape-edit))))
    (should (= (length (delete-dups (copy-sequence faces))) 4))
    (should (equal activities '("NAVIGATE" "CREATE" "PICK" "TRANSFORM")))))

(ert-deftest test-artist-core-mode-lifecycle-restores-header-and-cursor ()
  "Enabling artist mode adds strong indicators and disabling restores state."
  (test-artist-with-buffer
      "abc"
    (let ((header-line-format "original header")
          (cursor-type 'bar))
      (shape-artist-core-mode 1)
      (should (eq shape-artist--mode 'move))
      (should (member shape-artist--header-line-form header-line-format))
      (shape-artist-enter-draw)
      (should (equal cursor-type '(hbar . 3)))
      (shape-artist-core-mode -1)
      (should (equal header-line-format "original header"))
      (should (eq cursor-type 'bar)))))

(ert-deftest test-artist-core-mode-restores-whitespace-style ()
  "Disabling artist mode restores the buffer's prior `whitespace-style'."
  (test-artist-with-buffer
      "abc"
    (let ((shape-artist--update-sidebar-fn nil)
          (original nil))
      (shape-artist-core-mode 1)
      ;; Capture the style as it was right after `picture-mode' ran.
      (setq original (default-value 'whitespace-style))
      (should (equal whitespace-style '(spaces space-mark)))
      (shape-artist-core-mode -1)
      (should (equal whitespace-style original)))))

(ert-deftest test-artist-core-mode-saved-state-is-per-buffer ()
  "Enabling artist mode in another buffer must not clobber saved state.
Each buffer restores the cursor type it had when *it* entered artist mode."
  (let ((original-default (default-value 'cursor-type))
        (shape-artist--update-sidebar-fn nil)
        (buf-a (generate-new-buffer " *shape-artist-a*"))
        (buf-b (generate-new-buffer " *shape-artist-b*")))
    (unwind-protect
        (progn
          (setq-default cursor-type 'bar)
          (with-current-buffer buf-a
            (insert "abc")
            (shape-artist-core-mode 1))
          (setq-default cursor-type 'hbar)
          (with-current-buffer buf-b
            (insert "abc")
            (shape-artist-core-mode 1))
          (with-current-buffer buf-a
            (shape-artist-core-mode -1)
            (should (eq cursor-type 'bar))))
      (setq-default cursor-type original-default)
      (dolist (buf (list buf-a buf-b))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (ignore-errors (shape-artist-core-mode -1)))
          (kill-buffer buf))))))

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

;;; --- DRAW-mode Movement Tests ---

(defmacro test-artist-core-with-plain-buffer (content &rest body)
  "Run BODY in a temp buffer holding CONTENT with sidebar updates disabled."
  (declare (indent 1))
  `(test-artist-with-buffer
       ,content
     (let ((shape-artist--update-sidebar-fn nil))
       ,@body)))

(defun test-artist-core--grid-rectangular-p ()
  "Return non-nil when every line of the current buffer has the same width."
  (= 1 (length (delete-dups (mapcar #'length (shape-artist--get-lines))))))

(defun test-artist-core--char-at (row col)
  "Return the character at ROW,COL of the current buffer."
  (save-excursion
    (shape-artist--goto-row-col row col)
    (char-after)))

(ert-deftest test-artist-core-draw-move-right-draws-dash-and-advances ()
  "Moving right in DRAW mode leaves a dash behind and advances one column."
  (test-artist-core-with-plain-buffer "     "
    (shape-artist--goto-row-col 0 0)
    (shape-artist-draw-move-right)
    (should (eq (test-artist-core--char-at 0 0) ?-))
    (should (equal (shape-artist--current-row-col) '(0 . 1)))))

(ert-deftest test-artist-core-draw-move-left-draws-dash-and-retreats ()
  "Moving left in DRAW mode leaves a dash behind and retreats one column."
  (test-artist-core-with-plain-buffer "     "
    (shape-artist--goto-row-col 0 3)
    (shape-artist-draw-move-left)
    (should (eq (test-artist-core--char-at 0 3) ?-))
    (should (equal (shape-artist--current-row-col) '(0 . 2)))))

(ert-deftest test-artist-core-draw-move-down-draws-bar-and-descends ()
  "Moving down in DRAW mode leaves a vertical bar and advances one row."
  (test-artist-core-with-plain-buffer "  \n  "
    (shape-artist--goto-row-col 0 1)
    (shape-artist-draw-move-down)
    (should (eq (test-artist-core--char-at 0 1) ?|))
    (should (equal (shape-artist--current-row-col) '(1 . 1)))))

(ert-deftest test-artist-core-draw-move-up-draws-bar-and-ascends ()
  "Moving up in DRAW mode leaves a vertical bar and retreats one row."
  (test-artist-core-with-plain-buffer "  \n  "
    (shape-artist--goto-row-col 1 1)
    (shape-artist-draw-move-up)
    (should (eq (test-artist-core--char-at 1 1) ?|))
    (should (equal (shape-artist--current-row-col) '(0 . 1)))))

(ert-deftest test-artist-core-draw-moves-expand-canvas-at-trailing-edges ()
  "Drawing off the right or bottom edge grows the canvas and keeps drawing."
  (test-artist-core-with-plain-buffer "ab"
    (shape-artist--goto-row-col 0 1)
    (shape-artist-draw-move-right)
    (should (= (shape-artist--max-line-length) 3))
    (should (equal (shape-artist--current-row-col) '(0 . 2)))
    (should (eq (test-artist-core--char-at 0 1) ?-)))
  (test-artist-core-with-plain-buffer "ab"
    (shape-artist--goto-row-col 0 0)
    (shape-artist-draw-move-down)
    (should (= (shape-artist--line-count) 2))
    (should (equal (shape-artist--current-row-col) '(1 . 0)))
    (should (eq (test-artist-core--char-at 0 0) ?|))
    (should (test-artist-core--grid-rectangular-p))))

(ert-deftest test-artist-core-draw-moves-expand-canvas-at-leading-edges ()
  "Drawing off the left or top edge grows the canvas and keeps the stroke."
  (test-artist-core-with-plain-buffer "ab"
    (shape-artist--goto-row-col 0 0)
    (shape-artist-draw-move-left)
    (should (= (shape-artist--max-line-length) 3))
    (should (equal (shape-artist--current-row-col) '(0 . 0)))
    ;; The stroke shifted right with the content when the canvas grew.
    (should (eq (test-artist-core--char-at 0 1) ?-)))
  (test-artist-core-with-plain-buffer "ab"
    (shape-artist--goto-row-col 0 0)
    (shape-artist-draw-move-up)
    (should (= (shape-artist--line-count) 2))
    (should (equal (shape-artist--current-row-col) '(0 . 0)))
    (should (eq (test-artist-core--char-at 1 0) ?|))))

;;; --- MOVE-mode Edge Expansion Tests ---

(ert-deftest test-artist-core-move-commands-expand-canvas-at-every-edge ()
  "Cursor moves past any canvas edge grow the canvas instead of failing."
  (test-artist-core-with-plain-buffer "ab"
    (shape-artist--goto-row-col 0 1)
    (shape-artist-move-right)
    (should (= (shape-artist--max-line-length) 3))
    (should (equal (shape-artist--current-row-col) '(0 . 2))))
  (test-artist-core-with-plain-buffer "ab"
    (shape-artist--goto-row-col 0 0)
    (shape-artist-move-down)
    (should (= (shape-artist--line-count) 2))
    (should (equal (shape-artist--current-row-col) '(1 . 0)))
    ;; The new row spans the whole canvas, not just up to the cursor column.
    (should (test-artist-core--grid-rectangular-p)))
  (test-artist-core-with-plain-buffer "ab"
    (shape-artist--goto-row-col 0 0)
    (shape-artist-move-left)
    (should (= (shape-artist--max-line-length) 3))
    (should (equal (shape-artist--current-row-col) '(0 . 0))))
  (test-artist-core-with-plain-buffer "ab"
    (shape-artist--goto-row-col 0 0)
    (shape-artist-move-up)
    (should (= (shape-artist--line-count) 2))
    (should (equal (shape-artist--current-row-col) '(0 . 0)))))

;;; --- Commit, Tool, and Undo Tests ---

(ert-deftest test-artist-core-commit-stay-and-preview-keep-mode ()
  "Committing a shape via commit-stay or commit-preview does not change mode."
  (dolist (commit '(shape-artist-commit-stay shape-artist-commit-preview))
    (test-artist-core-with-plain-buffer "     \n     \n     "
      (setq shape-artist--mode 'draw)
      (setq shape-artist--anchor '(0 . 0))
      (shape-artist--goto-row-col 2 4)
      (funcall commit)
      (should (eq shape-artist--mode 'draw))
      (should-not shape-artist--anchor)
      (should (eq (test-artist-core--char-at 0 0) ?+))
      (should (eq (test-artist-core--char-at 2 4) ?+)))))

(ert-deftest test-artist-core-pick-tool-sets-draw-char ()
  "Picking a tool stores the chosen character as the draw character."
  (test-artist-core-with-plain-buffer "abc"
    (cl-letf (((symbol-function 'read-char) (lambda (&rest _) ?#)))
      (shape-artist-pick-tool))
    (should (eq shape-artist--draw-char ?#))))

(ert-deftest test-artist-core-undo-delegates-to-emacs-undo ()
  "`shape-artist-undo' is a thin wrapper over `undo'."
  (let ((called 0))
    (cl-letf (((symbol-function 'undo) (lambda (&rest _) (setq called (1+ called)))))
      (shape-artist-undo))
    (should (= called 1))))

;;; --- Normalize Interactive Spec Tests ---

(ert-deftest test-artist-core-normalize-interactive-reads-padding-and-title ()
  "Interactively, normalize reads padding numbers and an optional title."
  (let ((expected (test-artist-with-buffer "   \n a \n   "
                    (shape-artist-normalize '(1 1 1 1) "T")
                    (buffer-string))))
    (test-artist-core-with-plain-buffer "   \n a \n   "
      (cl-letf (((symbol-function 'read-string)
                 (let ((answers (list "1 1 1 1" "T")))
                   (lambda (&rest _) (pop answers)))))
        (call-interactively #'shape-artist-normalize))
      (should (equal (buffer-string) expected))
      (should (string-match-p "T" (buffer-string))))))

(ert-deftest test-artist-core-normalize-interactive-empty-title-skips-box ()
  "An empty title answer means no title box is drawn."
  (test-artist-core-with-plain-buffer "   \n a \n   "
    (cl-letf (((symbol-function 'read-string)
               (let ((answers (list "0 0 0 0" "")))
                 (lambda (&rest _) (pop answers)))))
      (call-interactively #'shape-artist-normalize))
    (should (equal (buffer-string) "a"))))

;;; --- Post-Command Hook Tests ---

(ert-deftest test-artist-core-post-command-debounces-sidebar-updates ()
  "Each command reschedules a single idle sidebar refresh for this buffer."
  (let ((scheduled nil)
        (cancelled nil)
        (updates 0))
    (test-artist-with-buffer "abc"
      (let ((shape-artist--update-sidebar-fn (lambda () (setq updates (1+ updates))))
            (shape-artist--sidebar-timer nil))
        (cl-letf (((symbol-function 'run-with-idle-timer)
                   (lambda (secs _repeat fn &rest _)
                     (let ((timer (list 'fake-timer secs fn)))
                       (push timer scheduled)
                       timer)))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer) (push timer cancelled))))
          (shape-artist--post-command)
          (should (= (length scheduled) 1))
          (should (null cancelled))
          ;; A second command cancels the pending timer before rescheduling.
          (let ((first (car scheduled)))
            (shape-artist--post-command)
            (should (equal cancelled (list first))))
          (should (= (length scheduled) 2))
          ;; Firing the latest timer refreshes the sidebar from this buffer.
          (funcall (nth 2 (car scheduled)))
          (should (= updates 1)))))
    ;; A timer firing after its buffer was killed must be harmless.
    (let ((fn (nth 2 (car scheduled))))
      (should-not (funcall fn))
      (should (= updates 1)))))

(ert-deftest test-artist-core-post-command-updates-preview-when-anchored ()
  "With an anchor set, every command refreshes the shape preview."
  (test-artist-core-with-plain-buffer "     \n     \n     "
    (setq shape-artist--anchor '(0 . 0))
    (shape-artist--goto-row-col 2 4)
    (shape-artist--post-command)
    (should shape-artist--preview-overlays)))

;;; --- Lifecycle Tests ---

(ert-deftest test-artist-core-disable-cancels-pending-sidebar-timer ()
  "Turning the mode off cancels and clears a pending sidebar timer."
  (test-artist-core-with-plain-buffer "abc"
    (shape-artist-core-mode 1)
    (let ((timer (run-with-idle-timer 3600 nil #'ignore)))
      (setq shape-artist--sidebar-timer timer)
      (shape-artist-core-mode -1)
      (should-not shape-artist--sidebar-timer)
      (should-not (memq timer timer-idle-list)))))

(ert-deftest test-artist-core-quit-disables-mode-and-returns-to-fundamental ()
  "Quitting artist mode switches the buffer to `fundamental-mode'."
  (test-artist-core-with-plain-buffer "abc"
    (shape-artist-core-mode 1)
    (shape-artist-quit)
    (should-not shape-artist-core-mode)
    (should (eq major-mode 'fundamental-mode))))

(ert-deftest test-artist-core-enter-mode-rejects-unknown-mode ()
  "Entering a mode with no presentation spec is a user error."
  (test-artist-core-with-plain-buffer "abc"
    (setq shape-artist--mode 'move)
    (should-error (shape-artist--enter-mode 'bogus) :type 'user-error)
    (should (eq shape-artist--mode 'move))))

;;; test-shape-artist-core.el ends here
