;;; test-shape-artist-shape-edit.el --- ERT tests for SHAPE-EDIT mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-core)
(require 'shape-artist-shapes)
(require 'shape-artist-shape-edit)

;;; --- Helper ---

(defmacro test-artist-with-locked-shape (content &rest body)
  "Create a buffer with CONTENT, parse shapes, lock first shape, run BODY."
  (declare (indent 1))
  `(test-artist-with-buffer
       ,content
     (let ((shape-artist--update-sidebar-fn nil))
       (shape-artist-shapes-parse)
       (setq shape-artist--mode 'shape-edit)
       (setq shape-artist--locked-shape (nth 0 shape-artist--last-shapes-list))
       ,@body)))

;;; --- Exit Tests ---

(ert-deftest test-shape-artist-shape-edit-exit-returns-to-move ()
  "Test that exit returns to move mode."
  (test-artist-with-locked-shape
      "+---+\n|   |\n+---+"
    (shape-artist-shape-edit-exit)
    (should (eq shape-artist--mode 'move))))

(ert-deftest test-shape-artist-shape-edit-exit-clears-locked ()
  "Test that exit clears the locked shape."
  (test-artist-with-locked-shape
      "+---+\n|   |\n+---+"
    (should shape-artist--locked-shape)
    (shape-artist-shape-edit-exit)
    (should-not shape-artist--locked-shape)))

(ert-deftest test-shape-artist-shape-edit-target-overlay-is-explicit ()
  "SHAPE-EDIT keeps a dedicated visual target overlay until exit."
  (test-artist-with-locked-shape
      "+---+\n|   |\n+---+"
    (shape-artist-shape-edit-refresh-overlays)
    (should shape-artist--edit-target-overlays)
    (should (eq (overlay-get (car shape-artist--edit-target-overlays) 'face)
                'shape-artist-edit-target-face))
    (shape-artist-shape-edit-exit)
    (should-not shape-artist--edit-target-overlays)))

;;; --- Interior Navigation Tests ---

(ert-deftest test-shape-artist-shape-edit-up ()
  "Test navigating up within shape interior."
  (test-artist-with-locked-shape
      "+---+\n|   |\n|   |\n|   |\n+---+"
    (shape-artist--goto-row-col 2 2)
    (shape-artist-shape-edit-up)
    (let ((pos (shape-artist--current-row-col)))
      (should (= (car pos) 1))
      (should (= (cdr pos) 2)))))

(ert-deftest test-shape-artist-shape-edit-up-at-boundary ()
  "Test that up stops at interior top boundary."
  (test-artist-with-locked-shape
      "+---+\n|   |\n|   |\n+---+"
    (shape-artist--goto-row-col 1 2)
    (shape-artist-shape-edit-up)
    (let ((pos (shape-artist--current-row-col)))
      (should (>= (car pos) 1)))))

(ert-deftest test-shape-artist-shape-edit-down ()
  "Test navigating down within shape interior."
  (test-artist-with-locked-shape
      "+---+\n|   |\n|   |\n|   |\n+---+"
    (shape-artist--goto-row-col 1 2)
    (shape-artist-shape-edit-down)
    (let ((pos (shape-artist--current-row-col)))
      (should (= (car pos) 2))
      (should (= (cdr pos) 2)))))

(ert-deftest test-shape-artist-shape-edit-down-at-boundary ()
  "Test that down stops at interior bottom boundary."
  (test-artist-with-locked-shape
      "+---+\n|   |\n|   |\n+---+"
    ;; y=0, h=3, max-y = 0+3-1 = 2
    (shape-artist--goto-row-col 2 2)
    (shape-artist-shape-edit-down)
    (let ((pos (shape-artist--current-row-col)))
      (should (<= (car pos) 2)))))

(ert-deftest test-shape-artist-shape-edit-left ()
  "Test navigating left within shape interior."
  (test-artist-with-locked-shape
      "+---+\n|   |\n+---+"
    (shape-artist--goto-row-col 1 2)
    (shape-artist-shape-edit-left)
    (let ((pos (shape-artist--current-row-col)))
      (should (= (cdr pos) 1)))))

(ert-deftest test-shape-artist-shape-edit-left-at-boundary ()
  "Test that left stops at interior left boundary."
  (test-artist-with-locked-shape
      "+---+\n|   |\n+---+"
    ;; x=0, min-x = 1
    (shape-artist--goto-row-col 1 1)
    (shape-artist-shape-edit-left)
    (let ((pos (shape-artist--current-row-col)))
      (should (>= (cdr pos) 1)))))

(ert-deftest test-shape-artist-shape-edit-right ()
  "Test navigating right within shape interior."
  (test-artist-with-locked-shape
      "+---+\n|   |\n+---+"
    (shape-artist--goto-row-col 1 1)
    (shape-artist-shape-edit-right)
    (let ((pos (shape-artist--current-row-col)))
      (should (= (cdr pos) 2)))))

(ert-deftest test-shape-artist-shape-edit-right-at-boundary ()
  "Test that right stops at interior right boundary."
  (test-artist-with-locked-shape
      "+---+\n|   |\n+---+"
    ;; x=0, w=4, max-x = 0+4-1 = 3
    (shape-artist--goto-row-col 1 3)
    (shape-artist-shape-edit-right)
    (let ((pos (shape-artist--current-row-col)))
      (should (<= (cdr pos) 3)))))

;;; --- No Locked Shape ---

(ert-deftest test-shape-artist-shape-edit-up-no-locked ()
  "Test that navigation is a no-op when no shape is locked."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (setq shape-artist--mode 'shape-edit)
    (setq shape-artist--locked-shape nil)
    (shape-artist--goto-row-col 1 2)
    (shape-artist-shape-edit-up)
    (let ((pos (shape-artist--current-row-col)))
      (should (= (car pos) 1))
      (should (= (cdr pos) 2)))))

(ert-deftest test-shape-artist-shape-edit-down-no-locked ()
  "Test down with no locked shape does nothing."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (setq shape-artist--mode 'shape-edit)
    (setq shape-artist--locked-shape nil)
    (shape-artist--goto-row-col 1 2)
    (shape-artist-shape-edit-down)
    (let ((pos (shape-artist--current-row-col)))
      (should (= (car pos) 1))
      (should (= (cdr pos) 2)))))

(ert-deftest test-shape-artist-shape-edit-left-no-locked ()
  "Test left with no locked shape does nothing."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (setq shape-artist--mode 'shape-edit)
    (setq shape-artist--locked-shape nil)
    (shape-artist--goto-row-col 1 2)
    (shape-artist-shape-edit-left)
    (let ((pos (shape-artist--current-row-col)))
      (should (= (cdr pos) 2)))))

(ert-deftest test-shape-artist-shape-edit-right-no-locked ()
  "Test right with no locked shape does nothing."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (setq shape-artist--mode 'shape-edit)
    (setq shape-artist--locked-shape nil)
    (shape-artist--goto-row-col 1 2)
    (shape-artist-shape-edit-right)
    (let ((pos (shape-artist--current-row-col)))
      (should (= (cdr pos) 2)))))

;;; --- Command Tests (edit, move, resize, style) ---

(defmacro test-shape-artist-shape-edit--with-lock (content predicate &rest body)
  "Parse CONTENT, lock the first shape matching PREDICATE, and run BODY."
  (declare (indent 2))
  `(test-artist-with-buffer
       ,content
     (let ((shape-artist--update-sidebar-fn nil))
       (shape-artist-shapes-parse)
       (setq shape-artist--mode 'shape-edit)
       (setq shape-artist--locked-shape
             (cl-find-if ,predicate shape-artist--last-shapes-list))
       (should shape-artist--locked-shape)
       ,@body)))

(defun test-shape-artist-shape-edit--box (rect)
  "Return (X Y WIDTH HEIGHT) of RECT."
  (list (oref rect x) (oref rect y) (oref rect width) (oref rect height)))

(defun test-shape-artist-shape-edit--ends (edge)
  "Return (X Y X2 Y2) of EDGE."
  (list (oref edge x) (oref edge y) (oref edge x2) (oref edge y2)))

(defconst test-shape-artist-shape-edit--roomy-box
  "          \n          \n   +--+   \n   |  |   \n   +--+   \n          "
  "A 3x2 rectangle at (3,2) with room to move in every direction.")

(ert-deftest test-shape-artist-shape-edit-label-edits-locked-rectangle ()
  "Editing the label rewrites the locked rectangle's interior."
  (test-shape-artist-shape-edit--with-lock "+----+\n|    |\n+----+"
      #'shape-artist-diagram-rectangle-p
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "ab")))
      (shape-artist-shape-edit-label))
    (should (string-match-p "|.*ab.*|" (nth 1 (shape-artist--get-lines))))
    (should (shape-artist-diagram-rectangle-p shape-artist--locked-shape))))

(ert-deftest test-shape-artist-shape-edit-label-without-lock-does-nothing ()
  "With no locked shape, label editing neither prompts nor changes anything."
  (test-artist-with-buffer "+----+\n|    |\n+----+"
    (let ((shape-artist--update-sidebar-fn nil)
          (shape-artist--locked-shape nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (ert-fail "must not prompt"))))
        (should-not (shape-artist-shape-edit-label)))
      (should (equal (shape-artist--get-lines) '("+----+" "|    |" "+----+"))))))

(ert-deftest test-shape-artist-shape-edit-move-prompts-for-offsets ()
  "Without arguments, moving the locked shape prompts for dx and dy."
  (test-shape-artist-shape-edit--with-lock test-shape-artist-shape-edit--roomy-box
      #'shape-artist-diagram-rectangle-p
    (let ((answers (list 2 1)))
      (cl-letf (((symbol-function 'read-number) (lambda (&rest _) (pop answers))))
        (shape-artist-shape-move)))
    (should (equal (test-shape-artist-shape-edit--box shape-artist--locked-shape)
                   '(5 3 3 2)))))

(ert-deftest test-shape-artist-shape-edit-counted-moves-honor-count ()
  "Counted moves shift by COUNT cells; a missing or non-positive count means one."
  (dolist (case '((shape-artist-shape-move-left 2 (1 2))
                  (shape-artist-shape-move-left nil (2 2))
                  (shape-artist-shape-move-left 0 (2 2))
                  (shape-artist-shape-move-left -3 (2 2))
                  (shape-artist-shape-move-up 2 (3 0))
                  (shape-artist-shape-move-up nil (3 1))
                  (shape-artist-shape-move-right 2 (5 2))
                  (shape-artist-shape-move-down 2 (3 4))))
    (pcase-let ((`(,command ,count (,x ,y)) case))
      (ert-info ((format "%s %S" command count))
        (test-shape-artist-shape-edit--with-lock test-shape-artist-shape-edit--roomy-box
            #'shape-artist-diagram-rectangle-p
          (funcall command count)
          (should (equal (list (oref shape-artist--locked-shape x)
                               (oref shape-artist--locked-shape y))
                         (list x y))))))))

(ert-deftest test-shape-artist-shape-edit-resize-prompts-for-size ()
  "Resizing the locked rectangle prompts for width and height when omitted."
  (test-shape-artist-shape-edit--with-lock test-shape-artist-shape-edit--roomy-box
      #'shape-artist-diagram-rectangle-p
    (let ((answers (list 5 3)))
      (cl-letf (((symbol-function 'read-number) (lambda (&rest _) (pop answers))))
        (shape-artist-shape-resize)))
    (should (equal (test-shape-artist-shape-edit--box shape-artist--locked-shape)
                   '(3 2 5 3)))))

(ert-deftest test-shape-artist-shape-edit-resize-without-lock-does-nothing ()
  "With no locked shape, resizing neither prompts nor changes anything."
  (test-artist-with-buffer "+--+\n|  |\n+--+"
    (let ((shape-artist--update-sidebar-fn nil)
          (shape-artist--locked-shape nil))
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _) (ert-fail "must not prompt"))))
        (should-not (shape-artist-shape-resize)))
      (should (equal (shape-artist--get-lines) '("+--+" "|  |" "+--+"))))))

(ert-deftest test-shape-artist-shape-edit-horizontal-resize-extends-and-shortens-edges ()
  "Wider/narrower lengthen and shorten horizontal edges, never vertical ones."
  (test-shape-artist-shape-edit--with-lock "---   " #'shape-artist-diagram-edge-p
    (shape-artist-shape-resize-wider 2)
    (should (equal (test-shape-artist-shape-edit--ends shape-artist--locked-shape)
                   '(0 0 4 0)))
    (shape-artist-shape-resize-narrower 3)
    (should (equal (test-shape-artist-shape-edit--ends shape-artist--locked-shape)
                   '(0 0 1 0)))
    (should (equal (shape-artist--get-lines) '("--    "))))
  (test-shape-artist-shape-edit--with-lock " |\n |\n |" #'shape-artist-diagram-edge-p
    (shape-artist-shape-resize-wider 2)
    (shape-artist-shape-resize-narrower 2)
    (should (equal (test-shape-artist-shape-edit--ends shape-artist--locked-shape)
                   '(1 0 1 2)))))

(ert-deftest test-shape-artist-shape-edit-vertical-resize-extends-and-shortens-edges ()
  "Taller/shorter lengthen and shorten vertical edges, never horizontal ones."
  (test-shape-artist-shape-edit--with-lock " |\n |\n  \n  \n  " #'shape-artist-diagram-edge-p
    (shape-artist-shape-resize-taller 2)
    (should (equal (test-shape-artist-shape-edit--ends shape-artist--locked-shape)
                   '(1 0 1 3)))
    (shape-artist-shape-resize-shorter 2)
    (should (equal (test-shape-artist-shape-edit--ends shape-artist--locked-shape)
                   '(1 0 1 1))))
  (test-shape-artist-shape-edit--with-lock "---" #'shape-artist-diagram-edge-p
    (shape-artist-shape-resize-taller 2)
    (shape-artist-shape-resize-shorter 2)
    (should (equal (test-shape-artist-shape-edit--ends shape-artist--locked-shape)
                   '(0 0 2 0)))))

(ert-deftest test-shape-artist-shape-edit-resize-ignores-labels ()
  "Resize commands leave non-rectangle, non-edge targets such as labels alone."
  (test-shape-artist-shape-edit--with-lock "+----+\n| ab |\n+----+"
      #'shape-artist-diagram-label-p
    (let ((before (buffer-string)))
      (dolist (command '(shape-artist-shape-resize-wider shape-artist-shape-resize-narrower
                         shape-artist-shape-resize-taller shape-artist-shape-resize-shorter
                         shape-artist-shape-resize-left shape-artist-shape-shrink-left
                         shape-artist-shape-resize-up shape-artist-shape-shrink-up))
        (funcall command 1)
        (should (equal (buffer-string) before))))))

(ert-deftest test-shape-artist-shape-edit-rectangle-shrinks-stop-at-minimum-size ()
  "Narrower/shorter shrink a rectangle but stop at the 2x2 minimum."
  (test-shape-artist-shape-edit--with-lock "+---+\n|   |\n|   |\n+---+"
      #'shape-artist-diagram-rectangle-p
    (shape-artist-shape-resize-narrower 9)
    (should (equal (test-shape-artist-shape-edit--box shape-artist--locked-shape)
                   '(0 0 2 3)))
    (shape-artist-shape-resize-shorter 9)
    (should (equal (test-shape-artist-shape-edit--box shape-artist--locked-shape)
                   '(0 0 2 2)))
    ;; Already minimal: further shrinking is a harmless no-op.
    (let ((before (buffer-string)))
      (shape-artist-shape-resize-narrower 1)
      (shape-artist-shape-resize-shorter 1)
      (should (equal (buffer-string) before)))))

(ert-deftest test-shape-artist-shape-edit-rectangle-grows-taller-and-wider ()
  "Taller/wider grow a rectangle from its bottom-right anchor."
  (test-shape-artist-shape-edit--with-lock test-shape-artist-shape-edit--roomy-box
      #'shape-artist-diagram-rectangle-p
    (shape-artist-shape-resize-taller 1)
    (shape-artist-shape-resize-wider 2)
    (should (equal (test-shape-artist-shape-edit--box shape-artist--locked-shape)
                   '(3 2 5 3)))))

(ert-deftest test-shape-artist-shape-edit-shrink-from-left-and-top ()
  "Shrinking from the left or top moves the opposite-anchored edge inward."
  (test-shape-artist-shape-edit--with-lock "+----+\n|    |\n|    |\n+----+"
      #'shape-artist-diagram-rectangle-p
    (shape-artist-shape-shrink-left 2)
    (should (equal (test-shape-artist-shape-edit--box shape-artist--locked-shape)
                   '(2 0 3 3)))
    (shape-artist-shape-shrink-up 1)
    (should (equal (test-shape-artist-shape-edit--box shape-artist--locked-shape)
                   '(2 1 3 2)))))

(ert-deftest test-shape-artist-shape-edit-set-style-rectangle-and-edge ()
  "Setting a style restyles rectangles and edges."
  (test-shape-artist-shape-edit--with-lock "+---+\n|   |\n+---+"
      #'shape-artist-diagram-rectangle-p
    (shape-artist-shape-set-style 'heavy)
    (should (equal (car (shape-artist--get-lines)) "+===+"))
    (should (eq (shape-artist-shapes-border-style shape-artist--locked-shape) 'heavy)))
  (test-shape-artist-shape-edit--with-lock "---" #'shape-artist-diagram-edge-p
    (shape-artist-shape-set-style 'arrow-end)
    (should (equal (shape-artist--get-lines) '("-->")))))

(ert-deftest test-shape-artist-shape-edit-set-style-prompts-and-ignores-labels ()
  "Interactively the style is read via completion; labels are never restyled."
  (test-shape-artist-shape-edit--with-lock "+---+\n|   |\n+---+"
      #'shape-artist-diagram-rectangle-p
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "light")))
      (call-interactively #'shape-artist-shape-set-style))
    (should (equal (car (shape-artist--get-lines)) "+...+")))
  (test-shape-artist-shape-edit--with-lock "+----+\n| ab |\n+----+"
      #'shape-artist-diagram-label-p
    (let ((before (buffer-string)))
      (shape-artist-shape-set-style 'heavy)
      (should (equal (buffer-string) before)))))

(ert-deftest test-shape-artist-shape-edit-cycle-style-rectangle-borders ()
  "Cycling rectangle styles goes standard, heavy, light, then back."
  (test-shape-artist-shape-edit--with-lock "+---+\n|   |\n+---+"
      #'shape-artist-diagram-rectangle-p
    (let (seen)
      (dotimes (_ 4)
        (shape-artist-shape-cycle-style)
        (push (shape-artist-shapes-border-style shape-artist--locked-shape) seen))
      (should (equal (nreverse seen) '(heavy light standard heavy))))))

(ert-deftest test-shape-artist-shape-edit-cycle-style-edges-and-replace-alias ()
  "Cycling edge styles goes plain, arrow-end, arrow-both, then back; replace aliases it."
  (test-shape-artist-shape-edit--with-lock "---" #'shape-artist-diagram-edge-p
    (let (seen)
      (dotimes (_ 4)
        (shape-artist-shape-cycle-style)
        (push (shape-artist-shapes-edge-style shape-artist--locked-shape) seen))
      (should (equal (nreverse seen) '(arrow-end arrow-both plain arrow-end)))))
  (test-shape-artist-shape-edit--with-lock "+---+\n|   |\n+---+"
      #'shape-artist-diagram-rectangle-p
    (shape-artist-shape-replace)
    (should (equal (car (shape-artist--get-lines)) "+===+"))))

(ert-deftest test-shape-artist-shape-edit-cycle-style-ignores-labels ()
  "Cycling styles leaves label targets untouched."
  (test-shape-artist-shape-edit--with-lock "+----+\n| ab |\n+----+"
      #'shape-artist-diagram-label-p
    (let ((before (buffer-string)))
      (shape-artist-shape-cycle-style)
      (should (equal (buffer-string) before)))))

;;; --- Complete Style Set Tests ---

(ert-deftest test-shape-artist-shape-edit-style-prompt-offers-every-style ()
  "The interactive style prompt lists every border and edge style."
  (test-shape-artist-shape-edit--with-lock "+---+\n|   |\n+---+"
      #'shape-artist-diagram-rectangle-p
    (let (offered)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq offered collection)
                   "unicode-double")))
        (call-interactively #'shape-artist-shape-set-style))
      (dolist (style (append shape-artist-border-styles shape-artist-edge-styles))
        (should (member (symbol-name style) offered)))
      (should (equal (car (shape-artist--get-lines)) "╔═══╗")))))

(ert-deftest test-shape-artist-shape-edit-set-style-skips-shapes-it-does-not-fit ()
  "In a mixed selection each style applies only to the shapes that can take it."
  (dolist (case '((arrow-end "+--+" "|  |->")
                  (heavy "+==+" "|  |--")))
    (pcase-let ((`(,style ,top ,middle) case))
      (ert-info ((format "style %s on a box plus connector" style))
        (test-shape-artist-shape-edit--with-lock "+--+  \n|  |--\n+--+  "
            #'shape-artist-diagram-rectangle-p
          (setq shape-artist--selected-shape-signatures
                (mapcar #'shape-artist-shapes-signature
                        shape-artist--last-shapes-list))
          (shape-artist-shape-set-style style)
          (let ((lines (mapcar #'string-trim-right (shape-artist--get-lines))))
            (should (equal (nth 0 lines) top))
            (should (equal (nth 1 lines) middle))))))))

(ert-deftest test-shape-artist-shape-edit-set-style-reports-when-nothing-fits ()
  "A style that fits none of the targets leaves them alone and says so."
  (test-shape-artist-shape-edit--with-lock "+----+\n| ab |\n+----+"
      #'shape-artist-diagram-label-p
    (let ((before (buffer-string)))
      (should (string-match-p
               "does not apply"
               (car (test-shape-artist-guides--capturing-messages
                      (shape-artist-shape-set-style 'heavy)))))
      (should (equal (buffer-string) before)))))

(ert-deftest test-shape-artist-shape-edit-cycle-style-stays-in-the-boxs-family ()
  "Cycling keeps a Unicode box Unicode and an ASCII box ASCII."
  (test-shape-artist-shape-edit--with-lock "┌───┐\n│   │\n└───┘"
      #'shape-artist-diagram-rectangle-p
    (let (seen)
      (dotimes (_ 4)
        (shape-artist-shape-cycle-style)
        (push (shape-artist-shapes-border-style shape-artist--locked-shape) seen))
      (should (equal (nreverse seen)
                     '(unicode-rounded unicode-double unicode-single unicode-rounded)))))
  (test-shape-artist-shape-edit--with-lock "/---\\\n|   |\n\\---/"
      #'shape-artist-diagram-rectangle-p
    (shape-artist-shape-cycle-style)
    (should (eq (shape-artist-shapes-border-style shape-artist--locked-shape)
                'standard))))

;;; test-shape-artist-shape-edit.el ends here
