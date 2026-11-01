;;; test-shape-artist-mouse.el --- Tests for mouse support -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'shape-artist)
(require 'shape-artist-mouse)

;;; --- Helpers -----------------------------------------------------------------

(defun test-shape-artist-mouse--posn (row col)
  "Return a synthetic mouse position for buffer ROW and COL in the current buffer."
  (let ((pos (save-excursion
               (goto-char (point-min))
               (forward-line row)
               (move-to-column col)
               (point))))
    (list (selected-window) pos (cons 0 0) 0 nil pos (cons col row)
          nil (cons 0 0) (cons 0 0))))

(defun test-shape-artist-mouse--down (type row col)
  "Return a TYPE press event at ROW, COL."
  (list type (test-shape-artist-mouse--posn row col)))

(defun test-shape-artist-mouse--motion (row col)
  "Return a mouse-movement event to ROW, COL."
  (list 'mouse-movement (test-shape-artist-mouse--posn row col)))

(defun test-shape-artist-mouse--release (from to)
  "Return a drag release event from (ROW . COL) FROM to (ROW . COL) TO."
  (list 'drag-mouse-1
        (test-shape-artist-mouse--posn (car from) (cdr from))
        (test-shape-artist-mouse--posn (car to) (cdr to))))

(defmacro test-shape-artist-mouse--with-events (events &rest body)
  "Run BODY with `read-event' returning EVENTS in order."
  (declare (indent 1))
  `(let ((queue ,events))
     (cl-letf (((symbol-function 'read-event) (lambda (&rest _) (pop queue))))
       ,@body)))

(defmacro test-shape-artist-mouse--with-diagram (content &rest body)
  "Parse CONTENT in a temp buffer with sidebar updates off and run BODY."
  (declare (indent 1))
  `(test-artist-with-buffer ,content
     (let ((shape-artist--update-sidebar-fn nil)
           (inhibit-message t))
       (setq shape-artist--mode 'move)
       (shape-artist-shapes-parse)
       ,@body)))

(defun test-shape-artist-mouse--rects ()
  "Return the parsed rectangles, sorted by position."
  (sort (cl-remove-if-not #'shape-artist-diagram-rectangle-p
                          shape-artist--last-shapes-list)
        (lambda (a b) (< (oref a x) (oref b x)))))

(defun test-shape-artist-mouse--box (rect)
  "Return (X Y W H) of RECT."
  (list (oref rect x) (oref rect y) (oref rect width) (oref rect height)))

(defconst test-shape-artist-mouse--canvas
  (concat "+---+       \n|   |       \n+---+       \n            \n"
          "            \n            \n            \n            ")
  "A box at (0,0) 4x2 on a roomy canvas.")

;;; --- Hit testing ---------------------------------------------------------------

(ert-deftest test-shape-artist-mouse-finds-the-shape-under-a-cell ()
  "Cells resolve to the right shape: border and interior, connectors, nesting."
  (test-shape-artist-mouse--with-diagram
      "+-------+\n| +---+ |\n| |   | |\n| +---+ |\n+-------+\n     \n+--+   \n|  |---\n+--+   "
    (let ((at (lambda (row col) (shape-artist-mouse--shape-at row col))))
      ;; Corner and interior of the outer box.
      (should (equal (test-shape-artist-mouse--box (funcall at 0 0)) '(0 0 8 4)))
      (should (equal (test-shape-artist-mouse--box (funcall at 1 1)) '(0 0 8 4)))
      ;; The nested box wins inside its own bounds.
      (should (equal (test-shape-artist-mouse--box (funcall at 2 3)) '(2 1 4 2)))
      ;; Blank space outside every shape.
      (should-not (funcall at 5 2))
      ;; A connector cell resolves to the edge, not a box.
      (should (shape-artist-diagram-edge-p (funcall at 7 5))))))

(ert-deftest test-shape-artist-mouse-pressing-a-label-selects-its-box ()
  "Pressing on the text inside a box selects the box, not a separate label."
  (test-shape-artist-mouse--with-diagram "+----+\n| ab |\n+----+"
    (should (shape-artist-diagram-rectangle-p (shape-artist-mouse--shape-at 1 2)))))

(ert-deftest test-shape-artist-mouse-ignores-events-without-a-buffer-position ()
  "Clicks on the mode line or fringe carry no position and are ignored."
  (test-shape-artist-mouse--with-diagram test-shape-artist-mouse--canvas
    (let ((event (list 'down-mouse-1 (list (selected-window) 'mode-line (cons 0 0) 0))))
      (should-not (shape-artist-mouse--event-cell event))
      (should-not (shape-artist-mouse-drag event))
      (should (eq shape-artist--mode 'move)))))

;;; --- Clicking ---------------------------------------------------------------------

(ert-deftest test-shape-artist-mouse-click-selects-a-shape-for-editing ()
  "A click without movement locks the shape and enters SHAPE-EDIT."
  (test-shape-artist-mouse--with-diagram test-shape-artist-mouse--canvas
    (test-shape-artist-mouse--with-events
        (list (test-shape-artist-mouse--release '(1 . 2) '(1 . 2)))
      (shape-artist-mouse-drag (test-shape-artist-mouse--down 'down-mouse-1 1 2)))
    (should (eq shape-artist--mode 'shape-edit))
    (should (equal (test-shape-artist-mouse--box shape-artist--locked-shape)
                   '(0 0 4 2)))))

(ert-deftest test-shape-artist-mouse-click-on-empty-space-leaves-edit-mode ()
  "Clicking blank canvas deselects, returns to MOVE, and puts point there."
  (test-shape-artist-mouse--with-diagram test-shape-artist-mouse--canvas
    (shape-artist-shape-edit-focus (car (test-shape-artist-mouse--rects)))
    (test-shape-artist-mouse--with-events
        (list (test-shape-artist-mouse--release '(5 . 7) '(5 . 7)))
      (shape-artist-mouse-drag (test-shape-artist-mouse--down 'down-mouse-1 5 7)))
    (should (eq shape-artist--mode 'move))
    (should-not shape-artist--locked-shape)
    (should (equal (shape-artist--current-row-col) '(5 . 7)))))

;;; --- Dragging to move ---------------------------------------------------------------

(ert-deftest test-shape-artist-mouse-drag-moves-the-shape ()
  "Dragging a shape moves it by the pointer's displacement."
  (test-shape-artist-mouse--with-diagram test-shape-artist-mouse--canvas
    (test-shape-artist-mouse--with-events
        (list (test-shape-artist-mouse--motion 2 3)
              (test-shape-artist-mouse--motion 3 5)
              (test-shape-artist-mouse--release '(1 . 2) '(3 . 5)))
      (shape-artist-mouse-drag (test-shape-artist-mouse--down 'down-mouse-1 1 2)))
    (shape-artist-shapes-parse)
    (should (equal (test-shape-artist-mouse--box (car (test-shape-artist-mouse--rects)))
                   '(3 2 4 2)))
    (should (eq shape-artist--mode 'shape-edit))))

(ert-deftest test-shape-artist-mouse-drag-shows-a-ghost-until-release ()
  "A ghost outline follows the pointer and never outlives the drag."
  (test-shape-artist-mouse--with-diagram test-shape-artist-mouse--canvas
    (let ((ghost-during nil)
          (ghost-faces nil)
          (before (buffer-string)))
      (let ((queue (list (test-shape-artist-mouse--motion 2 4)
                         (test-shape-artist-mouse--release '(1 . 2) '(2 . 4)))))
        (cl-letf (((symbol-function 'read-event)
                   (lambda (&rest _)
                     ;; The release is requested after the motion was handled.
                     (when (= (length queue) 1)
                       (setq ghost-during shape-artist-mouse--ghost-overlays)
                       (setq ghost-faces
                             (delq nil (mapcar (lambda (o) (overlay-get o 'face))
                                               shape-artist-mouse--ghost-overlays)))
                       ;; Previewing never edits the text.
                       (should (equal (buffer-string) before)))
                     (pop queue))))
          (shape-artist-mouse-drag (test-shape-artist-mouse--down 'down-mouse-1 1 2))))
      (should ghost-during)
      (should ghost-faces)
      (should (cl-every (lambda (f) (eq f 'shape-artist-ghost-face)) ghost-faces))
      (should-not shape-artist-mouse--ghost-overlays)
      (should-not (cl-some (lambda (o) (eq (overlay-get o 'face) 'shape-artist-ghost-face))
                           (overlays-in (point-min) (point-max)))))))

(ert-deftest test-shape-artist-mouse-ghost-covers-the-target-box ()
  "The ghost marks every cell of the target bounding box that exists in the buffer."
  (test-artist-with-buffer "          \n          \n          "
    (shape-artist-mouse--show-ghost 2 0 3 1)
    (let ((cells nil))
      (dolist (o shape-artist-mouse--ghost-overlays)
        (push (list (line-number-at-pos (overlay-start o))
                    (save-excursion (goto-char (overlay-start o)) (current-column))
                    (save-excursion (goto-char (overlay-end o)) (current-column)))
              cells))
      (should (equal (sort cells (lambda (a b) (< (car a) (car b))))
                     '((1 2 6) (2 2 6)))))
    (shape-artist-mouse--clear-ghost)
    (should-not shape-artist-mouse--ghost-overlays)))

(ert-deftest test-shape-artist-mouse-ghost-extends-past-short-lines ()
  "Where a line is shorter than the ghost, the remainder is drawn as padding."
  (test-artist-with-buffer "ab\n"
    (shape-artist-mouse--show-ghost 1 0 5 0)
    (should (cl-some (lambda (o) (overlay-get o 'after-string))
                     shape-artist-mouse--ghost-overlays))
    (shape-artist-mouse--clear-ghost)))

(ert-deftest test-shape-artist-mouse-drag-is-one-undo-step ()
  "One undo reverts a whole mouse drag."
  (test-shape-artist-mouse--with-diagram test-shape-artist-mouse--canvas
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (let ((original (buffer-string)))
      (undo-boundary)
      (test-shape-artist-mouse--with-events
          (list (test-shape-artist-mouse--motion 2 3)
                (test-shape-artist-mouse--release '(1 . 2) '(2 . 5)))
        (shape-artist-mouse-drag (test-shape-artist-mouse--down 'down-mouse-1 1 2)))
      (should-not (equal (buffer-string) original))
      (undo-boundary)
      (let ((last-command nil)) (undo))
      (should (equal (buffer-string) original)))))

;;; --- Handles and resizing -------------------------------------------------------------

(ert-deftest test-shape-artist-mouse-handles-mark-the-corners-of-the-edit-target ()
  "A focused rectangle shows a handle on each corner, and they go away on exit."
  (test-shape-artist-mouse--with-diagram test-shape-artist-mouse--canvas
    (shape-artist-shape-edit-focus (car (test-shape-artist-mouse--rects)))
    (let ((corners (mapcar (lambda (o)
                             (save-excursion
                               (goto-char (overlay-start o))
                               (cons (1- (line-number-at-pos)) (current-column))))
                           shape-artist--edit-handle-overlays)))
      (should (equal (sort corners (lambda (a b) (or (< (car a) (car b))
                                                     (and (= (car a) (car b))
                                                          (< (cdr a) (cdr b))))))
                     '((0 . 0) (0 . 4) (2 . 0) (2 . 4))))
      (dolist (o shape-artist--edit-handle-overlays)
        (should (eq (overlay-get o 'face) 'shape-artist-handle-face))))
    (shape-artist-shape-edit-exit)
    (should-not shape-artist--edit-handle-overlays)))

(ert-deftest test-shape-artist-mouse-handle-lookup-names-the-corner ()
  "Each corner cell of a rectangle maps to its anchors; other cells to nil."
  (test-shape-artist-mouse--with-diagram test-shape-artist-mouse--canvas
    (let ((rect (car (test-shape-artist-mouse--rects))))
      (should (equal (shape-artist-mouse--handle-at rect 0 0) '(left . top)))
      (should (equal (shape-artist-mouse--handle-at rect 0 4) '(right . top)))
      (should (equal (shape-artist-mouse--handle-at rect 2 0) '(left . bottom)))
      (should (equal (shape-artist-mouse--handle-at rect 2 4) '(right . bottom)))
      (should-not (shape-artist-mouse--handle-at rect 1 2)))))

(ert-deftest test-shape-artist-mouse-dragging-a-corner-handle-resizes ()
  "Dragging the focused box's corners grows it from the opposite corner."
  (dolist (case '(((2 . 4) (4 . 7) (0 0 7 4))     ; bottom-right by (+3,+2)
                  ((0 . 0) (0 . 0) nil)))         ; zero drag: nothing changes
    (pcase-let ((`(,from ,to ,expected) case))
      (test-shape-artist-mouse--with-diagram test-shape-artist-mouse--canvas
        (shape-artist-shape-edit-focus (car (test-shape-artist-mouse--rects)))
        (test-shape-artist-mouse--with-events
            (list (test-shape-artist-mouse--release from to))
          (shape-artist-mouse-drag
           (test-shape-artist-mouse--down 'down-mouse-1 (car from) (cdr from))))
        (shape-artist-shapes-parse)
        (should (equal (test-shape-artist-mouse--box (car (test-shape-artist-mouse--rects)))
                       (or expected '(0 0 4 2)))))))
  ;; Top-left corner pulled up-left grows the box towards the origin side.
  (test-shape-artist-mouse--with-diagram
      "            \n            \n   +---+    \n   |   |    \n   +---+    \n            "
    (shape-artist-shape-edit-focus (car (test-shape-artist-mouse--rects)))
    (test-shape-artist-mouse--with-events
        (list (test-shape-artist-mouse--release '(2 . 3) '(1 . 1)))
      (shape-artist-mouse-drag (test-shape-artist-mouse--down 'down-mouse-1 2 3)))
    (shape-artist-shapes-parse)
    (should (equal (test-shape-artist-mouse--box (car (test-shape-artist-mouse--rects)))
                   '(1 1 6 3)))))

;;; --- Drawing by dragging ------------------------------------------------------------------

(ert-deftest test-shape-artist-mouse-drag-on-empty-space-draws-a-box ()
  "Dragging across blank canvas draws a rectangle between the two corners."
  (test-shape-artist-mouse--with-diagram
      "                    \n                    \n                    \n                    "
    (test-shape-artist-mouse--with-events
        (list (test-shape-artist-mouse--motion 1 3)
              (test-shape-artist-mouse--release '(0 . 1) '(2 . 6)))
      (shape-artist-mouse-drag (test-shape-artist-mouse--down 'down-mouse-1 0 1)))
    (shape-artist-shapes-parse)
    (should (equal (test-shape-artist-mouse--box (car (test-shape-artist-mouse--rects)))
                   '(1 0 5 2)))
    ;; The mode is unchanged by drawing.
    (should (eq shape-artist--mode 'move))))

;;; --- Multi-select, double-click, context menu ------------------------------------------------

(defconst test-shape-artist-mouse--two-boxes
  "+--+    +--+  \n|  |    |  |  \n+--+    +--+  \n              "
  "Two boxes side by side.")

(ert-deftest test-shape-artist-mouse-shift-click-builds-a-multi-selection ()
  "Shift-clicking adds shapes to a group selection and toggles them off again."
  (test-shape-artist-mouse--with-diagram test-shape-artist-mouse--two-boxes
    (let ((click (lambda (row col)
                   (test-shape-artist-mouse--with-events
                       (list (test-shape-artist-mouse--release (cons row col) (cons row col)))
                     (shape-artist-mouse-toggle-select
                      (test-shape-artist-mouse--down 'S-down-mouse-1 row col))))))
      (funcall click 1 1)
      (funcall click 1 9)
      (should (= (length shape-artist--selected-shape-signatures) 2))
      (should (eq shape-artist--mode 'shape-edit))
      ;; A group move now carries both boxes.
      (shape-artist-shape-move-right 1)
      (shape-artist-shapes-parse)
      (should (equal (mapcar (lambda (r) (oref r x)) (test-shape-artist-mouse--rects))
                     '(1 9)))
      (funcall click 1 2)
      (should (= (length shape-artist--selected-shape-signatures) 1)))))

(ert-deftest test-shape-artist-mouse-double-click-edits-the-box-label ()
  "Double-clicking a box (or its label) prompts for new label text."
  (test-shape-artist-mouse--with-diagram "+------+\n|      |\n+------+"
    (test-shape-artist-mouse--with-events
        (list (test-shape-artist-mouse--release '(1 . 3) '(1 . 3)))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "hi")))
        (shape-artist-mouse-edit-label
         (test-shape-artist-mouse--down 'double-down-mouse-1 1 3))))
    (should (string-match-p "hi" (nth 1 (shape-artist--get-lines))))
    (should (eq shape-artist--mode 'shape-edit))))

(ert-deftest test-shape-artist-mouse-context-menu-offers-actions-for-the-shape ()
  "Right-click focuses the shape under the pointer and pops up a menu."
  (test-shape-artist-mouse--with-diagram test-shape-artist-mouse--canvas
    (let (menu)
      (cl-letf (((symbol-function 'popup-menu)
                 (lambda (m &rest _) (setq menu m) nil)))
        (shape-artist-mouse-context-menu
         (test-shape-artist-mouse--down 'down-mouse-3 1 2)))
      (should (keymapp menu))
      (should (eq shape-artist--mode 'shape-edit))
      (let ((labels (mapcar (lambda (item)
                              (and (consp item)
                                   (cond ((stringp (cadr item)) (cadr item))
                                         ((eq (cadr item) 'menu-item) (nth 2 item)))))
                            (cdr menu))))
        (should (cl-some (lambda (l) (and l (string-match-p "label" l))) labels))))))

(ert-deftest test-shape-artist-mouse-context-menu-on-empty-space-offers-general-actions ()
  "Right-clicking blank canvas still offers help, sidebar, and drawing."
  (test-shape-artist-mouse--with-diagram test-shape-artist-mouse--canvas
    (let (menu)
      (cl-letf (((symbol-function 'popup-menu)
                 (lambda (m &rest _) (setq menu m) nil)))
        (shape-artist-mouse-context-menu
         (test-shape-artist-mouse--down 'down-mouse-3 6 8)))
      (should (keymapp menu))
      (should-not shape-artist--locked-shape))))

;;; --- Bindings ---------------------------------------------------------------------------------

(ert-deftest test-shape-artist-mouse-bindings-are-installed ()
  "The mouse commands are reachable from the artist keymap."
  (should (eq (lookup-key shape-artist-core-mode-map (kbd "<down-mouse-1>"))
              #'shape-artist-mouse-drag))
  (should (eq (lookup-key shape-artist-core-mode-map (kbd "<S-down-mouse-1>"))
              #'shape-artist-mouse-toggle-select))
  (should (eq (lookup-key shape-artist-core-mode-map (kbd "<double-down-mouse-1>"))
              #'shape-artist-mouse-edit-label))
  (should (eq (lookup-key shape-artist-core-mode-map (kbd "<down-mouse-3>"))
              #'shape-artist-mouse-context-menu)))

(ert-deftest test-shape-artist-mouse-dragging-a-group-member-moves-the-whole-group ()
  "Pressing a member of the selected group drags every selected shape together."
  (test-shape-artist-mouse--with-diagram test-shape-artist-mouse--two-boxes
    (let ((rects (test-shape-artist-mouse--rects)))
      (setq shape-artist--selected-shape-signatures
            (mapcar #'shape-artist-shapes-signature rects))
      (setq shape-artist--locked-shape (car rects))
      (setq shape-artist--mode 'shape-edit)
      (test-shape-artist-mouse--with-events
          (list (test-shape-artist-mouse--motion 1 3)
                (test-shape-artist-mouse--release '(1 . 9) '(2 . 11)))
        (shape-artist-mouse-drag (test-shape-artist-mouse--down 'down-mouse-1 1 9)))
      (shape-artist-shapes-parse)
      (should (equal (mapcar (lambda (r) (list (oref r x) (oref r y)))
                             (test-shape-artist-mouse--rects))
                     '((2 1) (10 1))))
      (should (= (length shape-artist--selected-shape-signatures) 2)))))

(ert-deftest test-shape-artist-mouse-ghost-is-clipped-at-the-canvas-edge ()
  "A ghost dragged past the top or left of the canvas shows only its visible part."
  (test-artist-with-buffer "          \n          \n          "
    ;; Entirely off-canvas: nothing to show, and no error.
    (shape-artist-mouse--show-ghost -9 -9 3 1)
    (should-not shape-artist-mouse--ghost-overlays)
    ;; Partly off-canvas to the left and above: only the visible cells remain.
    (shape-artist-mouse--show-ghost -3 -1 5 2)
    (let ((cells nil))
      (dolist (o shape-artist-mouse--ghost-overlays)
        (when (overlay-get o 'face)
          (push (list (line-number-at-pos (overlay-start o))
                      (save-excursion (goto-char (overlay-start o)) (current-column))
                      (save-excursion (goto-char (overlay-end o)) (current-column)))
                cells)))
      ;; Rows -1..1 intersect the canvas at rows 0 and 1; columns -3..3 show 0..3.
      (should (equal (sort cells (lambda (a b) (< (car a) (car b))))
                     '((1 0 3) (2 0 3)))))
    (shape-artist-mouse--clear-ghost)))

(provide 'test-shape-artist-mouse)
;;; test-shape-artist-mouse.el ends here
