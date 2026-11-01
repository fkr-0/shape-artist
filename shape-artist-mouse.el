;;; shape-artist-mouse.el --- Mouse support for Shape Artist -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Shape Artist Contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Direct manipulation with the pointer, in a graphical Emacs or in a terminal
;; with `xterm-mouse-mode':
;;
;;  - press on a shape: select it for editing; drag to move it (a ghost outline
;;    follows the pointer and the buffer changes only on release);
;;  - drag a corner handle of the selected rectangle: resize it;
;;  - drag across blank canvas: draw a box (or a line, when the drag is
;;    straight);
;;  - shift-press: add or remove shapes from a group selection;
;;  - double-press a box: edit its label;
;;  - right-press: a context menu for the shape (or the diagram).
;;
;; Every drag is one command, so one undo reverts it.  The pure pieces
;; (hit-testing, handle lookup, ghost outline) are separate functions so they can
;; be tested without a pointer.

;;; Code:

(require 'cl-lib)
(require 'easymenu)
(require 'shape-artist-core)
(require 'shape-artist-shapes)
(require 'shape-artist-shape-edit)

(declare-function shape-artist-ui-toggle "shape-artist-ui")
(declare-function shape-artist-ui-show-outline "shape-artist-ui")
(declare-function shape-artist-ui-command-menu "shape-artist-ui")

;;; --- Events to cells -----------------------------------------------------------------

(defun shape-artist-mouse--posn-cell (posn)
  "Return the buffer (ROW . COL) cell that POSN points at, or nil.
Return nil when POSN is not over buffer text, for example on the mode line.
The row comes from the buffer position; the column from the pointer so that
clicks past the end of a short line still land on the right cell."
  (let ((pos (posn-point posn)))
    (when (integerp pos)
      (let* ((window (posn-window posn))
             (col-row (let ((cr (nth 6 posn)))
                        (and (consp cr) (integerp (car cr)) cr)))
             (row (save-excursion (goto-char pos) (1- (line-number-at-pos))))
             (col (if (and col-row (windowp window)
                           (eq (window-buffer window) (current-buffer)))
                      (+ (car col-row) (window-hscroll window))
                    (save-excursion (goto-char pos) (current-column)))))
        (cons row col)))))

(defun shape-artist-mouse--event-cell (event)
  "Return the (ROW . COL) cell where EVENT started, or nil."
  (and (consp event) (consp (event-start event))
       (shape-artist-mouse--posn-cell (event-start event))))

(defun shape-artist-mouse--release-cell (event)
  "Return the (ROW . COL) cell where the release EVENT happened, or nil."
  (and (consp event) (consp (event-end event))
       (shape-artist-mouse--posn-cell (event-end event))))

;;; --- Hit testing ------------------------------------------------------------------------

(defun shape-artist-mouse--refresh ()
  "Re-parse the diagram so hit-testing sees the current text."
  (let ((inhibit-message t))
    (shape-artist-shapes-parse)))

(defun shape-artist-mouse--shape-at (row col)
  "Return the shape under cell ROW, COL: a connector, else the innermost box.
Labels live inside boxes, so pressing on a label selects its box."
  (let (edge rect rect-area)
    (dolist (shape shape-artist--last-shapes-list)
      (cond
       ((shape-artist-diagram-edge-p shape)
        (when (and (<= (min (oref shape x) (oref shape x2)) col
                       (max (oref shape x) (oref shape x2)))
                   (<= (min (oref shape y) (oref shape y2)) row
                       (max (oref shape y) (oref shape y2))))
          (setq edge shape)))
       ((shape-artist-diagram-rectangle-p shape)
        (let ((x (oref shape x)) (y (oref shape y))
              (w (oref shape width)) (h (oref shape height)))
          (when (and (<= x col (+ x w)) (<= y row (+ y h)))
            (let ((area (* (1+ w) (1+ h))))
              (when (or (null rect-area) (< area rect-area))
                (setq rect shape rect-area area))))))))
    (or edge rect)))

(defun shape-artist-mouse--handle-at (rect row col)
  "Return the resize anchors for the corner of RECT at ROW, COL, or nil.
The result is (X-ANCHOR . Y-ANCHOR) naming the edges that move, for example
\(left . top) for the top-left corner."
  (let ((x (oref rect x)) (y (oref rect y))
        (w (oref rect width)) (h (oref rect height)))
    (cond ((and (= row y) (= col x)) '(left . top))
          ((and (= row y) (= col (+ x w))) '(right . top))
          ((and (= row (+ y h)) (= col x)) '(left . bottom))
          ((and (= row (+ y h)) (= col (+ x w))) '(right . bottom)))))

;;; --- Ghost preview ------------------------------------------------------------------------

(defvar-local shape-artist-mouse--ghost-overlays nil
  "Overlays showing where the shape being dragged would land.")

(defun shape-artist-mouse--clear-ghost ()
  "Remove the drag preview."
  (mapc #'delete-overlay shape-artist-mouse--ghost-overlays)
  (setq shape-artist-mouse--ghost-overlays nil))

(defun shape-artist-mouse--show-ghost (x y w h)
  "Preview the box with corner X,Y and size W by H, replacing any earlier one.
Only overlays are used, so the buffer text is never touched.  The preview is
clipped to the canvas, so dragging past the top or left edge is harmless, and
where a line is shorter than the box the missing cells are drawn as padding."
  (shape-artist-mouse--clear-ghost)
  (save-excursion
    (let ((last-line (line-number-at-pos (point-max)))
          (first-col (max 0 x))
          (end-col (+ x w 1)))
      (when (> end-col first-col)
        (dotimes (i (1+ h))
          (let ((line (+ y i 1)))
            (when (<= 1 line last-line)
              (goto-char (point-min))
              (forward-line (1- line))
              (let* ((eol (line-end-position))
                     (start (progn (move-to-column first-col) (point)))
                     (end (progn (move-to-column end-col) (point)))
                     (shortfall (max 0 (- end-col (progn (goto-char eol)
                                                         (current-column))))))
                (when (> end start)
                  (let ((o (make-overlay start end)))
                    (overlay-put o 'face 'shape-artist-ghost-face)
                    (overlay-put o 'priority 30)
                    (push o shape-artist-mouse--ghost-overlays)))
                (when (> shortfall 0)
                  (let ((o (make-overlay eol eol)))
                    (overlay-put o 'after-string
                                 (propertize (make-string shortfall ?\s)
                                             'face 'shape-artist-ghost-face))
                    (push o shape-artist-mouse--ghost-overlays)))))))))))

(defun shape-artist-mouse--targets ()
  "Return the shapes a drag would act on: the group selection, else the lock."
  (or (shape-artist-shapes-selected)
      (and shape-artist--locked-shape (list shape-artist--locked-shape))))

(defun shape-artist-mouse--bounds (shapes)
  "Return the (X Y W H) box covering SHAPES."
  (let ((x1 most-positive-fixnum) (y1 most-positive-fixnum)
        (x2 most-negative-fixnum) (y2 most-negative-fixnum))
    (dolist (s shapes)
      (setq x1 (min x1 (oref s x) (if (shape-artist-diagram-edge-p s) (oref s x2) (oref s x)))
            y1 (min y1 (oref s y) (if (shape-artist-diagram-edge-p s) (oref s y2) (oref s y)))
            x2 (max x2 (if (shape-artist-diagram-edge-p s) (oref s x2) (+ (oref s x) (oref s width))))
            y2 (max y2 (if (shape-artist-diagram-edge-p s) (oref s y2) (+ (oref s y) (oref s height))))))
    (list x1 y1 (- x2 x1) (- y2 y1))))

;;; --- Tracking a drag --------------------------------------------------------------------------

(defun shape-artist-mouse--track (start on-motion)
  "Follow the pointer from cell START until it is released.
Call ON-MOTION with each new (ROW . COL) cell while the button is down, and
return the cell where it was released.  The ghost preview is always removed."
  (let ((cell start) (event nil))
    (unwind-protect
        (track-mouse
          (while (mouse-movement-p (setq event (read-event)))
            (let ((moved (shape-artist-mouse--event-cell event)))
              (when moved
                (setq cell moved)
                (funcall on-motion cell))))
          (let ((released (shape-artist-mouse--release-cell event)))
            (when released (setq cell released))))
      (shape-artist-mouse--clear-ghost))
    cell))

(defun shape-artist-mouse--consume-release ()
  "Read events up to and including the button release; return its cell or nil."
  (let ((event nil))
    (track-mouse
      (while (mouse-movement-p (setq event (read-event)))))
    (shape-artist-mouse--release-cell event)))

;;; --- Drag actions ------------------------------------------------------------------------------

(defun shape-artist-mouse--drag-move (start)
  "Drag the already selected target(s) from cell START and apply the move."
  (let* ((targets (shape-artist-mouse--targets))
         (box (shape-artist-mouse--bounds targets))
         (end (shape-artist-mouse--track
               start
               (lambda (cell)
                 (shape-artist-mouse--show-ghost
                  (+ (nth 0 box) (- (cdr cell) (cdr start)))
                  (+ (nth 1 box) (- (car cell) (car start)))
                  (nth 2 box) (nth 3 box)))))
         (dx (- (cdr end) (cdr start)))
         (dy (- (car end) (car start))))
    (unless (and (= dx 0) (= dy 0))
      (shape-artist-shape-move dx dy))))

(defun shape-artist-mouse--drag-resize (rect start handle)
  "Drag RECT's corner HANDLE (anchors) from START and apply the resize."
  (let* ((x-anchor (car handle)) (y-anchor (cdr handle))
         (growth (lambda (cell)
                   (cons (* (if (eq x-anchor 'left) -1 1) (- (cdr cell) (cdr start)))
                         (* (if (eq y-anchor 'top) -1 1) (- (car cell) (car start))))))
         (end (shape-artist-mouse--track
               start
               (lambda (cell)
                 (let ((g (funcall growth cell)))
                   (shape-artist-mouse--show-ghost
                    (if (eq x-anchor 'left) (- (oref rect x) (car g)) (oref rect x))
                    (if (eq y-anchor 'top) (- (oref rect y) (cdr g)) (oref rect y))
                    (max 1 (+ (oref rect width) (car g)))
                    (max 1 (+ (oref rect height) (cdr g))))))))
         (g (funcall growth end)))
    (unless (and (= (car g) 0) (= (cdr g) 0))
      (condition-case err
          (shape-artist-shape-edit--set-targets
           (list (shape-artist-shapes-resize-anchored
                  rect x-anchor (car g) y-anchor (cdr g))))
        (user-error (message "%s" (cadr err)))))))

(defun shape-artist-mouse--drag-blank (start)
  "Handle a press on blank canvas at cell START.
Dragging rubber-bands a box (or a line, when the drag is straight) and draws it
on release; releasing where it started is a plain click, which deselects."
  (let ((end (shape-artist-mouse--track
              start
              (lambda (cell)
                (shape-artist-mouse--show-ghost
                 (min (cdr start) (cdr cell)) (min (car start) (car cell))
                 (abs (- (cdr cell) (cdr start))) (abs (- (car cell) (car start))))))))
    (if (equal end start)
        (shape-artist-mouse--click-blank start)
      ;; Committing a shape does not change the mode, so drawing leaves the user
      ;; in whatever mode they were in.
      (setq shape-artist--anchor start)
      (shape-artist--goto-row-col (car end) (cdr end))
      (shape-artist--commit-shape))))

(defun shape-artist-mouse--click-blank (cell)
  "Handle a plain click on blank canvas at CELL: deselect and move point there."
  (when (eq shape-artist--mode 'shape-edit)
    (shape-artist-shape-edit-exit))
  (shape-artist--goto-row-col (car cell) (cdr cell)))

;;;###autoload
(defun shape-artist-mouse-drag (event)
  "Handle the mouse press EVENT: select, move, resize, or draw by its target.
A press on a shape selects it and dragging moves it; a press on a corner handle
of the selected rectangle resizes it; a press on blank canvas followed by a drag
draws a box.  A press without movement just selects (or deselects)."
  (interactive "e")
  (let ((start (shape-artist-mouse--event-cell event)))
    (when start
      (shape-artist-mouse--refresh)
      (let* ((row (car start)) (col (cdr start))
             (locked shape-artist--locked-shape)
             (handle (and locked (eq shape-artist--mode 'shape-edit)
                          (null shape-artist--selected-shape-signatures)
                          (shape-artist-diagram-rectangle-p locked)
                          (shape-artist-mouse--handle-at locked row col)))
             (shape (and (not handle) (shape-artist-mouse--shape-at row col))))
        (cond
         (handle
          (shape-artist-mouse--drag-resize locked start handle))
         (shape
          ;; Pressing a member of a group keeps the group, so it drags together.
          (unless (member (shape-artist-shapes-signature shape)
                          shape-artist--selected-shape-signatures)
            (shape-artist-shape-edit-focus shape))
          (shape-artist-mouse--drag-move start))
         (t
          (shape-artist-mouse--drag-blank start)))))))

;;;###autoload
(defun shape-artist-mouse-toggle-select (event)
  "Add the shape under the press EVENT to the group selection, or remove it."
  (interactive "e")
  (let ((start (shape-artist-mouse--event-cell event)))
    (when start
      (shape-artist-mouse--consume-release)
      (shape-artist-mouse--refresh)
      (let ((shape (shape-artist-mouse--shape-at (car start) (cdr start))))
        (when shape
          (let ((signature (shape-artist-shapes-signature shape)))
            ;; Seed the group with the shape that was already selected.
            (when (and (null shape-artist--selected-shape-signatures)
                       shape-artist--locked-shape)
              (setq shape-artist--selected-shape-signatures
                    (list (shape-artist-shapes-signature shape-artist--locked-shape))))
            (setq shape-artist--selected-shape-signatures
                  (if (member signature shape-artist--selected-shape-signatures)
                      (delete signature shape-artist--selected-shape-signatures)
                    (append shape-artist--selected-shape-signatures
                            (list signature))))
            (let ((members (shape-artist-shapes-selected)))
              (setq shape-artist--locked-shape (or (car members) shape)))
            (unless (eq shape-artist--mode 'shape-edit)
              (shape-artist--enter-mode 'shape-edit))
            (shape-artist-shape-edit-refresh-overlays)
            (shape-artist--maybe-update-sidebar)))))))

;;;###autoload
(defun shape-artist-mouse-edit-label (event)
  "Edit the label of the box under the press EVENT."
  (interactive "e")
  (let ((start (shape-artist-mouse--event-cell event)))
    (when start
      (shape-artist-mouse--consume-release)
      (shape-artist-mouse--refresh)
      (let ((shape (shape-artist-mouse--shape-at (car start) (cdr start))))
        (when shape
          (shape-artist-shape-edit-focus shape)
          (when (shape-artist-diagram-rectangle-p shape)
            (shape-artist-shape-edit-label)))))))

;;; --- Context menu ---------------------------------------------------------------------------------

(defun shape-artist-mouse--menu (shape)
  "Return the context menu keymap for SHAPE, or for the diagram when it is nil."
  (easy-menu-create-menu
   (if shape "Shape" "Diagram")
   (append
    (when shape
      (list ["Edit label" shape-artist-shape-edit-label
             :active (shape-artist-diagram-rectangle-p shape)]
            ["Cycle style" shape-artist-shape-cycle-style]
            ["Move..." shape-artist-shape-move]
            ["Resize..." shape-artist-shape-resize
             :active (shape-artist-diagram-rectangle-p shape)]
            ["Done editing" shape-artist-shape-edit-exit]
            "---"))
    (list ["Toggle sidebar" shape-artist-ui-toggle]
          ["Show outline" shape-artist-ui-show-outline]
          ["Command palette" shape-artist-ui-command-menu]))))

;;;###autoload
(defun shape-artist-mouse-context-menu (event)
  "Pop up a menu of actions for the shape under the press EVENT.
The shape is selected first; on blank canvas the menu offers general actions."
  (interactive "e")
  (let ((start (shape-artist-mouse--event-cell event)))
    (when start
      ;; `popup-menu' tracks and swallows the button release itself.
      (shape-artist-mouse--refresh)
      (let ((shape (shape-artist-mouse--shape-at (car start) (cdr start))))
        (when shape (shape-artist-shape-edit-focus shape))
        (popup-menu (shape-artist-mouse--menu shape) event)))))

;;; --- Bindings -------------------------------------------------------------------------------------------

(define-key shape-artist-core-mode-map (kbd "<down-mouse-1>") #'shape-artist-mouse-drag)
(define-key shape-artist-core-mode-map (kbd "<S-down-mouse-1>") #'shape-artist-mouse-toggle-select)
(define-key shape-artist-core-mode-map (kbd "<double-down-mouse-1>") #'shape-artist-mouse-edit-label)
(define-key shape-artist-core-mode-map (kbd "<down-mouse-3>") #'shape-artist-mouse-context-menu)

(provide 'shape-artist-mouse)
;;; shape-artist-mouse.el ends here
