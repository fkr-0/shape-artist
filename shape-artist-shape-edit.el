;;; shape-artist-shape-edit.el --- SHAPE-EDIT state operations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Repeatable Evil-style editing for one locked shape or a SELECT-mode group.

;;; Code:

(require 'cl-lib)
(require 'shape-artist-core)
(require 'shape-artist-shapes)

(declare-function evil-normal-state "evil")

(defun shape-artist-shape-edit-exit ()
  "Exit SHAPE-EDIT mode, clear locks, and return to MOVE."
  (interactive)
  (setq shape-artist--locked-shape nil)
  (setq shape-artist--selected-shape-signatures nil)
  (setq shape-artist--mode 'move)
  (when (fboundp 'evil-normal-state)
    (evil-normal-state))
  (shape-artist--update-mode-line)
  (shape-artist--maybe-update-sidebar))

(defun shape-artist-shape-edit--count (count)
  "Normalize interactive COUNT to a positive integer."
  (max 1 (prefix-numeric-value (or count 1))))

(defun shape-artist-shape-edit--target-signatures ()
  "Return target signatures for current shape edit operation."
  (or shape-artist--selected-shape-signatures
      (and shape-artist--locked-shape
           (list (shape-artist-shapes-signature shape-artist--locked-shape)))))

(defun shape-artist-shape-edit--set-targets (shapes)
  "Refresh lock and group selection from SHAPES."
  (setq shapes (cl-remove nil shapes))
  (when shapes
    (setq shape-artist--locked-shape (car shapes))
    (when shape-artist--selected-shape-signatures
      (setq shape-artist--selected-shape-signatures
            (mapcar #'shape-artist-shapes-signature shapes)))
    (shape-artist--maybe-update-sidebar)
    (car shapes)))

(defun shape-artist-shape-edit--apply (fn)
  "Apply FN to locked shape or selected group."
  (let ((results nil))
    (dolist (sig (shape-artist-shape-edit--target-signatures))
      (let ((shape (shape-artist-shapes-find-by-signature sig)))
        (when shape
          (push (funcall fn shape) results))))
    (shape-artist-shape-edit--set-targets (nreverse results))))

(defun shape-artist-shape-edit-label ()
  "Edit the label of the locked-in rectangle."
  (interactive)
  (when shape-artist--locked-shape
    (shape-artist-shape-edit--set-targets
     (list (shape-artist-shapes-edit-label shape-artist--locked-shape)))))

(defun shape-artist-shape-move (&optional dx dy)
  "Move target shape or group by DX and DY."
  (interactive)
  (let ((dx (or dx (read-number "dx: " 0)))
        (dy (or dy (read-number "dy: " 0))))
    (if shape-artist--selected-shape-signatures
        (shape-artist-shape-edit--set-targets
         (shape-artist-shapes-move-group
          (shape-artist-shapes-selected) dx dy))
      (shape-artist-shape-edit--apply
       (lambda (shape) (shape-artist-shapes-move shape dx dy))))))

(defun shape-artist-shape-move-left (&optional count)
  "Move targets left by COUNT cells."
  (interactive "p")
  (shape-artist-shape-move (- (shape-artist-shape-edit--count count)) 0))

(defun shape-artist-shape-move-right (&optional count)
  "Move targets right by COUNT cells."
  (interactive "p")
  (shape-artist-shape-move (shape-artist-shape-edit--count count) 0))

(defun shape-artist-shape-move-up (&optional count)
  "Move targets up by COUNT cells."
  (interactive "p")
  (shape-artist-shape-move 0 (- (shape-artist-shape-edit--count count))))

(defun shape-artist-shape-move-down (&optional count)
  "Move targets down by COUNT cells."
  (interactive "p")
  (shape-artist-shape-move 0 (shape-artist-shape-edit--count count)))

(defun shape-artist-shape-resize (&optional width height)
  "Resize locked rectangle to WIDTH and HEIGHT."
  (interactive)
  (when shape-artist--locked-shape
    (let ((width (or width (read-number "New width: ")))
          (height (or height (read-number "New height: "))))
      (shape-artist-shape-edit--set-targets
       (list (shape-artist-shapes-resize shape-artist--locked-shape width height))))))

(defun shape-artist-shape-edit--resize-rect-targets (x-anchor dx y-anchor dy)
  "Resize all rectangle targets from X-ANCHOR/Y-ANCHOR by DX/DY."
  (shape-artist-shape-edit--apply
   (lambda (shape)
     (if (shape-artist-diagram-rectangle-p shape)
         (shape-artist-shapes-resize-anchored shape x-anchor dx y-anchor dy)
       shape))))

(defun shape-artist-shape-resize-wider (&optional count)
  "Widen rectangle targets by COUNT, or extend horizontal edges."
  (interactive "p")
  (let ((n (shape-artist-shape-edit--count count)))
    (shape-artist-shape-edit--apply
     (lambda (shape)
       (cond
        ((shape-artist-diagram-rectangle-p shape)
         (shape-artist-shapes-resize-anchored shape 'right n 'bottom 0))
        ((and (shape-artist-diagram-edge-p shape) (= (oref shape y) (oref shape y2)))
         (shape-artist-shapes-edge-extend shape n))
        (t shape))))))

(defun shape-artist-shape-resize-narrower (&optional count)
  "Narrow rectangle targets by COUNT, or shorten horizontal edges."
  (interactive "p")
  (let ((n (shape-artist-shape-edit--count count)))
    (shape-artist-shape-edit--apply
     (lambda (shape)
       (cond
        ((shape-artist-diagram-rectangle-p shape)
         (let ((delta (- (min n (- (oref shape width) 2)))))
           (if (= delta 0) shape
             (shape-artist-shapes-resize-anchored shape 'right delta 'bottom 0))))
        ((and (shape-artist-diagram-edge-p shape) (= (oref shape y) (oref shape y2)))
         (shape-artist-shapes-edge-extend shape (- n)))
        (t shape))))))

(defun shape-artist-shape-resize-taller (&optional count)
  "Make rectangle targets taller by COUNT, or extend vertical edges."
  (interactive "p")
  (let ((n (shape-artist-shape-edit--count count)))
    (shape-artist-shape-edit--apply
     (lambda (shape)
       (cond
        ((shape-artist-diagram-rectangle-p shape)
         (shape-artist-shapes-resize-anchored shape 'right 0 'bottom n))
        ((and (shape-artist-diagram-edge-p shape) (= (oref shape x) (oref shape x2)))
         (shape-artist-shapes-edge-extend shape n))
        (t shape))))))

(defun shape-artist-shape-resize-shorter (&optional count)
  "Make rectangle targets shorter by COUNT, or shorten vertical edges."
  (interactive "p")
  (let ((n (shape-artist-shape-edit--count count)))
    (shape-artist-shape-edit--apply
     (lambda (shape)
       (cond
        ((shape-artist-diagram-rectangle-p shape)
         (let ((delta (- (min n (- (oref shape height) 2)))))
           (if (= delta 0) shape
             (shape-artist-shapes-resize-anchored shape 'right 0 'bottom delta))))
        ((and (shape-artist-diagram-edge-p shape) (= (oref shape x) (oref shape x2)))
         (shape-artist-shapes-edge-extend shape (- n)))
        (t shape))))))

(defun shape-artist-shape-resize-left (&optional count)
  "Grow rectangle targets to the left by COUNT cells."
  (interactive "p")
  (shape-artist-shape-edit--resize-rect-targets
   'left (shape-artist-shape-edit--count count) 'bottom 0))

(defun shape-artist-shape-shrink-left (&optional count)
  "Shrink rectangle targets from the left by COUNT cells."
  (interactive "p")
  (shape-artist-shape-edit--resize-rect-targets
   'left (- (shape-artist-shape-edit--count count)) 'bottom 0))

(defun shape-artist-shape-resize-up (&optional count)
  "Grow rectangle targets upward by COUNT cells."
  (interactive "p")
  (shape-artist-shape-edit--resize-rect-targets
   'right 0 'top (shape-artist-shape-edit--count count)))

(defun shape-artist-shape-shrink-up (&optional count)
  "Shrink rectangle targets from the top by COUNT cells."
  (interactive "p")
  (shape-artist-shape-edit--resize-rect-targets
   'right 0 'top (- (shape-artist-shape-edit--count count))))

(defun shape-artist-shape-set-style (&optional style)
  "Set rectangle border or edge connector STYLE."
  (interactive)
  (let ((style (or style
                   (intern (completing-read
                            "Style: "
                            '("standard" "heavy" "light" "plain" "arrow-end" "arrow-both") nil t)))))
    (shape-artist-shape-edit--apply
     (lambda (shape)
       (cond
        ((shape-artist-diagram-rectangle-p shape)
         (shape-artist-shapes-replace shape style))
        ((shape-artist-diagram-edge-p shape)
         (shape-artist-shapes-set-edge-style shape style))
        (t shape))))))

(defun shape-artist-shape-cycle-style ()
  "Cycle rectangle border style or edge connector style."
  (interactive)
  (shape-artist-shape-edit--apply
   (lambda (shape)
     (cond
      ((shape-artist-diagram-rectangle-p shape)
       (let* ((current (shape-artist-shapes-border-style shape))
              (next (pcase current ('standard 'heavy) ('heavy 'light) (_ 'standard))))
         (shape-artist-shapes-replace shape next)))
      ((shape-artist-diagram-edge-p shape)
       (let* ((current (shape-artist-shapes-edge-style shape))
              (next (pcase current ('plain 'arrow-end) ('arrow-end 'arrow-both) (_ 'plain))))
         (shape-artist-shapes-set-edge-style shape next)))
      (t shape)))))

(defun shape-artist-shape-replace ()
  "Cycle the locked-in shape's style."
  (interactive)
  (shape-artist-shape-cycle-style))

(defun shape-artist-shape-edit-up ()
  "Navigate up within locked shape interior."
  (interactive)
  (when (and shape-artist--locked-shape
             (shape-artist-diagram-rectangle-p shape-artist--locked-shape))
    (let* ((shape shape-artist--locked-shape)
           (pos (shape-artist--current-row-col))
           (min-y (1+ (oref shape y))))
      (when (> (car pos) min-y)
        (shape-artist--goto-row-col (1- (car pos)) (cdr pos))))))

(defun shape-artist-shape-edit-down ()
  "Navigate down within locked shape interior."
  (interactive)
  (when (and shape-artist--locked-shape
             (shape-artist-diagram-rectangle-p shape-artist--locked-shape))
    (let* ((shape shape-artist--locked-shape)
           (pos (shape-artist--current-row-col))
           (max-y (1- (+ (oref shape y) (oref shape height)))))
      (when (< (car pos) max-y)
        (shape-artist--goto-row-col (1+ (car pos)) (cdr pos))))))

(defun shape-artist-shape-edit-left ()
  "Navigate left within locked shape interior."
  (interactive)
  (when (and shape-artist--locked-shape
             (shape-artist-diagram-rectangle-p shape-artist--locked-shape))
    (let* ((shape shape-artist--locked-shape)
           (pos (shape-artist--current-row-col))
           (min-x (1+ (oref shape x))))
      (when (> (cdr pos) min-x)
        (shape-artist--goto-row-col (car pos) (1- (cdr pos)))))))

(defun shape-artist-shape-edit-right ()
  "Navigate right within locked shape interior."
  (interactive)
  (when (and shape-artist--locked-shape
             (shape-artist-diagram-rectangle-p shape-artist--locked-shape))
    (let* ((shape shape-artist--locked-shape)
           (pos (shape-artist--current-row-col))
           (max-x (1- (+ (oref shape x) (oref shape width)))))
      (when (< (cdr pos) max-x)
        (shape-artist--goto-row-col (car pos) (1+ (cdr pos)))))))

(provide 'shape-artist-shape-edit)
;;; shape-artist-shape-edit.el ends here
