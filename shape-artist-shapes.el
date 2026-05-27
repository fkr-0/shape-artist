;;; shape-artist-shapes.el --- Shape parsing integration, label editing, move/resize stubs -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'artist-diagram-parse)
(require 'shape-artist-core)

;;; --- Shape Parsing Integration ---

(defun shape-artist-shapes-parse ()
  "Re-parse shapes in current buffer and update buffer-local state."
  (interactive)
  (let ((result (artist-diagram-parse-buffer)))
    (setq shape-artist--last-root (car result))
    (setq shape-artist--last-shapes-list (cdr result))
    (message "Detected %d shapes" (length shape-artist--last-shapes-list))
    (shape-artist--maybe-update-sidebar)))

;;; --- Shape Stepping ---

(defun shape-artist-shapes-current ()
  "Return the current shape candidate or nil."
  (when (and shape-artist--last-shapes-list
             (< shape-artist--select-index (length shape-artist--last-shapes-list)))
    (nth shape-artist--select-index shape-artist--last-shapes-list)))

(defun shape-artist-shapes-count ()
  "Return the number of detected shapes."
  (length shape-artist--last-shapes-list))

;;; --- Label Editing ---

(defun shape-artist-shapes-edit-label (shape &optional align)
  "Edit the label of SHAPE.

ALIGN is \\='left, \\='center, or \\='right (default: \\='center)."
  (let* ((align (or align 'center))
         (rect shape)
         (x (oref rect x))
         (y (oref rect y))
         (w (oref rect width))
         (h (oref rect height))
         (interior-width (max 0 (- w 2)))
         (interior-height (max 0 (- h 2)))
         ;; Get existing label text
         (existing-text "")
         (labels (cl-remove-if-not
                  (lambda (s)
                    (and (artist-diagram-label-p s)
                         (>= (oref s x) (1+ x))
                         (<= (oref s x) (+ x w -1))
                         (>= (oref s y) (1+ y))
                         (<= (oref s y) (+ y h -1))))
                  shape-artist--last-shapes-list))
         (prompt-text (if labels
                          (oref (car labels) text)
                        "")))
    ;; Prompt for new text
    (let ((new-text (read-string "Label text: " prompt-text)))
      (when new-text
        ;; Clear interior
        (save-excursion
          (dotimes (dr interior-height)
            (shape-artist--goto-row-col (+ y 1 dr) (+ x 1))
            (delete-char interior-width)
            (insert (make-string interior-width ?\s))))
        ;; Insert new text, centered
        (let ((text-len (length new-text)))
          (if (<= text-len interior-width)
              ;; Text fits, insert centered
              (let ((start-col (pcase align
                                 ('left (+ x 1))
                                 ('right (+ x 1 (- interior-width text-len)))
                                 (_ (+ x 1 (/ (- interior-width text-len) 2))))))
                (save-excursion
                  (shape-artist--goto-row-col (+ y 1) start-col)
                  (delete-char text-len)
                  (insert new-text)))
            ;; Text wider than rectangle - auto-expand right
            (let ((needed-width (- text-len interior-width)))
              (shape-artist--expand-rect-right rect needed-width)
              ;; Insert text
              (save-excursion
                (shape-artist--goto-row-col (+ y 1) (+ x 1))
                (delete-char text-len)
                (insert new-text))))))
      ;; Re-parse
      (shape-artist-shapes-parse))))

(defun shape-artist--expand-rect-right (rect amount)
  "Expand RECT to the right by AMOUNT columns."
  (let* ((x (oref rect x))
         (y (oref rect y))
         (w (oref rect width))
         (h (oref rect height)))
    ;; Expand the buffer grid if needed
    (dotimes (_ amount)
      (shape-artist--expand-right))
    ;; Redraw the rectangle with new width
    (save-excursion
      ;; Top border
      (shape-artist--goto-row-col y x)
      (delete-char (1+ w))
      (insert ?+ (make-string (+ w amount -1) ?-) ?+)
      ;; Middle rows
      (dotimes (dr (1- h))
        (shape-artist--goto-row-col (+ y 1 dr) x)
        (delete-char 1)
        (insert ?|)
        (shape-artist--goto-row-col (+ y 1 dr) (+ x w))
        (delete-char 1)
        (insert (make-string amount ?\s) ?|))
      ;; Bottom border
      (shape-artist--goto-row-col (+ y h) x)
      (delete-char (1+ w))
      (insert ?+ (make-string (+ w amount -1) ?-) ?+))
    ;; Update rectangle dimensions
    (oset rect width (+ w amount))
    (shape-artist--ensure-grid-rectangular)))

;;; --- Move/Resize Stubs ---

(defun shape-artist-shapes-move (shape dx dy)
  "Move SHAPE by DX,DY. Stub implementation."
  (interactive
   (list (shape-artist-shapes-current)
         (read-number "dx: " 0)
         (read-number "dy: " 0)))
  (when shape
    (message "Move shape by (%d, %d) - stub" dx dy)
    ;; TODO: Implement actual move
    (let ((x (oref shape x))
          (y (oref shape y)))
      (oset shape x (+ x dx))
      (oset shape y (+ y dy)))))

(defun shape-artist-shapes-resize (shape new-width new-height)
  "Resize SHAPE to NEW-WIDTH, NEW-HEIGHT. Stub implementation."
  (interactive
   (list (shape-artist-shapes-current)
         (read-number "New width: ")
         (read-number "New height: ")))
  (when shape
    (message "Resize to (%d, %d) - stub" new-width new-height)
    ;; TODO: Implement actual resize
    (oset shape width new-width)
    (oset shape height new-height)))

(defun shape-artist-shapes-replace (shape new-kind)
  "Replace SHAPE type with NEW-KIND. Stub implementation."
  (interactive
   (list (shape-artist-shapes-current)
         (intern (completing-read "New shape type: '(" '(rectangle diamond) nil t))))
  (when shape
    (message "Replace shape with %s - stub" new-kind)
    ;; TODO: Implement actual shape replacement
    nil))

;;; --- Provide ---

(provide 'shape-artist-shapes)
;;; shape-artist-shapes.el ends here
