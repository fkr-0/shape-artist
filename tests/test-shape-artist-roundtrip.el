;;; test-shape-artist-roundtrip.el --- Property tests: edits never corrupt a diagram -*- lexical-binding: t; -*-

;; Deterministic, seeded randomised tests.  They generate non-overlapping boxes
;; in all seven border families, apply random move / resize / restyle commands,
;; and assert invariants that must hold for ANY sequence of edits:
;;
;;  - the generated text parses back to exactly the generated boxes;
;;  - after every edit the same number of boxes is detected, with the geometry
;;    and style the edit asked for, and every label survives;
;;  - an edit changes no cell outside the region it is allowed to touch, and
;;    never grows the canvas when it stays in bounds;
;;  - moving a box and moving it back restores the text exactly;
;;  - one ordinary `undo' per edit restores the original text byte for byte.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'shape-artist-core)
(require 'shape-artist-shapes)
(require 'shape-artist-diagram-parse)

(defconst test-shape-artist-roundtrip--cols 40 "Width of the generated canvas.")
(defconst test-shape-artist-roundtrip--rows 16 "Height of the generated canvas.")
(defconst test-shape-artist-roundtrip--diagrams
  (let ((n (getenv "SHAPE_ARTIST_ROUNDTRIP_DIAGRAMS")))
    (if (and n (> (string-to-number n) 0)) (string-to-number n) 40))
  "How many seeded diagrams each property test checks.
Override with the environment variable SHAPE_ARTIST_ROUNDTRIP_DIAGRAMS; the
coverage script lowers it because edebug instrumentation is much slower.")

;;; --- Seeded generation ---------------------------------------------------

(defun test-shape-artist-roundtrip--seed (name n)
  "Seed the random generator deterministically from NAME and N."
  (random (format "shape-artist-roundtrip-%s-%d" name n)))

(defun test-shape-artist-roundtrip--between (low high)
  "Return a random integer from LOW to HIGH inclusive."
  (+ low (random (1+ (- high low)))))

(defun test-shape-artist-roundtrip--pick (list)
  "Return a random element of LIST."
  (nth (random (length list)) list))

(defun test-shape-artist-roundtrip--collides-p (a b margin)
  "Return non-nil when boxes A and B, each (X Y W H), come within MARGIN cells."
  (pcase-let ((`(,ax ,ay ,aw ,ah) a)
              (`(,bx ,by ,bw ,bh) b))
    (not (or (< (+ ax aw margin) bx) (< (+ bx bw margin) ax)
             (< (+ ay ah margin) by) (< (+ by bh margin) ay)))))

(defun test-shape-artist-roundtrip--in-bounds-p (box)
  "Return non-nil when BOX stays one cell clear of the canvas edge."
  (pcase-let ((`(,x ,y ,w ,h) box))
    (and (>= x 1) (>= y 1)
         (<= (+ x w) (- test-shape-artist-roundtrip--cols 2))
         (<= (+ y h) (- test-shape-artist-roundtrip--rows 2)))))

(defun test-shape-artist-roundtrip--generate (count)
  "Return up to COUNT well-separated boxes as ((X Y W H STYLE LABEL) ...)."
  (let (boxes (tries 0))
    (while (and (< (length boxes) count) (< tries 300))
      (cl-incf tries)
      (let* ((w (test-shape-artist-roundtrip--between 5 10))
             (h (test-shape-artist-roundtrip--between 2 4))
             (x (test-shape-artist-roundtrip--between
                 1 (- test-shape-artist-roundtrip--cols w 2)))
             (y (test-shape-artist-roundtrip--between
                 1 (- test-shape-artist-roundtrip--rows h 2)))
             (geometry (list x y w h)))
        (unless (or (not (test-shape-artist-roundtrip--in-bounds-p geometry))
                    (cl-some (lambda (b)
                               (test-shape-artist-roundtrip--collides-p
                                geometry (cl-subseq b 0 4) 3))
                             boxes))
          (push (append geometry
                        (list (test-shape-artist-roundtrip--pick
                               shape-artist-border-styles)
                              (format "n%d" (length boxes))))
                boxes))))
    (nreverse boxes)))

(defun test-shape-artist-roundtrip--render (boxes)
  "Replace the buffer with a blank canvas holding BOXES."
  (erase-buffer)
  (dotimes (row test-shape-artist-roundtrip--rows)
    (insert (make-string test-shape-artist-roundtrip--cols ?\s))
    (when (< row (1- test-shape-artist-roundtrip--rows)) (insert "\n")))
  (dolist (box boxes)
    (pcase-let ((`(,x ,y ,w ,h ,style ,label) box))
      (shape-artist--draw-rect-border x y w h style)
      (shape-artist--write-rect-interior x y w h (list label)))))

;;; --- Observation helpers --------------------------------------------------

(defun test-shape-artist-roundtrip--rects ()
  "Parse the buffer and return its rectangles sorted by position."
  (shape-artist-shapes-parse)
  (sort (cl-remove-if-not #'shape-artist-diagram-rectangle-p
                          shape-artist--last-shapes-list)
        (lambda (a b) (or (< (oref a y) (oref b y))
                          (and (= (oref a y) (oref b y))
                               (< (oref a x) (oref b x)))))))

(defun test-shape-artist-roundtrip--observed ()
  "Return the parsed boxes as sorted ((X Y W H STYLE) ...)."
  (mapcar (lambda (r) (list (oref r x) (oref r y) (oref r width) (oref r height)
                            (oref r detected-style)))
          (test-shape-artist-roundtrip--rects)))

(defun test-shape-artist-roundtrip--sorted (boxes)
  "Return BOXES (with or without labels) as sorted (X Y W H STYLE) lists."
  (sort (mapcar (lambda (b) (cl-subseq b 0 5)) (copy-sequence boxes))
        (lambda (a b) (or (< (nth 1 a) (nth 1 b))
                          (and (= (nth 1 a) (nth 1 b)) (< (nth 0 a) (nth 0 b)))))))

(defun test-shape-artist-roundtrip--outside-changed-p (before after allowed)
  "Return the first cell outside ALLOWED that differs between BEFORE and AFTER.
BEFORE and AFTER are line lists; ALLOWED is a list of (X Y W H) boxes whose
cells may change.  Return nil when nothing outside them changed."
  (cl-block nil
    (cl-loop for row from 0 below (length before)
             for old = (nth row before)
             for new = (nth row after)
             do (cl-loop for col from 0 below (length old)
                         unless (or (cl-some (lambda (b)
                                               (pcase-let ((`(,x ,y ,w ,h) b))
                                                 (and (<= x col (+ x w))
                                                      (<= y row (+ y h)))))
                                             allowed)
                                    (eq (aref old col) (aref new col)))
                         do (cl-return (list row col))))
    nil))

(defun test-shape-artist-roundtrip--interiors-clean-p (boxes)
  "Return non-nil when each box interior holds only its label, then blanks.
Catches ghosts: stray border glyphs left inside a box after it was resized,
moved, or restyled."
  (let ((lines (shape-artist--get-lines)))
    (cl-every
     (lambda (b)
       (pcase-let ((`(,x ,y ,w ,h ,_style ,label) b))
         (cl-loop for row from (1+ y) below (+ y h)
                  always (let ((inside (substring (nth row lines) (1+ x) (+ x w))))
                           (if (= row (1+ y))
                               (equal (string-trim inside) label)
                             (string-blank-p inside))))))
     boxes)))

(defun test-shape-artist-roundtrip--labels-present-p (boxes)
  "Return non-nil when every label of BOXES appears in the buffer text."
  (let ((text (buffer-string)))
    (cl-every (lambda (b) (string-match-p (regexp-quote (nth 5 b)) text)) boxes)))

;;; --- Random edits -----------------------------------------------------------

(defun test-shape-artist-roundtrip--others (boxes index)
  "Return the geometry of every box in BOXES except the one at INDEX."
  (cl-loop for b in boxes for i from 0
           unless (= i index) collect (cl-subseq b 0 4)))

(defun test-shape-artist-roundtrip--clear-p (candidate boxes index)
  "Return non-nil when CANDIDATE geometry is in bounds and clear of other boxes."
  (and (test-shape-artist-roundtrip--in-bounds-p candidate)
       (not (cl-some (lambda (o) (test-shape-artist-roundtrip--collides-p
                                  candidate o 2))
                     (test-shape-artist-roundtrip--others boxes index)))))

(defun test-shape-artist-roundtrip--apply-random-edit (boxes)
  "Apply one random valid edit to the buffer and return (NEW-BOXES . ALLOWED).
NEW-BOXES is the model after the edit; ALLOWED lists the regions the edit may
change.  Return nil when the random draw produced no valid edit."
  (let* ((index (random (length boxes)))
         (box (nth index boxes))
         (rect (nth index (test-shape-artist-roundtrip--rects)))
         (kind (test-shape-artist-roundtrip--pick '(move move resize restyle))))
    ;; The parsed list is position-sorted; align the model with it.
    (setq rect (cl-find-if (lambda (r) (and (= (oref r x) (nth 0 box))
                                            (= (oref r y) (nth 1 box))))
                           (test-shape-artist-roundtrip--rects)))
    (pcase-let ((`(,x ,y ,w ,h ,style ,label) box))
      (pcase kind
        ('move
         (let* ((dx (test-shape-artist-roundtrip--between -4 4))
                (dy (test-shape-artist-roundtrip--between -3 3))
                (candidate (list (+ x dx) (+ y dy) w h)))
           (when (and (not (and (= dx 0) (= dy 0)))
                      (test-shape-artist-roundtrip--clear-p candidate boxes index))
             (shape-artist-shapes-move rect dx dy)
             (cons (let ((new (copy-sequence boxes)))
                     (setf (nth index new) (append candidate (list style label)))
                     new)
                   (list (list x y w h) candidate)))))
        ('resize
         ;; Grow or shrink from any corner: the moving edges are the ones named
         ;; by the anchors, so left/top resizes shift the box origin.
         (let* ((x-anchor (test-shape-artist-roundtrip--pick '(left right)))
                (y-anchor (test-shape-artist-roundtrip--pick '(top bottom)))
                (dw (test-shape-artist-roundtrip--between -2 2))
                (dh (test-shape-artist-roundtrip--between -2 2))
                (candidate (list (if (eq x-anchor 'left) (- x dw) x)
                                 (if (eq y-anchor 'top) (- y dh) y)
                                 (+ w dw) (+ h dh))))
           (when (and (not (and (= dw 0) (= dh 0)))
                      (>= (nth 2 candidate) 4)
                      (>= (nth 3 candidate) 2)
                      (test-shape-artist-roundtrip--clear-p candidate boxes index))
             (shape-artist-shapes-resize-anchored rect x-anchor dw y-anchor dh)
             (cons (let ((new (copy-sequence boxes)))
                     (setf (nth index new) (append candidate (list style label)))
                     new)
                   (list (list x y w h) candidate)))))
        ('restyle
         (let ((new-style (test-shape-artist-roundtrip--pick
                           (remq style shape-artist-border-styles))))
           (shape-artist-shapes-replace rect new-style)
           (cons (let ((new (copy-sequence boxes)))
                   (setf (nth index new) (list x y w h new-style label))
                   new)
                 (list (list x y w h)))))))))

;;; --- Properties ---------------------------------------------------------------

(ert-deftest test-shape-artist-roundtrip-generated-diagrams-parse-back-exactly ()
  "Every generated diagram parses to exactly the boxes that were drawn."
  (dotimes (n test-shape-artist-roundtrip--diagrams)
    (test-shape-artist-roundtrip--seed "parse" n)
    (let ((boxes (test-shape-artist-roundtrip--generate
                  (test-shape-artist-roundtrip--between 1 4))))
      (ert-info ((format "seed %d, boxes %S" n boxes))
        (test-artist-with-buffer ""
          (let ((shape-artist--update-sidebar-fn nil))
            (test-shape-artist-roundtrip--render boxes)
            (should (equal (test-shape-artist-roundtrip--observed)
                           (test-shape-artist-roundtrip--sorted boxes)))
            (should (test-shape-artist-roundtrip--labels-present-p boxes))
            (should (test-shape-artist-roundtrip--interiors-clean-p boxes))))))))

(ert-deftest test-shape-artist-roundtrip-random-edit-sequences-never-corrupt ()
  "Any sequence of valid edits keeps boxes, labels, geometry, and neighbours intact."
  (let ((edits 0))
    (dotimes (n test-shape-artist-roundtrip--diagrams)
      (test-shape-artist-roundtrip--seed "edits" n)
      (let ((boxes (test-shape-artist-roundtrip--generate
                    (test-shape-artist-roundtrip--between 2 4))))
        (test-artist-with-buffer ""
          (let ((shape-artist--update-sidebar-fn nil))
            (test-shape-artist-roundtrip--render boxes)
            (dotimes (step 8)
              (ert-info ((format "seed %d step %d boxes %S" n step boxes))
                (let* ((before (shape-artist--get-lines))
                       (result (test-shape-artist-roundtrip--apply-random-edit boxes)))
                  (when result
                    (cl-incf edits)
                    (setq boxes (car result))
                    (let ((after (shape-artist--get-lines)))
                      ;; Nothing outside the touched regions changed, and an
                      ;; in-bounds edit never grew the canvas.
                      (should-not (test-shape-artist-roundtrip--outside-changed-p
                                   before after (cdr result)))
                      (should (equal (mapcar #'length before) (mapcar #'length after)))
                      (should (= (length before) (length after))))
                    (should (equal (test-shape-artist-roundtrip--observed)
                                   (test-shape-artist-roundtrip--sorted boxes)))
                    (should (test-shape-artist-roundtrip--labels-present-p boxes))
                    (should (test-shape-artist-roundtrip--interiors-clean-p boxes))))))))))
    ;; Guard against a generator that silently stops producing edits.
    (should (> edits (* 2 test-shape-artist-roundtrip--diagrams)))))

(ert-deftest test-shape-artist-roundtrip-move-and-move-back-is-the-identity ()
  "Moving a box and then moving it back restores the text exactly."
  (let ((checked 0))
    (dotimes (n test-shape-artist-roundtrip--diagrams)
      (test-shape-artist-roundtrip--seed "back" n)
      (let ((boxes (test-shape-artist-roundtrip--generate
                    (test-shape-artist-roundtrip--between 2 4))))
        (test-artist-with-buffer ""
          (let ((shape-artist--update-sidebar-fn nil))
            (test-shape-artist-roundtrip--render boxes)
            (let* ((index (random (length boxes)))
                   (box (nth index boxes))
                   (dx (test-shape-artist-roundtrip--between -4 4))
                   (dy (test-shape-artist-roundtrip--between -3 3))
                   (candidate (list (+ (nth 0 box) dx) (+ (nth 1 box) dy)
                                    (nth 2 box) (nth 3 box)))
                   (original (buffer-string)))
              (when (and (not (and (= dx 0) (= dy 0)))
                         (test-shape-artist-roundtrip--clear-p candidate boxes index))
                (cl-incf checked)
                (ert-info ((format "seed %d move %S by (%d,%d)" n box dx dy))
                  (let ((moved (shape-artist-shapes-move
                                (cl-find-if (lambda (r) (and (= (oref r x) (nth 0 box))
                                                             (= (oref r y) (nth 1 box))))
                                            (test-shape-artist-roundtrip--rects))
                                dx dy)))
                    (should-not (equal (buffer-string) original))
                    (shape-artist-shapes-move moved (- dx) (- dy))
                    (should (equal (buffer-string) original))))))))))
    (should (> checked (/ test-shape-artist-roundtrip--diagrams 3)))))

(ert-deftest test-shape-artist-roundtrip-one-undo-per-edit-restores-the-original ()
  "Undoing once per edit returns the text to exactly what it was."
  (let ((undone 0))
    (dotimes (n test-shape-artist-roundtrip--diagrams)
      (test-shape-artist-roundtrip--seed "undo" n)
      (let ((boxes (test-shape-artist-roundtrip--generate
                    (test-shape-artist-roundtrip--between 2 4))))
        (test-artist-with-buffer ""
          (let ((shape-artist--update-sidebar-fn nil)
                (applied 0))
            (test-shape-artist-roundtrip--render boxes)
            (let ((original (buffer-string)))
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              (dotimes (_ 6)
                (undo-boundary)
                (let ((result (test-shape-artist-roundtrip--apply-random-edit boxes)))
                  (when result
                    (setq boxes (car result))
                    (cl-incf applied))))
              (undo-boundary)
              (ert-info ((format "seed %d after %d edits" n applied))
                (dotimes (i applied)
                  (let ((last-command (if (= i 0) nil 'undo)))
                    (undo)))
                (cl-incf undone applied)
                (should (equal (buffer-string) original))))))))
    (should (> undone test-shape-artist-roundtrip--diagrams))))

(provide 'test-shape-artist-roundtrip)
;;; test-shape-artist-roundtrip.el ends here
