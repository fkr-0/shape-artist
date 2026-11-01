;;; test-shape-artist-guides.el --- ERT tests for guide snapping -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-core)
(require 'shape-artist-shapes)
(require 'shape-artist-guides)
(require 'shape-artist-ui)

(defmacro test-artist-with-guides-buffer (content &rest body)
  "Create a temp Shape Artist CONTENT buffer and run BODY."
  (declare (indent 1))
  `(test-artist-with-buffer
       ,content
     (let ((shape-artist--update-sidebar-fn nil)
           (shape-artist-guide-snap-distance 2)
           (shape-artist-guides--guides nil)
           (shape-artist-guides--next-id 1)
           (shape-artist-guides--overlays nil)
           (shape-artist-guides--grid-overlays nil)
           (shape-artist-guides-visible t)
           (shape-artist-guides-snap-enabled t)
           (shape-artist-grid-visible nil)
           (shape-artist-grid-snap-enabled nil)
           (shape-artist-guides--last-snap-report nil)
           (shape-artist-guides--suppress-snapping nil)
           (shape-artist-guides--undo-stack nil)
           (shape-artist-guides--redo-stack nil)
           (shape-artist-guides--history-inhibit nil))
       (shape-artist-shapes-parse)
       ,@body)))

(defun test-shape-artist-guides--first-rect ()
  "Return first parsed rectangle in the current test buffer."
  (cl-find-if #'shape-artist-diagram-rectangle-p shape-artist--last-shapes-list))

(defun test-shape-artist-guides--rects ()
  "Return parsed rectangles in the current test buffer."
  (cl-remove-if-not #'shape-artist-diagram-rectangle-p shape-artist--last-shapes-list))

(ert-deftest test-shape-artist-guides-adds-horizontal-and-vertical ()
  "Guide creation stores orientation, position, and monotonic ids."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (let ((h (shape-artist-guides-add 'horizontal 1 "row" 'manual))
          (v (shape-artist-guides-add 'vertical 3 "col" 'manual)))
      (should (= (shape-artist-guide-id h) 1))
      (should (= (shape-artist-guide-id v) 2))
      (should (eq (shape-artist-guide-orientation h) 'horizontal))
      (should (eq (shape-artist-guide-orientation v) 'vertical))
      (should (= (shape-artist-guide-position h) 1))
      (should (= (shape-artist-guide-position v) 3)))))

(ert-deftest test-shape-artist-guides-nearest-is-deterministic ()
  "Nearest guide lookup uses distance, then lower id for ties."
  (test-artist-with-guides-buffer
      "abc"
    (let ((g1 (shape-artist-guides-add 'vertical 2 nil 'manual))
          (_g2 (shape-artist-guides-add 'vertical 4 nil 'manual)))
      (should (eq (shape-artist-guides-find-nearest 'vertical 3) g1)))))

(ert-deftest test-shape-artist-guides-hidden-policy ()
  "Hidden guides are still active unless visible-snaps-only is set."
  (test-artist-with-guides-buffer
      "abc"
    (let ((guide (shape-artist-guides-add 'vertical 2 nil 'manual)))
      (setf (shape-artist-guide-visible guide) nil)
      (let ((shape-artist-guide-visible-snaps-only nil))
        (should (shape-artist-guides-find-nearest 'vertical 2 t)))
      (let ((shape-artist-guide-visible-snaps-only t))
        (should-not (shape-artist-guides-find-nearest 'vertical 2 t))))))

(ert-deftest test-shape-artist-guides-snap-rectangle-move-left-edge ()
  "Moving near a vertical guide snaps the rectangle left edge."
  (test-artist-with-guides-buffer
      "    +---+\n    |   |\n    +---+"
    (shape-artist-guides-add 'vertical 2 nil 'manual)
    (let* ((rect (test-shape-artist-guides--first-rect))
           (snap (shape-artist-guides-snap-rectangle-move rect -1 0)))
      (should (equal (butlast snap 1) '(-2 0))))))

(ert-deftest test-shape-artist-guides-snap-rectangle-move-outside-distance ()
  "Movement outside snap distance remains unchanged."
  (test-artist-with-guides-buffer
      "    +---+\n    |   |\n    +---+"
    (shape-artist-guides-add 'vertical 0 nil 'manual)
    (let* ((rect (test-shape-artist-guides--first-rect))
           (snap (shape-artist-guides-snap-rectangle-move rect -1 0)))
      (should (equal (butlast snap 1) '(-1 0))))))

(ert-deftest test-shape-artist-guides-move-rectangle-snaps-buffer-shape ()
  "The existing rectangle move command uses snapped deltas."
  (test-artist-with-guides-buffer
      "    +---+\n    |   |\n    +---+"
    (shape-artist-guides-add 'vertical 2 nil 'manual)
    (let ((rect (test-shape-artist-guides--first-rect)))
      (shape-artist-shapes-move rect -1 0)
      (let ((moved (test-shape-artist-guides--first-rect)))
        (should (= (oref moved x) 2))))))

(ert-deftest test-shape-artist-guides-group-move-snaps-once ()
  "Group move applies one snapped delta to every selected rectangle."
  (test-artist-with-guides-buffer
      "    +---+   +---+\n    | A |---| B |\n    +---+   +---+"
    (let* ((rects (test-shape-artist-guides--rects))
           (x0 (mapcar (lambda (r) (oref r x)) rects)))
      (shape-artist-guides-add 'vertical 2 nil 'manual)
      (shape-artist-shapes-move-group rects -1 0)
      (let ((x1 (mapcar (lambda (r) (oref r x)) (test-shape-artist-guides--rects))))
        (should (equal x1 (mapcar (lambda (x) (- x 2)) x0)))))))

(ert-deftest test-shape-artist-guides-resize-snaps-right-edge ()
  "Anchored resize snaps the moving right edge to a vertical guide."
  (test-artist-with-guides-buffer
      "    +---+\n    |   |\n    +---+"
    (shape-artist-guides-add 'vertical 10 nil 'manual)
    (let* ((rect (test-shape-artist-guides--first-rect))
           (old-width (oref rect width)))
      (shape-artist-shapes-resize-anchored rect 'right 1 'bottom 0)
      (let ((resized (test-shape-artist-guides--first-rect)))
        (should (= (+ (oref resized x) (oref resized width)) 10))
        (should (= (oref resized width) (+ old-width 2)))))))

(ert-deftest test-shape-artist-guides-overlay-does-not_mutate_buffer ()
  "Guide overlay refresh must not alter buffer text."
  (test-artist-with-guides-buffer
      "abc\ndef"
    (let ((before (buffer-string)))
      (shape-artist-guides-add 'horizontal 0 nil 'manual)
      (shape-artist-guides-refresh-overlays)
      (should shape-artist-guides--overlays)
      (should (equal (buffer-string) before))
      (shape-artist-guide-clear-all)
      (should-not shape-artist-guides--overlays)
      (should (equal (buffer-string) before)))))

(ert-deftest test-shape-artist-guides-grid-overlay-does-not_mutate_buffer ()
  "Grid overlay toggling must not alter buffer text."
  (test-artist-with-guides-buffer
      "abcd\nefgh"
    (let ((before (buffer-string)))
      (shape-artist-grid-toggle)
      (should shape-artist-guides--grid-overlays)
      (should (equal (buffer-string) before))
      (shape-artist-grid-toggle)
      (should-not shape-artist-guides--grid-overlays)
      (should (equal (buffer-string) before)))))

(ert-deftest test-shape-artist-guides-sidebar-section ()
  "Guide sidebar text lists guide state and positions."
  (test-artist-with-guides-buffer
      "abc"
    (shape-artist-guides-add 'horizontal 1 "row" 'manual)
    (shape-artist-guides-add 'vertical 2 "col" 'manual)
    (let ((text (shape-artist-guides-section-text)))
      (should (string-match-p "=== Guides ===" text))
      (should (string-match-p "horizontal" text))
      (should (string-match-p "vertical" text))
      (should (string-match-p "Snap: on" text)))))

(ert-deftest test-shape-artist-guides-commands-use-current-position ()
  "Interactive guide creation commands default to current row/column."
  (test-artist-with-guides-buffer
      "abc\ndef"
    (shape-artist--goto-row-col 1 2)
    (shape-artist-guide-create-horizontal)
    (shape-artist-guide-create-vertical)
    (should (shape-artist-guides-find-nearest 'horizontal 1))
    (should (shape-artist-guides-find-nearest 'vertical 2))))


(ert-deftest test-shape-artist-guides-grid-snap-rectangle-move ()
  "Grid snapping moves rectangle top-left to nearest grid point."
  (test-artist-with-guides-buffer
      "     +---+\n     |   |\n     +---+"
    (setq shape-artist-guides-snap-enabled nil)
    (setq shape-artist-grid-snap-enabled t)
    (setq shape-artist-grid-size 4)
    (let ((rect (test-shape-artist-guides--first-rect)))
      (shape-artist-shapes-move rect 0 0)
      (let ((moved (test-shape-artist-guides--first-rect)))
        (should (= (oref moved x) 4))
        (should (plist-get shape-artist-guides--last-snap-report :grid-x))))))

(ert-deftest test-shape-artist-guides-grid-snap-group-move ()
  "Grid snapping applies one top-left correction to a shape group."
  (test-artist-with-guides-buffer
      "     +---+   +---+\n     | A |---| B |\n     +---+   +---+"
    (setq shape-artist-guides-snap-enabled nil)
    (setq shape-artist-grid-snap-enabled t)
    (setq shape-artist-grid-size 4)
    (let* ((rects (test-shape-artist-guides--rects))
           (x0 (mapcar (lambda (r) (oref r x)) rects)))
      (shape-artist-shapes-move-group rects 0 0)
      (let ((x1 (mapcar (lambda (r) (oref r x))
                        (test-shape-artist-guides--rects))))
        (should (equal x1 (mapcar (lambda (x) (- x 1)) x0)))))))

(ert-deftest test-shape-artist-guides-grid-snap-resize-right-edge ()
  "Grid snapping can resize a moving rectangle side to the grid."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (setq shape-artist-guides-snap-enabled nil)
    (setq shape-artist-grid-snap-enabled t)
    (setq shape-artist-grid-size 4)
    (let ((rect (test-shape-artist-guides--first-rect)))
      (shape-artist-shapes-resize-anchored rect 'right 3 'bottom 0)
      (let ((resized (test-shape-artist-guides--first-rect)))
        (should (= (+ (oref resized x) (oref resized width)) 8))))))

(ert-deftest test-shape-artist-guides-grid-snap-edge-move ()
  "Grid snapping applies to standalone edge movement."
  (test-artist-with-guides-buffer
      "     ----"
    (setq shape-artist-guides-snap-enabled nil)
    (setq shape-artist-grid-snap-enabled t)
    (setq shape-artist-grid-size 4)
    (let ((edge (cl-find-if #'shape-artist-diagram-edge-p
                            shape-artist--last-shapes-list)))
      (shape-artist-shapes-move edge 0 0)
      (let ((moved (cl-find-if #'shape-artist-diagram-edge-p
                               shape-artist--last-shapes-list)))
        (should (= (oref moved x) 4))))))

(ert-deftest test-shape-artist-guides-guide-beats-grid-on-same-axis ()
  "Guide snapping has priority over grid snapping per axis."
  (test-artist-with-guides-buffer
      "     +---+\n     |   |\n     +---+"
    (setq shape-artist-grid-snap-enabled t)
    (setq shape-artist-grid-size 4)
    (shape-artist-guides-add 'vertical 7 nil 'manual)
    (let ((rect (test-shape-artist-guides--first-rect)))
      (shape-artist-shapes-move rect 0 0)
      (let ((moved (test-shape-artist-guides--first-rect)))
        (should (= (oref moved x) 7))
        (should (plist-get shape-artist-guides--last-snap-report :x))
        (should-not (plist-get shape-artist-guides--last-snap-report :grid-x))))))

(ert-deftest test-shape-artist-guides-selection-to-guide-adopts-shape ()
  "Selection-derived guides adopt the source shape signatures."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (setq shape-artist--locked-shape (test-shape-artist-guides--first-rect))
    (shape-artist-guide-selection-to-guide)
    (should (= (length shape-artist-guides--guides) 4))
    (dolist (guide shape-artist-guides--guides)
      (should (= (length (shape-artist-guide-adopted-shape-signatures guide)) 1)))
    (should (string-match-p "adopted=1" (shape-artist-guides-section-text)))))

(ert-deftest test-shape-artist-guides-moving-guide-drags-adopted-shape ()
  "Moving a guide drags adopted shapes without recursively snapping them."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (setq shape-artist-grid-snap-enabled t)
    (setq shape-artist-grid-size 8)
    (let* ((rect (test-shape-artist-guides--first-rect))
           (guide (shape-artist-guides-add 'vertical 0 nil 'manual)))
      (shape-artist-guides--adopt-signatures
       guide (list (shape-artist-shapes-signature rect)))
      (shape-artist-guide-move (shape-artist-guide-id guide) 3)
      (let ((moved (test-shape-artist-guides--first-rect)))
        (should (= (oref moved x) 3))
        (should (= (shape-artist-guide-position guide) 3))
        (should (= (length (shape-artist-guide-adopted-shape-signatures guide))
                   1))))))


(ert-deftest test-shape-artist-guides-property-commands ()
  "Guide property commands update label, snap distance, and visibility."
  (test-artist-with-guides-buffer
      "abc"
    (let ((guide (shape-artist-guides-add 'vertical 2 nil 'manual)))
      (shape-artist-guide-set-label (shape-artist-guide-id guide) "main")
      (shape-artist-guide-set-snap-distance (shape-artist-guide-id guide) 5)
      (shape-artist-guide-toggle-nearest-visibility (shape-artist-guide-id guide))
      (should (equal (shape-artist-guide-label guide) "main"))
      (should (= (shape-artist-guide-snap-distance guide) 5))
      (should-not (shape-artist-guide-visible guide))
      (should (string-match-p "hidden main" (shape-artist-guides-section-text))))))

(ert-deftest test-shape-artist-guides-undo-redo-guide-state ()
  "Guide undo/redo restores non-text guide state."
  (test-artist-with-guides-buffer
      "abc"
    (let ((guide (shape-artist-guides-add 'vertical 2 nil 'manual)))
      (shape-artist-guide-set-label (shape-artist-guide-id guide) "main")
      (shape-artist-guides-undo)
      (let ((restored (shape-artist-guides--guide-at-id
                       (shape-artist-guide-id guide))))
        (should restored)
        (should-not (shape-artist-guide-label restored)))
      (shape-artist-guides-redo)
      (let ((restored (shape-artist-guides--guide-at-id
                       (shape-artist-guide-id guide))))
        (should (equal (shape-artist-guide-label restored) "main"))))))

(ert-deftest test-shape-artist-guides-undo-redo-adopted-guide-move ()
  "Guide undo/redo moves adopted shapes back and forth with the guide."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (let* ((rect (test-shape-artist-guides--first-rect))
           (guide (shape-artist-guides-add 'vertical 0 nil 'manual)))
      (shape-artist-guides--adopt-signatures
       guide (list (shape-artist-shapes-signature rect)))
      (shape-artist-guide-move (shape-artist-guide-id guide) 3)
      (should (= (oref (test-shape-artist-guides--first-rect) x) 3))
      (shape-artist-guides-undo)
      (should (= (oref (test-shape-artist-guides--first-rect) x) 0))
      (should (= (shape-artist-guide-position
                  (shape-artist-guides--guide-at-id
                   (shape-artist-guide-id guide)))
                 0))
      (shape-artist-guides-redo)
      (should (= (oref (test-shape-artist-guides--first-rect) x) 3))
      (should (= (shape-artist-guide-position
                  (shape-artist-guides--guide-at-id
                   (shape-artist-guide-id guide)))
                 3)))))

(ert-deftest test-shape-artist-guides-save-load-roundtrip ()
  "Guide sidecar persistence preserves IDs, settings, and adoptions."
  (let* ((dir (make-temp-file "shape-artist-guides" t))
         (default-directory dir)
         (file (expand-file-name "diagram.txt" dir))
         (shape-artist-guides-persistence-file ".shape-artist/guides.el"))
    (write-region "+---+\n|   |\n+---+" nil file nil 'silent)
    (let ((buffer (find-file-noselect file)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((shape-artist--update-sidebar-fn nil))
              (shape-artist-shapes-parse)
              (let* ((rect (test-shape-artist-guides--first-rect))
                     (guide (shape-artist-guides-add 'vertical 0 "left" 'manual)))
                (shape-artist-guides--adopt-signatures
                 guide (list (shape-artist-shapes-signature rect)))
                (setf (shape-artist-guide-snap-distance guide) 7)
                (setq shape-artist-grid-size 6)
                (setq shape-artist-grid-snap-enabled t)
                (shape-artist-guides-save)
                (shape-artist-guide-clear-all)
                (setq shape-artist-grid-size 4)
                (setq shape-artist-grid-snap-enabled nil)
                (shape-artist-guides-load)
                (should (= (length shape-artist-guides--guides) 1))
                (let ((loaded (car shape-artist-guides--guides)))
                  (should (= (shape-artist-guide-id loaded)
                             (shape-artist-guide-id guide)))
                  (should (equal (shape-artist-guide-label loaded) "left"))
                  (should (= (shape-artist-guide-snap-distance loaded) 7))
                  (should (= shape-artist-grid-size 6))
                  (should shape-artist-grid-snap-enabled)
                  (should (= (length
                              (shape-artist-guide-adopted-shape-signatures
                               loaded))
                             1)))
                (should (file-exists-p
                         (expand-file-name ".shape-artist/guides.el" dir))))))
        (kill-buffer buffer)))))

(ert-deftest test-shape-artist-guides-save-requires-file-backed-buffer ()
  "Guide persistence refuses anonymous buffers."
  (test-artist-with-guides-buffer
      "abc"
    (should-error (shape-artist-guides-save) :type 'user-error)))


;;; --- Guide Command Tests ---

(defmacro test-shape-artist-guides--capturing-messages (&rest body)
  "Run BODY, returning the `message' strings it produced, oldest first."
  (declare (indent 0))
  `(let ((captured nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format fmt args) captured)))))
       ,@body)
     (nreverse captured)))

(defun test-shape-artist-guides--guide-ids ()
  "Return the sorted ids of all guides in the current buffer."
  (sort (mapcar #'shape-artist-guide-id shape-artist-guides--guides) #'<))

(ert-deftest test-shape-artist-guides-delete-nearest-prefers-closer-guide ()
  "Deleting the nearest guide removes the closer of the two orientations."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (let ((row-guide (shape-artist-guides-add 'horizontal 2 nil 'manual))
          (col-guide (shape-artist-guides-add 'vertical 0 nil 'manual)))
      ;; Point at row 0, column 1: row guide is 2 away, column guide is 1 away.
      (shape-artist--goto-row-col 0 1)
      (should (equal (test-shape-artist-guides--capturing-messages
                       (shape-artist-guide-delete-nearest))
                     (list (format "Deleted guide %d"
                                   (shape-artist-guide-id col-guide)))))
      (should (equal (test-shape-artist-guides--guide-ids)
                     (list (shape-artist-guide-id row-guide)))))))

(ert-deftest test-shape-artist-guides-delete-nearest-tie-prefers-horizontal ()
  "When both orientations are equally close the horizontal guide is deleted."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (let ((row-guide (shape-artist-guides-add 'horizontal 1 nil 'manual))
          (col-guide (shape-artist-guides-add 'vertical 1 nil 'manual)))
      (shape-artist--goto-row-col 0 0)
      (shape-artist-guide-delete-nearest)
      (should (equal (test-shape-artist-guides--guide-ids)
                     (list (shape-artist-guide-id col-guide))))
      (should-not (shape-artist-guides--guide-at-id
                   (shape-artist-guide-id row-guide))))))

(ert-deftest test-shape-artist-guides-delete-nearest-without-guides ()
  "Deleting the nearest guide with no guides reports it and changes nothing."
  (test-artist-with-guides-buffer
      "abc"
    (should (equal (test-shape-artist-guides--capturing-messages
                     (shape-artist-guide-delete-nearest))
                   '("No guides to delete")))
    (should-not shape-artist-guides--guides)))

(ert-deftest test-shape-artist-guides-adopt-selection-nearest-adopts-and-undoes ()
  "Adopting a locked shape into the nearest guide is recorded for undo."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (let* ((rect (test-shape-artist-guides--first-rect))
           (guide (shape-artist-guides-add 'vertical 0 nil 'manual)))
      (setq shape-artist--locked-shape rect)
      (shape-artist--goto-row-col 1 0)
      (should (equal (test-shape-artist-guides--capturing-messages
                       (shape-artist-guide-adopt-selection-nearest))
                     (list (format "Guide %d adopted 1 shape(s)"
                                   (shape-artist-guide-id guide)))))
      (should (= (length (shape-artist-guide-adopted-shape-signatures guide)) 1))
      (shape-artist-guides-undo)
      (should-not (shape-artist-guide-adopted-shape-signatures
                   (shape-artist-guides--guide-at-id
                    (shape-artist-guide-id guide)))))))

(ert-deftest test-shape-artist-guides-adopt-selection-nearest-explicit-id ()
  "An explicit guide id wins over the guide nearest to point."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (let ((near (shape-artist-guides-add 'vertical 0 nil 'manual))
          (far (shape-artist-guides-add 'vertical 4 nil 'manual)))
      (setq shape-artist--locked-shape (test-shape-artist-guides--first-rect))
      (shape-artist--goto-row-col 1 0)
      (shape-artist-guide-adopt-selection-nearest (shape-artist-guide-id far))
      (should (= (length (shape-artist-guide-adopted-shape-signatures far)) 1))
      (should-not (shape-artist-guide-adopted-shape-signatures near)))))

(ert-deftest test-shape-artist-guides-adopt-selection-nearest-reports-nothing-to-do ()
  "Adopting reports a missing guide or a missing selection and changes nothing."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    ;; No guide at all.
    (setq shape-artist--locked-shape (test-shape-artist-guides--first-rect))
    (should (equal (test-shape-artist-guides--capturing-messages
                     (shape-artist-guide-adopt-selection-nearest))
                   '("No guide to adopt into")))
    ;; A guide, but nothing selected or locked.
    (let ((guide (shape-artist-guides-add 'vertical 0 nil 'manual)))
      (setq shape-artist--locked-shape nil)
      (should (equal (test-shape-artist-guides--capturing-messages
                       (shape-artist-guide-adopt-selection-nearest))
                     '("No selected or locked shapes to adopt")))
      (should-not (shape-artist-guide-adopted-shape-signatures guide)))))

(ert-deftest test-shape-artist-guides-select-guide-by-id-or-prompt ()
  "Guide selection honors an id, prompts otherwise, and never prompts when empty."
  (test-artist-with-guides-buffer
      "abc"
    (let ((prompted 0))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (setq prompted (1+ prompted)) "unreachable")))
        ;; No guides: nothing to choose from, so no prompt.
        (should-not (shape-artist-guides--select-guide nil "Pick: "))
        (should (= prompted 0)))
      (let ((a (shape-artist-guides-add 'vertical 1 "left" 'manual))
            (b (shape-artist-guides-add 'horizontal 2 "top" 'manual)))
        (should (eq (shape-artist-guides--select-guide
                     (shape-artist-guide-id a) "Pick: ")
                    a))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt choices &rest _)
                     ;; Choices are labelled "ID ORIENTATION POSITION LABEL".
                     (should (member (format "%d horizontal 2 top"
                                             (shape-artist-guide-id b))
                                     (mapcar #'car choices)))
                     (format "%d horizontal 2 top" (shape-artist-guide-id b)))))
          (should (eq (shape-artist-guides--select-guide nil "Pick: ") b)))))))

(ert-deftest test-shape-artist-guides-set-snap-distance-prompts-and-validates ()
  "Snap distance can be set, cleared back to default, and rejects negatives."
  (test-artist-with-guides-buffer
      "abc"
    (let ((guide (shape-artist-guides-add 'vertical 2 nil 'manual))
          (answer "4"))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) answer)))
        (shape-artist-guide-set-snap-distance (shape-artist-guide-id guide))
        (should (= (shape-artist-guide-snap-distance guide) 4))
        ;; An empty answer clears the per-guide override.
        (setq answer "")
        (shape-artist-guide-set-snap-distance (shape-artist-guide-id guide))
        (should-not (shape-artist-guide-snap-distance guide)))
      ;; Negative distances are rejected before anything changes.
      (should-error (shape-artist-guide-set-snap-distance
                     (shape-artist-guide-id guide) -1)
                    :type 'user-error)
      (should-not (shape-artist-guide-snap-distance guide)))))

(ert-deftest test-shape-artist-guides-set-snap-distance-without-guides ()
  "Setting a snap distance with no guides reports that nothing is selected."
  (test-artist-with-guides-buffer
      "abc"
    (should (equal (test-shape-artist-guides--capturing-messages
                     (shape-artist-guide-set-snap-distance))
                   '("No guide selected")))))

(ert-deftest test-shape-artist-guides-edit-properties-applies-all-fields ()
  "Editing properties updates position, label, visibility, and snap distance."
  (test-artist-with-guides-buffer
      "abc"
    (let ((guide (shape-artist-guides-add 'vertical 2 "old" 'manual)))
      (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 5))
                ((symbol-function 'read-string)
                 (let ((answers (list "new" "3")))
                   (lambda (&rest _) (pop answers))))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (shape-artist-guide-edit-properties (shape-artist-guide-id guide)))
      (should (= (shape-artist-guide-position guide) 5))
      (should (equal (shape-artist-guide-label guide) "new"))
      (should-not (shape-artist-guide-visible guide))
      (should (= (shape-artist-guide-snap-distance guide) 3)))))

(ert-deftest test-shape-artist-guides-edit-properties-clears-and-clamps ()
  "Empty label/snap answers clear those fields and positions clamp at zero."
  (test-artist-with-guides-buffer
      "abc"
    (let ((guide (shape-artist-guides-add 'vertical 2 "old" 'manual)))
      (setf (shape-artist-guide-snap-distance guide) 9)
      (cl-letf (((symbol-function 'read-number) (lambda (&rest _) -4))
                ((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (shape-artist-guide-edit-properties (shape-artist-guide-id guide)))
      (should (= (shape-artist-guide-position guide) 0))
      (should-not (shape-artist-guide-label guide))
      (should (shape-artist-guide-visible guide))
      (should-not (shape-artist-guide-snap-distance guide)))))

(ert-deftest test-shape-artist-guides-edit-properties-rejects-negative-snap ()
  "A negative snap override aborts the edit without changing the guide."
  (test-artist-with-guides-buffer
      "abc"
    (let ((guide (shape-artist-guides-add 'vertical 2 "keep" 'manual)))
      (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 7))
                ((symbol-function 'read-string)
                 (let ((answers (list "changed" "-2")))
                   (lambda (&rest _) (pop answers))))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (should-error (shape-artist-guide-edit-properties
                       (shape-artist-guide-id guide))
                      :type 'user-error))
      (should (= (shape-artist-guide-position guide) 2))
      (should (equal (shape-artist-guide-label guide) "keep"))
      (should (shape-artist-guide-visible guide)))))

(ert-deftest test-shape-artist-guides-move-interactive-prompts-for-guide-and-position ()
  "Without arguments, moving a guide prompts for the guide and its position."
  (test-artist-with-guides-buffer
      "abc"
    (let ((guide (shape-artist-guides-add 'vertical 2 "main" 'manual)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _) (car (car choices))))
                ((symbol-function 'read-number) (lambda (&rest _) 6)))
        (shape-artist-guide-move))
      (should (= (shape-artist-guide-position guide) 6)))))

(ert-deftest test-shape-artist-guides-move-clamps-to-zero-and-handles-no-guide ()
  "Guides never move below zero, and moving with no guide reports it."
  (test-artist-with-guides-buffer
      "abc"
    (should (equal (test-shape-artist-guides--capturing-messages
                     (cl-letf (((symbol-function 'completing-read)
                                (lambda (&rest _) "")))
                       (shape-artist-guide-move)))
                   '("No guide selected")))
    (let ((guide (shape-artist-guides-add 'vertical 2 nil 'manual)))
      (shape-artist-guide-move (shape-artist-guide-id guide) -5)
      (should (= (shape-artist-guide-position guide) 0)))))

(ert-deftest test-shape-artist-guides-toggle-visibility-uses-nearest-guide ()
  "Toggling visibility without an id acts on the guide nearest to point."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (should (equal (test-shape-artist-guides--capturing-messages
                     (shape-artist-guide-toggle-nearest-visibility))
                   '("No guide selected")))
    (let ((near (shape-artist-guides-add 'vertical 0 nil 'manual))
          (far (shape-artist-guides-add 'vertical 4 nil 'manual)))
      (shape-artist--goto-row-col 1 0)
      (shape-artist-guide-toggle-nearest-visibility)
      (should-not (shape-artist-guide-visible near))
      (should (shape-artist-guide-visible far))
      (shape-artist-guide-toggle-nearest-visibility)
      (should (shape-artist-guide-visible near)))))

;;; --- Guide Creation, Toggle, and Grid Command Tests ---

(defun test-shape-artist-guides--summary ()
  "Return (ORIENTATION POSITION LABEL) for every guide, in creation order."
  (mapcar (lambda (guide)
            (list (shape-artist-guide-orientation guide)
                  (shape-artist-guide-position guide)
                  (shape-artist-guide-label guide)))
          (sort (copy-sequence shape-artist-guides--guides)
                (lambda (a b) (< (shape-artist-guide-id a) (shape-artist-guide-id b))))))

(ert-deftest test-shape-artist-guides-create-at-point-or-explicit-position ()
  "Guides are created at point by default, or at an explicit position and label."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (shape-artist--goto-row-col 2 3)
    (should (equal (test-shape-artist-guides--capturing-messages
                     (shape-artist-guide-create-horizontal))
                   '("Horizontal guide at row 2")))
    (should (equal (test-shape-artist-guides--capturing-messages
                     (shape-artist-guide-create-vertical))
                   '("Vertical guide at column 3")))
    (shape-artist-guide-create-horizontal 0 "top")
    (shape-artist-guide-create-vertical 4 "right")
    (should (equal (test-shape-artist-guides--summary)
                   '((horizontal 2 nil) (vertical 3 nil)
                     (horizontal 0 "top") (vertical 4 "right"))))))

(ert-deftest test-shape-artist-guides-create-interactive-prefix-prompts-for-position ()
  "With a prefix argument the guide position is read from the minibuffer."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (let ((current-prefix-arg '(4)))
      (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 1)))
        (call-interactively #'shape-artist-guide-create-horizontal)
        (call-interactively #'shape-artist-guide-create-vertical)))
    (should (equal (test-shape-artist-guides--summary)
                   '((horizontal 1 nil) (vertical 1 nil))))))

(ert-deftest test-shape-artist-guides-global-toggles-flip-and-undo ()
  "Visibility, guide snapping, and grid snapping toggle and can be undone."
  (dolist (spec '((shape-artist-guide-toggle-visibility . shape-artist-guides-visible)
                  (shape-artist-snap-toggle . shape-artist-guides-snap-enabled)
                  (shape-artist-grid-snap-toggle . shape-artist-grid-snap-enabled)))
    (ert-info ((format "%s" (car spec)))
      (test-artist-with-guides-buffer "abc"
        (let ((before (and (symbol-value (cdr spec)) t)))
          (funcall (car spec))
          (should (eq (and (symbol-value (cdr spec)) t) (not before)))
          (shape-artist-guides-undo)
          (should (eq (and (symbol-value (cdr spec)) t) before)))))))

(ert-deftest test-shape-artist-guides-toggle-messages-report-new-state ()
  "Each toggle reports the state it switched to."
  (test-artist-with-guides-buffer "abc"
    (should (equal (test-shape-artist-guides--capturing-messages
                     (shape-artist-snap-toggle) (shape-artist-snap-toggle))
                   '("Guide snapping disabled" "Guide snapping enabled")))
    (should (equal (test-shape-artist-guides--capturing-messages
                     (shape-artist-grid-snap-toggle))
                   '("Grid snapping enabled")))
    (should (equal (test-shape-artist-guides--capturing-messages
                     (shape-artist-guide-toggle-visibility))
                   '("Guide overlays hidden")))))

(ert-deftest test-shape-artist-guides-grid-set-size-validates ()
  "The grid size can be set, prompted for, and must be at least one."
  (test-artist-with-guides-buffer "abc"
    (shape-artist-grid-set-size 8)
    (should (= shape-artist-grid-size 8))
    (should-error (shape-artist-grid-set-size 0) :type 'user-error)
    (should (= shape-artist-grid-size 8))
    (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 5)))
      (call-interactively #'shape-artist-grid-set-size))
    (should (= shape-artist-grid-size 5))
    (shape-artist-guides-undo)
    (should (= shape-artist-grid-size 8))))

(ert-deftest test-shape-artist-guides-clear-adoptions-one-or-all ()
  "Adoptions can be cleared for one guide or all guides, and undone."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (let* ((sig (shape-artist-shapes-signature (test-shape-artist-guides--first-rect)))
           (a (shape-artist-guides-add 'vertical 0 nil 'manual))
           (b (shape-artist-guides-add 'vertical 4 nil 'manual)))
      (shape-artist-guides--adopt-signatures a (list sig))
      (shape-artist-guides--adopt-signatures b (list sig))
      (shape-artist-guide-clear-adoptions (shape-artist-guide-id a))
      (should-not (shape-artist-guide-adopted-shape-signatures a))
      (should (shape-artist-guide-adopted-shape-signatures b))
      (shape-artist-guide-clear-adoptions)
      (should-not (shape-artist-guide-adopted-shape-signatures b))
      (shape-artist-guides-undo)
      (should (shape-artist-guide-adopted-shape-signatures
               (shape-artist-guides--guide-at-id (shape-artist-guide-id b)))))))

(ert-deftest test-shape-artist-guides-set-label-explicit-prompted-and-cleared ()
  "Guide labels can be given, prompted for, and cleared with an empty answer."
  (test-artist-with-guides-buffer "abc"
    (let* ((guide (shape-artist-guides-add 'vertical 2 nil 'manual))
           (id (shape-artist-guide-id guide)))
      (shape-artist-guide-set-label id "main")
      (should (equal (shape-artist-guide-label guide) "main"))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "renamed")))
        (shape-artist-guide-set-label id))
      (should (equal (shape-artist-guide-label guide) "renamed"))
      (shape-artist-guide-set-label id "")
      (should-not (shape-artist-guide-label guide)))))

(ert-deftest test-shape-artist-guides-nearest-visibility-compares-both-orientations ()
  "The nearest guide is chosen across orientations, preferring a tie to the row guide."
  (test-artist-with-guides-buffer
      "+---+\n|   |\n+---+"
    (let ((row (shape-artist-guides-add 'horizontal 2 nil 'manual))
          (col (shape-artist-guides-add 'vertical 0 nil 'manual)))
      ;; Row 0, column 1: the column guide is 1 away, the row guide 2.
      (shape-artist--goto-row-col 0 1)
      (shape-artist-guide-toggle-nearest-visibility)
      (should-not (shape-artist-guide-visible col))
      (should (shape-artist-guide-visible row))
      ;; Row 1, column 1: both guides are 1 away, so the row guide wins.
      (shape-artist--goto-row-col 1 1)
      (shape-artist-guide-toggle-nearest-visibility)
      (should-not (shape-artist-guide-visible row)))))

(ert-deftest test-shape-artist-guides-adopt-uses-multi-selection ()
  "A multi-selection is adopted as a whole, ahead of any locked shape."
  (test-artist-with-guides-buffer
      "+--+  +--+\n|  |  |  |\n+--+  +--+"
    (let ((guide (shape-artist-guides-add 'horizontal 0 nil 'manual)))
      (setq shape-artist--selected-shape-signatures
            (mapcar #'shape-artist-shapes-signature (test-shape-artist-guides--rects)))
      (should (= (length (shape-artist-guides--current-selection)) 2))
      (shape-artist--goto-row-col 0 0)
      (should (equal (test-shape-artist-guides--capturing-messages
                       (shape-artist-guide-adopt-selection-nearest))
                     (list (format "Guide %d adopted 2 shape(s)"
                                   (shape-artist-guide-id guide))))))))

;;; --- Resize and Grid Snapping Tests ---

(defconst test-shape-artist-guides--snap-box
  "      \n      \n  +--+\n  |  |\n  +--+\n      \n      \n"
  "A 3x2 rectangle at (2,2) with room around it.")

(ert-deftest test-shape-artist-guides-resize-snaps-left-top-and-bottom-edges ()
  "Resizing from the left, top, or bottom snaps the moving edge to a guide."
  (test-artist-with-guides-buffer test-shape-artist-guides--snap-box
    (let ((rect (test-shape-artist-guides--first-rect)))
      ;; Growing left by 2 would put the edge on column 0; the guide at column 1
      ;; pulls it there, so the left edge grows by only 1.
      (shape-artist-guides-add 'vertical 1 nil 'manual)
      (should (equal (seq-take (shape-artist-guides-snap-rectangle-resize
                                rect 'left 2 'bottom 0)
                               2)
                     '(1 0)))
      ;; Growing up by 2 would put the edge on row 0; the guide at row 1 pulls it
      ;; to row 1, so the top edge grows by only 1.
      (shape-artist-guides-add 'horizontal 1 nil 'manual)
      (should (equal (seq-take (shape-artist-guides-snap-rectangle-resize
                                rect 'right 0 'top 2)
                               2)
                     '(0 1)))
      ;; Growing down by 1 would put the edge on row 5; the guide at row 6 pulls
      ;; it there, so the bottom edge grows by 2.
      (shape-artist-guides-add 'horizontal 6 nil 'manual)
      (should (equal (seq-take (shape-artist-guides-snap-rectangle-resize
                                rect 'right 0 'bottom 1)
                               2)
                     '(0 2))))))

(ert-deftest test-shape-artist-guides-resize-snapping-respects-suppression-and-type ()
  "Resize snapping is skipped when suppressed, disabled, or for non-rectangles."
  (test-artist-with-guides-buffer test-shape-artist-guides--snap-box
    (let ((rect (test-shape-artist-guides--first-rect)))
      (shape-artist-guides-add 'vertical 1 nil 'manual)
      ;; Unsuppressed, this resize would snap to 1 (see the test above).
      (should (equal (seq-take (shape-artist-guides-snap-rectangle-resize
                                rect 'left 2 'bottom 0)
                               2)
                     '(1 0)))
      (let ((shape-artist-guides--suppress-snapping t))
        (should (equal (shape-artist-guides-snap-rectangle-resize
                        rect 'left 2 'bottom 0)
                       '(2 0 nil))))
      (let ((shape-artist-guides-snap-enabled nil))
        (should (equal (seq-take (shape-artist-guides-snap-rectangle-resize
                                  rect 'left 2 'bottom 0)
                                 2)
                       '(2 0))))
      (should (equal (shape-artist-guides-snap-rectangle-resize nil 'left 1 'bottom 0)
                     '(1 0 nil))))))

(ert-deftest test-shape-artist-guides-grid-snaps-resize-and-deltas ()
  "With grid snapping on, resizes and deltas land on grid lines."
  (test-artist-with-guides-buffer test-shape-artist-guides--snap-box
    (let ((rect (test-shape-artist-guides--first-rect)))
      (setq shape-artist-grid-size 4)
      ;; Inactive grid leaves values untouched.
      (setq shape-artist-grid-snap-enabled nil)
      (should (= (shape-artist-guides--grid-snap-delta 1 2) 2))
      (should (equal (seq-take (shape-artist-guides-snap-rectangle-resize
                                rect 'right 2 'bottom 0)
                               2)
                     '(2 0)))
      ;; Active grid: right edge 2+3+2=7 snaps to column 8 (a delta of 3).
      (setq shape-artist-grid-snap-enabled t)
      (should (= (shape-artist-guides--grid-snap-delta 1 2) 3))
      (should (equal (seq-take (shape-artist-guides-snap-rectangle-resize
                                rect 'right 2 'bottom 0)
                               2)
                     '(3 0))))))

;;; --- Persistence and Reporting Tests ---

(ert-deftest test-shape-artist-guides-delete-saved-removes-sidecar-entry ()
  "Deleting saved guides makes a later load find nothing."
  (let* ((dir (make-temp-file "shape-artist-guides" t))
         (default-directory dir)
         (file (expand-file-name "diagram.txt" dir))
         (shape-artist-guides-persistence-file ".shape-artist/guides.el"))
    (write-region "+---+\n|   |\n+---+" nil file nil 'silent)
    (let ((buffer (find-file-noselect file)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((shape-artist--update-sidebar-fn nil)
                  (shape-artist-guides--guides nil)
                  (shape-artist-guides--next-id 1))
              (shape-artist-guides-add 'vertical 2 "saved" 'manual)
              (shape-artist-guides-save)
              (shape-artist-guide-clear-all)
              (shape-artist-guides-delete-saved)
              (should (string-prefix-p
                       "No saved Shape Artist guides"
                       (car (test-shape-artist-guides--capturing-messages
                              (shape-artist-guides-load)))))
              (should-not shape-artist-guides--guides)))
        (kill-buffer buffer)
        (delete-directory dir t)))))

(ert-deftest test-shape-artist-guides-snap-report-line-and-section ()
  "The last snap is summarized in one line and surfaced in the sidebar section."
  (test-artist-with-guides-buffer "abc"
    (should-not (shape-artist-guides--snap-report-line))
    (setq shape-artist-guides--last-snap-report
          '(:source group-move :requested (1 2) :snapped (3 4)))
    (should (equal (shape-artist-guides--snap-report-line)
                   "Last snap: group-move (1,2) -> (3,4)\n"))
    (should (string-match-p "Last snap: group-move"
                            (shape-artist-guides-section-text)))))

(provide 'test-shape-artist-guides)
;;; test-shape-artist-guides.el ends here
