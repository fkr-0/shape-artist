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

(provide 'test-shape-artist-guides)
;;; test-shape-artist-guides.el ends here
