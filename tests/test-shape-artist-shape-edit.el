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

;;; test-shape-artist-shape-edit.el ends here
