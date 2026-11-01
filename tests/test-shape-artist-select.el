;;; test-shape-artist-select.el --- ERT tests for SELECT mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-core)
(require 'shape-artist-select)

;;; --- Helper ---

(defmacro test-artist-with-rect-buffer (content &rest body)
  "Create a buffer with CONTENT, parse shapes, and run BODY."
  (declare (indent 1))
  `(test-artist-with-buffer
       ,content
     (let ((shape-artist--update-sidebar-fn nil))
       (shape-artist-shapes-parse)
       (setq shape-artist--mode 'move)
       ,@body)))

;;; --- SELECT Mode Entry Tests ---

(ert-deftest test-shape-artist-select-start-enters-mode ()
  "Test that select-start enters select mode."
  (test-artist-with-rect-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-select-start)
    (should (eq shape-artist--mode 'select))))

(ert-deftest test-shape-artist-select-start-index-zero ()
  "Test that select-start sets index to 0."
  (test-artist-with-rect-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-select-start)
    (should (= shape-artist--select-index 0))))

(ert-deftest test-shape-artist-select-start-saves-position ()
  "Test that select-start saves the pre-selection cursor position."
  (test-artist-with-rect-buffer
      "+---+\n|   |\n+---+"
    (goto-char (point-min))
    (shape-artist--goto-row-col 1 2)
    (shape-artist-select-start)
    (should shape-artist--pre-select-pos)
    (should (= (car shape-artist--pre-select-pos) 1))
    (should (= (cdr shape-artist--pre-select-pos) 2))))

(ert-deftest test-shape-artist-select-start-no-shapes ()
  "Test that select-start does nothing when no shapes exist."
  (test-artist-with-buffer
      "hello"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (setq shape-artist--mode 'move)
      (shape-artist-select-start)
      (should (eq shape-artist--mode 'move)))))

;;; --- SELECT Mode Cancel Tests ---

(ert-deftest test-shape-artist-select-cancel-returns-to-move ()
  "Test that cancel returns to move mode."
  (test-artist-with-rect-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-select-start)
    (should (eq shape-artist--mode 'select))
    (shape-artist-select-cancel)
    (should (eq shape-artist--mode 'move))))

(ert-deftest test-shape-artist-select-cancel-restores-position ()
  "Test that cancel restores the pre-selection cursor position."
  (test-artist-with-rect-buffer
      "+---+\n|   |\n+---+"
    (shape-artist--goto-row-col 0 0)
    (shape-artist-select-start)
    (shape-artist-select-cancel)
    (let ((pos (shape-artist--current-row-col)))
      (should (= (car pos) 0))
      (should (= (cdr pos) 0)))))

(ert-deftest test-shape-artist-select-cancel-clears-overlays ()
  "Test that cancel clears select overlays."
  (test-artist-with-rect-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-select-start)
    (should shape-artist--select-overlays)
    (shape-artist-select-cancel)
    (should-not shape-artist--select-overlays)))

;;; --- SELECT Mode Confirm Tests ---

(ert-deftest test-shape-artist-select-confirm-enters-shape-edit ()
  "Test that confirm enters shape-edit mode."
  (test-artist-with-rect-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-select-start)
    (shape-artist-select-confirm)
    (should (eq shape-artist--mode 'shape-edit))))

(ert-deftest test-shape-artist-select-confirm-locks-shape ()
  "Test that confirm sets the locked shape."
  (test-artist-with-rect-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-select-start)
    (shape-artist-select-confirm)
    (should shape-artist--locked-shape)
    (should (shape-artist-diagram-rectangle-p shape-artist--locked-shape))))

(ert-deftest test-shape-artist-select-confirm-clears-overlays ()
  "Test that confirm clears select overlays."
  (test-artist-with-rect-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-select-start)
    (shape-artist-select-confirm)
    (should-not shape-artist--select-overlays)))

;;; --- Navigation Tests ---

(ert-deftest test-shape-artist-select-next-advances-index ()
  "Test that next advances the select index."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+\n\n+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (shape-artist-select-start)
      (let ((old-index shape-artist--select-index))
        (shape-artist-select-next)
        (should (= shape-artist--select-index (1+ old-index)))))))

(ert-deftest test-shape-artist-select-next-wraps ()
  "Test that next wraps around at the end of the list."
  (test-artist-with-rect-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-select-start)
    (shape-artist-select-next)
    (should (= shape-artist--select-index 0))))

(ert-deftest test-shape-artist-select-prev-goes-back ()
  "Test that prev decrements the select index."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+\n\n+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (shape-artist-select-start)
      (shape-artist-select-next)
      (shape-artist-select-prev)
      (should (= shape-artist--select-index 0)))))

(ert-deftest test-shape-artist-select-prev-wraps ()
  "Test that prev wraps around at the beginning of the list."
  (test-artist-with-rect-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-select-start)
    (shape-artist-select-prev)
    (should (= shape-artist--select-index
               (1- (length shape-artist--last-shapes-list))))))

(ert-deftest test-shape-artist-select-next-shows-overlays ()
  "Test that next creates overlays for the current shape."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+\n\n+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (shape-artist-select-start)
      (shape-artist-select-next)
      (should shape-artist--select-overlays))))

(ert-deftest test-shape-artist-select-current-overlay-uses-mode-face ()
  "The active SELECT candidate uses the dedicated high-salience face."
  (test-artist-with-rect-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-select-start)
    (should shape-artist--select-overlays)
    (should (eq (overlay-get (car shape-artist--select-overlays) 'face)
                'shape-artist-select-current-face))))

;;; --- shapes-current Integration Tests ---

(ert-deftest test-shape-artist-select-current-shape-match ()
  "Test that shapes-current returns the correct shape at current index."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+\n\n+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (shape-artist-select-start)
      (let ((first-shape (shape-artist-shapes-current)))
        (should first-shape)
        (should (= (oref first-shape x) 0))
        (should (= (oref first-shape y) 0))
        (shape-artist-select-next)
        (let ((second-shape (shape-artist-shapes-current)))
          (should second-shape)
          (should-not (eq first-shape second-shape)))))))

;;; --- Confirm with Index ---

(ert-deftest test-shape-artist-select-confirm-second-shape ()
  "Test that confirm locks the shape at the current index."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+\n\n+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (shape-artist-select-start)
      (shape-artist-select-next)
      (shape-artist-select-confirm)
      (should (= (oref shape-artist--locked-shape y) 4)))))

;;; --- Multi-Selection Tests ---

(ert-deftest test-shape-artist-select-toggle-current-adds-and-removes ()
  "Toggling the current candidate adds it to, then removes it from, the selection."
  (test-artist-with-rect-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-select-start)
    (let ((sig (shape-artist-shapes-signature (shape-artist-shapes-current))))
      (shape-artist-select-toggle-current)
      (should (equal shape-artist--selected-shape-signatures (list sig)))
      (should shape-artist--selected-overlays)
      (shape-artist-select-toggle-current)
      (should-not shape-artist--selected-shape-signatures)
      (should-not shape-artist--selected-overlays))))

(ert-deftest test-shape-artist-select-all-skips-labels-and-clear-empties ()
  "Select-all picks every non-label shape; clear-selected empties the set."
  (test-artist-with-rect-buffer
      "+----+  +--+\n| ab |  |  |\n+----+  +--+"
    (shape-artist-select-start)
    (shape-artist-select-all)
    (let ((expected (mapcar #'shape-artist-shapes-signature
                            (cl-remove-if #'shape-artist-diagram-label-p
                                          shape-artist--last-shapes-list))))
      (should (= (length expected) 2))
      (should (equal shape-artist--selected-shape-signatures expected)))
    (should-not (cl-find-if
                 (lambda (sig) (eq (car sig) 'label))
                 shape-artist--selected-shape-signatures))
    ;; Each selected shape is highlighted by overlays covering its rows.
    (should shape-artist--selected-overlays)
    (shape-artist-select-clear-selected)
    (should-not shape-artist--selected-shape-signatures)
    (should-not shape-artist--selected-overlays)
    ;; Clearing the selection keeps SELECT mode active.
    (should (eq shape-artist--mode 'select))))

(ert-deftest test-shape-artist-select-confirm-locks-first-target-without-candidate ()
  "With a multi-selection but no current candidate, confirm locks the first target."
  (test-artist-with-rect-buffer
      "+--+  +--+\n|  |  |  |\n+--+  +--+"
    (shape-artist-select-start)
    (shape-artist-select-all)
    (let ((first (shape-artist-shapes-signature
                  (car (shape-artist-shapes-selected)))))
      (setq shape-artist--select-index -1)
      (shape-artist-select-confirm)
      (should (eq shape-artist--mode 'shape-edit))
      (should (equal (shape-artist-shapes-signature shape-artist--locked-shape)
                     first))
      (should (= (length shape-artist--selected-shape-signatures) 2)))))

(ert-deftest test-shape-artist-select-confirm-with-nothing-to-lock-is-a-noop ()
  "Confirming with no candidate and no selection leaves SELECT mode untouched."
  (test-artist-with-rect-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-select-start)
    (setq shape-artist--select-index -1)
    (shape-artist-select-confirm)
    (should (eq shape-artist--mode 'select))
    (should-not shape-artist--locked-shape)))

;;; test-shape-artist-select.el ends here
