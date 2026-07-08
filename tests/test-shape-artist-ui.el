;;; test-shape-artist-ui.el --- ERT tests for UI sidebar -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-core)
(require 'shape-artist-ui)

;;; --- Helper ---

(defmacro test-artist-with-ui-buffer (content &rest body)
  "Create a temp buffer with CONTENT, disable sidebar, run BODY."
  (declare (indent 1))
  `(test-artist-with-buffer
       ,content
     (let ((shape-artist--update-sidebar-fn nil))
       ,@body)))

;;; --- State Section Tests ---

(ert-deftest test-shape-artist-ui-state-section-move ()
  "Test state section shows MOVE."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--mode 'move)
    (let ((text (shape-artist--ui-state-section)))
      (should (string-match-p "MOVE" text)))))

(ert-deftest test-shape-artist-ui-state-section-draw ()
  "Test state section shows DRAW."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--mode 'draw)
    (let ((text (shape-artist--ui-state-section)))
      (should (string-match-p "DRAW" text)))))

(ert-deftest test-shape-artist-ui-state-section-select ()
  "Test state section shows SELECT."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--mode 'select)
    (let ((text (shape-artist--ui-state-section)))
      (should (string-match-p "SELECT" text)))))

(ert-deftest test-shape-artist-ui-state-section-shape-edit ()
  "Test state section shows SHAPE-EDIT."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--mode 'shape-edit)
    (let ((text (shape-artist--ui-state-section)))
      (should (string-match-p "SHAPE-EDIT" text)))))

(ert-deftest test-shape-artist-ui-state-section-unknown ()
  "Test state section shows UNKNOWN for unrecognized mode."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--mode 'bogus)
    (let ((text (shape-artist--ui-state-section)))
      (should (string-match-p "UNKNOWN" text)))))

;;; --- Cursor Section Tests ---

(ert-deftest test-shape-artist-ui-cursor-section ()
  "Test cursor section shows correct position."
  (test-artist-with-ui-buffer
      "abc\ndef"
    (shape-artist--goto-row-col 1 2)
    (let ((text (shape-artist--ui-cursor-section)))
      (should (string-match-p "Cursor: (1 . 2)" text)))))

(ert-deftest test-shape-artist-ui-cursor-section-origin ()
  "Test cursor section at origin."
  (test-artist-with-ui-buffer
      "abc"
    (goto-char (point-min))
    (let ((text (shape-artist--ui-cursor-section)))
      (should (string-match-p "Cursor: (0 . 0)" text)))))

;;; --- Anchor Section Tests ---

(ert-deftest test-shape-artist-ui-anchor-section-none ()
  "Test anchor section shows none when no anchor."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--anchor nil)
    (let ((text (shape-artist--ui-anchor-section)))
      (should (string-match-p "none" text)))))

(ert-deftest test-shape-artist-ui-anchor-section-set ()
  "Test anchor section shows coordinates when anchor is set."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--anchor '(2 . 3))
    (let ((text (shape-artist--ui-anchor-section)))
      (should (string-match-p "(2, 3)" text)))))

;;; --- Shapes Section Tests ---

(ert-deftest test-shape-artist-ui-shapes-section-none ()
  "Test shapes section shows none when no shapes detected."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--last-shapes-list nil)
    (let ((text (shape-artist--ui-shapes-section)))
      (should (string-match-p "none detected" text)))))

(ert-deftest test-shape-artist-ui-shapes-section-with-shapes ()
  "Test shapes section shows a shape tree."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((text (shape-artist--ui-shapes-section)))
        (should (string-match-p "=== Shape Tree ===" text))
        (should (string-match-p "root" text))
        (should (string-match-p "rect" text))))))

(ert-deftest test-shape-artist-ui-shapes-section-marker ()
  "Test tree section shows current marker for selected shape in select mode."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (setq shape-artist--mode 'select)
      (setq shape-artist--select-index 0)
      (let ((text (shape-artist--ui-shapes-section)))
        (should (string-match-p ">   rect" text))))))

(ert-deftest test-shape-artist-ui-shapes-section-locked ()
  "Test tree section shows locked marker for locked shape."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (setq shape-artist--mode 'shape-edit)
      (setq shape-artist--locked-shape (nth 0 shape-artist--last-shapes-list))
      (let ((text (shape-artist--ui-shapes-section)))
        (should (string-match-p " L  rect" text))))))

(ert-deftest test-shape-artist-ui-shapes-section-selected-marker ()
  "Test tree section shows selected marker for multi-selected shapes."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (setq shape-artist--selected-shape-signatures
            (list (shape-artist-shapes-signature
                   (nth 0 shape-artist--last-shapes-list))))
      (let ((text (shape-artist--ui-shapes-section)))
        (should (string-match-p "  \\* rect" text))))))

;;; --- Layout Section Tests ---

(ert-deftest test-shape-artist-ui-diagram-bounds-none ()
  "Diagram bounds are nil when no shapes are parsed."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--last-shapes-list nil)
    (should-not (shape-artist--ui-diagram-bounds))))

(ert-deftest test-shape-artist-ui-diagram-bounds-rectangle ()
  "Diagram bounds cover parsed rectangles."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (should (equal (shape-artist--ui-diagram-bounds)
                     '(0 0 4 2 1))))))

(ert-deftest test-shape-artist-ui-layout-section-with-shapes ()
  "Layout section shows bounds, size, and shape count."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((text (shape-artist--ui-layout-section)))
        (should (string-match-p "=== Layout ===" text))
        (should (string-match-p "Bounds: (0,0)-(4,2)" text))
        (should (string-match-p "Size: 4x2" text))
        (should (string-match-p "Shapes: 1" text))))))

(ert-deftest test-shape-artist-ui-center-diagram-moves-point ()
  "Centering a parsed diagram moves point to its geometric center."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (goto-char (point-min))
      (shape-artist-ui-center-diagram)
      (should (equal (shape-artist--current-row-col) '(1 . 2))))))

;;; --- Keys Section Tests ---

(ert-deftest test-shape-artist-ui-keys-section-move ()
  "Test keys section for move mode."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--mode 'move)
    (let ((text (shape-artist--ui-keys-section)))
      (should (string-match-p "h/j/k/l  Move" text))
      (should (string-match-p "SPC" text))
      (should (string-match-p "Quick actions" text)))))

(ert-deftest test-shape-artist-ui-keys-section-draw ()
  "Test keys section for draw mode."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--mode 'draw)
    (let ((text (shape-artist--ui-keys-section)))
      (should (string-match-p "h/j/k/l  Draw" text))
      (should (string-match-p "Set anchor" text)))))

(ert-deftest test-shape-artist-ui-keys-section-select ()
  "Test keys section for select mode."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--mode 'select)
    (let ((text (shape-artist--ui-keys-section)))
      (should (string-match-p "n/p      Next/prev shape" text))
      (should (string-match-p "RET      Lock/edit" text)))))

(ert-deftest test-shape-artist-ui-keys-section-shape-edit ()
  "Test keys section for shape-edit mode."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--mode 'shape-edit)
    (let ((text (shape-artist--ui-keys-section)))
      (should (string-match-p "H/J/K/L  Move shape/group" text))
      (should (string-match-p "Navigate interior" text)))))

(ert-deftest test-shape-artist-ui-quick-actions-move-mode ()
  "Quick actions include global and mode-specific commands."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--mode 'move)
    (let ((labels (mapcar #'shape-artist--ui-quick-action-label
                          (shape-artist--ui-quick-actions))))
      (should (member "o        Show outline tree" labels))
      (should (member "z        Center diagram" labels))
      (should (member "SPC      Start drawing from cursor" labels))
      (should-not (member "RET      Lock/edit selection" labels)))))

(ert-deftest test-shape-artist-ui-quick-actions-select-mode ()
  "Quick actions switch to SELECT commands in select mode."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--mode 'select)
    (let ((labels (mapcar #'shape-artist--ui-quick-action-label
                          (shape-artist--ui-quick-actions))))
      (should (member "RET      Lock/edit selection" labels))
      (should (member "t        Toggle current candidate" labels))
      (should-not (member "SPC      Start drawing from cursor" labels)))))

(ert-deftest test-shape-artist-ui-keys-section-empty ()
  "Test keys section for unknown mode returns just the header."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--mode 'bogus)
    (let ((text (shape-artist--ui-keys-section)))
      ;; Unknown mode returns format with empty pcase clause
      (should (string-match-p "=== Keys ===" text)))))

;;; --- Locked Shape Section Tests ---

(ert-deftest test-shape-artist-ui-locked-shape-section-edit-mode ()
  "Test locked shape section shows details in shape-edit mode."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (setq shape-artist--mode 'shape-edit)
      (setq shape-artist--locked-shape (nth 0 shape-artist--last-shapes-list))
      (let ((text (shape-artist--ui-locked-shape-section)))
        (should (string-match-p "=== Locked Shape ===" text))
        (should (string-match-p "rectangle" text))
        (should (string-match-p "Pos:" text))
        (should (string-match-p "Size:" text))))))

(ert-deftest test-shape-artist-ui-locked-shape-section-other-mode ()
  "Test locked shape section is empty outside shape-edit mode."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--mode 'move)
    (let ((text (shape-artist--ui-locked-shape-section)))
      (should (string= text "")))))

(ert-deftest test-shape-artist-ui-locked-shape-section-no-shape ()
  "Test locked shape section is empty when no shape is locked."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--mode 'shape-edit)
    (setq shape-artist--locked-shape nil)
    (let ((text (shape-artist--ui-locked-shape-section)))
      (should (string= text "")))))

;;; --- Labels Section Tests ---

(ert-deftest test-shape-artist-ui-labels-section-none ()
  "Test labels section shows none when no labels."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((text (shape-artist--ui-labels-section)))
        (should (string-match-p "none" text))))))

(ert-deftest test-shape-artist-ui-labels-section-with-label ()
  "Test labels section shows label text."
  (test-artist-with-buffer
      "+----+\n|foo |\n+----+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((text (shape-artist--ui-labels-section)))
        (should (string-match-p "foo" text))))))

;;; --- Non-blank Section Tests ---

(ert-deftest test-shape-artist-ui-nonblank-section ()
  "Test non-blank section counts correctly."
  (test-artist-with-ui-buffer
      "abc\nde"
    (let ((text (shape-artist--ui-nonblank-section)))
      (should (string-match-p "Non-blank chars: 5" text)))))

(ert-deftest test-shape-artist-ui-nonblank-section-empty ()
  "Test non-blank section with empty buffer."
  (test-artist-with-ui-buffer
      ""
    (let ((text (shape-artist--ui-nonblank-section)))
      (should (string-match-p "Non-blank chars: 0" text)))))

;;; --- Sidebar Buffer Creation Tests ---

(ert-deftest test-shape-artist-ui-get-or-create-sidebar-buffer ()
  "Test that get-or-create-sidebar-buffer returns a live buffer."
  (test-artist-with-ui-buffer
      "abc"
    (let ((buf (shape-artist--get-or-create-sidebar-buffer)))
      (should (buffer-live-p buf))
      (should (string-match-p "\\*artist-sidebar:" (buffer-name buf))))))

(ert-deftest test-shape-artist-ui-sidebar-is-special-mode ()
  "Test that sidebar buffer is in special-mode."
  (test-artist-with-ui-buffer
      "abc"
    (let ((buf (shape-artist--get-or-create-sidebar-buffer)))
      (with-current-buffer buf
        (should (eq major-mode 'special-mode))))))

;;; test-shape-artist-ui.el ends here
