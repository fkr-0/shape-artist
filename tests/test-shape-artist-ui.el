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

(ert-deftest test-shape-artist-ui-layout-section-includes-graph-health ()
  "Sidebar layout reports graph nodes, connections, and components."
  (test-artist-with-buffer
      "+--+---+--+\n|  |   |  |\n+--+   +--+"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse)
      (let ((text (shape-artist--ui-layout-section)))
        (should (string-match-p "Graph:" text))
        (should (string-match-p "nodes" text))
        (should (string-match-p "components" text))))))

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
        (should (derived-mode-p 'special-mode))))))

;;; --- Shape Summary Tests ---

(ert-deftest test-shape-artist-ui-shape-summary-rectangle-shows-id ()
  "Rectangle summaries carry the stable metadata ID, position, and size."
  (test-artist-with-ui-buffer
      "+--+\n|  |\n+--+"
    (shape-artist-shapes-parse)
    (let* ((rect (cl-find-if #'shape-artist-diagram-rectangle-p
                             shape-artist--last-shapes-list))
           (id (shape-artist-metadata-shape-id rect)))
      (should (stringp id))
      (should (equal (shape-artist--ui-shape-summary rect)
                     (format "rect#%s (0,0) 3x2" id))))))

(ert-deftest test-shape-artist-ui-shape-summary-unidentified-shape-uses-placeholder ()
  "A shape without an assigned ID is summarized with a `?' placeholder."
  (let ((rect (make-instance 'shape-artist-diagram-rectangle
                             :x 1 :y 2 :width 3 :height 4)))
    (should-not (shape-artist-metadata-shape-id rect))
    (should (equal (shape-artist--ui-shape-summary rect)
                   "rect#? (1,2) 3x4"))))

(ert-deftest test-shape-artist-ui-shape-summary-edge ()
  "Edge summaries include ID, edge type, and both endpoints."
  (let ((edge (make-instance 'shape-artist-diagram-edge
                             :x 0 :y 1 :x2 5 :y2 1 :edge-type 'horizontal)))
    (should (equal (shape-artist--ui-shape-summary edge)
                   "edge#?[horizontal] (0,1)->(5,1)"))))

(ert-deftest test-shape-artist-ui-shape-summary-label ()
  "Label summaries include ID, quoted text, and position."
  (let ((label (make-instance 'shape-artist-diagram-label
                              :text "db" :x 3 :y 4)))
    (should (equal (shape-artist--ui-shape-summary label)
                   "label#? \"db\" (3,4)"))))

(ert-deftest test-shape-artist-ui-shape-summary-root-and-generic-node ()
  "The virtual root and bare nodes have their own summary forms."
  (should (equal (shape-artist--ui-shape-summary
                  (make-instance 'shape-artist-diagram-root))
                 "root"))
  (should (equal (shape-artist--ui-shape-summary
                  (make-instance 'shape-artist-diagram-node
                                 :x 1 :y 2 :width 3 :height 4))
                 "node (1,2) 3x4")))

;;; --- Sidebar Update Tests ---

(ert-deftest test-shape-artist-ui-update-sidebar-renders-all-sections ()
  "Updating the sidebar fills its buffer with every section."
  (test-artist-with-ui-buffer
      "+--+\n|  |\n+--+"
    (let ((sidebar (shape-artist--get-or-create-sidebar-buffer)))
      (unwind-protect
          (progn
            (setq shape-artist--mode 'move)
            (shape-artist-shapes-parse)
            (shape-artist--update-sidebar)
            (with-current-buffer sidebar
              (let ((text (substring-no-properties (buffer-string))))
                (dolist (heading '("=== State ===" "=== Layout ===" "=== Guides ==="
                                   "=== Shape Tree ===" "=== Labels ==="
                                   "=== Keys ==="))
                  (should (string-match-p (regexp-quote heading) text)))
                (should (string-match-p "rect#" text)))))
        (kill-buffer sidebar)))))

(ert-deftest test-shape-artist-ui-update-sidebar-replaces-stale-content ()
  "Each update erases the previous sidebar content."
  (test-artist-with-ui-buffer
      "+--+\n|  |\n+--+"
    (let ((sidebar (shape-artist--get-or-create-sidebar-buffer)))
      (unwind-protect
          (progn
            (with-current-buffer sidebar
              (let ((inhibit-read-only t))
                (insert "STALE-MARKER")))
            (shape-artist--update-sidebar)
            (with-current-buffer sidebar
              (should-not (string-match-p "STALE-MARKER" (buffer-string)))))
        (kill-buffer sidebar)))))

(ert-deftest test-shape-artist-ui-update-sidebar-without-sidebar-is-noop ()
  "With no sidebar buffer name or a killed buffer, updating does nothing."
  (test-artist-with-ui-buffer
      "abc"
    (setq shape-artist--sidebar-buffer-name nil)
    (should-not (shape-artist--update-sidebar))
    (setq shape-artist--sidebar-buffer-name "*artist-sidebar:does-not-exist*")
    (should-not (shape-artist--update-sidebar))
    (should-not (get-buffer "*artist-sidebar:does-not-exist*"))))

;;; --- Sidebar Toggle, Menu, and Outline Tests ---

(defmacro test-shape-artist-ui--with-sidebar-cleanup (&rest body)
  "Run BODY, then delete any sidebar window and buffers it created."
  (declare (indent 0))
  `(save-window-excursion
     (unwind-protect
         (progn ,@body)
       (dolist (buf (buffer-list))
         (when (string-prefix-p "*artist-sidebar:" (buffer-name buf))
           (kill-buffer buf))))))

(ert-deftest test-shape-artist-ui-toggle-shows-then-hides-sidebar ()
  "Toggling displays the sidebar when hidden and removes its window when shown.
Window creation differs between Emacs versions and a batch frame, so the two
window primitives are simulated on the selected window; this pins the toggle
logic, not Emacs's window management."
  (test-shape-artist-ui--with-sidebar-cleanup
    (let ((deleted nil)
          (original (window-buffer (selected-window))))
      (save-window-excursion
        (cl-letf (((symbol-function 'display-buffer-in-side-window)
                   (lambda (buffer _alist)
                     (set-window-buffer (selected-window) buffer)
                     (selected-window)))
                  ((symbol-function 'delete-window)
                   (lambda (&optional window)
                     (push window deleted)
                     (set-window-buffer (or window (selected-window)) original))))
          (test-artist-with-ui-buffer "+--+\n|  |\n+--+"
            (shape-artist-shapes-parse)
            (shape-artist-ui-toggle)
            (let ((sidebar (get-buffer shape-artist--sidebar-buffer-name)))
              (should (buffer-live-p sidebar))
              (should (get-buffer-window sidebar))
              (should (window-live-p shape-artist--sidebar-window))
              (should-not deleted)
              (shape-artist-ui-toggle)
              (should (= (length deleted) 1))
              (should-not (get-buffer-window sidebar))
              (should-not shape-artist--sidebar-window))))))))

(ert-deftest test-shape-artist-ui-show-fills-sidebar-content ()
  "Showing the sidebar immediately renders the current diagram state."
  (test-shape-artist-ui--with-sidebar-cleanup
    (test-artist-with-ui-buffer "+--+\n|  |\n+--+"
      (setq shape-artist--mode 'move)
      (shape-artist-shapes-parse)
      (shape-artist-ui-show)
      (with-current-buffer shape-artist--sidebar-buffer-name
        (should (string-match-p "=== Shape Tree ===" (buffer-string)))
        (should (string-match-p "rect#" (buffer-string)))))))

(ert-deftest test-shape-artist-ui-completing-read-menu-runs-chosen-action ()
  "The quick-action menu runs the command whose label was chosen."
  (test-artist-with-ui-buffer "abc"
    (setq shape-artist--mode 'move)
    (let ((ran nil)
          (offered nil))
      (cl-letf (((symbol-function 'shape-artist-ui-center-diagram)
                 (lambda () (interactive) (setq ran t)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt choices &rest _)
                   (setq offered (mapcar #'car choices))
                   (car (cl-find-if (lambda (choice)
                                      (string-match-p "Center diagram" (car choice)))
                                    choices)))))
        (shape-artist-ui-completing-read-menu))
      (should ran)
      (should (cl-find-if (lambda (label) (string-match-p "Start drawing" label)) offered)))))

(ert-deftest test-shape-artist-ui-quick-actions-are-mode-specific ()
  "Only actions valid in the active mode, plus the global ones, are offered."
  (test-artist-with-ui-buffer "abc"
    (let ((labels-for
           (lambda (mode)
             (setq shape-artist--mode mode)
             (mapcar (lambda (action) (nth 2 action)) (shape-artist--ui-quick-actions)))))
      (let ((move (funcall labels-for 'move))
            (select (funcall labels-for 'select))
            (edit (funcall labels-for 'shape-edit)))
        (dolist (labels (list move select edit))
          (should (member "Center diagram" labels)))
        (should (member "Start drawing from cursor" move))
        (should-not (member "Start drawing from cursor" select))
        (should (member "Select all candidates" select))
        (should-not (member "Select all candidates" edit))
        (should (member "Prompt resize" edit))))))

(ert-deftest test-shape-artist-ui-command-menu-delegates-to-completing-read ()
  "The public command menu is the completing-read menu."
  (let ((called 0))
    (cl-letf (((symbol-function 'shape-artist-ui-completing-read-menu)
               (lambda () (setq called (1+ called)))))
      (shape-artist-ui-command-menu))
    (should (= called 1))))

(ert-deftest test-shape-artist-ui-show-outline-builds-tree-buffer ()
  "Showing the outline renders the parsed shape tree in its own buffer."
  (save-window-excursion
    (test-artist-with-ui-buffer "+--+\n|  |\n+--+"
      (shape-artist-shapes-parse)
      (unwind-protect
          (progn
            (shape-artist-ui-show-outline)
            (let ((outline (get-buffer shape-artist-diagram--outline-buffer-name)))
              (should (buffer-live-p outline))
              (with-current-buffer outline
                (should (derived-mode-p 'shape-artist-diagram-outline-mode))
                (should (string-match-p "\\[rect (0,0) 3x2\\]" (buffer-string))))))
        (when (get-buffer shape-artist-diagram--outline-buffer-name)
          (kill-buffer shape-artist-diagram--outline-buffer-name))))))

(ert-deftest test-shape-artist-ui-locked-shape-section-names-each-shape-type ()
  "The locked-shape section reports the type of rectangles, edges, and labels."
  (dolist (case '(("+----+\n| ab |\n+----+" shape-artist-diagram-rectangle-p "Type: rectangle")
                  ("+----+\n| ab |\n+----+" shape-artist-diagram-label-p "Type: label")
                  ("---" shape-artist-diagram-edge-p "Type: edge")))
    (pcase-let ((`(,content ,predicate ,expected) case))
      (ert-info (expected)
        (test-artist-with-ui-buffer content
          (shape-artist-shapes-parse)
          (setq shape-artist--mode 'shape-edit)
          (setq shape-artist--locked-shape
                (cl-find-if predicate shape-artist--last-shapes-list))
          (should (string-match-p (regexp-quote expected)
                                  (shape-artist--ui-locked-shape-section))))))))

;;; --- Interactive Sidebar Tests ---

(defun test-shape-artist-ui--sidebar-line-for (sidebar needle)
  "Move point in SIDEBAR to the first line matching NEEDLE and return t."
  (with-current-buffer sidebar
    (goto-char (point-min))
    (when (search-forward needle nil t)
      (beginning-of-line)
      t)))

(ert-deftest test-shape-artist-ui-sidebar-rows-are-actionable ()
  "Shape rows carry their signature and hover affordance; the root does not."
  (test-shape-artist-ui--with-sidebar-cleanup
    (test-artist-with-ui-buffer "+--+\n|  |\n+--+"
      (let ((sidebar (shape-artist--get-or-create-sidebar-buffer)))
        (shape-artist-shapes-parse)
        (shape-artist--update-sidebar)
        (should (test-shape-artist-ui--sidebar-line-for sidebar "rect#"))
        (with-current-buffer sidebar
          (let ((sig (get-text-property (point) 'shape-artist-signature)))
            (should (equal sig (list 'rectangle 0 0 3 2)))
            (should (get-text-property (point) 'mouse-face))
            (should (stringp (get-text-property (point) 'help-echo)))))
        (should (test-shape-artist-ui--sidebar-line-for sidebar "root"))
        (with-current-buffer sidebar
          (should-not (get-text-property (point) 'shape-artist-signature)))))))

(ert-deftest test-shape-artist-ui-sidebar-mode-binds-jump-keys ()
  "The sidebar is a special-mode derivative with RET and click jumping."
  (test-shape-artist-ui--with-sidebar-cleanup
    (test-artist-with-ui-buffer "abc"
      (with-current-buffer (shape-artist--get-or-create-sidebar-buffer)
        (should (derived-mode-p 'special-mode))
        (should (eq (key-binding (kbd "RET")) #'shape-artist-ui-sidebar-goto))
        (should (eq (key-binding (kbd "<mouse-1>")) #'shape-artist-ui-sidebar-goto))))))

(ert-deftest test-shape-artist-ui-sidebar-goto-selects-the-shape-for-editing ()
  "RET on a sidebar row focuses that shape in the artist buffer in SHAPE-EDIT mode."
  (test-shape-artist-ui--with-sidebar-cleanup
    (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b))))
      (test-artist-with-ui-buffer "+--+  +---+\n|  |  |   |\n+--+  +---+"
        (let ((artist (current-buffer))
              (sidebar (shape-artist--get-or-create-sidebar-buffer)))
          (setq shape-artist--mode 'move)
          (shape-artist-shapes-parse)
          (shape-artist--update-sidebar)
          ;; Jump to the second (wider) rectangle.
          (with-current-buffer sidebar
            (goto-char (point-min))
            (search-forward "rect#" nil t 2)
            (beginning-of-line)
            (shape-artist-ui-sidebar-goto))
          (should (eq (current-buffer) artist))
          (should (eq shape-artist--mode 'shape-edit))
          (should (equal (shape-artist-shapes-signature shape-artist--locked-shape)
                         '(rectangle 6 0 4 2)))
          ;; Point lands inside the focused shape.
          (should (equal (shape-artist--current-row-col) '(1 . 8))))))))

(ert-deftest test-shape-artist-ui-sidebar-goto-off-a-shape-row-explains ()
  "Pressing RET where no shape is listed says what to do instead of failing."
  (test-shape-artist-ui--with-sidebar-cleanup
    (test-artist-with-ui-buffer "+--+\n|  |\n+--+"
      (let ((sidebar (shape-artist--get-or-create-sidebar-buffer)))
        (shape-artist-shapes-parse)
        (shape-artist--update-sidebar)
        (with-current-buffer sidebar
          (goto-char (point-min))
          (let ((text (error-message-string
                       (should-error (shape-artist-ui-sidebar-goto) :type 'user-error))))
            (should (string-match-p "shape row" text))))))))

(ert-deftest test-shape-artist-ui-sidebar-goto-reports-a-stale-row ()
  "A row for a shape that has since changed asks for a refresh."
  (test-shape-artist-ui--with-sidebar-cleanup
    (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b))))
      (test-artist-with-ui-buffer "+--+\n|  |\n+--+"
        (let ((sidebar (shape-artist--get-or-create-sidebar-buffer)))
          (shape-artist-shapes-parse)
          (shape-artist--update-sidebar)
          ;; The diagram changes without the sidebar being refreshed.
          (erase-buffer)
          (insert "    \n    \n    ")
          (shape-artist-shapes-parse)
          (with-current-buffer sidebar
            (test-shape-artist-ui--sidebar-line-for sidebar "rect#")
            (let ((text (error-message-string
                         (should-error (shape-artist-ui-sidebar-goto)
                                       :type 'user-error))))
              (should (string-match-p "no longer" text))
              (should (string-match-p "refresh" text)))))))))

;;; test-shape-artist-ui.el ends here
