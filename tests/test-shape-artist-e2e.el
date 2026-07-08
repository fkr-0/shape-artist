;;; test-shape-artist-e2e.el --- End-to-end modal workflow tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'shape-artist)

(defun test-shape-artist-e2e--press (key)
  "Invoke KEY through the active keymaps of the current buffer."
  (let ((command (key-binding (kbd key) t)))
    (should (commandp command))
    (call-interactively command)))

(defun test-shape-artist-e2e--first-rectangle ()
  "Return the first parsed rectangle in the current buffer."
  (cl-find-if #'shape-artist-diagram-rectangle-p
              shape-artist--last-shapes-list))

(defun test-shape-artist-e2e--rectangle-count ()
  "Return the number of parsed rectangles in the current buffer."
  (length (cl-remove-if-not #'shape-artist-diagram-rectangle-p
                            shape-artist--last-shapes-list)))

(defmacro test-shape-artist-e2e-with-artist-buffer (content &rest body)
  "Create an artist buffer containing CONTENT and run BODY."
  (declare (indent 1))
  `(test-artist-with-buffer
       ,content
     (let ((shape-artist--update-sidebar-fn nil))
       (shape-artist-core-mode 1)
       ,@body
       (shape-artist-core-mode -1))))

(ert-deftest test-shape-artist-e2e-key-driven-draw-select-edit-flow ()
  "Exercise MOVE -> DRAW -> SELECT -> SHAPE-EDIT via real key bindings."
  (test-shape-artist-e2e-with-artist-buffer
      "      \n      \n      \n      "
    (goto-char (point-min))
    ;; MOVE: space anchors and enters DRAW, then vim-like motions define a box.
    (test-shape-artist-e2e--press "SPC")
    (should (eq shape-artist--mode 'draw))
    (test-shape-artist-e2e--press "l")
    (test-shape-artist-e2e--press "l")
    (test-shape-artist-e2e--press "j")
    (test-shape-artist-e2e--press "j")
    (test-shape-artist-e2e--press "RET")
    (should (eq shape-artist--mode 'move))
    (should (= (shape-artist-shapes-count) 1))

    ;; SELECT and lock the detected rectangle.
    (test-shape-artist-e2e--press "C-n")
    (should (eq shape-artist--mode 'select))
    (test-shape-artist-e2e--press "RET")
    (should (eq shape-artist--mode 'shape-edit))
    (should shape-artist--locked-shape)

    ;; SHAPE-EDIT: uppercase hjkl moves the whole shape, not the cursor.
    (test-shape-artist-e2e--press "L")
    (test-shape-artist-e2e--press "J")
    ;; SHAPE-EDIT: > and + resize width/height one cell at a time.
    (test-shape-artist-e2e--press ">")
    (test-shape-artist-e2e--press "+")
    ;; SHAPE-EDIT: x cycles border style to heavy.
    (test-shape-artist-e2e--press "x")

    (let ((shape shape-artist--locked-shape))
      (should (shape-artist-diagram-rectangle-p shape))
      (should (= (oref shape x) 1))
      (should (= (oref shape y) 1))
      (should (= (oref shape width) 3))
      (should (= (oref shape height) 3))
      (should (eq (shape-artist-shapes-border-style shape) 'heavy)))
    (should (= (shape-artist-shapes-count) 1))))

(ert-deftest test-shape-artist-e2e-discoverability-keybindings ()
  "Quick help and outline helpers are discoverable from the active keymap."
  (test-shape-artist-e2e-with-artist-buffer
      "+---+\n|   |\n+---+"
    (should (eq (key-binding (kbd "?") t)
                #'shape-artist-ui-command-menu))
    (should (eq (key-binding (kbd "o") t)
                #'shape-artist-ui-show-outline))
    (should (eq (key-binding (kbd "z") t)
                #'shape-artist-ui-center-diagram))))

(ert-deftest test-shape-artist-e2e-select-edit-preserves-lock-across-repeated-ops ()
  "Repeated SHAPE-EDIT commands keep the lock on the current parsed shape."
  (test-shape-artist-e2e-with-artist-buffer
      "+---+\n| a |\n+---+"
    (test-shape-artist-e2e--press "C-n")
    (test-shape-artist-e2e--press "RET")
    (let ((initial shape-artist--locked-shape))
      (should initial)
      (test-shape-artist-e2e--press "L")
      (test-shape-artist-e2e--press "L")
      (test-shape-artist-e2e--press "J")
      (test-shape-artist-e2e--press ">")
      (test-shape-artist-e2e--press "<")
      (test-shape-artist-e2e--press "x")
      (test-shape-artist-e2e--press "x")
      (test-shape-artist-e2e--press "x")
      (let ((shape shape-artist--locked-shape))
        (should shape)
        (should (not (eq shape initial)))
        (should (= (oref shape x) 2))
        (should (= (oref shape y) 1))
        (should (= (oref shape width) 4))
        (should (= (oref shape height) 2))
        (should (eq (shape-artist-shapes-border-style shape) 'standard))
        (should (= (test-shape-artist-e2e--rectangle-count) 1))))))

(ert-deftest test-shape-artist-e2e-style-borders-remain-parseable ()
  "Heavy and light borders remain first-class parseable rectangles."
  (dolist (diagram '("+===+\n|   |\n+===+"
                     "+...+\n|   |\n+...+"))
    (test-shape-artist-e2e-with-artist-buffer
        diagram
      (shape-artist-shapes-parse)
      (let ((shape (test-shape-artist-e2e--first-rectangle)))
        (should shape)
        (should (= (oref shape width) 4))
        (should (= (oref shape height) 2))))))

(ert-deftest test-shape-artist-e2e-shape-edit-min-size-key-clamps ()
  "Key-driven shrinking clamps at the valid minimum rectangle size."
  (test-shape-artist-e2e-with-artist-buffer
      "+---+\n|   |\n+---+"
    (test-shape-artist-e2e--press "C-n")
    (test-shape-artist-e2e--press "RET")
    (dotimes (_ 10)
      (test-shape-artist-e2e--press "<")
      (test-shape-artist-e2e--press "_"))
    (should (= (oref shape-artist--locked-shape width) 2))
    (should (= (oref shape-artist--locked-shape height) 2))))

(ert-deftest test-shape-artist-e2e-evil-delayed-load-defines-states ()
  "Loading Evil after shape-artist still installs custom state support."
  (skip-unless (require 'evil nil t))
  (test-shape-artist-e2e-with-artist-buffer
      "+---+\n|   |\n+---+"
    (should (fboundp 'evil-artist-state))
    (should (fboundp 'evil-select-state))
    (should (fboundp 'evil-shape-edit-state))))

;;; test-shape-artist-e2e.el ends here


(ert-deftest test-shape-artist-e2e-arrow-edge-manipulation ()
  "Edges can be selected, moved, extended, and styled as arrows."
  (test-shape-artist-e2e-with-artist-buffer
      "----  "
    (shape-artist-shapes-parse)
    (test-shape-artist-e2e--press "C-n")
    (test-shape-artist-e2e--press "RET")
    (should (shape-artist-diagram-edge-p shape-artist--locked-shape))
    (test-shape-artist-e2e--press "L")
    (test-shape-artist-e2e--press ">")
    (test-shape-artist-e2e--press "x")
    (let ((edge shape-artist--locked-shape))
      (should (shape-artist-diagram-edge-p edge))
      (should (= (oref edge x) 1))
      (should (= (oref edge x2) 5))
      (should (oref edge arrow-end)))))

(ert-deftest test-shape-artist-e2e-anchored-left-and-top-resize ()
  "SHAPE-EDIT can grow a rectangle from left and top anchors."
  (test-shape-artist-e2e-with-artist-buffer
      "       \n +--+  \n |  |  \n +--+  "
    (shape-artist-shapes-parse)
    (test-shape-artist-e2e--press "C-n")
    (test-shape-artist-e2e--press "RET")
    (test-shape-artist-e2e--press "[")
    (test-shape-artist-e2e--press "{")
    (let ((shape shape-artist--locked-shape))
      (should (= (oref shape x) 0))
      (should (= (oref shape y) 0))
      (should (= (oref shape width) 4))
      (should (= (oref shape height) 3)))))

(ert-deftest test-shape-artist-e2e-multi-select-move-group ()
  "SELECT can mark multiple rectangles and move them as a group."
  (test-shape-artist-e2e-with-artist-buffer
      "+--+  +--+\n|  |  |  |\n+--+  +--+"
    (shape-artist-shapes-parse)
    (test-shape-artist-e2e--press "C-n")
    (test-shape-artist-e2e--press "t")
    (test-shape-artist-e2e--press "n")
    (test-shape-artist-e2e--press "t")
    (should (= (length shape-artist--selected-shape-signatures) 2))
    (test-shape-artist-e2e--press "RET")
    (test-shape-artist-e2e--press "J")
    (shape-artist-shapes-parse)
    (let ((ys (sort (mapcar (lambda (shape) (oref shape y))
                            (cl-remove-if-not #'shape-artist-diagram-rectangle-p
                                              shape-artist--last-shapes-list))
                    #'<)))
      (should (equal ys '(1 1))))))

(ert-deftest test-shape-artist-e2e-multi-select-move-sticks-connector ()
  "Key-driven group movement keeps a connector between selected boxes attached."
  (test-shape-artist-e2e-with-artist-buffer
      "+--+   +--+\n|  |---|  |\n+--+   +--+\n           "
    (shape-artist-shapes-parse)
    (test-shape-artist-e2e--press "C-n")
    (test-shape-artist-e2e--press "t")
    (test-shape-artist-e2e--press "n")
    (test-shape-artist-e2e--press "t")
    (test-shape-artist-e2e--press "RET")
    (test-shape-artist-e2e--press "J")
    (shape-artist-shapes-parse)
    (let ((edges (cl-remove-if-not #'shape-artist-diagram-edge-p
                                   shape-artist--last-shapes-list))
          (ys (sort (mapcar (lambda (shape) (oref shape y))
                            (cl-remove-if-not #'shape-artist-diagram-rectangle-p
                                              shape-artist--last-shapes-list))
                    #'<)))
      (should (equal ys '(1 1)))
      (should (= (length edges) 1))
      (should (= (oref (car edges) y) 2))
      (should (= (oref (car edges) y2) 2))
      (should (eq (oref (car edges) edge-type) 'connected)))))

(ert-deftest test-shape-artist-e2e-parser-arrow-connectors ()
  "Parser keeps arrow metadata on horizontal and vertical connectors."
  (test-shape-artist-e2e-with-artist-buffer
      "-->\n ^ \n | \n v "
    (shape-artist-shapes-parse)
    (let ((edges (cl-remove-if-not #'shape-artist-diagram-edge-p
                                   shape-artist--last-shapes-list)))
      (should (= (length edges) 2))
      (should (cl-some (lambda (edge) (and (= (oref edge y) 0)
                                           (oref edge arrow-end)))
                       edges))
      (should (cl-some (lambda (edge) (and (= (oref edge x) 1)
                                           (oref edge arrow-start)
                                           (oref edge arrow-end)))
                       edges)))))
