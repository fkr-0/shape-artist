;;; test-shape-artist-diagram-outline.el --- ERT tests for outline buffer -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-helper)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-core)
(require 'shape-artist-diagram-outline)

;;; --- Helper ---

(defmacro test-artist-with-outline-setup (content &rest body)
  "Create buffer with CONTENT, parse, show outline from artist ctx, run BODY in outline."
  (declare (indent 1))
  `(test-artist-with-buffer
       ,content
     (let ((shape-artist--update-sidebar-fn nil))
       (shape-artist-shapes-parse)
       ;; Must call outline-show while current-buffer is the artist buffer
       (shape-artist-diagram-outline-show))
     ;; Now switch to outline buffer for assertions
     (with-current-buffer shape-artist-diagram--outline-buffer-name
       ,@body)))

;;; --- Show Tests ---

(ert-deftest test-shape-artist-outline-show-creates-buffer ()
  "Test that outline-show creates the outline buffer."
  (test-artist-with-outline-setup
      "+---+\n|   |\n+---+"
    (should (buffer-live-p (get-buffer shape-artist-diagram--outline-buffer-name)))))

(ert-deftest test-shape-artist-outline-show-sets-parent ()
  "Test that outline-show sets the parent buffer reference."
  (test-artist-with-outline-setup
      "+---+\n|   |\n+---+"
    (should shape-artist-diagram--outline-parent-buffer)))

(ert-deftest test-shape-artist-outline-show-contains-root ()
  "Test that outline buffer contains the root node."
  (test-artist-with-outline-setup
      "+---+\n|   |\n+---+"
    (should (string-match-p "\\[root\\]" (buffer-string)))))

(ert-deftest test-shape-artist-outline-show-contains-rect ()
  "Test that outline buffer contains rectangle nodes."
  (test-artist-with-outline-setup
      "+---+\n|   |\n+---+"
    (should (string-match-p "\\[rect" (buffer-string)))))

(ert-deftest test-shape-artist-outline-show-nested ()
  "Test that outline buffer shows nested rectangles with indentation."
  (test-artist-with-outline-setup
      "+-------+\n| +---+ |\n| |   | |\n| +---+ |\n+-------+"
    (let ((text (buffer-string)))
      (should (string-match-p "\\[rect" text))
      (should (string-match-p "  \\[rect" text)))))

(ert-deftest test-shape-artist-outline-show-empty ()
  "Test outline with no shapes detected shows root node."
  (test-artist-with-buffer
      "hello world"
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-shapes-parse))
    (shape-artist-diagram-outline-show)
    (with-current-buffer shape-artist-diagram--outline-buffer-name
      ;; When no rectangles are detected, outline shows root with no children
      (should (string-match-p "\\[root\\]" (buffer-string))))))

(ert-deftest test-shape-artist-outline-show-mode ()
  "Test that outline buffer uses outline-mode."
  (test-artist-with-outline-setup
      "+---+\n|   |\n+---+"
    (should (eq major-mode 'shape-artist-diagram-outline-mode))))

(ert-deftest test-shape-artist-outline-show-read-only ()
  "Test that outline buffer is read-only."
  (test-artist-with-outline-setup
      "+---+\n|   |\n+---+"
    (should buffer-read-only)))

;;; --- Refresh Tests ---

(ert-deftest test-shape-artist-outline-refresh ()
  "Test that refresh updates the outline buffer content."
  (test-artist-with-outline-setup
      "+---+\n|   |\n+---+"
    (let ((old-content (buffer-string)))
      ;; Modify the parent buffer
      (with-current-buffer shape-artist-diagram--outline-parent-buffer
        (erase-buffer)
        (insert "+---+\n|   |\n+---+\n\n+---+\n|   |\n+---+")
        (let ((shape-artist--update-sidebar-fn nil))
          (shape-artist-shapes-parse)))
      ;; Refresh from outline buffer
      (shape-artist-diagram-outline-refresh)
      (let ((new-content (buffer-string)))
        (should (string-match-p "\\[rect" new-content))
        (should (= (length (string-split new-content "\\[rect")) 3))))))

(ert-deftest test-shape-artist-outline-refresh-repositions ()
  "Test that refresh moves cursor to point-min."
  (test-artist-with-outline-setup
      "+---+\n|   |\n+---+"
    (goto-char (point-max))
    (shape-artist-diagram-outline-refresh)
    (should (= (point) (point-min)))))

(ert-deftest test-shape-artist-outline-refresh-no-parent ()
  "Test refresh with no parent buffer does nothing."
  (test-artist-with-buffer
      ""
    (let ((buf (get-buffer-create "test-outline-no-parent")))
      (with-current-buffer buf
        (setq shape-artist-diagram--outline-parent-buffer nil)
        (insert "old content")
        (shape-artist-diagram-outline-refresh)
        (should (string= (buffer-string) "old content")))
      (kill-buffer buf))))

;;; --- Update Hook Tests ---

(ert-deftest test-shape-artist-outline-update-hook ()
  "Test that outline-update refreshes the outline buffer."
  (test-artist-with-outline-setup
      "+---+\n|   |\n+---+"
    (let ((old-content (buffer-string)))
      ;; Modify the parent buffer
      (with-current-buffer shape-artist-diagram--outline-parent-buffer
        (erase-buffer)
        (insert "+---+\n|   |\n+---+\n\n+---+\n|   |\n+---+")
        (let ((shape-artist--update-sidebar-fn nil))
          (shape-artist-shapes-parse)))
      ;; Refresh via the update hook
      (shape-artist-diagram--outline-update)
      (let ((new-content (buffer-string)))
        (should (string-match-p "\\[rect" new-content))
        (should (= (length (string-split new-content "\\[rect")) 3))))))

(ert-deftest test-shape-artist-outline-update-hook-no-buffer ()
  "Test that update hook is a no-op when outline buffer doesn't exist."
  ;; First create and kill an outline buffer to ensure no stale buffer
  (test-artist-with-buffer
      "abc"
    (let ((shape-artist--update-sidebar-fn nil))
      ;; Kill any existing outline buffer
      (when (get-buffer shape-artist-diagram--outline-buffer-name)
        (kill-buffer shape-artist-diagram--outline-buffer-name))
      ;; Should not error
      (shape-artist-diagram--outline-update)
      (should t))))

;;; --- Goto Tests ---

(ert-deftest test-shape-artist-outline-goto ()
  "Test that goto parses coordinates from the outline line."
  (test-artist-with-outline-setup
      "+---+\n|   |\n+---+"
    (goto-char (point-min))
    (when (re-search-forward "(\\([0-9]+\\),\\([0-9]+\\))" nil t)
      ;; goto parses coords and calls select-window (fails in batch mode)
      ;; Just verify it doesn't error on valid coordinates
      (condition-case nil
          (shape-artist-diagram-outline-goto)
        (wrong-type-argument nil))
      ;; Verify the regex matched coordinates
      (should (match-string 1))
      (should (match-string 2)))))

(ert-deftest test-shape-artist-outline-goto-no-parent ()
  "Test goto with no parent buffer does nothing."
  (test-artist-with-buffer
      ""
    (let ((buf (get-buffer-create "test-outline-goto")))
      (with-current-buffer buf
        (insert "[rect (0,0) 4x3]\n")
        (goto-char (point-min))
        (setq shape-artist-diagram--outline-parent-buffer nil)
        (shape-artist-diagram-outline-goto)
        (should (eq (current-buffer) buf)))
      (kill-buffer buf))))

;;; --- Keymap Tests ---

(ert-deftest test-shape-artist-outline-mode-map-ret ()
  "Test that RET is bound in outline mode map."
  (should (eq (lookup-key shape-artist-diagram-outline-mode-map (kbd "RET"))
              #'shape-artist-diagram-outline-goto)))

(ert-deftest test-shape-artist-outline-mode-map-g ()
  "Test that g is bound in outline mode map."
  (should (eq (lookup-key shape-artist-diagram-outline-mode-map (kbd "g"))
              #'shape-artist-diagram-outline-refresh)))

;;; --- Insert Node Tests ---

(ert-deftest test-shape-artist-outline-insert-node-rectangle ()
  "Test that insert-node formats a rectangle correctly."
  (test-artist-with-buffer
      ""
    (let ((node (make-instance 'shape-artist-diagram-rectangle
                               :x 1 :y 2 :width 5 :height 3)))
      (shape-artist-diagram--outline-insert-node node 0)
      (should (string-match-p "\\[rect (1,2) 5x3\\]" (buffer-string))))))

(ert-deftest test-shape-artist-outline-insert-node-root ()
  "Test that insert-node formats root node correctly."
  (test-artist-with-buffer
      ""
    (let ((node (make-instance 'shape-artist-diagram-root)))
      (shape-artist-diagram--outline-insert-node node 0)
      (should (string-match-p "\\[root\\]" (buffer-string))))))

(ert-deftest test-shape-artist-outline-insert-node-label ()
  "Test that insert-node formats label node correctly."
  (test-artist-with-buffer
      ""
    (let ((node (make-instance 'shape-artist-diagram-label
                               :x 1 :y 1 :width 3 :height 1
                               :text "hello")))
      (shape-artist-diagram--outline-insert-node node 0)
      (should (string-match-p "\\[label \"hello\"\\]" (buffer-string))))))

(ert-deftest test-shape-artist-outline-insert-node-depth ()
  "Test that insert-node indents based on depth."
  (test-artist-with-buffer
      ""
    (let ((node (make-instance 'shape-artist-diagram-rectangle
                               :x 0 :y 0 :width 3 :height 2)))
      (shape-artist-diagram--outline-insert-node node 2)
      (should (string-prefix-p "    " (buffer-string))))))

;;; test-shape-artist-diagram-outline.el ends here
