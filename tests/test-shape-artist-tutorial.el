;;; test-shape-artist-tutorial.el --- ERT tests for the interactive tutorial -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'test-helper)
(require 'shape-artist)
(require 'shape-artist-tutorial)

;;; --- Fixtures and helpers ---

(defconst test-shape-artist-tutorial--task-order
  '(draw-box label-box second-box connect move-box restyle undo)
  "The required task ids, in order.")

(defconst test-shape-artist-tutorial--one-box
  (mapconcat #'identity
             '("                    "
               "   +-------+        "
               "   |       |        "
               "   |       |        "
               "   +-------+        "
               "                    ")
             "\n")
  "A canvas holding one standard box at (3 . 1), 8 wide and 3 tall.")

(defconst test-shape-artist-tutorial--two-boxes
  (mapconcat #'identity
             '("                                  "
               "   +-------+         +-------+    "
               "   |       |         |       |    "
               "   |       |         |       |    "
               "   +-------+         +-------+    "
               "                                  ")
             "\n")
  "A canvas with two unconnected boxes at (3 . 1) and (21 . 1).")

(defconst test-shape-artist-tutorial--connected-boxes
  (mapconcat #'identity
             '("                                  "
               "   +-------+         +-------+    "
               "   |       |         |       |    "
               "   |       |---------|       |    "
               "   +-------+         +-------+    "
               "                                  ")
             "\n")
  "Two boxes joined by a horizontal line touching both walls.")

(defconst test-shape-artist-tutorial--half-connected
  (mapconcat #'identity
             '("                                  "
               "   +-------+         +-------+    "
               "   |       |         |       |    "
               "   |       |-----    |       |    "
               "   +-------+         +-------+    "
               "                                  ")
             "\n")
  "Two boxes with a line that touches only the first one.")

(defun test-shape-artist-tutorial--parse (text)
  "Parse the canvas TEXT and return its shapes."
  (test-artist-with-buffer text
    (let ((shape-artist--update-sidebar-fn nil))
      (cdr (shape-artist-diagram-parse-buffer)))))

(defun test-shape-artist-tutorial--state (text &optional initial commands)
  "Return a task state for the parsed canvas TEXT with INITIAL and COMMANDS."
  (list :shapes (test-shape-artist-tutorial--parse text)
        :initial-boxes initial
        :commands commands))

(defun test-shape-artist-tutorial--complete-ids (state)
  "Return the task ids complete in STATE, in task order."
  (seq-filter (lambda (id) (shape-artist-tutorial--task-complete-p id state))
              test-shape-artist-tutorial--task-order))

(defun test-shape-artist-tutorial--temp-file ()
  "Return the name of a fresh, not yet existing progress file."
  (let ((file (make-temp-file "sa-tutorial-test-" nil ".eld")))
    (delete-file file)
    file))

(defmacro test-shape-artist-tutorial--with-progress-file (&rest body)
  "Run BODY with `shape-artist-tutorial-progress-file' set to a temp file."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "sa-tutorial-dir-" t))
          (shape-artist-tutorial-progress-file (expand-file-name "p.eld" dir))
          (shape-artist-tutorial--hint-shown nil))
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(defmacro test-shape-artist-tutorial--capturing-messages (var &rest body)
  "Run BODY with `message' recorded, newest first, in the list bound to VAR."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest args)
                  (when format-string
                    (push (apply #'format format-string args) ,var)))))
       ,@body)))

(defvar test-shape-artist-tutorial--scheduled nil
  "Arguments of stubbed `run-with-idle-timer' calls, newest first.")

(defvar test-shape-artist-tutorial--cancelled nil
  "Timers passed to the stubbed `cancel-timer', newest first.")

(defvar test-shape-artist-tutorial--displayed nil
  "Buffers handed to the stubbed `display-buffer', newest first.")

(defmacro test-shape-artist-tutorial--with-session (&rest body)
  "Run BODY with a temp progress file and all windows and timers stubbed.
Both tutorial buffers and the progress directory are removed afterwards."
  (declare (indent 0))
  `(test-shape-artist-tutorial--with-progress-file
     (let ((test-shape-artist-tutorial--scheduled nil)
           (test-shape-artist-tutorial--cancelled nil)
           (test-shape-artist-tutorial--displayed nil)
           (shape-artist--update-sidebar-fn nil))
       (unwind-protect
           (cl-letf (((symbol-function 'display-buffer)
                      (lambda (buffer &rest _)
                        (push buffer test-shape-artist-tutorial--displayed)
                        nil))
                     ((symbol-function 'switch-to-buffer)
                      (lambda (buffer &rest _) (set-buffer buffer)))
                     ((symbol-function 'run-with-idle-timer)
                      (lambda (&rest args)
                        (push args test-shape-artist-tutorial--scheduled)
                        (timer-create)))
                     ((symbol-function 'cancel-timer)
                      (lambda (timer)
                        (push timer test-shape-artist-tutorial--cancelled))))
             ,@body)
         (shape-artist-tutorial-quit)))))

(defun test-shape-artist-tutorial--press (key)
  "Run the command bound to KEY in the current buffer, then its hooks."
  (let ((command (key-binding (kbd (if (equal key "ESC") "<escape>" key)) t)))
    (should (commandp command))
    (setq this-command command)
    (call-interactively command)
    (run-hooks 'post-command-hook)))

(defun test-shape-artist-tutorial--check ()
  "Run the debounced check directly in the practice buffer; return new ids."
  (let ((practice (get-buffer shape-artist-tutorial-practice-buffer-name)))
    (with-current-buffer practice
      (shape-artist-tutorial--evaluate))))

(defun test-shape-artist-tutorial--guide-text ()
  "Return the current contents of the guide buffer."
  (with-current-buffer shape-artist-tutorial-guide-buffer-name
    (buffer-string)))

(defun test-shape-artist-tutorial--saved-ids ()
  "Return the completed ids stored in the progress file."
  (plist-get (shape-artist-tutorial--load-progress) :completed))

(defun test-shape-artist-tutorial--select-rectangle ()
  "In MOVE mode, enter SELECT, step to a rectangle and lock it."
  (test-shape-artist-tutorial--press "C-n")
  (should (eq shape-artist--mode 'select))
  (let ((guard 0))
    (while (and (< guard 20)
                (not (shape-artist-diagram-rectangle-p
                      (nth shape-artist--select-index
                           shape-artist--last-shapes-list))))
      (setq guard (1+ guard))
      (test-shape-artist-tutorial--press "n")))
  (test-shape-artist-tutorial--press "RET")
  (should (eq shape-artist--mode 'shape-edit)))

(defun test-shape-artist-tutorial--draw-box (row col width height)
  "Draw a box from ROW and COL, WIDTH columns across and HEIGHT rows down."
  (shape-artist--goto-row-col row col)
  (test-shape-artist-tutorial--press "SPC")
  (dotimes (_ width) (test-shape-artist-tutorial--press "l"))
  (dotimes (_ height) (test-shape-artist-tutorial--press "j"))
  (test-shape-artist-tutorial--press "RET")
  (should (eq shape-artist--mode 'move)))

;;; --- Task model ---

(ert-deftest test-shape-artist-tutorial-tasks-are-the-required-seven-in-order ()
  "The task table has the required ids, in order, with complete plists."
  (should (equal (shape-artist-tutorial--task-ids)
                 test-shape-artist-tutorial--task-order))
  (dolist (task shape-artist-tutorial--tasks)
    (should (symbolp (plist-get task :id)))
    (dolist (property '(:title :instruction :hint))
      (should (> (length (plist-get task property)) 5))))
  (should (= (length (delete-dups (mapcar (lambda (task) (plist-get task :title))
                                          shape-artist-tutorial--tasks)))
             7)))

(ert-deftest test-shape-artist-tutorial-options-have-documented-defaults ()
  "The user options exist with the documented defaults."
  (should (eq (default-value 'shape-artist-first-run-hint) t))
  (should (equal (eval (car (get 'shape-artist-tutorial-progress-file
                                 'standard-value)))
                 (locate-user-emacs-file "shape-artist-tutorial.eld")))
  (should (memq 'shape-artist-first-run-hint
                (mapcar #'car (get 'shape-artist 'custom-group))))
  (should (memq 'shape-artist-tutorial-progress-file
                (mapcar #'car (get 'shape-artist 'custom-group)))))

(ert-deftest test-shape-artist-tutorial-instructions-name-real-keys ()
  "Expanded text has no placeholders, no unbound fallbacks and real keys."
  (dolist (id test-shape-artist-tutorial--task-order)
    (ert-info ((format "task %s" id))
      (dolist (text (list (shape-artist-tutorial--instruction id)
                          (shape-artist-tutorial--hint id)))
        (should-not (string-match-p "[{}]" text))
        (should-not (string-match-p "M-x" text)))))
  (let ((draw (shape-artist-tutorial--instruction 'draw-box))
        (label (shape-artist-tutorial--instruction 'label-box))
        (restyle (shape-artist-tutorial--instruction 'restyle))
        (undo (shape-artist-tutorial--instruction 'undo)))
    (should (string-match-p "press SPC to drop an anchor" draw))
    (should (string-match-p "with l (right) and j (down)" draw))
    (should (string-match-p "press RET to commit" draw))
    (should (string-match-p "Press C-n to enter SELECT mode, n to step" label))
    (should (string-match-p "then e and type" label))
    (should (string-match-p "press x to cycle" restyle))
    (should (string-match-p "Enter DRAW mode with d, then press u" undo))))

(ert-deftest test-shape-artist-tutorial-instructions-follow-the-keymaps ()
  "Rebinding a key in the documentation map changes the instruction."
  (let ((shape-artist-move-map (copy-keymap shape-artist-move-map))
        (shape-artist-shape-edit-map (copy-keymap shape-artist-shape-edit-map)))
    (define-key shape-artist-move-map (kbd "SPC") nil)
    (define-key shape-artist-move-map (kbd "w")
                #'shape-artist-enter-draw-and-anchor)
    (define-key shape-artist-shape-edit-map (kbd "x") nil)
    (define-key shape-artist-shape-edit-map (kbd "Y")
                #'shape-artist-shape-cycle-style)
    (should (string-match-p "press w to drop an anchor"
                            (shape-artist-tutorial--instruction 'draw-box)))
    (should (string-match-p "press Y to cycle"
                            (shape-artist-tutorial--instruction 'restyle)))
    (should-not (string-match-p "SPC"
                                (shape-artist-tutorial--instruction 'draw-box)))))

(ert-deftest test-shape-artist-tutorial-expand-handles-placeholders ()
  "Placeholders turn into keys; unknown commands fall back to M-x."
  (should (equal (shape-artist-tutorial--expand "Go {move:shape-artist-move-left}!")
                 "Go h!"))
  (should (equal (shape-artist-tutorial--expand
                  "{shape-edit:shape-artist-shape-edit-exit} or {draw:shape-artist-undo}")
                 "ESC or u"))
  (should (equal (shape-artist-tutorial--expand "{move:no-such-command}")
                 "M-x no-such-command"))
  (should (equal (shape-artist-tutorial--expand "no placeholders {here") "no placeholders {here")))

;;; --- Task predicates ---

(ert-deftest test-shape-artist-tutorial-predicates-on-an-empty-canvas ()
  "Nothing is complete on a blank canvas."
  (should-not (test-shape-artist-tutorial--complete-ids
               (list :shapes nil :initial-boxes nil :commands nil)))
  (should-not (test-shape-artist-tutorial--complete-ids
               (test-shape-artist-tutorial--state "          \n          "))))

(ert-deftest test-shape-artist-tutorial-predicates-count-boxes ()
  "draw-box needs one rectangle and second-box needs two, exactly."
  (let ((none (test-shape-artist-tutorial--state "       \n       "))
        (one (test-shape-artist-tutorial--state
              test-shape-artist-tutorial--one-box))
        (two (test-shape-artist-tutorial--state
              test-shape-artist-tutorial--two-boxes)))
    (should-not (shape-artist-tutorial--task-complete-p 'draw-box none))
    (should (shape-artist-tutorial--task-complete-p 'draw-box one))
    (should (shape-artist-tutorial--task-complete-p 'draw-box two))
    (should-not (shape-artist-tutorial--task-complete-p 'second-box none))
    (should-not (shape-artist-tutorial--task-complete-p 'second-box one))
    (should (shape-artist-tutorial--task-complete-p 'second-box two))))

(ert-deftest test-shape-artist-tutorial-draw-box-ignores-bare-lines ()
  "A line is not a box."
  (should-not (shape-artist-tutorial--task-complete-p
               'draw-box
               (test-shape-artist-tutorial--state "  -----  \n         "))))

(ert-deftest test-shape-artist-tutorial-label-needs-text-inside-a-box ()
  "Text counts only when its position is inside a rectangle."
  (let* ((labelled (concat "+---------+\n"
                           "|         |\n"
                           "|  Hello  |\n"
                           "|         |\n"
                           "+---------+"))
         (outside (concat "+---------+            \n"
                          "|         |            \n"
                          "|         |  Outside   \n"
                          "|         |            \n"
                          "+---------+            "))
         (above (concat "  Title                \n"
                        "+---------+            \n"
                        "|         |            \n"
                        "|         |            \n"
                        "+---------+            ")))
    (should (shape-artist-tutorial--task-complete-p
             'label-box (test-shape-artist-tutorial--state labelled)))
    (should-not (shape-artist-tutorial--task-complete-p
                 'label-box (test-shape-artist-tutorial--state outside)))
    (should-not (shape-artist-tutorial--task-complete-p
                 'label-box (test-shape-artist-tutorial--state above)))
    (should-not (shape-artist-tutorial--task-complete-p
                 'label-box (test-shape-artist-tutorial--state
                             test-shape-artist-tutorial--one-box)))))

(ert-deftest test-shape-artist-tutorial-label-inside-is-strict ()
  "A label on or beyond a rectangle's border is not inside."
  (let ((rectangle (make-instance 'shape-artist-diagram-rectangle
                                  :x 10 :y 5 :width 6 :height 3)))
    (dolist (case '((11 6 t) (15 7 t) (10 6 nil) (16 6 nil)
                    (12 5 nil) (12 8 nil) (12 6 t) (9 6 nil) (12 9 nil)))
      (let ((label (make-instance 'shape-artist-diagram-label
                                  :x (nth 0 case) :y (nth 1 case) :text "x")))
        (ert-info ((format "label at %S" case))
          (should (eq (and (shape-artist-tutorial--label-inside-p label rectangle)
                           t)
                      (nth 2 case))))))))

(ert-deftest test-shape-artist-tutorial-connect-needs-two-different-boxes ()
  "Only a line attached to two different boxes connects."
  (let ((connected (test-shape-artist-tutorial--state
                    test-shape-artist-tutorial--connected-boxes))
        (half (test-shape-artist-tutorial--state
               test-shape-artist-tutorial--half-connected))
        (separate (test-shape-artist-tutorial--state
                   test-shape-artist-tutorial--two-boxes)))
    (should (shape-artist-tutorial--task-complete-p 'connect connected))
    (should-not (shape-artist-tutorial--task-complete-p 'connect half))
    (should-not (shape-artist-tutorial--task-complete-p 'connect separate))
    ;; The connected canvas completes exactly the box, count and connect tasks.
    (should (equal (test-shape-artist-tutorial--complete-ids connected)
                   '(draw-box second-box connect)))))

(ert-deftest test-shape-artist-tutorial-connect-rejects-same-box-and-bare-edges ()
  "An edge attached twice to one box, or to nothing, does not connect."
  (let* ((same '(:rect (3 1 8 3) :side right :offset 1))
         (other '(:rect (21 1 8 3) :side left :offset 1))
         (edge (lambda (start end)
                 (make-instance 'shape-artist-diagram-edge
                                :x 12 :y 2 :x2 20 :y2 2
                                :start-attachment start :end-attachment end))))
    (should (shape-artist-tutorial--task-complete-p
             'connect (list :shapes (list (funcall edge same other)))))
    (should-not (shape-artist-tutorial--task-complete-p
                 'connect (list :shapes (list (funcall edge same same)))))
    (should-not (shape-artist-tutorial--task-complete-p
                 'connect (list :shapes (list (funcall edge same nil)))))
    (should-not (shape-artist-tutorial--task-complete-p
                 'connect (list :shapes (list (funcall edge nil other)))))
    (should-not (shape-artist-tutorial--task-complete-p
                 'connect (list :shapes (list (funcall edge nil nil)))))
    ;; One good edge among bad ones is enough.
    (should (shape-artist-tutorial--task-complete-p
             'connect (list :shapes (list (funcall edge nil nil)
                                          (funcall edge same other)))))))

(ert-deftest test-shape-artist-tutorial-move-compares-with-first-seen-positions ()
  "move-box fires when any rectangle differs from its first-seen position."
  (let ((shapes (lambda () (test-shape-artist-tutorial--parse
                            test-shape-artist-tutorial--two-boxes))))
    (should-not (shape-artist-tutorial--task-complete-p
                 'move-box (list :shapes (funcall shapes)
                                 :initial-boxes '((0 . (3 . 1)) (1 . (21 . 1))))))
    ;; First box moved, second did not.
    (should (shape-artist-tutorial--task-complete-p
             'move-box (list :shapes (funcall shapes)
                             :initial-boxes '((0 . (4 . 1)) (1 . (21 . 1))))))
    ;; Only the second box moved: still counts.
    (should (shape-artist-tutorial--task-complete-p
             'move-box (list :shapes (funcall shapes)
                             :initial-boxes '((0 . (3 . 1)) (1 . (21 . 2))))))
    ;; Moving vertically only counts as well.
    (should (shape-artist-tutorial--task-complete-p
             'move-box (list :shapes (funcall shapes)
                             :initial-boxes '((0 . (3 . 0)) (1 . (21 . 1))))))
    ;; Boxes without a recorded origin cannot have moved.
    (should-not (shape-artist-tutorial--task-complete-p
                 'move-box (list :shapes (funcall shapes) :initial-boxes nil)))
    (should-not (shape-artist-tutorial--task-complete-p
                 'move-box (list :shapes (funcall shapes)
                                 :initial-boxes '((5 . (0 . 0))))))
    ;; Labels and edges are never compared.
    (should-not (shape-artist-tutorial--task-complete-p
                 'move-box (list :shapes (test-shape-artist-tutorial--parse
                                          test-shape-artist-tutorial--connected-boxes)
                                 :initial-boxes '((0 . (3 . 1)) (1 . (21 . 1))))))))

(ert-deftest test-shape-artist-tutorial-restyle-needs-a-non-standard-border ()
  "Only a rectangle whose detected style is not standard counts."
  (dolist (case '(("+=======+\n|       |\n+=======+" t)
                  ("+-------+\n|       |\n+-------+" nil)
                  ("┌───────┐\n│       │\n└───────┘" t)
                  ("/-------\\\n|       |\n\\-------/" t)
                  ("+.......+\n|       |\n+.......+" t)))
    (ert-info ((format "canvas %S" (car case)))
      (should (eq (and (shape-artist-tutorial--task-complete-p
                        'restyle (test-shape-artist-tutorial--state (car case)))
                       t)
                  (cadr case)))))
  ;; One restyled box among standard ones is enough.
  (should (shape-artist-tutorial--task-complete-p
           'restyle (test-shape-artist-tutorial--state
                     (concat "+---+  +===+\n|   |  |   |\n+---+  +===+"))))
  ;; A rectangle without a style is not restyled.
  (should-not (shape-artist-tutorial--task-complete-p
               'restyle (list :shapes (list (make-instance
                                             'shape-artist-diagram-rectangle
                                             :x 0 :y 0 :width 3 :height 2))))))

(ert-deftest test-shape-artist-tutorial-undo-looks-at-recorded-commands ()
  "undo needs `undo' or `shape-artist-undo' among the recorded commands."
  (dolist (case '(((undo) t)
                  ((shape-artist-undo) t)
                  ((a b undo c) t)
                  ((b shape-artist-undo) t)
                  ((shape-artist-move-left shape-artist-commit-preview) nil)
                  ((undo-redo) nil)
                  ((shape-artist--dispatch-utility) nil)
                  (nil nil)))
    (ert-info ((format "commands %S" (car case)))
      (should (eq (and (shape-artist-tutorial--task-complete-p
                        'undo (list :commands (car case)))
                       t)
                  (cadr case))))))

(ert-deftest test-shape-artist-tutorial-unknown-task-is-never-complete ()
  "An unknown task id is not complete, whatever the state."
  (should-not (shape-artist-tutorial--task-complete-p
               'no-such-task (test-shape-artist-tutorial--state
                              test-shape-artist-tutorial--connected-boxes
                              '((0 . (0 . 0))) '(undo))))
  (should-not (shape-artist-tutorial--task-complete-p nil nil)))

;;; --- Box tracking ---

(ert-deftest test-shape-artist-tutorial-track-boxes-lifecycle ()
  "Tracking records first positions and follows boxes as they move."
  (let ((tracked nil))
    (setq tracked (shape-artist-tutorial--track-boxes tracked '((3 . 1))))
    (should (equal tracked '(((3 . 1) . (3 . 1)))))
    ;; Unchanged.
    (should (equal (shape-artist-tutorial--track-boxes tracked '((3 . 1)))
                   tracked))
    ;; A second box appears above the first (parse order changes).
    (setq tracked (shape-artist-tutorial--track-boxes tracked '((10 . 0) (3 . 1))))
    (should (equal tracked '(((3 . 1) . (3 . 1)) ((10 . 0) . (10 . 0)))))
    (should (equal (shape-artist-tutorial--initial-boxes tracked '((10 . 0) (3 . 1)))
                   '((0 . (10 . 0)) (1 . (3 . 1)))))
    ;; The first-seen box moves; its origin is remembered.
    (setq tracked (shape-artist-tutorial--track-boxes tracked '((10 . 0) (5 . 1))))
    (should (equal tracked '(((3 . 1) . (5 . 1)) ((10 . 0) . (10 . 0)))))
    (should (equal (shape-artist-tutorial--initial-boxes tracked '((10 . 0) (5 . 1)))
                   '((0 . (10 . 0)) (1 . (3 . 1)))))
    ;; It moves again, and a third box appears in the same step.
    (setq tracked (shape-artist-tutorial--track-boxes
                   tracked '((10 . 0) (6 . 1) (20 . 8))))
    (should (equal tracked '(((3 . 1) . (6 . 1)) ((10 . 0) . (10 . 0))
                             ((20 . 8) . (20 . 8)))))
    ;; A box vanishes.
    (setq tracked (shape-artist-tutorial--track-boxes tracked '((10 . 0) (20 . 8))))
    (should (equal tracked '(((10 . 0) . (10 . 0)) ((20 . 8) . (20 . 8)))))))

(ert-deftest test-shape-artist-tutorial-track-boxes-edge-cases ()
  "Duplicates, emptiness and misaligned positions are handled."
  (should-not (shape-artist-tutorial--track-boxes nil nil))
  (should-not (shape-artist-tutorial--track-boxes '(((1 . 1) . (1 . 1))) nil))
  (should (equal (shape-artist-tutorial--track-boxes nil '((1 . 1) (1 . 1)))
                 '(((1 . 1) . (1 . 1)) ((1 . 1) . (1 . 1)))))
  (should (equal (shape-artist-tutorial--initial-boxes nil '((1 . 1))) nil))
  ;; Two boxes: only the second moves, order of positions flips.
  (let ((tracked '(((2 . 2) . (2 . 2)) ((9 . 2) . (9 . 2)))))
    (should (equal (shape-artist-tutorial--track-boxes tracked '((1 . 2) (2 . 2)))
                   '(((2 . 2) . (2 . 2)) ((9 . 2) . (1 . 2)))))))

(ert-deftest test-shape-artist-tutorial-move-detected-through-tracking ()
  "A real move on a real canvas is detected, a new box is not mistaken for one."
  (let* ((tracked (shape-artist-tutorial--track-boxes
                   nil (list '(3 . 1) '(21 . 1))))
         (moved (test-shape-artist-tutorial--parse
                 (replace-regexp-in-string
                  "^   " "    "
                  test-shape-artist-tutorial--one-box)))
         (positions (mapcar (lambda (rectangle)
                              (cons (oref rectangle x) (oref rectangle y)))
                            (seq-filter #'shape-artist-diagram-rectangle-p moved))))
    (should (equal positions '((4 . 1))))
    ;; The box at (21 . 1) disappeared while (3 . 1) moved to (4 . 1).
    (setq tracked (shape-artist-tutorial--track-boxes tracked positions))
    (should (equal tracked '(((3 . 1) . (4 . 1)))))
    (should (shape-artist-tutorial--task-complete-p
             'move-box (list :shapes moved
                             :initial-boxes (shape-artist-tutorial--initial-boxes
                                             tracked positions))))
    ;; A new box above the unmoved one changes the order but is not a move.
    (let* ((two (test-shape-artist-tutorial--parse
                 (concat "                  +---+\n                  |   |\n"
                         "                  +---+\n"
                         test-shape-artist-tutorial--one-box)))
           (pos (mapcar (lambda (r) (cons (oref r x) (oref r y)))
                        (seq-filter #'shape-artist-diagram-rectangle-p two)))
           (track (shape-artist-tutorial--track-boxes
                   (shape-artist-tutorial--track-boxes nil '((3 . 4))) pos)))
      (should-not (shape-artist-tutorial--task-complete-p
                   'move-box
                   (list :shapes two
                         :initial-boxes (shape-artist-tutorial--initial-boxes
                                         track pos)))))))

;;; --- Persistence ---

(ert-deftest test-shape-artist-tutorial-progress-round-trip ()
  "Saved progress reads back, in the written form."
  (test-shape-artist-tutorial--with-progress-file
    (should (equal (shape-artist-tutorial--load-progress)
                   '(:completed nil :first-run-seen nil)))
    (should (shape-artist-tutorial--save-progress
             '(:completed (draw-box connect undo) :first-run-seen t)))
    (should (file-exists-p shape-artist-tutorial-progress-file))
    (should (equal (shape-artist-tutorial--load-progress)
                   '(:completed (draw-box connect undo) :first-run-seen t)))
    (should (shape-artist-tutorial--save-progress '(:completed nil)))
    (should (equal (shape-artist-tutorial--load-progress)
                   '(:completed nil :first-run-seen nil)))))

(ert-deftest test-shape-artist-tutorial-progress-creates-parent-directories ()
  "Saving creates missing parent directories."
  (test-shape-artist-tutorial--with-progress-file
    (let ((shape-artist-tutorial-progress-file
           (expand-file-name "deep/er/progress.eld"
                             (file-name-directory
                              shape-artist-tutorial-progress-file))))
      (should (shape-artist-tutorial--save-progress '(:completed (draw-box))))
      (should (equal (plist-get (shape-artist-tutorial--load-progress) :completed)
                     '(draw-box))))))

(ert-deftest test-shape-artist-tutorial-progress-tolerates-corrupt-files ()
  "A missing or damaged file reads as no progress and never signals."
  (test-shape-artist-tutorial--with-progress-file
    (dolist (content '("" "(((" "not a list at all" "42" "(:completed)"
                        "(:completed draw-box)" "(:completed (draw-box) :first-run-seen)"
                        "(:completed . 5)" "\"string\"" "(1 2 3)" "\0\0\0"))
      (with-temp-file shape-artist-tutorial-progress-file (insert content))
      (ert-info ((format "content %S" content))
        (let ((progress (shape-artist-tutorial--load-progress)))
          (should (equal (plist-get progress :completed) nil))
          (should (eq (plist-get progress :first-run-seen) nil)))))
    ;; A directory where the file should be is unreadable as a file.
    (delete-file shape-artist-tutorial-progress-file)
    (make-directory shape-artist-tutorial-progress-file)
    (should (equal (shape-artist-tutorial--load-progress)
                   '(:completed nil :first-run-seen nil)))
    (should-not (let ((shape-artist-tutorial-progress-file nil))
                  (plist-get (shape-artist-tutorial--load-progress) :completed)))))

(ert-deftest test-shape-artist-tutorial-progress-filters-unknown-and-duplicate-ids ()
  "Only known task ids survive loading, each once."
  (test-shape-artist-tutorial--with-progress-file
    (with-temp-file shape-artist-tutorial-progress-file
      (insert "(:completed (draw-box bogus undo draw-box 7 \"x\") :first-run-seen t)"))
    (should (equal (shape-artist-tutorial--load-progress)
                   '(:completed (draw-box undo) :first-run-seen t)))))

(ert-deftest test-shape-artist-tutorial-save-never-signals ()
  "An unwritable location makes saving fail quietly."
  (test-shape-artist-tutorial--with-progress-file
    (let ((blocker (expand-file-name "blocker"
                                     (file-name-directory
                                      shape-artist-tutorial-progress-file))))
      (with-temp-file blocker (insert "I am a file"))
      (let ((shape-artist-tutorial-progress-file
             (expand-file-name "sub/progress.eld" blocker)))
        (should-not (shape-artist-tutorial--save-progress '(:completed (draw-box))))
        (should-not (shape-artist-tutorial--persist-completed '(draw-box))))
      (let ((shape-artist-tutorial-progress-file nil))
        (should-not (shape-artist-tutorial--save-progress '(:completed nil)))))))

(ert-deftest test-shape-artist-tutorial-persist-merges-and-keeps-the-flag ()
  "Persisting adds ids in task order and leaves the first-run flag alone."
  (test-shape-artist-tutorial--with-progress-file
    (shape-artist-tutorial--save-progress
     '(:completed (second-box draw-box) :first-run-seen t))
    (should (shape-artist-tutorial--persist-completed '(restyle draw-box)))
    (should (equal (shape-artist-tutorial--load-progress)
                   '(:completed (draw-box second-box restyle) :first-run-seen t)))))

;;; --- First-run hint ---

(ert-deftest test-shape-artist-tutorial-hint-shows-once-and-persists-the-flag ()
  "The first interactive call speaks, remembers, and later calls stay silent."
  (test-shape-artist-tutorial--with-progress-file
    (let ((noninteractive nil)
          (shape-artist-first-run-hint t))
      (test-shape-artist-tutorial--capturing-messages messages
        (should (shape-artist-tutorial-maybe-hint))
        (should (= (length messages) 1))
        (should (equal (car messages)
                       "Shape Artist: ? shows keys, M-x shape-artist-tutorial teaches the basics"))
        (should (plist-get (shape-artist-tutorial--load-progress) :first-run-seen))
        ;; Same session, again.
        (should-not (shape-artist-tutorial-maybe-hint))
        (should (= (length messages) 1))
        ;; A new session: only the file remembers.
        (setq shape-artist-tutorial--hint-shown nil)
        (should-not (shape-artist-tutorial-maybe-hint))
        (should (= (length messages) 1))))))

(ert-deftest test-shape-artist-tutorial-hint-guards ()
  "No hint when disabled, in batch mode, or when the flag is already set."
  (test-shape-artist-tutorial--with-progress-file
    (test-shape-artist-tutorial--capturing-messages messages
      (let ((noninteractive nil) (shape-artist-first-run-hint nil))
        (should-not (shape-artist-tutorial-maybe-hint)))
      (let ((noninteractive t) (shape-artist-first-run-hint t))
        (should-not (shape-artist-tutorial-maybe-hint)))
      (should-not messages)
      (should-not (file-exists-p shape-artist-tutorial-progress-file))
      (should-not shape-artist-tutorial--hint-shown)
      ;; Flag already set in the file.
      (shape-artist-tutorial--save-progress '(:completed nil :first-run-seen t))
      (let ((noninteractive nil) (shape-artist-first-run-hint t))
        (should-not (shape-artist-tutorial-maybe-hint)))
      (should-not messages)
      ;; Once the guards pass, it speaks.
      (shape-artist-tutorial--save-progress '(:completed nil :first-run-seen nil))
      (let ((noninteractive nil) (shape-artist-first-run-hint t))
        (should (shape-artist-tutorial-maybe-hint)))
      (should (= (length messages) 1)))))

(ert-deftest test-shape-artist-tutorial-hint-keeps-completed-tasks ()
  "Setting the first-run flag does not discard saved task progress."
  (test-shape-artist-tutorial--with-progress-file
    (shape-artist-tutorial--save-progress '(:completed (draw-box undo)))
    (let ((noninteractive nil) (shape-artist-first-run-hint t))
      (test-shape-artist-tutorial--capturing-messages _messages
        (shape-artist-tutorial-maybe-hint)))
    (should (equal (shape-artist-tutorial--load-progress)
                   '(:completed (draw-box undo) :first-run-seen t)))))

(ert-deftest test-shape-artist-tutorial-hint-is-once-per-session-even-if-saving-fails ()
  "When the flag cannot be saved the hint still does not repeat in a session."
  (test-shape-artist-tutorial--with-progress-file
    (let ((noninteractive nil) (shape-artist-first-run-hint t))
      (test-shape-artist-tutorial--capturing-messages messages
        (cl-letf (((symbol-function 'shape-artist-tutorial--save-progress)
                   (lambda (_progress) nil)))
          (should (shape-artist-tutorial-maybe-hint))
          (should-not (shape-artist-tutorial-maybe-hint))
          (should-not (shape-artist-tutorial-maybe-hint)))
        (should (= (length messages) 1))))))

(ert-deftest test-shape-artist-tutorial-hint-names-the-help-key-from-the-keymaps ()
  "The hint's key comes from the keymaps, not from a literal."
  (test-shape-artist-tutorial--with-progress-file
    (let ((noninteractive nil)
          (shape-artist-first-run-hint t)
          (shape-artist-move-map (copy-keymap shape-artist-move-map)))
      (define-key shape-artist-move-map (kbd "?") nil)
      (define-key shape-artist-move-map (kbd "h") nil)
      (define-key shape-artist-move-map (kbd "H") #'shape-artist-ui-command-menu)
      (test-shape-artist-tutorial--capturing-messages messages
        (shape-artist-tutorial-maybe-hint)
        (should (string-match-p "^Shape Artist: H shows keys" (car messages)))))))

;;; --- Guide text ---

(ert-deftest test-shape-artist-tutorial-guide-text-marks-progress ()
  "The checklist shows [x]/[ ] marks, the count and the current task."
  (let ((text (shape-artist-tutorial--guide-text nil)))
    (should (string-match-p "0 of 7 done" text))
    (should (string-match-p "^\\[ \\] 1\\. Draw a box   <- now$" text))
    (should (string-match-p "^\\[ \\] 7\\. Undo a change$" text))
    (should-not (string-match-p "\\[x\\]" text))
    (should (string-match-p "NOW: Draw a box" text))
    (should (string-match-p "Hint: " text))
    (should (string-match-p "q here" text))
    (should (string-match-p "shape-artist-tutorial-quit" text))
    (should (string-match-p "shape-artist-tutorial-reset" text)))
  (let ((text (shape-artist-tutorial--guide-text '(second-box draw-box))))
    (should (string-match-p "2 of 7 done" text))
    (should (string-match-p "^\\[x\\] 1\\. Draw a box$" text))
    (should (string-match-p "^\\[ \\] 2\\. Label the box   <- now$" text))
    (should (string-match-p "^\\[x\\] 3\\. Draw a second box$" text))
    (should (string-match-p "NOW: Label the box" text))
    (should (= (length (split-string text "<- now")) 2)))
  (let ((text (shape-artist-tutorial--guide-text
               test-shape-artist-tutorial--task-order)))
    (should (string-match-p "7 of 7 done" text))
    (should (string-match-p "Congratulations" text))
    (should-not (string-match-p "<- now\\|NOW:\\|\\[ \\]" text))
    (should (= (seq-count (lambda (char) (eq char ?x))
                          (apply #'concat
                                 (mapcar (lambda (line)
                                           (if (string-prefix-p "[x]" line) "x" ""))
                                         (split-string text "\n"))))
               7))))

(ert-deftest test-shape-artist-tutorial-guide-shows-current-instruction ()
  "The instruction and hint of the current task are in the text, filled."
  (let ((text (shape-artist-tutorial--guide-text '(draw-box label-box))))
    (should (string-match-p "NOW: Draw a second box" text))
    (let ((flat (replace-regexp-in-string "[ \n]+" " " text)))
      (should (string-match-p
               (regexp-quote (replace-regexp-in-string
                              "[ \n]+" " "
                              (shape-artist-tutorial--instruction 'second-box)))
               flat))
      (should (string-match-p
               (regexp-quote (replace-regexp-in-string
                              "[ \n]+" " "
                              (shape-artist-tutorial--hint 'second-box)))
               flat)))
    (dolist (line (split-string text "\n"))
      (should (<= (length line) 50)))))

;;; --- Session ---

(ert-deftest test-shape-artist-tutorial-starts-both-buffers ()
  "The tutorial builds a 60x20 canvas in Shape Artist mode plus a guide."
  (test-shape-artist-tutorial--with-session
    (let ((practice (shape-artist-tutorial)))
      (should (equal (buffer-name practice) "*shape-artist-tutorial*"))
      (with-current-buffer practice
        (should shape-artist-core-mode)
        (should (eq shape-artist--mode 'move))
        (should (null shape-artist--update-sidebar-fn))
        (should (= (shape-artist--line-count) 20))
        (should (= (shape-artist--max-line-length) 60))
        (should (string-match-p "\\`[ \n]*\\'" (buffer-string)))
        (should (equal (shape-artist--current-row-col) '(3 . 4)))
        (should (memq 'shape-artist-tutorial--post-command
                      (buffer-local-value 'post-command-hook practice)))
        (should (local-variable-p 'post-command-hook))
        (should (eq shape-artist-tutorial--guide-buffer
                    (get-buffer "*shape-artist-tutorial-guide*"))))
      (with-current-buffer "*shape-artist-tutorial-guide*"
        (should buffer-read-only)
        (should (derived-mode-p 'shape-artist-tutorial-guide-mode))
        (should (eq (key-binding (kbd "q")) #'shape-artist-tutorial-quit))
        (should (string-match-p "0 of 7 done" (buffer-string)))
        (should (string-match-p "press SPC to drop an anchor"
                                (replace-regexp-in-string "[ \n]+" " "
                                                          (buffer-string)))))
      (should (memq (get-buffer "*shape-artist-tutorial-guide*")
                    test-shape-artist-tutorial--displayed))
      (should-not (seq-some (lambda (buffer)
                              (string-prefix-p "*artist-sidebar" (buffer-name buffer)))
                            (buffer-list))))))

(ert-deftest test-shape-artist-tutorial-marks-first-run-seen ()
  "Starting the tutorial counts as having seen the first-run hint."
  (test-shape-artist-tutorial--with-session
    (shape-artist-tutorial)
    (should (plist-get (shape-artist-tutorial--load-progress) :first-run-seen))
    (let ((noninteractive nil) (shape-artist-first-run-hint t))
      (test-shape-artist-tutorial--capturing-messages messages
        (should-not (shape-artist-tutorial-maybe-hint))
        (should-not messages)))))

(ert-deftest test-shape-artist-tutorial-post-command-records-and-schedules ()
  "The hook records the command and schedules one debounced check."
  (test-shape-artist-tutorial--with-session
    (with-current-buffer (shape-artist-tutorial)
      (let ((this-command 'shape-artist-move-left))
        (shape-artist-tutorial--post-command))
      (should (equal shape-artist-tutorial--commands '(shape-artist-move-left)))
      (should (= (length test-shape-artist-tutorial--scheduled) 1))
      (let ((call (car test-shape-artist-tutorial--scheduled)))
        (should (= (nth 0 call) shape-artist-tutorial-eval-delay))
        (should-not (nth 1 call))
        (should (eq (nth 2 call) #'shape-artist-tutorial--run-timer))
        (should (eq (nth 3 call) (current-buffer))))
      (should (timerp shape-artist-tutorial--timer))
      (let ((first-timer shape-artist-tutorial--timer)
            (this-command 'shape-artist-move-right))
        (shape-artist-tutorial--post-command)
        (should (memq first-timer test-shape-artist-tutorial--cancelled))
        (should (= (length test-shape-artist-tutorial--scheduled) 2))
        (should-not (eq first-timer shape-artist-tutorial--timer)))
      (should (equal shape-artist-tutorial--commands
                     '(shape-artist-move-right shape-artist-move-left))))))

(ert-deftest test-shape-artist-tutorial-post-command-ignores-non-symbol-commands ()
  "Anonymous or missing commands are not recorded but still schedule a check."
  (test-shape-artist-tutorial--with-session
    (with-current-buffer (shape-artist-tutorial)
      (let ((this-command nil)) (shape-artist-tutorial--post-command))
      (let ((this-command (lambda () nil))) (shape-artist-tutorial--post-command))
      (should-not shape-artist-tutorial--commands)
      (should (= (length test-shape-artist-tutorial--scheduled) 2)))))

(ert-deftest test-shape-artist-tutorial-post-command-caps-the-history ()
  "Only the newest commands are kept."
  (test-shape-artist-tutorial--with-session
    (with-current-buffer (shape-artist-tutorial)
      (dotimes (index (+ shape-artist-tutorial--max-commands 25))
        (let ((this-command (intern (format "cmd-%d" index))))
          (shape-artist-tutorial--post-command)))
      (should (= (length shape-artist-tutorial--commands)
                 shape-artist-tutorial--max-commands))
      (should (eq (car shape-artist-tutorial--commands)
                  (intern (format "cmd-%d" (+ shape-artist-tutorial--max-commands 24)))))
      (should-not (memq 'cmd-0 shape-artist-tutorial--commands)))))

(ert-deftest test-shape-artist-tutorial-post-command-records-draw-undo ()
  "The DRAW-mode undo key is recorded as `shape-artist-undo'."
  (test-shape-artist-tutorial--with-session
    (with-current-buffer (shape-artist-tutorial)
      (setq shape-artist--mode 'draw)
      (let ((this-command 'shape-artist--dispatch-utility))
        (shape-artist-tutorial--post-command))
      (should (equal shape-artist-tutorial--commands '(shape-artist-undo)))
      ;; Outside DRAW mode the same key toggles the sidebar: not an undo.
      (setq shape-artist--mode 'move
            shape-artist-tutorial--commands nil)
      (let ((this-command 'shape-artist--dispatch-utility))
        (shape-artist-tutorial--post-command))
      (should (equal shape-artist-tutorial--commands
                     '(shape-artist--dispatch-utility))))))

(ert-deftest test-shape-artist-tutorial-hooks-never-signal ()
  "Errors inside the hook or the timer are swallowed."
  (test-shape-artist-tutorial--with-session
    (let ((practice (shape-artist-tutorial)))
      (with-current-buffer practice
        (cl-letf (((symbol-function 'shape-artist-tutorial--schedule)
                   (lambda () (error "Boom in schedule"))))
          (let ((this-command 'ignore))
            (should-not (shape-artist-tutorial--post-command))))
        (test-shape-artist-tutorial--capturing-messages messages
          (cl-letf (((symbol-function 'shape-artist-tutorial--evaluate)
                     (lambda () (error "Boom in evaluate"))))
            (shape-artist-tutorial--run-timer practice))
          (should (= (length messages) 1))
          (should (string-match-p "Boom in evaluate" (car messages))))
        (should-not shape-artist-tutorial--timer))
      ;; A dead buffer is a no-op.
      (kill-buffer practice)
      (should-not (shape-artist-tutorial--run-timer practice)))))

(ert-deftest test-shape-artist-tutorial-timer-skips-buffers-that-left-the-mode ()
  "After `shape-artist-quit' nothing is evaluated."
  (test-shape-artist-tutorial--with-session
    (let ((practice (shape-artist-tutorial))
          (evaluated 0))
      (with-current-buffer practice (shape-artist-core-mode -1))
      (cl-letf (((symbol-function 'shape-artist-tutorial--evaluate)
                 (lambda () (setq evaluated (1+ evaluated)))))
        (shape-artist-tutorial--run-timer practice))
      (should (= evaluated 0)))))

(ert-deftest test-shape-artist-tutorial-evaluation-updates-guide-and-progress ()
  "A drawn box completes the first task, updates the guide and the file."
  (test-shape-artist-tutorial--with-session
    (with-current-buffer (shape-artist-tutorial)
      (should-not (test-shape-artist-tutorial--saved-ids))
      (test-shape-artist-tutorial--draw-box 3 4 8 4)
      (should (equal (shape-artist-tutorial--run-timer (current-buffer)) '(draw-box)))
      (should (equal shape-artist-tutorial--completed '(draw-box)))
      (should (equal (test-shape-artist-tutorial--saved-ids) '(draw-box)))
      (let ((text (test-shape-artist-tutorial--guide-text)))
        (should (string-match-p "1 of 7 done" text))
        (should (string-match-p "^\\[x\\] 1\\. Draw a box$" text))
        (should (string-match-p "^\\[ \\] 2\\. Label the box   <- now$" text)))
      ;; Nothing new on the next check; the file is untouched.
      (should-not (shape-artist-tutorial--evaluate))
      (should (equal (test-shape-artist-tutorial--saved-ids) '(draw-box))))))

(ert-deftest test-shape-artist-tutorial-completed-tasks-stay-completed ()
  "Undoing the work does not un-check a task."
  (test-shape-artist-tutorial--with-session
    (with-current-buffer (shape-artist-tutorial)
      (test-shape-artist-tutorial--draw-box 3 4 8 4)
      (test-shape-artist-tutorial--check)
      (erase-buffer)
      (insert (shape-artist-tutorial--blank-canvas))
      (should-not (test-shape-artist-tutorial--check))
      (should (equal shape-artist-tutorial--completed '(draw-box)))
      (should (string-match-p "^\\[x\\] 1\\." (test-shape-artist-tutorial--guide-text))))))

(ert-deftest test-shape-artist-tutorial-evaluation-keeps-select-state ()
  "Re-parsing for the tutorial leaves the parse state SELECT mode relies on."
  (test-shape-artist-tutorial--with-session
    (with-current-buffer (shape-artist-tutorial)
      (test-shape-artist-tutorial--draw-box 3 4 8 4)
      (let ((root shape-artist--last-root)
            (shapes shape-artist--last-shapes-list))
        (should shapes)
        (test-shape-artist-tutorial--check)
        (should (eq shape-artist--last-root root))
        (should (eq shape-artist--last-shapes-list shapes))))))

(ert-deftest test-shape-artist-tutorial-full-walkthrough-with-real-keys ()
  "Drive every task through the real keymaps and watch the checklist fill."
  (test-shape-artist-tutorial--with-session
    (test-shape-artist-tutorial--capturing-messages messages
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Hi"))
                ((symbol-function 'undo) #'ignore))
        (with-current-buffer (shape-artist-tutorial)
          ;; 1. Draw a box.
          (test-shape-artist-tutorial--draw-box 3 4 8 4)
          (should (equal (test-shape-artist-tutorial--check) '(draw-box)))
          ;; 2. Label it: C-n, RET, e.
          (test-shape-artist-tutorial--select-rectangle)
          (test-shape-artist-tutorial--press "e")
          (test-shape-artist-tutorial--press "ESC")
          (should (eq shape-artist--mode 'move))
          (should (equal (test-shape-artist-tutorial--check) '(label-box)))
          ;; 3. Second box.
          (test-shape-artist-tutorial--draw-box 3 30 8 4)
          (should (equal (test-shape-artist-tutorial--check) '(second-box)))
          ;; 4. Connect: from beside the first wall to beside the second.
          (shape-artist--goto-row-col 5 13)
          (test-shape-artist-tutorial--press "SPC")
          (dotimes (_ 16) (test-shape-artist-tutorial--press "l"))
          (test-shape-artist-tutorial--press "RET")
          (should (equal (test-shape-artist-tutorial--check) '(connect)))
          ;; 5. Move a box with L.
          (test-shape-artist-tutorial--select-rectangle)
          (test-shape-artist-tutorial--press "L")
          (test-shape-artist-tutorial--press "L")
          (should (equal (test-shape-artist-tutorial--check) '(move-box)))
          ;; 6. Restyle with x.
          (test-shape-artist-tutorial--press "x")
          (should (equal (test-shape-artist-tutorial--check) '(restyle)))
          (test-shape-artist-tutorial--press "ESC")
          (should-not (seq-filter (lambda (text)
                                    (string-match-p "tutorial complete" text))
                                  messages))
          ;; 7. Undo with u in DRAW mode.
          (test-shape-artist-tutorial--press "d")
          (should (eq shape-artist--mode 'draw))
          (test-shape-artist-tutorial--press "u")
          (should (equal (test-shape-artist-tutorial--check) '(undo)))
          (should (equal shape-artist-tutorial--completed
                         test-shape-artist-tutorial--task-order))
          (should (equal (test-shape-artist-tutorial--saved-ids)
                         test-shape-artist-tutorial--task-order))
          ;; Congratulated exactly once.
          (should (= (seq-count (lambda (text)
                                  (string-match-p "tutorial complete" text))
                                messages)
                     1))
          (should-not (test-shape-artist-tutorial--check))
          (should (= (seq-count (lambda (text)
                                  (string-match-p "tutorial complete" text))
                                messages)
                     1))
          (let ((text (test-shape-artist-tutorial--guide-text)))
            (should (string-match-p "7 of 7 done" text))
            (should (string-match-p "Congratulations" text))))))))

(ert-deftest test-shape-artist-tutorial-resumes-saved-progress ()
  "Tasks saved earlier start checked and the next open task is current."
  (test-shape-artist-tutorial--with-session
    (shape-artist-tutorial--save-progress
     '(:completed (draw-box second-box undo) :first-run-seen t))
    (with-current-buffer (shape-artist-tutorial)
      (should (equal shape-artist-tutorial--completed
                     '(draw-box second-box undo)))
      (let ((text (test-shape-artist-tutorial--guide-text)))
        (should (string-match-p "3 of 7 done" text))
        (should (string-match-p "^\\[x\\] 1\\." text))
        (should (string-match-p "^\\[ \\] 2\\. Label the box   <- now$" text))
        (should (string-match-p "^\\[x\\] 3\\." text))
        (should (string-match-p "^\\[x\\] 7\\." text)))
      (should (equal (test-shape-artist-tutorial--saved-ids)
                     '(draw-box second-box undo))))))

(ert-deftest test-shape-artist-tutorial-starts-clean-after-corrupt-progress ()
  "A damaged progress file does not stop the tutorial from starting."
  (test-shape-artist-tutorial--with-session
    (with-temp-file shape-artist-tutorial-progress-file (insert "(((garbage"))
    (with-current-buffer (shape-artist-tutorial)
      (should-not shape-artist-tutorial--completed)
      (should (string-match-p "0 of 7 done" (test-shape-artist-tutorial--guide-text))))))

(ert-deftest test-shape-artist-tutorial-rerun-resumes-the-open-session ()
  "Running the tutorial again keeps the canvas, progress and a single hook."
  (test-shape-artist-tutorial--with-session
    (let ((practice (shape-artist-tutorial)))
      (with-current-buffer practice
        (test-shape-artist-tutorial--draw-box 3 4 8 4)
        (test-shape-artist-tutorial--check))
      (should (eq (shape-artist-tutorial) practice))
      (with-current-buffer practice
        (should (string-match-p "\\+-------\\+" (buffer-string)))
        (should (equal shape-artist-tutorial--completed '(draw-box)))
        (should (= (seq-count (lambda (fn) (eq fn 'shape-artist-tutorial--post-command))
                              post-command-hook)
                   1))
        (should (string-match-p "1 of 7 done" (test-shape-artist-tutorial--guide-text)))))))

(ert-deftest test-shape-artist-tutorial-rerun-reinitialises-a-left-canvas ()
  "If the canvas left Shape Artist mode, running again rebuilds it."
  (test-shape-artist-tutorial--with-session
    (let ((practice (shape-artist-tutorial)))
      (with-current-buffer practice
        (shape-artist-core-mode -1)
        (insert "junk"))
      (shape-artist-tutorial)
      (with-current-buffer practice
        (should shape-artist-core-mode)
        (should-not (string-match-p "junk" (buffer-string)))
        (should (local-variable-p 'post-command-hook))
        (should (memq 'shape-artist-tutorial--post-command post-command-hook))))))

(ert-deftest test-shape-artist-tutorial-quit-and-kill-clean-up ()
  "Quitting kills both buffers; killing the canvas closes the guide and timer."
  (test-shape-artist-tutorial--with-session
    (shape-artist-tutorial)
    (shape-artist-tutorial-quit)
    (should-not (get-buffer "*shape-artist-tutorial*"))
    (should-not (get-buffer "*shape-artist-tutorial-guide*"))
    ;; Quitting twice is harmless.
    (shape-artist-tutorial-quit)
    ;; Killing only the canvas takes the guide and the pending timer along.
    (let ((practice (shape-artist-tutorial))
          (timer (timer-create)))
      (with-current-buffer practice (setq shape-artist-tutorial--timer timer))
      (kill-buffer practice)
      (should-not (get-buffer "*shape-artist-tutorial-guide*"))
      (should (memq timer test-shape-artist-tutorial--cancelled)))))

(ert-deftest test-shape-artist-tutorial-quit-key-in-the-guide ()
  "Pressing q in the guide closes the tutorial."
  (test-shape-artist-tutorial--with-session
    (shape-artist-tutorial)
    (with-current-buffer "*shape-artist-tutorial-guide*"
      (call-interactively (key-binding (kbd "q"))))
    (should-not (get-buffer "*shape-artist-tutorial*"))
    (should-not (get-buffer "*shape-artist-tutorial-guide*"))))

;;; --- Reset ---

(ert-deftest test-shape-artist-tutorial-reset-asks-first ()
  "Declining the question leaves the saved progress alone."
  (test-shape-artist-tutorial--with-progress-file
    (shape-artist-tutorial--save-progress
     '(:completed (draw-box undo) :first-run-seen t))
    (let ((asked nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (push prompt asked) nil)))
        (should-not (shape-artist-tutorial-reset)))
      (should (= (length asked) 1))
      (should (string-match-p "progress" (car asked))))
    (should (equal (shape-artist-tutorial--load-progress)
                   '(:completed (draw-box undo) :first-run-seen t)))))

(ert-deftest test-shape-artist-tutorial-reset-clears-progress-but-keeps-the-flag ()
  "Confirming clears the task list; the first-run flag stays set."
  (test-shape-artist-tutorial--with-progress-file
    (shape-artist-tutorial--save-progress
     '(:completed (draw-box undo) :first-run-seen t))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (should (shape-artist-tutorial-reset)))
    (should (equal (shape-artist-tutorial--load-progress)
                   '(:completed nil :first-run-seen t)))))

(ert-deftest test-shape-artist-tutorial-reset-restarts-a-running-session ()
  "A running tutorial gets a blank canvas and an empty checklist."
  (test-shape-artist-tutorial--with-session
    (let ((practice (shape-artist-tutorial)))
      (with-current-buffer practice
        (test-shape-artist-tutorial--draw-box 3 4 8 4)
        (test-shape-artist-tutorial--check)
        (should (equal shape-artist-tutorial--completed '(draw-box))))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
        (shape-artist-tutorial-reset))
      (with-current-buffer practice
        (should shape-artist-core-mode)
        (should-not shape-artist-tutorial--completed)
        (should (string-match-p "\\`[ \n]*\\'" (buffer-string)))
        (should (eq shape-artist-tutorial--guide-buffer
                    (get-buffer "*shape-artist-tutorial-guide*")))
        (should (= (seq-count (lambda (fn) (eq fn 'shape-artist-tutorial--post-command))
                              post-command-hook)
                   1)))
      (should (string-match-p "0 of 7 done" (test-shape-artist-tutorial--guide-text)))
      (should-not (test-shape-artist-tutorial--saved-ids))
      ;; The tutorial still works after the reset.
      (with-current-buffer practice
        (test-shape-artist-tutorial--draw-box 3 4 8 4)
        (should (equal (test-shape-artist-tutorial--check) '(draw-box)))))))

(provide 'test-shape-artist-tutorial)
;;; test-shape-artist-tutorial.el ends here
