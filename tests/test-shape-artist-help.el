;;; test-shape-artist-help.el --- ERT tests for key hints, palette and cheat sheet -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'test-helper)
(require 'shape-artist-help)

;;; --- Helpers ---

(defconst test-shape-artist-help--modes '(move draw select shape-edit)
  "Every Shape Artist mode.")

(defconst test-shape-artist-help--mode-labels
  '((move . "MOVE") (draw . "DRAW") (select . "SELECT") (shape-edit . "EDIT"))
  "Expected display label for each mode.")

(defmacro test-shape-artist-help--in-mode (mode &rest body)
  "Run BODY in a temp buffer whose `shape-artist--mode' is MODE."
  (declare (indent 1))
  `(with-temp-buffer
     (setq shape-artist--mode ,mode)
     ,@body))

(defun test-shape-artist-help--map-var (mode)
  "Return the doc keymap variable symbol for MODE."
  (pcase mode
    ('move 'shape-artist-move-map)
    ('draw 'shape-artist-draw-map)
    ('select 'shape-artist-select-map)
    ('shape-edit 'shape-artist-shape-edit-map)))

(defun test-shape-artist-help--binding-count (map)
  "Return how many bindings the sparse keymap MAP defines."
  (let ((count 0))
    (map-keymap (lambda (_event _binding) (setq count (1+ count))) map)
    count))

(defmacro test-shape-artist-help--capturing-messages (var &rest body)
  "Run BODY with `message' recorded, newest first, in the list bound to VAR."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format-string &rest args)
                  (when format-string
                    (push (apply #'format format-string args) ,var)))))
       ,@body)))

(defun test-shape-artist-help--quick-actions-for (rows)
  "Return a replacement for `shape-artist--ui-quick-actions' serving ROWS."
  (lambda ()
    (let ((mode shape-artist--mode))
      (seq-filter (lambda (row)
                    (or (memq 'all (nth 0 row)) (memq mode (nth 0 row))))
                  rows))))

(defun test-shape-artist-help--entry (entries command)
  "Return the palette entry for COMMAND in ENTRIES."
  (seq-find (lambda (entry) (eq (plist-get entry :command) command)) entries))

;;; --- Key sources ---

(ert-deftest test-shape-artist-help-mode-keys-cover-every-doc-binding ()
  "Each mode lists exactly the bindings of its documentation keymap."
  (dolist (mode test-shape-artist-help--modes)
    (ert-info ((format "mode %s" mode))
      (let* ((map (symbol-value (test-shape-artist-help--map-var mode)))
             (pairs (shape-artist-help-mode-keys mode)))
        (should (= (length pairs) (test-shape-artist-help--binding-count map)))
        (should (> (length pairs) 5))
        (dolist (pair pairs)
          (should (eq (lookup-key map (kbd (car pair))) (cdr pair))))
        (should (= (length pairs) (length (delete-dups (mapcar #'car pairs)))))))))

(ert-deftest test-shape-artist-help-mode-keys-order-puts-movement-first ()
  "Movement keys lead in h j k l (then H J K L) order; the rest is sorted."
  (should (equal (mapcar #'car (seq-take (shape-artist-help-mode-keys 'move) 4))
                 '("h" "j" "k" "l")))
  (should (equal (mapcar #'car (seq-take (shape-artist-help-mode-keys 'draw) 4))
                 '("h" "j" "k" "l")))
  (should (equal (mapcar #'car (seq-take (shape-artist-help-mode-keys 'shape-edit) 8))
                 '("h" "j" "k" "l" "H" "J" "K" "L")))
  (dolist (mode test-shape-artist-help--modes)
    (let* ((keys (mapcar #'car (shape-artist-help-mode-keys mode)))
           (rest (seq-drop keys (pcase mode
                                  ('shape-edit 8)
                                  ((or 'move 'draw) 4)
                                  (_ 0)))))
      (ert-info ((format "rest of %s" mode))
        (should (equal rest (sort (copy-sequence rest) #'string<)))))))

(ert-deftest test-shape-artist-help-mode-keys-reads-the-live-keymap ()
  "A key added to a documentation keymap shows up: nothing is hard-coded."
  (let ((shape-artist-move-map (copy-keymap shape-artist-move-map)))
    (define-key shape-artist-move-map (kbd "w") #'ignore)
    (should (equal (cdr (assoc "w" (shape-artist-help-mode-keys 'move)))
                   'ignore))
    (define-key shape-artist-move-map (kbd "h") nil)
    (should-not (assoc "h" (shape-artist-help-mode-keys 'move))))
  (should-not (assoc "w" (shape-artist-help-mode-keys 'move))))

(ert-deftest test-shape-artist-help-mode-keys-unknown-mode-is-empty ()
  "An unknown mode has no keys."
  (should-not (shape-artist-help-mode-keys 'no-such-mode)))

(ert-deftest test-shape-artist-help-mode-keys-keep-duplicate-bindings ()
  "A command bound to two keys is listed under both."
  (let ((shape-artist-move-map (copy-keymap shape-artist-move-map)))
    (define-key shape-artist-move-map (kbd "Q") #'shape-artist-quit)
    (let ((keys (mapcar #'car (seq-filter
                               (lambda (pair) (eq (cdr pair) 'shape-artist-quit))
                               (shape-artist-help-mode-keys 'move)))))
      (should (equal (sort (copy-sequence keys) #'string<) '("Q" "q"))))))

(ert-deftest test-shape-artist-help-key-for-table ()
  "Keys are looked up from doc keymaps first, then through dispatchers."
  (dolist (case '((shape-artist-move-left move "h")
                  (shape-artist-draw-move-down draw "j")
                  (shape-artist-enter-draw-and-anchor move "SPC")
                  (shape-artist-cancel-and-exit draw "ESC")
                  (shape-artist-shape-move-left shape-edit "H")
                  (shape-artist-select-start move "C-n")
                  (shape-artist-select-start draw nil)
                  (shape-artist-undo draw "u")
                  (shape-artist-undo move nil)
                  (shape-artist-ui-toggle select "u")
                  (shape-artist-move-left draw nil)
                  (no-such-command move nil)))
    (ert-info ((format "%S" case))
      (should (equal (shape-artist-help-key-for (nth 0 case) (nth 1 case))
                     (nth 2 case))))))

(ert-deftest test-shape-artist-help-key-for-defaults-to-current-mode ()
  "Without a MODE argument the buffer's mode decides."
  (test-shape-artist-help--in-mode 'draw
    (should (equal (shape-artist-help-key-for 'shape-artist-commit-and-exit)
                   "RET"))
    (should-not (shape-artist-help-key-for 'shape-artist-move-left)))
  (with-temp-buffer
    (setq shape-artist--mode nil)
    (should (equal (shape-artist-help-key-for 'shape-artist-move-left) "h"))))

;;; --- Hint ---

(ert-deftest test-shape-artist-help-hint-string-shape-per-mode ()
  "Each hint has its mode label, fits the echo area and ends with the pointer."
  (dolist (mode test-shape-artist-help--modes)
    (ert-info ((format "mode %s" mode))
      (let* ((hint (shape-artist-help-hint-string mode))
             (label (cdr (assq mode test-shape-artist-help--mode-labels)))
             (groups (split-string (substring hint (length label) nil) " · ")))
        (should (string-prefix-p (concat label ": ") hint))
        (should (string-suffix-p "? again for all" hint))
        (should (<= (string-width hint) 80))
        (should-not (string-match-p "\n" hint))
        ;; label+first group counts as one chunk; subtract the trailing pointer.
        (should (<= (1- (length groups)) 8))
        (should (>= (length groups) 3))))))

(ert-deftest test-shape-artist-help-hint-string-content-comes-from-the-keymaps ()
  "Hints show the keys the maps bind, grouped, with a short verb."
  (dolist (case '((move "h/j/k/l move" "SPC draw" "C-n select")
                  (draw "h/j/k/l draw" "SPC anchor" "RET commit" "ESC cancel")
                  (select "n/p next/prev" "t mark" "RET edit" "ESC cancel")
                  (shape-edit "H/J/K/L move" "x style" "e label" "ESC finish")))
    (let ((hint (shape-artist-help-hint-string (car case))))
      (dolist (needle (cdr case))
        (ert-info ((format "%s hint %S contains %S" (car case) hint needle))
          (should (string-match-p (regexp-quote needle) hint)))))))

(ert-deftest test-shape-artist-help-hint-string-follows-rebinding ()
  "Rebinding a key in the map changes the hint; unbinding removes the entry."
  (let ((shape-artist-move-map (copy-keymap shape-artist-move-map)))
    (define-key shape-artist-move-map (kbd "SPC") nil)
    (define-key shape-artist-move-map (kbd "w") #'shape-artist-enter-draw-and-anchor)
    (let ((hint (shape-artist-help-hint-string 'move)))
      (should (string-match-p "w draw" hint))
      (should-not (string-match-p "SPC draw" hint)))
    (define-key shape-artist-move-map (kbd "w") nil)
    (should-not (string-match-p " draw" (shape-artist-help-hint-string 'move)))))

(ert-deftest test-shape-artist-help-hint-string-limits-keys-and-width ()
  "The key count and column limits are enforced by dropping the least useful."
  (let ((full (shape-artist-help-hint-string 'move)))
    (let ((shape-artist-help-hint-max-keys 2))
      (let ((hint (shape-artist-help-hint-string 'move)))
        (should (equal hint "MOVE: h/j/k/l move · SPC draw · ? again for all"))
        (should (string-prefix-p (substring hint 0 -16) full))))
    (let ((shape-artist-help-hint-max-width 40))
      (let ((hint (shape-artist-help-hint-string 'move)))
        (should (<= (string-width hint) 40))
        (should (string-prefix-p "MOVE: h/j/k/l move" hint))
        (should (< (string-width hint) (string-width full)))))
    ;; The limit is inclusive: a line exactly as wide as the limit stays.
    (let ((shape-artist-help-hint-max-width (string-width full)))
      (should (equal (shape-artist-help-hint-string 'move) full)))
    (let ((shape-artist-help-hint-max-width (1- (string-width full))))
      (should (< (string-width (shape-artist-help-hint-string 'move))
                 (string-width full))))
    (let ((shape-artist-help-hint-max-width 5))
      (should (equal (shape-artist-help-hint-string 'move)
                     "MOVE: ? again for all")))))

(ert-deftest test-shape-artist-help-hint-string-skips-stale-priorities ()
  "A priority entry for a mode without keymap data is dropped quietly."
  (let ((shape-artist-help--hint-priorities
         '((move (shape-artist-removed-command . "gone")
                 (shape-artist-move-left . "left")))))
    (should (equal (shape-artist-help-hint-string 'move)
                   "MOVE: h left · ? again for all"))))

(ert-deftest test-shape-artist-help-hint-command-shows-then-opens-palette ()
  "First press echoes the hint; an immediate second press opens the palette."
  (let ((palette-calls 0))
    (cl-letf (((symbol-function 'shape-artist-help-palette)
               (lambda () (setq palette-calls (1+ palette-calls)))))
      (test-shape-artist-help--in-mode 'draw
        (test-shape-artist-help--capturing-messages messages
          (let ((last-command 'self-insert-command))
            (shape-artist-help-hint))
          (should (= palette-calls 0))
          (should (equal messages
                         (list (shape-artist-help-hint-string 'draw))))
          (should (string-prefix-p "DRAW: " (car messages)))
          (let ((last-command 'shape-artist-help-hint))
            (shape-artist-help-hint))
          (should (= palette-calls 1))
          (should (= (length messages) 1)))))))

(ert-deftest test-shape-artist-help-hint-command-without-mode-uses-move ()
  "In a buffer without a Shape Artist mode the MOVE hint is shown."
  (with-temp-buffer
    (setq shape-artist--mode nil)
    (test-shape-artist-help--capturing-messages messages
      (let ((last-command nil))
        (shape-artist-help-hint))
      (should (string-prefix-p "MOVE: " (car messages))))))

(ert-deftest test-shape-artist-help-hint-command-is-interactive ()
  "The hint is a command the lead can bind."
  (should (commandp 'shape-artist-help-hint))
  (should (commandp 'shape-artist-help-palette))
  (should (commandp 'shape-artist-help-cheatsheet)))

;;; --- Palette entries ---

(defun test-shape-artist-help--expected-commands ()
  "Return every command in the real quick-action table, computed independently."
  (let ((commands nil))
    (dolist (mode test-shape-artist-help--modes)
      (let ((shape-artist--mode mode))
        (dolist (row (shape-artist--ui-quick-actions))
          (push (nth 3 row) commands))))
    (delete-dups (nreverse commands))))

(ert-deftest test-shape-artist-help-palette-entries-unite-all-modes ()
  "Every command of every mode appears exactly once, whatever the mode."
  (let ((expected (test-shape-artist-help--expected-commands)))
    (should (> (length expected) 40))
    (dolist (current test-shape-artist-help--modes)
      (ert-info ((format "current mode %s" current))
        (let* ((entries (shape-artist-help--palette-entries current))
               (commands (mapcar (lambda (entry) (plist-get entry :command))
                                 entries)))
          (should (equal (sort (copy-sequence commands) #'string<)
                         (sort (copy-sequence expected) #'string<)))
          (should (= (length commands) (length (delete-dups (copy-sequence commands)))))
          (let ((candidates (mapcar (lambda (entry) (plist-get entry :candidate))
                                    entries)))
            (should (= (length candidates)
                       (length (delete-dups (copy-sequence candidates)))))
            (should-not (member nil candidates))))))))

(ert-deftest test-shape-artist-help-palette-groups-in-display-order ()
  "Groups appear in a fixed order and cover all five categories."
  (dolist (current test-shape-artist-help--modes)
    (ert-info ((format "current mode %s" current))
      (let* ((entries (shape-artist-help--palette-entries current))
             (groups (mapcar (lambda (entry) (plist-get entry :group)) entries))
             (ranks (mapcar (lambda (group)
                              (seq-position shape-artist-help--group-order group))
                            groups)))
        (should-not (member nil ranks))
        (should (equal ranks (sort (copy-sequence ranks) #'<)))
        (should (equal (delete-dups (copy-sequence groups))
                       '("This mode" "Navigate & view" "Guides & grid"
                         "Metadata" "Other modes")))))))

(ert-deftest test-shape-artist-help-palette-group-membership ()
  "Actions land in the group that matches their command and availability."
  (let ((entries (shape-artist-help--palette-entries 'draw)))
    (dolist (case '((shape-artist-commit-and-exit "This mode")
                    (shape-artist-pick-tool "This mode")
                    (shape-artist-undo "This mode")
                    (shape-artist-ui-show "Navigate & view")
                    (shape-artist-shapes-parse "Navigate & view")
                    (shape-artist-guide-create-horizontal "Guides & grid")
                    (shape-artist-snap-toggle "Guides & grid")
                    (shape-artist-grid-toggle "Guides & grid")
                    (shape-artist-guides-save "Guides & grid")
                    (shape-artist-metadata-save "Metadata")
                    (shape-artist-select-next "Other modes")
                    (shape-artist-enter-draw "Other modes")
                    (shape-artist-shape-cycle-style "Other modes")))
      (ert-info ((format "%S" case))
        (should (equal (plist-get (test-shape-artist-help--entry entries (car case))
                                  :group)
                       (cadr case))))))
  (let ((entries (shape-artist-help--palette-entries 'select)))
    (should (equal (plist-get (test-shape-artist-help--entry
                               entries 'shape-artist-select-next)
                              :group)
                   "This mode"))
    (should (equal (plist-get (test-shape-artist-help--entry
                               entries 'shape-artist-commit-and-exit)
                              :group)
                   "Other modes"))))

(ert-deftest test-shape-artist-help-palette-keeps-source-order-within-group ()
  "Within a group the quick-action table's order is preserved."
  (let* ((entries (shape-artist-help--palette-entries 'select))
         (this-mode (seq-filter (lambda (entry)
                                  (equal (plist-get entry :group) "This mode"))
                                entries)))
    (should (equal (mapcar (lambda (entry) (plist-get entry :command)) this-mode)
                   '(shape-artist-select-next shape-artist-select-prev
                                              shape-artist-select-toggle-current
                                              shape-artist-select-all
                                              shape-artist-select-clear-selected
                                              shape-artist-select-confirm)))))

(ert-deftest test-shape-artist-help-palette-other-mode-entries-record-their-modes ()
  "Entries of other modes keep the modes in which they are available."
  (let ((entries (shape-artist-help--palette-entries 'move)))
    (should (equal (plist-get (test-shape-artist-help--entry
                               entries 'shape-artist-select-next)
                              :modes)
                   '(select)))
    (should (equal (plist-get (test-shape-artist-help--entry
                               entries 'shape-artist-shape-edit-label)
                              :modes)
                   '(shape-edit)))
    (should (equal (plist-get (test-shape-artist-help--entry
                               entries 'shape-artist-ui-show)
                              :modes)
                   test-shape-artist-help--modes))))

(ert-deftest test-shape-artist-help-palette-colliding-descriptions-stay-unique ()
  "Two actions with one description get distinguishable candidates."
  (cl-letf (((symbol-function 'shape-artist--ui-quick-actions)
             (test-shape-artist-help--quick-actions-for
              '(((all) "a" "Same words" shape-artist-ui-show)
                ((all) "z" "Same words" shape-artist-ui-center-diagram)))))
    (let ((candidates (mapcar (lambda (entry) (plist-get entry :candidate))
                              (shape-artist-help--palette-entries 'move))))
      (should (equal candidates '("Same words" "Same words [z]"))))))

(ert-deftest test-shape-artist-help-palette-prefers-the-live-table-row ()
  "When one command has two table rows the live-bound row names it."
  (cl-letf (((symbol-function 'shape-artist--ui-quick-actions)
             (test-shape-artist-help--quick-actions-for
              '(((shape-edit) "H/J/K/L" "Move group" shape-artist-shape-move)
                ((shape-edit) "m" "Prompt move" shape-artist-shape-move)))))
    (let ((entries (shape-artist-help--palette-entries 'shape-edit)))
      (should (= (length entries) 1))
      (should (equal (plist-get (car entries) :desc) "Prompt move"))
      (should (equal (plist-get (car entries) :key) "m")))))

;;; --- Palette annotations and completion table ---

(ert-deftest test-shape-artist-help-palette-annotation-uses-live-key-not-table ()
  "The annotation shows the key the maps bind; the table key is a fallback."
  (cl-letf (((symbol-function 'shape-artist--ui-quick-actions)
             (test-shape-artist-help--quick-actions-for
              '(((all) "Z" "Leave artist" shape-artist-quit)
                ((all) "G h" "Make guide" shape-artist-guide-create-horizontal)))))
    (let* ((entries (shape-artist-help--palette-entries 'move))
           (quit (test-shape-artist-help--entry entries 'shape-artist-quit))
           (guide (test-shape-artist-help--entry
                   entries 'shape-artist-guide-create-horizontal)))
      (should (string-match-p "\\`  q\\(?: \\|\\'\\)"
                              (shape-artist-help--entry-annotation quit 'move)))
      (should-not (string-match-p "Z" (shape-artist-help--entry-annotation
                                       quit 'move)))
      (should (string-match-p "\\`  G h"
                              (shape-artist-help--entry-annotation guide 'move))))))

(ert-deftest test-shape-artist-help-palette-annotation-flags-other-modes ()
  "Other-mode actions say which mode they need; current-mode ones do not."
  (let ((entries (shape-artist-help--palette-entries 'move)))
    (dolist (case '((shape-artist-select-next "(SELECT mode only)" "  n")
                    (shape-artist-commit-and-exit "(DRAW mode only)" "  RET")
                    (shape-artist-shape-cycle-style "(EDIT mode only)" "  x")
                    (shape-artist-pick-tool "(DRAW mode only)" "  p")))
      (let ((annotation (shape-artist-help--entry-annotation
                         (test-shape-artist-help--entry entries (car case))
                         'move)))
        (ert-info ((format "%S -> %S" (car case) annotation))
          (should (string-match-p (regexp-quote (nth 1 case)) annotation))
          (should (string-prefix-p (nth 2 case) annotation))))))
  (let ((annotation (shape-artist-help--entry-annotation
                     (test-shape-artist-help--entry
                      (shape-artist-help--palette-entries 'move)
                      'shape-artist-enter-draw)
                     'move)))
    (should (string-prefix-p "  d" annotation))
    (should-not (string-match-p "mode only" annotation)))
  ;; The same command is a current-mode action when that mode is active.
  (let ((annotation (shape-artist-help--entry-annotation
                     (test-shape-artist-help--entry
                      (shape-artist-help--palette-entries 'select)
                      'shape-artist-select-next)
                     'select)))
    (should-not (string-match-p "mode only" annotation))))

(ert-deftest test-shape-artist-help-palette-annotation-shows-intent-keywords ()
  "Intent keywords follow the key; commands without keywords show none."
  (let* ((entries (shape-artist-help--palette-entries 'shape-edit))
         (style (shape-artist-help--entry-annotation
                 (test-shape-artist-help--entry
                  entries 'shape-artist-shape-cycle-style)
                 'shape-edit))
         (guide (shape-artist-help--entry-annotation
                 (test-shape-artist-help--entry
                  entries 'shape-artist-guide-delete-nearest)
                 'shape-edit)))
    (should (string-match-p "x  · style, arrow" style))
    (should (string-match-p "restyle" style))
    (should (string-match-p "delete" guide)))
  (cl-letf (((symbol-function 'shape-artist--ui-quick-actions)
             (test-shape-artist-help--quick-actions-for
              '(((all) "q" "Quit" shape-artist-quit)))))
    (should (equal (shape-artist-help--entry-annotation
                    (car (shape-artist-help--palette-entries 'move)) 'move)
                   "  q"))))

(ert-deftest test-shape-artist-help-intent-keywords-are-sound ()
  "Keywords exist only for real commands, and every table command has some."
  (dolist (entry shape-artist-help--intent-keywords)
    (should (commandp (car entry)))
    (should (cdr entry))
    (should (cl-every #'stringp (cdr entry))))
  (should (= (length shape-artist-help--intent-keywords)
             (length (delete-dups (mapcar #'car shape-artist-help--intent-keywords)))))
  (dolist (command (test-shape-artist-help--expected-commands))
    (ert-info ((format "keywords for %s" command))
      (should (assq command shape-artist-help--intent-keywords)))))

(ert-deftest test-shape-artist-help-palette-table-metadata ()
  "The completion table declares category, grouping, annotation and order."
  (let* ((entries (shape-artist-help--palette-entries 'draw))
         (table (shape-artist-help--palette-table entries 'draw))
         (metadata (completion-metadata "" table nil))
         (group-fn (completion-metadata-get metadata 'group-function))
         (annotate (completion-metadata-get metadata 'annotation-function))
         (sort-fn (completion-metadata-get metadata 'display-sort-function))
         (undo (test-shape-artist-help--entry entries 'shape-artist-undo))
         (next (test-shape-artist-help--entry entries 'shape-artist-select-next)))
    (should (eq (completion-metadata-get metadata 'category)
                'shape-artist-action))
    (should (equal (funcall group-fn (plist-get undo :candidate) nil)
                   "This mode"))
    (should (equal (funcall group-fn (plist-get next :candidate) nil)
                   "Other modes"))
    (should (equal (funcall group-fn "anything" t) "anything"))
    (should (string-match-p "u  · undo"
                            (funcall annotate (plist-get undo :candidate))))
    (should (string-match-p "(SELECT mode only)"
                            (funcall annotate (plist-get next :candidate))))
    (should-not (funcall annotate "no such candidate"))
    (let ((sample '("b" "a" "c")))
      (should (equal (funcall sort-fn sample) sample)))
    ;; Completion itself runs over the candidates, in source order.
    (should (equal (all-completions "" table)
                   (mapcar (lambda (entry) (plist-get entry :candidate)) entries)))
    (should (equal (all-completions "Undo" table)
                   '("Undo draw step" "Undo guide change")))
    (should (equal (try-completion "Undo draw" table) "Undo draw step"))))

(ert-deftest test-shape-artist-help-palette-table-groups-through-completion-api ()
  "Group titles reach consumers via the standard completion-metadata API."
  (test-shape-artist-help--in-mode 'move
    (let* ((entries (shape-artist-help--palette-entries 'move))
           (table (shape-artist-help--palette-table entries 'move))
           (group-fn (completion-metadata-get (completion-metadata "" table nil)
                                              'group-function))
           (groups (delete-dups
                    (mapcar (lambda (candidate) (funcall group-fn candidate nil))
                            (all-completions "" table)))))
      (should (equal groups '("This mode" "Navigate & view" "Guides & grid"
                              "Metadata" "Other modes"))))))

;;; --- Palette command ---

(defvar test-shape-artist-help--read nil
  "Arguments of the last stubbed `completing-read' call.")

(defmacro test-shape-artist-help--with-palette-choice (choice-fn &rest body)
  "Run BODY with `completing-read' answered by CHOICE-FN.
CHOICE-FN receives the candidate list.  The prompt, collection and
REQUIRE-MATCH argument of the call are stored in `test-shape-artist-help--read'."
  (declare (indent 1))
  `(let ((test-shape-artist-help--read nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &optional predicate require-match
                                &rest _)
                  (setq test-shape-artist-help--read
                        (list prompt collection predicate require-match))
                  (funcall ,choice-fn (all-completions "" collection)))))
       ,@body)))

(ert-deftest test-shape-artist-help-palette-runs-available-action ()
  "Choosing an action of the current mode calls it interactively."
  (let ((calls nil))
    (cl-letf (((symbol-function 'shape-artist-pick-tool)
               (lambda () (interactive) (push 'pick-tool calls))))
      (test-shape-artist-help--in-mode 'draw
        (test-shape-artist-help--with-palette-choice
            (lambda (candidates)
              (should (member "Pick draw tool" candidates))
              "Pick draw tool")
          (shape-artist-help-palette)
          (should (equal calls '(pick-tool)))
          (should (equal (nth 0 test-shape-artist-help--read)
                         "Shape Artist action: "))
          (should (nth 3 test-shape-artist-help--read))
          (should (functionp (nth 1 test-shape-artist-help--read))))))))

(ert-deftest test-shape-artist-help-palette-other-mode-action-explains-instead ()
  "Choosing another mode's action runs nothing and says how to get there."
  (let ((calls nil))
    (cl-letf (((symbol-function 'shape-artist-select-next)
               (lambda () (interactive) (push 'select-next calls))))
      (test-shape-artist-help--in-mode 'move
        (test-shape-artist-help--capturing-messages messages
          (test-shape-artist-help--with-palette-choice
              (lambda (_candidates) "Next candidate")
            (shape-artist-help-palette))
          (should-not calls)
          (should (= (length messages) 1))
          (should (string-match-p "Next candidate" (car messages)))
          (should (string-match-p "SELECT" (car messages)))
          (should (string-match-p "press C-n in MOVE mode" (car messages))))))))

(ert-deftest test-shape-artist-help-palette-ignores-unknown-choice ()
  "An empty or unknown answer does nothing."
  (test-shape-artist-help--in-mode 'move
    (test-shape-artist-help--capturing-messages messages
      (test-shape-artist-help--with-palette-choice (lambda (_c) "")
        (shape-artist-help-palette))
      (test-shape-artist-help--with-palette-choice (lambda (_c) "Nope")
        (shape-artist-help-palette))
      (should-not messages))))

(ert-deftest test-shape-artist-help-palette-lists-every-action-to-completing-read ()
  "The collection offered to the user contains all actions, current mode first."
  (test-shape-artist-help--in-mode 'shape-edit
    (test-shape-artist-help--with-palette-choice (lambda (_c) "")
      (shape-artist-help-palette)
      (let ((candidates (all-completions "" (nth 1 test-shape-artist-help--read))))
        (should (= (length candidates)
                   (length (test-shape-artist-help--expected-commands))))
        (should (equal (car candidates) "Prompt move"))
        (should (member "Commit shape and exit draw" candidates))
        (should (member "Next candidate" candidates))))))

(ert-deftest test-shape-artist-help-how-to-enter-names-real-keys ()
  "Mode-entry instructions use keys looked up from the maps."
  (dolist (case '((draw "DRAW mode: press d in MOVE mode.")
                  (select "SELECT mode: press C-n in MOVE mode.")
                  (shape-edit "EDIT mode: press C-n in MOVE mode, then press n to choose a shape, then press RET to lock it.")
                  (move "MOVE mode: press ESC in DRAW mode or press ESC in SELECT mode or press ESC in EDIT mode.")))
    (should (equal (shape-artist-help-how-to-enter (car case)) (cadr case))))
  (let ((shape-artist-move-map (copy-keymap shape-artist-move-map)))
    (define-key shape-artist-move-map (kbd "d") nil)
    (define-key shape-artist-move-map (kbd "D") #'shape-artist-enter-draw)
    (should (equal (shape-artist-help-how-to-enter 'draw)
                   "DRAW mode: press D in MOVE mode."))
    (define-key shape-artist-move-map (kbd "D") nil)
    (should (string-match-p "M-x shape-artist-enter-draw"
                            (shape-artist-help-how-to-enter 'draw)))))

;;; --- Cheat sheet ---

(defmacro test-shape-artist-help--with-cheatsheet (&rest body)
  "Run BODY after generating the cheat sheet; display and buffer are cleaned up."
  (declare (indent 0))
  `(unwind-protect
       (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
         (shape-artist-help-cheatsheet)
         (with-current-buffer shape-artist-help-cheatsheet-buffer-name
           ,@body))
     (when (get-buffer shape-artist-help-cheatsheet-buffer-name)
       (kill-buffer shape-artist-help-cheatsheet-buffer-name))))

(defun test-shape-artist-help--doc-line (command)
  "Return the first docstring line of COMMAND, computed independently."
  (car (split-string (documentation command t) "\n")))

(ert-deftest test-shape-artist-help-cheatsheet-buffer-is-read-only-and-shown ()
  "The command returns a read-only buffer with the documented name and shows it."
  (let ((shown nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (buffer &rest _) (setq shown buffer) nil)))
          (let ((buffer (shape-artist-help-cheatsheet)))
            (should (equal (buffer-name buffer) "*shape-artist-cheatsheet*"))
            (should (eq shown buffer))
            (with-current-buffer buffer
              (should buffer-read-only)
              (should (derived-mode-p 'special-mode))
              (should (looking-at "SHAPE ARTIST CHEAT SHEET")))))
      (when (get-buffer "*shape-artist-cheatsheet*")
        (kill-buffer "*shape-artist-cheatsheet*")))))

(ert-deftest test-shape-artist-help-cheatsheet-has-one-section-per-mode ()
  "Each mode gets a header, its spec hint and every documented key."
  (test-shape-artist-help--with-cheatsheet
    (let ((text (buffer-string))
          (last-position -1))
      (dolist (mode test-shape-artist-help--modes)
        (ert-info ((format "mode %s" mode))
          (let* ((header (format "== %s · %s =="
                                 (shape-artist--mode-property :label mode)
                                 (shape-artist--mode-property :activity mode)))
                 (position (string-match (regexp-quote header) text)))
            (should position)
            (should (> position last-position))
            (setq last-position position)
            (should (string-match-p
                     (regexp-quote (shape-artist--mode-property :hint mode))
                     text))
            (dolist (pair (shape-artist-help-mode-keys mode))
              (let ((key (shape-artist-help-display-key (car pair))))
                (should (string-match-p
                         (format "^  %s +%s$" (regexp-quote key)
                                 (regexp-quote
                                  (test-shape-artist-help--doc-line (cdr pair))))
                         (substring text position)))))))))))

(ert-deftest test-shape-artist-help-cheatsheet-is-generated-from-the-maps ()
  "A binding added to a map appears with its docstring summary."
  (defalias 'test-shape-artist-help--dummy-command
    (lambda () "Dummy summary line.\n\nSecond paragraph." (interactive) nil))
  (put 'test-shape-artist-help--dummy-command 'function-documentation
       "Dummy summary line.\n\nSecond paragraph.")
  (let ((shape-artist-draw-map (copy-keymap shape-artist-draw-map)))
    (define-key shape-artist-draw-map (kbd "w") #'test-shape-artist-help--dummy-command)
    (test-shape-artist-help--with-cheatsheet
      (should (string-match-p "^  w +Dummy summary line\\.$" (buffer-string)))
      (should-not (string-match-p "Second paragraph" (buffer-string)))))
  (test-shape-artist-help--with-cheatsheet
    (should-not (string-match-p "Dummy summary" (buffer-string)))))

(ert-deftest test-shape-artist-help-cheatsheet-regenerates-without-duplication ()
  "Running the command twice leaves one copy of the sheet."
  (test-shape-artist-help--with-cheatsheet
    (let ((first (buffer-string)))
      (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
        (shape-artist-help-cheatsheet))
      (should (equal (buffer-string) first))
      (should (= (how-many "SHAPE ARTIST CHEAT SHEET" (point-min)) 1)))))

(ert-deftest test-shape-artist-help-cheatsheet-marks-undocumented-commands ()
  "A command without a docstring gets a placeholder, not an error."
  (defalias 'test-shape-artist-help--bare-command (lambda () (interactive) nil))
  (let ((shape-artist-move-map (copy-keymap shape-artist-move-map)))
    (define-key shape-artist-move-map (kbd "w") #'test-shape-artist-help--bare-command)
    (test-shape-artist-help--with-cheatsheet
      (should (string-match-p "^  w +(undocumented)$" (buffer-string))))))

;;; --- Audit ---

(defun test-shape-artist-help--audit-summary (findings)
  "Return FINDINGS reduced to (KEY MODE) pairs for easy comparison."
  (mapcar (lambda (finding)
            (list (plist-get finding :key) (plist-get finding :mode)))
          findings))

(ert-deftest test-shape-artist-help-audit-accepts-correct-rows ()
  "Rows whose key is bound to the command in each mode produce no finding."
  (should-not (shape-artist-help-audit
               '(((move) "h" "Left" shape-artist-move-left)
                 ((draw) "h" "Left" shape-artist-draw-move-left)
                 ((move) "SPC" "Draw" shape-artist-enter-draw-and-anchor)
                 ((move) "RET" "Commit" shape-artist-commit-preview)
                 ((draw) "ESC" "Leave" shape-artist-cancel-and-exit)
                 ((select) "ESC" "Leave" shape-artist-select-cancel)
                 ((select) "RET" "Edit" shape-artist-select-confirm)
                 ((shape-edit) "H" "Move" shape-artist-shape-move-left)))))

(ert-deftest test-shape-artist-help-audit-reports-wrong-command ()
  "A key bound to another command is reported with both names."
  (let ((findings (shape-artist-help-audit
                   '(((move) "h" "Wrong" shape-artist-draw-move-left)))))
    (should (= (length findings) 1))
    (let ((finding (car findings)))
      (should (equal (plist-get finding :key) "h"))
      (should (eq (plist-get finding :command) 'shape-artist-draw-move-left))
      (should (eq (plist-get finding :mode) 'move))
      (should (string-match-p "bound to shape-artist-move-left"
                              (plist-get finding :problem)))
      (should (string-match-p "not shape-artist-draw-move-left"
                              (plist-get finding :problem)))
      (should (string-match-p "MOVE" (plist-get finding :problem))))))

(ert-deftest test-shape-artist-help-audit-reports-unbound-key ()
  "A key the documentation keymap does not bind is reported."
  (let ((findings (shape-artist-help-audit
                   '(((draw) "w" "Nothing" shape-artist-pick-tool)
                     ((move) "C-n" "Select" shape-artist-select-start)))))
    (should (equal (test-shape-artist-help--audit-summary findings)
                   '(("w" draw) ("C-n" move))))
    (should (string-match-p "not bound in the DRAW documentation keymap"
                            (plist-get (car findings) :problem)))
    (should (eq (plist-get (car findings) :command) 'shape-artist-pick-tool))))

(ert-deftest test-shape-artist-help-audit-expands-all-to-every-mode ()
  "An (all) row is checked in each mode and reported per failing mode."
  (let ((findings (shape-artist-help-audit
                   '(((all) "h" "Left" shape-artist-move-left)))))
    ;; draw binds h to another command, select binds nothing, shape-edit binds
    ;; h to yet another; only MOVE is right.
    (should (equal (test-shape-artist-help--audit-summary findings)
                   '(("h" draw) ("h" select) ("h" shape-edit))))
    (should (string-match-p "bound to shape-artist-draw-move-left"
                            (plist-get (nth 0 findings) :problem)))
    (should (string-match-p "not bound"
                            (plist-get (nth 1 findings) :problem)))
    (should (string-match-p "bound to shape-artist-shape-edit-left"
                            (plist-get (nth 2 findings) :problem)))))

(ert-deftest test-shape-artist-help-audit-skips-mnemonics ()
  "Multi-key and alternative-list strings are not real bindings and are skipped."
  (should-not (shape-artist-help-audit
               '(((all) "G h" "Guide" shape-artist-guide-create-horizontal)
                 ((all) "M w" "Save" shape-artist-metadata-save)
                 ((shape-edit) "H/J/K/L" "Move" shape-artist-shape-move)
                 ((move) "C-x C-s" "Save" save-buffer)
                 ((move) "" "Empty" ignore)))))

(ert-deftest test-shape-artist-help-audit-treats-esc-as-escape-event ()
  "The ESC mnemonic maps to the <escape> event stored in the keymaps."
  (should-not (shape-artist-help-audit
               '(((shape-edit) "ESC" "Exit" shape-artist-shape-edit-exit))))
  (should (shape-artist-help-audit
           '(((shape-edit) "ESC" "Exit" shape-artist-shape-edit-label)))))

(ert-deftest test-shape-artist-help-audit-handles-unknown-mode ()
  "A row for a mode without a documentation keymap is reported, not signalled."
  (let ((findings (shape-artist-help-audit
                   '(((nowhere) "h" "Left" shape-artist-move-left)))))
    (should (= (length findings) 1))
    (should (string-match-p "no documentation keymap"
                            (plist-get (car findings) :problem)))))

(ert-deftest test-shape-artist-help-audit-follows-the-keymaps ()
  "Changing the documentation keymap changes the verdict."
  (let ((rows '(((move) "w" "Draw" shape-artist-enter-draw-and-anchor))))
    (should (shape-artist-help-audit rows))
    (let ((shape-artist-move-map (copy-keymap shape-artist-move-map)))
      (define-key shape-artist-move-map (kbd "w") #'shape-artist-enter-draw-and-anchor)
      (should-not (shape-artist-help-audit rows)))))

(ert-deftest test-shape-artist-help-audit-real-table-returns-well-formed-findings ()
  "The audit runs over the real table and reports well-formed plists."
  (let ((findings (shape-artist-help-audit)))
    (should (proper-list-p findings))
    (dolist (finding findings)
      (should (stringp (plist-get finding :key)))
      (should (commandp (plist-get finding :command)))
      (should (memq (plist-get finding :mode) test-shape-artist-help--modes))
      (should (stringp (plist-get finding :problem)))
      (should (> (length (plist-get finding :problem)) 10)))
    ;; Guide and metadata mnemonics are never audited.
    (should-not (seq-some (lambda (finding)
                            (string-match-p " " (plist-get finding :key)))
                          findings))))

(ert-deftest test-shape-artist-help-audit-real-table-strict-gate ()
  "FUTURE STRICT GATE: the real quick-action key strings match the key maps.
Currently the table disagrees with the documentation keymaps (see
`shape-artist-help-audit'), so this is expected to fail.  Remove the
`:expected-result' marker once the table or the maps are fixed."
  :expected-result :failed
  (should-not (shape-artist-help-audit)))

;;; --- Small helpers ---

(ert-deftest test-shape-artist-help-display-key-and-labels ()
  "Escape is shown as ESC; labels come from the mode specs."
  (should (equal (shape-artist-help-display-key "<escape>") "ESC"))
  (should (equal (shape-artist-help-display-key "RET") "RET"))
  (dolist (case test-shape-artist-help--mode-labels)
    (should (equal (shape-artist-help--mode-label (car case)) (cdr case))))
  (should (equal (shape-artist-help--mode-label 'weird) "WEIRD")))

(ert-deftest test-shape-artist-help-more-key-prefers-hint-binding ()
  "The key that opens more help is the hint command's key when it is bound."
  (should (equal (shape-artist-help-more-key 'move) "?"))
  (let ((shape-artist-move-map (copy-keymap shape-artist-move-map)))
    (define-key shape-artist-move-map (kbd "?") nil)
    (define-key shape-artist-move-map (kbd "H") #'shape-artist-help-hint)
    (should (equal (shape-artist-help-more-key 'move) "H"))
    (should (string-suffix-p "H again for all"
                             (shape-artist-help-hint-string 'move)))))

(provide 'test-shape-artist-help)
;;; test-shape-artist-help.el ends here
