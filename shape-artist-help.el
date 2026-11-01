;;; shape-artist-help.el --- Key hints, cheat sheet and command palette -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Shape Artist Contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Discoverability helpers for Shape Artist.  Everything here is derived from
;; data the rest of the package already owns, so the help cannot silently go
;; stale:
;;
;; - The documentation keymaps (`shape-artist-move-map' and friends) are the
;;   canonical per-mode key lists.
;; - `shape-artist--mode-specs' supplies mode labels, activities and hints.
;; - `shape-artist--ui-quick-actions' supplies the command table behind the
;;   palette.
;;
;; Entry points:
;;
;; - `shape-artist-help-hint'       one-line hint in the echo area; pressed a
;;                                  second time it opens the palette.  It is
;;                                  meant to be bound to ?.
;; - `shape-artist-help-palette'    searchable palette of every action, grouped,
;;                                  annotated with live keys and intent
;;                                  keywords, including actions of other modes.
;; - `shape-artist-help-cheatsheet' generated read-only cheat sheet buffer.
;; - `shape-artist-help-audit'      report quick-action key strings that
;;                                  disagree with the documentation keymaps.
;;
;; Binding policy: this module binds nothing itself.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'shape-artist-keys)
(require 'shape-artist-ui)

;;; --- Key sources ---

(defconst shape-artist-help--mode-maps
  '((move . shape-artist-move-map)
    (draw . shape-artist-draw-map)
    (select . shape-artist-select-map)
    (shape-edit . shape-artist-shape-edit-map))
  "Alist mapping each mode to the variable holding its documentation keymap.")

(defconst shape-artist-help--live-aliases
  '((shape-artist-select-start shape-artist--dispatch-select-start (move))
    (shape-artist-ui-toggle shape-artist--dispatch-utility
                            (move select shape-edit))
    (shape-artist-undo shape-artist--dispatch-utility (draw))
    (shape-artist-commit-stay shape-artist--dispatch-commit-stay (draw)))
  "Commands whose live key is bound through a dispatcher, not a doc keymap.

Each element is (COMMAND DISPATCHER MODES).  The documentation keymaps do not
list these commands, but the non-Evil keymap `shape-artist-core-mode-map'
reaches COMMAND through DISPATCHER while one of MODES is active.")

(defconst shape-artist-help--movement-regexp
  "-\\(?:move\\|edit\\)-\\(left\\|down\\|up\\|right\\)\\'"
  "Regexp matching the names of cursor or shape movement commands.
Group 1 is the direction.")

(defun shape-artist-help--modes ()
  "Return the list of Shape Artist modes, in presentation order."
  (mapcar #'car shape-artist--mode-specs))

(defun shape-artist-help--mode-label (mode)
  "Return the display label of MODE, falling back to its upcased name."
  (or (shape-artist--mode-property :label mode)
      (upcase (format "%s" mode))))

(defun shape-artist-help--doc-map (mode)
  "Return the documentation keymap of MODE, or nil when MODE has none."
  (let ((var (cdr (assq mode shape-artist-help--mode-maps))))
    (and var (boundp var) (symbol-value var))))

(defun shape-artist-help-display-key (key)
  "Return KEY, a key description, in its friendly display form."
  (if (equal key "<escape>") "ESC" key))

(defun shape-artist-help--direction-rank (command)
  "Return the sort rank of movement COMMAND's direction, or nil."
  (when (string-match shape-artist-help--movement-regexp (symbol-name command))
    (seq-position '("left" "down" "up" "right") (match-string 1 (symbol-name command)))))

(defun shape-artist-help--shifted-p (key)
  "Return non-nil when the key description KEY contains an upper-case letter."
  (let ((case-fold-search nil))
    (string-match-p "[[:upper:]]" key)))

(defun shape-artist-help--key-less-p (a b)
  "Return non-nil when key/command pair A sorts before pair B.

Movement keys come first: the lower-case ones in h j k l order, then the
shifted ones.  All remaining keys follow, sorted by key description."
  (let ((rank-a (shape-artist-help--direction-rank (cdr a)))
        (rank-b (shape-artist-help--direction-rank (cdr b))))
    (cond
     ((and rank-a rank-b)
      (let ((shift-a (and (shape-artist-help--shifted-p (car a)) t))
            (shift-b (and (shape-artist-help--shifted-p (car b)) t)))
        (cond ((not (eq shift-a shift-b)) (not shift-a))
              ((/= rank-a rank-b) (< rank-a rank-b))
              (t (string< (car a) (car b))))))
     (rank-a t)
     (rank-b nil)
     (t (string< (car a) (car b))))))

(defun shape-artist-help-mode-keys (mode)
  "Return the (KEY-DESCRIPTION . COMMAND) pairs documented for MODE.

The pairs come from MODE's documentation keymap.  Movement keys (h j k l, then
H J K L) come first, followed by all other keys sorted by key description.
Every binding is kept: a command bound to several keys appears once per key.
Only bindings to command symbols are listed; prefix maps and menu items are
ignored."
  (let ((map (shape-artist-help--doc-map mode))
        (pairs nil))
    (when map
      (map-keymap
       (lambda (event binding)
         (when (and binding (symbolp binding) (not (keymapp binding)))
           (push (cons (key-description (vector event)) binding) pairs)))
       map))
    (sort pairs #'shape-artist-help--key-less-p)))

(defun shape-artist-help--live-key (command mode)
  "Return the live key of COMMAND in MODE reached via a dispatcher, or nil."
  (let* ((alias (assq command shape-artist-help--live-aliases))
         (dispatcher (nth 1 alias)))
    (when (and alias (memq mode (nth 2 alias))
               (boundp 'shape-artist-core-mode-map))
      (let ((key (where-is-internal dispatcher shape-artist-core-mode-map t)))
        (and key (key-description key))))))

(defun shape-artist-help-key-for (command &optional mode)
  "Return the display key of COMMAND in MODE, or nil when it has none.

MODE defaults to the current `shape-artist--mode' (or `move').  The
documentation keymap of MODE is consulted first; commands reached only through
a modal dispatcher (for example `shape-artist-select-start') fall back to the
live keymap."
  (let* ((mode (or mode shape-artist--mode 'move))
         (key (or (car (rassq command (shape-artist-help-mode-keys mode)))
                  (shape-artist-help--live-key command mode))))
    (and key (shape-artist-help-display-key key))))

;;; --- Echo-area hint ---

(defconst shape-artist-help--hint-priorities
  '((move
     ((shape-artist-move-left shape-artist-move-down shape-artist-move-up
                              shape-artist-move-right)
      . "move")
     (shape-artist-enter-draw-and-anchor . "draw")
     (shape-artist-select-start . "select")
     (shape-artist-commit-preview . "commit")
     (shape-artist-ui-show-outline . "outline")
     (shape-artist-ui-center-diagram . "center")
     (shape-artist-quit . "quit"))
    (draw
     ((shape-artist-draw-move-left shape-artist-draw-move-down
                                   shape-artist-draw-move-up
                                   shape-artist-draw-move-right)
      . "draw")
     (shape-artist-set-anchor-here . "anchor")
     (shape-artist-commit-and-exit . "commit")
     (shape-artist-cancel-and-exit . "cancel")
     (shape-artist-undo . "undo")
     (shape-artist-pick-tool . "tool"))
    (select
     ((shape-artist-select-next shape-artist-select-prev) . "next/prev")
     (shape-artist-select-toggle-current . "mark")
     (shape-artist-select-confirm . "edit")
     (shape-artist-select-cancel . "cancel")
     (shape-artist-select-all . "all")
     (shape-artist-select-clear-selected . "clear")
     (shape-artist-ui-show-outline . "outline"))
    (shape-edit
     ((shape-artist-shape-move-left shape-artist-shape-move-down
                                    shape-artist-shape-move-up
                                    shape-artist-shape-move-right)
      . "move")
     (shape-artist-shape-cycle-style . "style")
     (shape-artist-shape-edit-label . "label")
     (shape-artist-shape-edit-exit . "finish")
     ((shape-artist-shape-resize-wider shape-artist-shape-resize-narrower)
      . "wider/narrower")
     (shape-artist-shape-set-style . "set style")))
  "Per-mode priority list for the echo-area hint, most useful first.

Each element after the mode is (COMMANDS . WORD).  COMMANDS is a command or a
list of commands whose keys are shown together (joined with /); WORD says what
they do.  Keys are never written here: they are looked up from the key maps, and
an entry whose commands have no key in the mode is skipped, so a stale entry
cannot show a wrong key.")

(defconst shape-artist-help-hint-max-keys 8
  "Maximum number of key groups shown by `shape-artist-help-hint-string'.")

(defconst shape-artist-help-hint-max-width 80
  "Maximum width in columns of the string from `shape-artist-help-hint-string'.")

(defun shape-artist-help--hint-entry-text (entry mode)
  "Return the \"KEYS WORD\" text for hint ENTRY in MODE, or nil if unbound."
  (let* ((commands (if (listp (car entry)) (car entry) (list (car entry))))
         (keys (delq nil (mapcar (lambda (command)
                                   (shape-artist-help-key-for command mode))
                                 commands))))
    (and keys (format "%s %s" (string-join keys "/") (cdr entry)))))

(defun shape-artist-help-more-key (mode)
  "Return the display key that opens the full help in MODE (default ?)."
  (or (shape-artist-help-key-for 'shape-artist-help-hint mode)
      (shape-artist-help-key-for 'shape-artist-ui-command-menu mode)
      "?"))

(defun shape-artist-help-hint-string (mode)
  "Return a one-line hint for MODE, at most 80 columns wide.

The line starts with MODE's label, lists at most 8 of the most useful keys
taken from the curated priority list and filtered through the key maps, and
ends with \"? again for all\"."
  (let* ((entries (delq nil
                        (mapcar (lambda (entry)
                                  (shape-artist-help--hint-entry-text entry mode))
                                (cdr (assq mode shape-artist-help--hint-priorities)))))
         (entries (seq-take entries shape-artist-help-hint-max-keys))
         (prefix (format "%s: " (shape-artist-help--mode-label mode)))
         (suffix (format "%s again for all" (shape-artist-help-more-key mode)))
         (build (lambda (items)
                  (concat prefix (mapconcat #'identity (append items (list suffix))
                                            " · ")))))
    (while (and entries
                (> (string-width (funcall build entries))
                   shape-artist-help-hint-max-width))
      (setq entries (butlast entries)))
    (funcall build entries)))

;;;###autoload
(defun shape-artist-help-hint ()
  "Show a one-line key hint; pressed again immediately, open the palette.

The first press shows the hint for the current mode in the echo area.  Pressing
the same key again right away (when `last-command' is this command) opens
`shape-artist-help-palette'.  Intended to be bound to ?."
  (interactive)
  (if (eq last-command 'shape-artist-help-hint)
      (shape-artist-help-palette)
    (message "%s" (shape-artist-help-hint-string
                   (or shape-artist--mode 'move)))))

;;; --- Palette v2 ---

(defconst shape-artist-help--intent-keywords
  '((shape-artist-ui-show "sidebar" "panel" "info" "show")
    (shape-artist-ui-toggle "sidebar" "hide" "panel")
    (shape-artist-ui-show-outline "outline" "tree" "structure")
    (shape-artist-ui-center-diagram "center" "view" "scroll" "find")
    (shape-artist-shapes-parse "refresh" "detect" "parse" "reload")
    (shape-artist-enter-draw-and-anchor "draw" "box" "rectangle" "start" "line")
    (shape-artist-enter-draw "draw" "mode" "freehand")
    (shape-artist-select-start "select" "pick" "choose" "shape")
    (shape-artist-commit-and-exit "commit" "finish" "box" "line")
    (shape-artist-commit-stay "commit" "keep" "repeat" "box")
    (shape-artist-pick-tool "tool" "character" "brush")
    (shape-artist-undo "undo" "revert" "back")
    (shape-artist-select-next "next" "select" "walk")
    (shape-artist-select-prev "previous" "select" "walk")
    (shape-artist-select-toggle-current "mark" "toggle" "select" "group")
    (shape-artist-select-all "all" "select" "group")
    (shape-artist-select-clear-selected "clear" "deselect" "select")
    (shape-artist-select-confirm "lock" "edit" "select" "transform")
    (shape-artist-shape-move "move" "shift" "translate" "group")
    (shape-artist-shape-edit-label "label" "text" "rename" "caption")
    (shape-artist-shape-resize "resize" "size" "grow" "shrink")
    (shape-artist-shape-cycle-style "style" "arrow" "border" "restyle")
    (shape-artist-shape-set-style "style" "border" "arrow" "restyle")
    (shape-artist-shape-edit-exit "exit" "finish" "leave")
    (shape-artist-guide-create-horizontal "guide" "align" "row" "ruler")
    (shape-artist-guide-create-vertical "guide" "align" "column" "ruler")
    (shape-artist-guide-delete-nearest "guide" "delete" "remove")
    (shape-artist-guide-clear-all "guide" "delete" "clear" "remove")
    (shape-artist-snap-toggle "snap" "guide" "align" "magnet")
    (shape-artist-grid-snap-toggle "snap" "grid" "align" "magnet")
    (shape-artist-guide-toggle-visibility "guide" "hide" "show" "visible")
    (shape-artist-grid-toggle "grid" "overlay" "show" "hide")
    (shape-artist-guide-adopt-selection-nearest "guide" "align" "attach" "adopt")
    (shape-artist-guide-clear-adoptions "guide" "detach" "unalign" "adopt")
    (shape-artist-guide-edit-properties "guide" "edit" "properties")
    (shape-artist-guide-set-label "guide" "label" "name")
    (shape-artist-guide-set-snap-distance "guide" "snap" "distance")
    (shape-artist-guide-toggle-nearest-visibility "guide" "hide" "show")
    (shape-artist-guides-save "guide" "save" "write")
    (shape-artist-guides-load "guide" "load" "read" "restore")
    (shape-artist-guides-undo "guide" "undo" "revert")
    (shape-artist-guides-redo "guide" "redo" "reapply")
    (shape-artist-metadata-save "metadata" "ids" "save" "write")
    (shape-artist-metadata-load "metadata" "ids" "load" "restore")
    (shape-artist-metadata-delete-saved "metadata" "ids" "delete" "forget"))
  "Alist mapping a command to short intent keywords shown in the palette.

The keywords describe what a user might be trying to do (align, arrow, delete,
style, undo, select ...) so the palette can be scanned by intent, not only by
command name.")

(defconst shape-artist-help--group-order
  '("This mode" "Navigate & view" "Guides & grid" "Metadata" "Other modes")
  "Palette group names in display order.")

(defconst shape-artist-help--mode-entry-steps
  '((move " or " (shape-artist-cancel-and-exit draw "in DRAW mode")
          (shape-artist-select-cancel select "in SELECT mode")
          (shape-artist-shape-edit-exit shape-edit "in EDIT mode"))
    (draw ", then " (shape-artist-enter-draw move "in MOVE mode"))
    (select ", then " (shape-artist-select-start move "in MOVE mode"))
    (shape-edit ", then "
                (shape-artist-select-start move "in MOVE mode")
                (shape-artist-select-next select "to choose a shape")
                (shape-artist-select-confirm select "to lock it")))
  "How to reach each mode, as data so the keys are looked up, not written.

Each element is (MODE JOINER STEP...).  A STEP is (COMMAND LOOKUP-MODE WHEN):
the key of COMMAND is looked up in LOOKUP-MODE and WHEN says when to press
it.  The steps are joined with JOINER.")

(defun shape-artist-help--collect-actions (&optional current)
  "Return the full action set, united by command, as a list of plists.

`shape-artist--ui-quick-actions' is evaluated once per mode with
`shape-artist--mode' bound to that mode (CURRENT, default the active mode,
first so its actions keep source order at the front).  Each plist has :command,
:key (the table mnemonic), :desc and :modes (the modes in which the action is
available).  When several table rows share a command, the row whose key is a
live binding supplies :key and :desc; otherwise the first row does."
  (let* ((current (or current shape-artist--mode 'move))
         (modes (cons current (remq current (shape-artist-help--modes))))
         (entries nil))
    (dolist (mode modes)
      (let ((shape-artist--mode mode))
        (dolist (action (shape-artist--ui-quick-actions))
          (let* ((command (nth 3 action))
                 (entry (assq command entries)))
            (if entry
                (let ((plist (cdr entry)))
                  (unless (memq mode (plist-get plist :modes))
                    (plist-put plist :modes
                               (append (plist-get plist :modes) (list mode))))
                  (when (and (not (plist-get plist :live))
                             (equal (nth 1 action)
                                    (shape-artist-help-key-for command mode)))
                    (plist-put plist :key (nth 1 action))
                    (plist-put plist :desc (nth 2 action))
                    (plist-put plist :live t)))
              (push (cons command
                          (list :command command
                                :key (nth 1 action)
                                :desc (nth 2 action)
                                :modes (list mode)
                                :live (and (equal (nth 1 action)
                                                  (shape-artist-help-key-for
                                                   command mode))
                                           t)))
                    entries))))))
    (mapcar #'cdr (nreverse entries))))

(defun shape-artist-help--entry-group (entry current)
  "Return the palette group name for action ENTRY when in mode CURRENT."
  (let ((name (symbol-name (plist-get entry :command)))
        (modes (plist-get entry :modes)))
    (cond
     ((not (memq current modes)) "Other modes")
     ((string-match-p "\\`shape-artist-\\(?:guides?\\|snap\\|grid\\)-" name)
      "Guides & grid")
     ((string-prefix-p "shape-artist-metadata-" name) "Metadata")
     ((>= (length modes) (length (shape-artist-help--modes))) "Navigate & view")
     (t "This mode"))))

(defun shape-artist-help--palette-entries (&optional current)
  "Return palette entries for mode CURRENT, grouped and in source order.

Each entry is the plist from `shape-artist-help--collect-actions' plus :group
and :candidate (the unique completion string)."
  (let* ((current (or current shape-artist--mode 'move))
         (entries (mapcar (lambda (entry)
                            (append (list :group
                                          (shape-artist-help--entry-group
                                           entry current))
                                    entry))
                          (shape-artist-help--collect-actions current)))
         (ordered nil)
         (seen nil))
    (dolist (group shape-artist-help--group-order)
      (dolist (entry entries)
        (when (equal (plist-get entry :group) group)
          (let* ((desc (plist-get entry :desc))
                 (candidate (if (member desc seen)
                                (format "%s [%s]" desc (plist-get entry :key))
                              desc)))
            (push candidate seen)
            (push (append (list :candidate candidate) entry) ordered)))))
    (nreverse ordered)))

(defun shape-artist-help--entry-key (entry current)
  "Return the live key of ENTRY, preferring mode CURRENT, else its table key."
  (let* ((modes (plist-get entry :modes))
         (mode (if (memq current modes) current (car modes))))
    (or (shape-artist-help-key-for (plist-get entry :command) mode)
        (plist-get entry :key))))

(defun shape-artist-help--entry-annotation (entry current)
  "Return the palette annotation string for ENTRY when in mode CURRENT."
  (let* ((modes (plist-get entry :modes))
         (keywords (cdr (assq (plist-get entry :command)
                              shape-artist-help--intent-keywords))))
    (concat "  "
            (shape-artist-help--entry-key entry current)
            (unless (memq current modes)
              (format " (%s mode only)"
                      (mapconcat #'shape-artist-help--mode-label modes "/")))
            (when keywords
              (concat "  · " (string-join keywords ", "))))))

(defun shape-artist-help--palette-table (entries current)
  "Return a programmed completion table for palette ENTRIES in mode CURRENT.

The table supplies the `shape-artist-action' category, a group function, an
annotation function with the live key and intent keywords, and keeps ENTRIES in
source order."
  (let ((candidates (mapcar (lambda (entry) (plist-get entry :candidate)) entries)))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          `(metadata
            (category . shape-artist-action)
            (group-function
             . ,(lambda (candidate transform)
                  (if transform
                      candidate
                    (plist-get (shape-artist-help--entry-for candidate entries)
                               :group))))
            (annotation-function
             . ,(lambda (candidate)
                  (let ((entry (shape-artist-help--entry-for candidate entries)))
                    (and entry
                         (shape-artist-help--entry-annotation entry current)))))
            (display-sort-function . identity)
            (cycle-sort-function . identity))
        (complete-with-action action candidates string predicate)))))

(defun shape-artist-help--entry-for (candidate entries)
  "Return the entry in ENTRIES whose :candidate is CANDIDATE, or nil."
  (seq-find (lambda (entry) (equal (plist-get entry :candidate) candidate))
            entries))

(defun shape-artist-help-how-to-enter (mode)
  "Return a sentence explaining how to get into MODE from another mode."
  (let ((spec (cdr (assq mode shape-artist-help--mode-entry-steps))))
    (format "%s mode: %s."
            (shape-artist-help--mode-label mode)
            (mapconcat
             (lambda (step)
               (format "press %s %s"
                       (or (shape-artist-help-key-for (nth 0 step) (nth 1 step))
                           (format "M-x %s" (nth 0 step)))
                       (nth 2 step)))
             (cdr spec) (car spec)))))

(defun shape-artist-help--act (entry current)
  "Run palette ENTRY if it is available in mode CURRENT, else explain how.

Return the command's value, or the explanatory message for another mode's
action."
  (if (memq current (plist-get entry :modes))
      (call-interactively (plist-get entry :command))
    (let ((text (format "%s is a %s action.  %s"
                        (plist-get entry :desc)
                        (mapconcat #'shape-artist-help--mode-label
                                   (plist-get entry :modes) "/")
                        (shape-artist-help-how-to-enter
                         (car (plist-get entry :modes))))))
      (message "%s" text)
      text)))

;;;###autoload
(defun shape-artist-help-palette ()
  "Choose any Shape Artist action from a grouped, annotated palette.

Actions of the current mode run when chosen.  Actions that belong to another
mode are listed too, marked such as \"(SELECT mode only)\"; choosing one does not
run it but explains how to get into that mode."
  (interactive)
  (let* ((current (or shape-artist--mode 'move))
         (entries (shape-artist-help--palette-entries current))
         (choice (completing-read
                  "Shape Artist action: "
                  (shape-artist-help--palette-table entries current)
                  nil t))
         (entry (shape-artist-help--entry-for choice entries)))
    (when entry
      (shape-artist-help--act entry current))))

;;; --- Cheat sheet ---

(defconst shape-artist-help-cheatsheet-buffer-name "*shape-artist-cheatsheet*"
  "Name of the buffer shown by `shape-artist-help-cheatsheet'.")

(defun shape-artist-help--doc-summary (command)
  "Return the first docstring line of COMMAND, or a placeholder."
  (let ((doc (ignore-errors (documentation command t))))
    (if (and (stringp doc) (not (string-empty-p doc)))
        (car (split-string doc "\n"))
      "(undocumented)")))

(defun shape-artist-help--cheatsheet-section (mode)
  "Return the cheat sheet text for MODE."
  (let* ((keys (shape-artist-help-mode-keys mode))
         (width (apply #'max 3 (mapcar (lambda (pair)
                                         (string-width
                                          (shape-artist-help-display-key (car pair))))
                                       keys))))
    (concat
     (format "== %s · %s ==\n" (shape-artist-help--mode-label mode)
             (shape-artist--mode-property :activity mode))
     (format "%s\n\n" (shape-artist--mode-property :hint mode))
     (mapconcat
      (lambda (pair)
        (format (format "  %%-%ds  %%s\n" width)
                (shape-artist-help-display-key (car pair))
                (shape-artist-help--doc-summary (cdr pair))))
      keys "")
     "\n")))

;;;###autoload
(defun shape-artist-help-cheatsheet ()
  "Display a generated cheat sheet with one section per mode.

Each section shows the mode's label, activity and hint from
`shape-artist--mode-specs', then every key of its documentation keymap with the
first line of the bound command's docstring.  Nothing is written by hand."
  (interactive)
  (let ((buffer (get-buffer-create shape-artist-help-cheatsheet-buffer-name)))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "SHAPE ARTIST CHEAT SHEET\n"
                "(generated from the key maps; ? shows a hint, ? twice opens "
                "the palette)\n\n")
        (dolist (mode (shape-artist-help--modes))
          (insert (shape-artist-help--cheatsheet-section mode)))
        (goto-char (point-min))))
    (display-buffer buffer)
    buffer))

;;; --- Audit ---

(defun shape-artist-help--audit-key (key-string)
  "Return the key vector for quick-action KEY-STRING, or nil if not auditable.

Only single real key bindings are auditable.  Palette mnemonics that contain a
space (other than the key SPC itself) or list alternatives with / are skipped."
  (unless (or (string-match-p "/" key-string)
              (and (string-match-p " " key-string)
                   (not (equal key-string "SPC"))))
    (condition-case nil
        (kbd (if (equal key-string "ESC") "<escape>" key-string))
      (error nil))))

(defun shape-artist-help--all-quick-actions ()
  "Return every row of the quick-action table, duplicates removed."
  (let ((rows nil))
    (dolist (mode (shape-artist-help--modes))
      (let ((shape-artist--mode mode))
        (dolist (action (shape-artist--ui-quick-actions))
          (unless (member action rows)
            (push action rows)))))
    (nreverse rows)))

(defun shape-artist-help-audit (&optional actions)
  "Return the quick-action key strings that disagree with the key maps.

ACTIONS is a list of (MODES KEY-STRING DESCRIPTION COMMAND) rows and defaults
to the whole table behind `shape-artist--ui-quick-actions'.  A row whose
MODES is (all) is checked in every mode.  Only single-key strings that are
real bindings are checked; multi-key palette mnemonics such as \"G h\" are
skipped.  Each finding is a plist (:key :command :mode :problem) where
:problem says whether the key is unbound in the mode's documentation keymap or
bound to a different command."
  (let ((actions (or actions (shape-artist-help--all-quick-actions)))
        (findings nil))
    (dolist (action actions)
      (let* ((key-string (nth 1 action))
             (command (nth 3 action))
             (vector (and (stringp key-string)
                          (shape-artist-help--audit-key key-string)))
             (modes (if (memq 'all (nth 0 action))
                        (shape-artist-help--modes)
                      (nth 0 action))))
        (when vector
          (dolist (mode modes)
            (let* ((map (shape-artist-help--doc-map mode))
                   (binding (and map (lookup-key map vector)))
                   (problem
                    (cond
                     ((null map)
                      (format "no documentation keymap for mode %s" mode))
                     ((not (and binding (symbolp binding)))
                      (format "%s is not bound in the %s documentation keymap"
                              key-string (shape-artist-help--mode-label mode)))
                     ((not (eq binding command))
                      (format "%s is bound to %s in the %s documentation keymap, not %s"
                              key-string binding
                              (shape-artist-help--mode-label mode) command)))))
              (when problem
                (push (list :key key-string :command command
                            :mode mode :problem problem)
                      findings)))))))
    (nreverse findings)))

(provide 'shape-artist-help)
;;; shape-artist-help.el ends here
