;;; shape-artist-tutorial.el --- Interactive tutorial for Shape Artist -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Shape Artist Contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `shape-artist-tutorial' opens a practice canvas next to a checklist.  The
;; checklist advances by watching the diagram you actually draw: after each
;; command the practice buffer is re-parsed (debounced by an idle timer) and
;; every task is checked against the shapes found, not against key presses, so
;; any route to the result counts.
;;
;; Tasks, in order: draw a box, label it, draw a second box, connect the two,
;; move a box, restyle a box, undo.
;;
;; The task predicates are pure functions of a STATE plist:
;;
;;   (:shapes LIST :initial-boxes ALIST :commands LIST)
;;
;; - :shapes        the parsed shapes of the practice buffer.
;; - :initial-boxes ((INDEX . (X . Y)) ...), the position at which each box was
;;                  first seen.  Boxes are matched by order of first appearance:
;;                  the K-th rectangle in :shapes is compared with entry K, and
;;                  the caller aligns the alist with the current order of
;;                  :shapes (see `shape-artist-tutorial--track-boxes').
;; - :commands      the commands recorded by the post-command hook.
;;
;; Progress (completed task ids and a first-run flag) is stored in
;; `shape-artist-tutorial-progress-file'.  `shape-artist-tutorial-maybe-hint'
;; shows a one-time pointer to the help and the tutorial; it is meant to be
;; called when a Shape Artist session starts.
;;
;; Instructions are templates; {MODE:COMMAND} is replaced by the key of COMMAND
;; in MODE, looked up from the key maps through `shape-artist-help', so the
;; tutorial names real keys.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'shape-artist-core)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-help)

;;; --- Options ---

(defcustom shape-artist-first-run-hint t
  "When non-nil, show a one-time pointer to the help and the tutorial."
  :type 'boolean
  :group 'shape-artist)

(defcustom shape-artist-tutorial-progress-file
  (locate-user-emacs-file "shape-artist-tutorial.eld")
  "File in which the tutorial remembers completed tasks and the first-run flag."
  :type 'file
  :group 'shape-artist)

(defcustom shape-artist-tutorial-eval-delay 0.3
  "Idle seconds to wait after a command before re-checking the tutorial tasks."
  :type 'number
  :group 'shape-artist)

;;; --- Task model ---

(defconst shape-artist-tutorial--tasks
  '((:id draw-box
     :title "Draw a box"
     :instruction "In MOVE mode press {move:shape-artist-enter-draw-and-anchor} to drop an anchor at the cursor.  Steer to the opposite corner with {draw:shape-artist-draw-move-right} (right) and {draw:shape-artist-draw-move-down} (down), then press {draw:shape-artist-commit-and-exit} to commit the box."
     :hint "Make it at least 3 cells wide and tall.  {draw:shape-artist-cancel-and-exit} cancels and returns to MOVE mode.")
    (:id label-box
     :title "Label the box"
     :instruction "Press {move:shape-artist-select-start} to enter SELECT mode, {select:shape-artist-select-next} to step to the box, {select:shape-artist-select-confirm} to lock it, then {shape-edit:shape-artist-shape-edit-label} and type the label text."
     :hint "Any text will do.  Press {shape-edit:shape-artist-shape-edit-exit} afterwards to return to MOVE mode.")
    (:id second-box
     :title "Draw a second box"
     :instruction "Move to empty space and draw another box the same way: {move:shape-artist-enter-draw-and-anchor}, move, then {draw:shape-artist-commit-and-exit}."
     :hint "Leave a few blank cells between the boxes; you will connect them next.")
    (:id connect
     :title "Connect the boxes"
     :instruction "Put the cursor on the cell right beside the first box's wall, press {move:shape-artist-enter-draw-and-anchor}, move across with {draw:shape-artist-draw-move-right} until you are right beside the second box's wall, then press {draw:shape-artist-commit-and-exit}."
     :hint "Both ends of the line must touch a box wall, with no gap.  Boxes on the same row make this easy.")
    (:id move-box
     :title "Move a box"
     :instruction "Select a box ({move:shape-artist-select-start}, {select:shape-artist-select-next}, {select:shape-artist-select-confirm}), then nudge it with {shape-edit:shape-artist-shape-move-right} (right) or {shape-edit:shape-artist-shape-move-down} (down) a few times."
     :hint "The shifted direction keys move the whole shape; the plain ones only walk around inside it.  {shape-edit:shape-artist-shape-edit-exit} leaves SHAPE-EDIT.")
    (:id restyle
     :title "Change a box's border style"
     :instruction "Lock a box as before, then press {shape-edit:shape-artist-shape-cycle-style} to cycle the border style."
     :hint "A different border, for example ==== instead of ----, means it worked.  {shape-edit:shape-artist-shape-set-style} asks for a style by name.")
    (:id undo
     :title "Undo a change"
     :instruction "Enter DRAW mode with {move:shape-artist-enter-draw}, then press {draw:shape-artist-undo} to undo the last edit."
     :hint "The ordinary Emacs undo command counts as well.  {draw:shape-artist-cancel-and-exit} leaves DRAW mode."))
  "The tutorial tasks, in order.

Each task is a plist with :id, :title, :instruction and :hint.  Instruction
and hint are templates expanded by `shape-artist-tutorial--expand'.")

(defun shape-artist-tutorial--task-ids ()
  "Return the ids of all tutorial tasks, in order."
  (mapcar (lambda (task) (plist-get task :id)) shape-artist-tutorial--tasks))

(defun shape-artist-tutorial--task (task-id)
  "Return the task plist with id TASK-ID, or nil."
  (seq-find (lambda (task) (eq (plist-get task :id) task-id))
            shape-artist-tutorial--tasks))

(defun shape-artist-tutorial--expand (template)
  "Expand the {MODE:COMMAND} placeholders in TEMPLATE into key descriptions.

A command without a key in MODE is shown as an `execute-extended-command'
invocation of its name."
  (replace-regexp-in-string
   "{\\([a-z-]+\\):\\([^}]+\\)}"
   (lambda (match)
     (save-match-data
       (string-match "{\\([a-z-]+\\):\\([^}]+\\)}" match)
       (let ((mode (intern (match-string 1 match)))
             (command (intern (match-string 2 match))))
         (or (shape-artist-help-key-for command mode)
             (format "M-x %s" command)))))
   template t t))

(defun shape-artist-tutorial--instruction (task-id)
  "Return the expanded instruction text of TASK-ID."
  (shape-artist-tutorial--expand
   (plist-get (shape-artist-tutorial--task task-id) :instruction)))

(defun shape-artist-tutorial--hint (task-id)
  "Return the expanded hint text of TASK-ID."
  (shape-artist-tutorial--expand
   (plist-get (shape-artist-tutorial--task task-id) :hint)))

;;; --- Task predicates (pure) ---

(defun shape-artist-tutorial--rectangles (shapes)
  "Return the rectangles among SHAPES, in order."
  (seq-filter #'shape-artist-diagram-rectangle-p shapes))

(defun shape-artist-tutorial--label-inside-p (label rectangle)
  "Return non-nil when the position of LABEL is strictly inside RECTANGLE."
  (let ((x (oref label x))
        (y (oref label y))
        (rx (oref rectangle x))
        (ry (oref rectangle y)))
    (and (< rx x (+ rx (oref rectangle width)))
         (< ry y (+ ry (oref rectangle height))))))

(defun shape-artist-tutorial--connected-p (edge)
  "Return non-nil when both ends of EDGE attach to two different rectangles."
  (let ((start (plist-get (oref edge start-attachment) :rect))
        (end (plist-get (oref edge end-attachment) :rect)))
    (and start end (not (equal start end)))))

(defun shape-artist-tutorial--moved-p (rectangles initial-boxes)
  "Return non-nil when a rectangle of RECTANGLES left its initial position.

The K-th rectangle is compared with the K-th entry of INITIAL-BOXES, an alist
of (INDEX . (X . Y)).  A rectangle without an entry has not moved."
  (let ((index -1))
    (seq-some
     (lambda (rectangle)
       (setq index (1+ index))
       (let ((initial (cdr (assq index initial-boxes))))
         (and initial
              (not (equal initial (cons (oref rectangle x)
                                        (oref rectangle y)))))))
     rectangles)))

(defun shape-artist-tutorial--task-complete-p (task-id state)
  "Return non-nil when task TASK-ID is complete in STATE.

STATE is a plist (:shapes LIST :initial-boxes ALIST :commands LIST); see the
Commentary for the exact meaning.  An unknown TASK-ID is never complete."
  (let* ((shapes (plist-get state :shapes))
         (rectangles (shape-artist-tutorial--rectangles shapes)))
    (pcase task-id
      ('draw-box (>= (length rectangles) 1))
      ('label-box
       (and (seq-some
             (lambda (shape)
               (and (shape-artist-diagram-label-p shape)
                    (seq-some (lambda (rectangle)
                                (shape-artist-tutorial--label-inside-p
                                 shape rectangle))
                              rectangles)))
             shapes)
            t))
      ('second-box (>= (length rectangles) 2))
      ('connect
       (and (seq-some (lambda (shape)
                        (and (shape-artist-diagram-edge-p shape)
                             (shape-artist-tutorial--connected-p shape)))
                      shapes)
            t))
      ('move-box
       (and (shape-artist-tutorial--moved-p rectangles
                                            (plist-get state :initial-boxes))
            t))
      ('restyle
       (and (seq-some (lambda (rectangle)
                        (let ((style (oref rectangle detected-style)))
                          (and style (not (eq style 'standard)))))
                      rectangles)
            t))
      ('undo
       (and (seq-some (lambda (command)
                        (memq command '(undo shape-artist-undo)))
                      (plist-get state :commands))
            t))
      (_ nil))))

;;; --- Box tracking ---

(defun shape-artist-tutorial--remove-first (item list)
  "Return LIST without its first element `equal' to ITEM."
  (let ((found nil))
    (seq-remove (lambda (element)
                  (and (not found) (equal element item) (setq found t)))
                list)))

(defun shape-artist-tutorial--track-boxes (tracked positions)
  "Update TRACKED boxes with the current rectangle POSITIONS.

TRACKED is a list of (INITIAL . CURRENT) pairs of (X . Y) conses in order of
first appearance.  POSITIONS lists the (X . Y) of the current rectangles.
Boxes that stayed put keep their record; boxes that vanished from their old
spot are paired, in order, with the positions nobody claimed (they moved) and
dropped when none is left; leftover positions are new boxes.  Return the new
TRACKED list."
  (let ((free (copy-sequence positions))
        (stayed nil)
        (result nil))
    ;; Pass 1: boxes that did not move.
    (dolist (record tracked)
      (when (member (cdr record) free)
        (setq free (shape-artist-tutorial--remove-first (cdr record) free))
        (push record stayed)))
    ;; Pass 2: boxes that moved take the free positions in order.
    (dolist (record tracked)
      (cond
       ((memq record stayed) (push record result))
       (free (push (cons (car record) (pop free)) result))))
    ;; Pass 3: whatever is left is new.
    (dolist (position free)
      (push (cons position position) result))
    (nreverse result)))

(defun shape-artist-tutorial--initial-boxes (tracked positions)
  "Return the :initial-boxes alist aligning TRACKED with POSITIONS.

The result lists (INDEX . INITIAL) for the rectangle at each index of
POSITIONS, where INITIAL is the position at which that box was first seen."
  (let ((remaining (copy-sequence tracked))
        (index -1)
        (alist nil))
    (dolist (position positions)
      (setq index (1+ index))
      (let ((record (seq-find (lambda (record) (equal (cdr record) position))
                              remaining)))
        (when record
          (setq remaining (remq record remaining))
          (push (cons index (car record)) alist))))
    (nreverse alist)))

;;; --- Progress persistence ---

(defun shape-artist-tutorial--load-progress ()
  "Return the saved progress as a plist (:completed IDS :first-run-seen FLAG).

A missing, unreadable or corrupt file yields no progress; unknown task ids are
dropped.  Never signals an error."
  (let ((completed nil)
        (seen nil))
    (condition-case nil
        (let ((file shape-artist-tutorial-progress-file))
          (when (and (stringp file) (file-readable-p file))
            (let ((data (with-temp-buffer
                          (insert-file-contents file)
                          (goto-char (point-min))
                          (read (current-buffer)))))
              (when (and (proper-list-p data) (cl-evenp (length data)))
                (let ((ids (plist-get data :completed)))
                  (when (proper-list-p ids)
                    (setq completed
                          (seq-filter (lambda (id)
                                        (memq id (shape-artist-tutorial--task-ids)))
                                      (delete-dups (copy-sequence ids))))))
                (setq seen (and (plist-get data :first-run-seen) t))))))
      (error (setq completed nil seen nil)))
    (list :completed completed :first-run-seen seen)))

(defun shape-artist-tutorial--save-progress (progress)
  "Write PROGRESS, a plist (:completed IDS :first-run-seen FLAG), to disk.

Create parent directories as needed.  Return non-nil on success; never signal
an error, so this is safe to call from hooks and timers."
  (condition-case nil
      (let ((file shape-artist-tutorial-progress-file))
        (make-directory (file-name-directory (expand-file-name file)) t)
        (with-temp-buffer
          (insert ";; Shape Artist tutorial progress -- generated, safe to delete\n")
          (let ((print-length nil) (print-level nil))
            (prin1 (list :completed (plist-get progress :completed)
                         :first-run-seen (and (plist-get progress :first-run-seen) t))
                   (current-buffer)))
          (insert "\n")
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (point-min) (point-max) file nil 'silent)))
        t)
    (error nil)))

(defun shape-artist-tutorial--persist-completed (ids)
  "Merge the task IDS into the saved progress, keeping the first-run flag."
  (let* ((progress (shape-artist-tutorial--load-progress))
         (merged (seq-filter
                  (lambda (id)
                    (or (memq id ids)
                        (memq id (plist-get progress :completed))))
                  (shape-artist-tutorial--task-ids))))
    (shape-artist-tutorial--save-progress
     (list :completed merged
           :first-run-seen (plist-get progress :first-run-seen)))))

;;; --- First-run hint ---

(defvar shape-artist-tutorial--hint-shown nil
  "Non-nil once the first-run hint was shown in this Emacs session.")

(defun shape-artist-tutorial-maybe-hint ()
  "Show a one-line first-run hint once, then remember that it was shown.

Do nothing unless `shape-artist-first-run-hint' is non-nil, Emacs is
interactive and the first-run flag in the progress file is unset.  Return
non-nil when the hint was shown."
  (when (and shape-artist-first-run-hint
             (not noninteractive)
             (not shape-artist-tutorial--hint-shown)
             (not (plist-get (shape-artist-tutorial--load-progress)
                             :first-run-seen)))
    (setq shape-artist-tutorial--hint-shown t)
    (message "Shape Artist: %s shows keys, M-x shape-artist-tutorial teaches the basics"
             (shape-artist-help-more-key 'move))
    (shape-artist-tutorial--save-progress
     (list :completed (plist-get (shape-artist-tutorial--load-progress)
                                 :completed)
           :first-run-seen t))
    t))

;;; --- Session state ---

(defconst shape-artist-tutorial-practice-buffer-name "*shape-artist-tutorial*"
  "Name of the tutorial practice canvas buffer.")

(defconst shape-artist-tutorial-guide-buffer-name "*shape-artist-tutorial-guide*"
  "Name of the tutorial checklist buffer.")

(defconst shape-artist-tutorial-canvas-width 60
  "Width in columns of the tutorial practice canvas.")

(defconst shape-artist-tutorial-canvas-height 20
  "Height in rows of the tutorial practice canvas.")

(defconst shape-artist-tutorial--max-commands 100
  "Maximum number of recent commands remembered by the practice buffer.")

(defvar-local shape-artist-tutorial--completed nil
  "Ids of the tasks completed so far, in task order (practice buffer).")

(defvar-local shape-artist-tutorial--commands nil
  "Recent commands, newest first (practice buffer).")

(defvar-local shape-artist-tutorial--tracked nil
  "Tracked box records, see `shape-artist-tutorial--track-boxes'.")

(defvar-local shape-artist-tutorial--timer nil
  "Pending idle timer that re-checks the tasks (practice buffer).")

(defvar-local shape-artist-tutorial--guide-buffer nil
  "The checklist buffer belonging to this practice buffer.")

;;; --- Guide buffer ---

(define-derived-mode shape-artist-tutorial-guide-mode special-mode "SA-Tutorial"
  "Major mode of the read-only Shape Artist tutorial checklist.")

(define-key shape-artist-tutorial-guide-mode-map (kbd "q")
  #'shape-artist-tutorial-quit)

(defun shape-artist-tutorial--fill (text width)
  "Return TEXT filled to WIDTH columns."
  (with-temp-buffer
    (insert text)
    (let ((fill-column width))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun shape-artist-tutorial--current-task (completed)
  "Return the id of the first task not in COMPLETED, or nil when all are done."
  (seq-find (lambda (id) (not (memq id completed)))
            (shape-artist-tutorial--task-ids)))

(defun shape-artist-tutorial--guide-text (completed)
  "Return the checklist text for the COMPLETED task ids."
  (let* ((ids (shape-artist-tutorial--task-ids))
         (current (shape-artist-tutorial--current-task completed))
         (number 0))
    (concat
     (format "SHAPE ARTIST TUTORIAL  ·  %d of %d done\n\n"
             (seq-count (lambda (id) (memq id completed)) ids)
             (length ids))
     (mapconcat
      (lambda (id)
        (setq number (1+ number))
        (format "[%s] %d. %s%s\n"
                (if (memq id completed) "x" " ")
                number
                (plist-get (shape-artist-tutorial--task id) :title)
                (if (eq id current) "   <- now" "")))
      ids "")
     "\n"
     (if current
         (concat
          (format "NOW: %s\n\n" (plist-get (shape-artist-tutorial--task current)
                                           :title))
          (shape-artist-tutorial--fill
           (shape-artist-tutorial--instruction current) 46)
          "\n\n"
          (shape-artist-tutorial--fill
           (concat "Hint: " (shape-artist-tutorial--hint current)) 46)
          "\n")
       (concat "Congratulations!  You know the basics of Shape Artist.\n\n"
               (shape-artist-tutorial--fill
                (format "Keep drawing, or press %s for a key hint."
                        (shape-artist-help-more-key 'move))
                46)
               "\n"))
     "\n"
     "----------------------------------------\n"
     "Close this tutorial: q here, or\n"
     "  M-x shape-artist-tutorial-quit\n"
     "Start over: M-x shape-artist-tutorial-reset\n")))

(defun shape-artist-tutorial--update-guide (guide completed)
  "Show the checklist for COMPLETED in the live buffer GUIDE."
  (when (buffer-live-p guide)
    (with-current-buffer guide
      (let ((inhibit-read-only t)
            (line (line-number-at-pos)))
        (erase-buffer)
        (insert (shape-artist-tutorial--guide-text completed))
        (goto-char (point-min))
        (forward-line (1- line))))))

;;; --- Evaluation ---

(defun shape-artist-tutorial--parse-shapes ()
  "Parse the current buffer and return its shapes without disturbing state.

The buffer-local parse results used by SELECT and SHAPE-EDIT are restored."
  (let ((root shape-artist--last-root)
        (shapes shape-artist--last-shapes-list)
        (graph shape-artist--last-graph))
    (unwind-protect
        (cdr (shape-artist-diagram-parse-buffer))
      (setq shape-artist--last-root root
            shape-artist--last-shapes-list shapes
            shape-artist--last-graph graph))))

(defun shape-artist-tutorial--build-state ()
  "Parse the practice buffer and return the task STATE plist.

Also updates the box tracking."
  (let* ((shapes (shape-artist-tutorial--parse-shapes))
         (positions (mapcar (lambda (rectangle)
                              (cons (oref rectangle x) (oref rectangle y)))
                            (shape-artist-tutorial--rectangles shapes))))
    (setq shape-artist-tutorial--tracked
          (shape-artist-tutorial--track-boxes shape-artist-tutorial--tracked
                                              positions))
    (list :shapes shapes
          :initial-boxes (shape-artist-tutorial--initial-boxes
                          shape-artist-tutorial--tracked positions)
          :commands shape-artist-tutorial--commands)))

(defun shape-artist-tutorial--evaluate ()
  "Re-check all tasks in the practice buffer, then refresh guide and progress.

Completed tasks stay completed.  Newly completed tasks are saved to the
progress file.  The first time every task is complete, congratulate the user."
  (let* ((state (shape-artist-tutorial--build-state))
         (ids (shape-artist-tutorial--task-ids))
         (newly (seq-filter
                 (lambda (id)
                   (and (not (memq id shape-artist-tutorial--completed))
                        (shape-artist-tutorial--task-complete-p id state)))
                 ids)))
    (when newly
      (setq shape-artist-tutorial--completed
            (seq-filter (lambda (id)
                          (or (memq id shape-artist-tutorial--completed)
                              (memq id newly)))
                        ids))
      (shape-artist-tutorial--persist-completed shape-artist-tutorial--completed))
    (shape-artist-tutorial--update-guide shape-artist-tutorial--guide-buffer
                                         shape-artist-tutorial--completed)
    ;; Only a check that completes the last open task gets here with NEWLY, so
    ;; the congratulation is shown exactly once per completion.
    (when (and newly
               (null (shape-artist-tutorial--current-task
                      shape-artist-tutorial--completed)))
      (message "Shape Artist tutorial complete: you know the basics!"))
    newly))

(defun shape-artist-tutorial--run-timer (buffer)
  "Idle-timer entry point: re-check the tasks in the live practice BUFFER.

Errors are demoted to a message; a timer must never break editing."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq shape-artist-tutorial--timer nil)
      (condition-case err
          (when shape-artist-core-mode
            (shape-artist-tutorial--evaluate))
        (error (message "Shape Artist tutorial: %s" (error-message-string err))
               nil)))))

(defun shape-artist-tutorial--normalize-command (command)
  "Return COMMAND as the tutorial should record it.

The DRAW-mode undo key goes through a dispatcher that calls
`shape-artist-undo' as a function, so `this-command' names the dispatcher;
record the undo instead."
  (if (and (memq command '(shape-artist--dispatch-utility
                           shape-artist--dispatch-undo))
           (eq shape-artist--mode 'draw))
      'shape-artist-undo
    command))

(defun shape-artist-tutorial--schedule ()
  "Schedule a debounced task re-check for the current practice buffer."
  (when (timerp shape-artist-tutorial--timer)
    (cancel-timer shape-artist-tutorial--timer))
  (setq shape-artist-tutorial--timer
        (run-with-idle-timer shape-artist-tutorial-eval-delay nil
                             #'shape-artist-tutorial--run-timer
                             (current-buffer))))

(defun shape-artist-tutorial--post-command ()
  "Record `this-command' and schedule a task re-check.  Never signals."
  (condition-case nil
      (progn
        (when (and this-command (symbolp this-command))
          (setq shape-artist-tutorial--commands
                (seq-take (cons (shape-artist-tutorial--normalize-command
                                 this-command)
                                shape-artist-tutorial--commands)
                          shape-artist-tutorial--max-commands)))
        (shape-artist-tutorial--schedule))
    (error nil)))

;;; --- Setup and teardown ---

(defun shape-artist-tutorial--blank-canvas ()
  "Return the text of an empty practice canvas."
  (mapconcat (lambda (_) (make-string shape-artist-tutorial-canvas-width ?\s))
             (number-sequence 1 shape-artist-tutorial-canvas-height)
             "\n"))

(defun shape-artist-tutorial--cleanup-practice ()
  "Cancel the pending timer and close the guide; run when the canvas dies."
  (when (timerp shape-artist-tutorial--timer)
    (cancel-timer shape-artist-tutorial--timer)
    (setq shape-artist-tutorial--timer nil))
  (let ((guide shape-artist-tutorial--guide-buffer))
    (setq shape-artist-tutorial--guide-buffer nil)
    (when (buffer-live-p guide)
      (kill-buffer guide))))

(defun shape-artist-tutorial--init-practice ()
  "Turn the current buffer into an empty practice canvas in Shape Artist mode."
  (let ((guide shape-artist-tutorial--guide-buffer))
    (when (timerp shape-artist-tutorial--timer)
      (cancel-timer shape-artist-tutorial--timer))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (shape-artist-tutorial--blank-canvas)))
    ;; Enabling an enabled `shape-artist-core-mode' again would make
    ;; `picture-mode' fail, so only enable it when it is off.  `picture-mode'
    ;; resets buffer-local state, so all tutorial setup happens after it.
    (unless shape-artist-core-mode
      (when (eq major-mode 'picture-mode)
        (fundamental-mode))
      (shape-artist-core-mode 1))
    (shape-artist--clear-preview-state)
    (shape-artist-enter-move)
    (setq-local shape-artist--update-sidebar-fn nil)
    (setq shape-artist-tutorial--guide-buffer guide
          shape-artist-tutorial--commands nil
          shape-artist-tutorial--tracked nil
          shape-artist-tutorial--timer nil
          shape-artist-tutorial--completed nil))
  (shape-artist--goto-row-col 3 4)
  (add-hook 'post-command-hook #'shape-artist-tutorial--post-command nil t)
  (add-hook 'kill-buffer-hook #'shape-artist-tutorial--cleanup-practice nil t))

;;;###autoload
(defun shape-artist-tutorial ()
  "Start, or resume, the interactive Shape Artist tutorial.

Open a practice canvas in Shape Artist mode with a checklist beside it.  The
checklist follows what you draw; completed tasks are remembered between
sessions in `shape-artist-tutorial-progress-file'."
  (interactive)
  (let* ((existing (get-buffer shape-artist-tutorial-practice-buffer-name))
         (practice (or existing
                       (get-buffer-create
                        shape-artist-tutorial-practice-buffer-name)))
         (guide (get-buffer-create shape-artist-tutorial-guide-buffer-name))
         (progress (shape-artist-tutorial--load-progress)))
    (with-current-buffer guide
      (shape-artist-tutorial-guide-mode))
    (with-current-buffer practice
      (unless (and existing shape-artist-core-mode)
        (shape-artist-tutorial--init-practice))
      (setq shape-artist-tutorial--guide-buffer guide)
      (setq shape-artist-tutorial--completed
            (seq-filter (lambda (id)
                          (or (memq id shape-artist-tutorial--completed)
                              (memq id (plist-get progress :completed))))
                        (shape-artist-tutorial--task-ids)))
      (shape-artist-tutorial--evaluate))
    (shape-artist-tutorial--save-progress
     (list :completed (plist-get progress :completed) :first-run-seen t))
    (switch-to-buffer practice)
    (display-buffer guide '((display-buffer-in-side-window)
                            (side . right)
                            (slot . 0)
                            (window-width . 52)))
    practice))

(defun shape-artist-tutorial-quit ()
  "Close the tutorial: kill the practice canvas and its checklist."
  (interactive)
  (dolist (name (list shape-artist-tutorial-practice-buffer-name
                      shape-artist-tutorial-guide-buffer-name))
    (let ((buffer (get-buffer name)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun shape-artist-tutorial-reset ()
  "Forget all saved tutorial progress, after asking for confirmation.

The first-run flag is kept, so the one-time hint does not come back.  A running
tutorial gets a fresh canvas and an empty checklist.  Return non-nil when the
progress was reset."
  (interactive)
  (when (y-or-n-p "Forget all Shape Artist tutorial progress? ")
    (shape-artist-tutorial--save-progress
     (list :completed nil
           :first-run-seen (plist-get (shape-artist-tutorial--load-progress)
                                      :first-run-seen)))
    (let ((practice (get-buffer shape-artist-tutorial-practice-buffer-name)))
      (when (buffer-live-p practice)
        (with-current-buffer practice
          (shape-artist-tutorial--init-practice)
          (shape-artist-tutorial--evaluate))))
    t))

(provide 'shape-artist-tutorial)
;;; shape-artist-tutorial.el ends here
