;;; test-shape-artist-keys.el --- ERT tests for modal key dispatch -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'shape-artist)

(defconst test-shape-artist-keys--all-modes '(move draw select shape-edit nil)
  "Every mode a dispatcher can be invoked in, including no mode at all.")

(defun test-shape-artist-keys--calls (dispatcher mode targets &rest args)
  "Call DISPATCHER with ARGS while MODE is active; return calls made to TARGETS.
Each TARGET symbol is temporarily replaced by a recorder, so the result is the
list of (TARGET . ARGS) calls in order and no real command runs."
  (let ((calls nil)
        (saved (mapcar (lambda (target)
                         (cons target (and (fboundp target) (symbol-function target))))
                       targets)))
    (unwind-protect
        (with-temp-buffer
          (setq shape-artist--mode mode)
          (dolist (target targets)
            (fset target (let ((target target))
                           (lambda (&rest call-args)
                             (push (cons target call-args) calls)))))
          (apply dispatcher args))
      (dolist (entry saved)
        (if (cdr entry)
            (fset (car entry) (cdr entry))
          (fmakunbound (car entry)))))
    (nreverse calls)))

;;; --- Mode-routing dispatchers ---

(defconst test-shape-artist-keys--routing
  '((shape-artist--dispatch-left
     (draw . shape-artist-draw-move-left)
     (shape-edit . shape-artist-shape-edit-left)
     (t . shape-artist-move-left))
    (shape-artist--dispatch-down
     (draw . shape-artist-draw-move-down)
     (shape-edit . shape-artist-shape-edit-down)
     (t . shape-artist-move-down))
    (shape-artist--dispatch-up
     (draw . shape-artist-draw-move-up)
     (shape-edit . shape-artist-shape-edit-up)
     (t . shape-artist-move-up))
    (shape-artist--dispatch-right
     (draw . shape-artist-draw-move-right)
     (shape-edit . shape-artist-shape-edit-right)
     (t . shape-artist-move-right))
    (shape-artist--dispatch-space
     (draw . shape-artist-set-anchor-here)
     (t . shape-artist-enter-draw-and-anchor))
    (shape-artist--dispatch-return
     (draw . shape-artist-commit-and-exit)
     (select . shape-artist-select-confirm)
     (t . shape-artist-commit-preview))
    (shape-artist--dispatch-escape
     (draw . shape-artist-cancel-and-exit)
     (select . shape-artist-select-cancel)
     (shape-edit . shape-artist-shape-edit-exit)
     (t . nil)))
  "For each mode-routing dispatcher, the target command per mode.
The `t' entry is the fallback for every mode not listed; a nil target means
the dispatcher intentionally does nothing in that mode.")

(ert-deftest test-shape-artist-keys-routing-dispatchers-pick-target-by-mode ()
  "Each routing dispatcher invokes exactly the command for the active mode."
  (dolist (spec test-shape-artist-keys--routing)
    (let* ((dispatcher (car spec))
           (table (cdr spec))
           (targets (delete-dups (delq nil (mapcar #'cdr table)))))
      (dolist (mode test-shape-artist-keys--all-modes)
        (ert-info ((format "%s in mode %S" dispatcher mode))
          (let ((expected (cdr (or (assq mode table) (assq t table)))))
            (should (equal (mapcar #'car (test-shape-artist-keys--calls
                                          dispatcher mode targets))
                           (and expected (list expected))))))))))

;;; --- Mode-gated dispatchers ---

(defconst test-shape-artist-keys--gated
  '((select
     (shape-artist--dispatch-select-next . shape-artist-select-next)
     (shape-artist--dispatch-select-prev . shape-artist-select-prev)
     (shape-artist--dispatch-select-refresh . shape-artist-shapes-parse)
     (shape-artist--dispatch-select-toggle . shape-artist-select-toggle-current)
     (shape-artist--dispatch-select-all . shape-artist-select-all)
     (shape-artist--dispatch-select-clear . shape-artist-select-clear-selected))
    (shape-edit
     (shape-artist--dispatch-shape-edit-label . shape-artist-shape-edit-label)
     (shape-artist--dispatch-shape-move . shape-artist-shape-move)
     (shape-artist--dispatch-shape-resize . shape-artist-shape-resize)
     (shape-artist--dispatch-shape-cycle-style . shape-artist-shape-cycle-style)
     (shape-artist--dispatch-shape-set-style . shape-artist-shape-set-style))
    (draw
     (shape-artist--dispatch-commit-stay . shape-artist-commit-stay)
     (shape-artist--dispatch-undo . shape-artist-undo)))
  "Dispatchers that act only in one mode, grouped by that mode.")

(ert-deftest test-shape-artist-keys-gated-dispatchers-act-only-in-their-mode ()
  "Mode-gated dispatchers run their command in their mode and nowhere else."
  (dolist (group test-shape-artist-keys--gated)
    (let ((active-mode (car group)))
      (dolist (entry (cdr group))
        (dolist (mode test-shape-artist-keys--all-modes)
          (ert-info ((format "%s in mode %S" (car entry) mode))
            (should (equal (mapcar #'car (test-shape-artist-keys--calls
                                          (car entry) mode (list (cdr entry))))
                           (and (eq mode active-mode) (list (cdr entry)))))))))))

(ert-deftest test-shape-artist-keys-undo-and-sidebar-keys-mean-one-thing ()
  "`u' is undo and `a' toggles the sidebar in every mode's documented keymap."
  (dolist (map (list shape-artist-move-map shape-artist-draw-map
                     shape-artist-select-map shape-artist-shape-edit-map
                     shape-artist-core-mode-map))
    (should (eq (lookup-key map (kbd "u")) #'shape-artist-undo))
    (should (eq (lookup-key map (kbd "a")) #'shape-artist-ui-toggle))))

;;; --- Counted shape-edit dispatchers ---

(defconst test-shape-artist-keys--counted
  '((shape-artist--dispatch-shape-move-left . shape-artist-shape-move-left)
    (shape-artist--dispatch-shape-move-down . shape-artist-shape-move-down)
    (shape-artist--dispatch-shape-move-up . shape-artist-shape-move-up)
    (shape-artist--dispatch-shape-move-right . shape-artist-shape-move-right)
    (shape-artist--dispatch-shape-resize-wider . shape-artist-shape-resize-wider)
    (shape-artist--dispatch-shape-resize-narrower . shape-artist-shape-resize-narrower)
    (shape-artist--dispatch-shape-resize-taller . shape-artist-shape-resize-taller)
    (shape-artist--dispatch-shape-resize-shorter . shape-artist-shape-resize-shorter)
    (shape-artist--dispatch-shape-resize-left . shape-artist-shape-resize-left)
    (shape-artist--dispatch-shape-shrink-left . shape-artist-shape-shrink-left)
    (shape-artist--dispatch-shape-resize-up . shape-artist-shape-resize-up)
    (shape-artist--dispatch-shape-shrink-up . shape-artist-shape-shrink-up))
  "SHAPE-EDIT dispatchers taking a repeat COUNT, with their target commands.")

(ert-deftest test-shape-artist-keys-counted-dispatchers-forward-count-in-shape-edit ()
  "Counted dispatchers pass the repeat count through, only in SHAPE-EDIT mode."
  (dolist (entry test-shape-artist-keys--counted)
    (ert-info ((format "%s forwards its count" (car entry)))
      (should (equal (test-shape-artist-keys--calls
                      (car entry) 'shape-edit (list (cdr entry)) 3)
                     (list (list (cdr entry) 3)))))
    (dolist (mode '(move draw select nil))
      (ert-info ((format "%s is inert in mode %S" (car entry) mode))
        (should-not (test-shape-artist-keys--calls
                     (car entry) mode (list (cdr entry)) 3))))))

(ert-deftest test-shape-artist-keys-counted-dispatchers-are-prefix-argument-commands ()
  "Counted dispatchers are interactive commands that take a numeric prefix."
  (dolist (entry test-shape-artist-keys--counted)
    (should (commandp (car entry)))
    (should (equal (cadr (interactive-form (car entry))) "p"))))

;;; --- Entry points (shape-artist.el) ---

(ert-deftest test-shape-artist-keys-scratch-creates-active-buffer ()
  "`shape-artist-scratch' opens a fresh buffer with artist mode on and UI shown."
  (let ((shown 0)
        (shape-artist--update-sidebar-fn nil)
        (original (current-buffer))
        (scratch nil))
    (unwind-protect
        (cl-letf (((symbol-function 'shape-artist-ui-show)
                   (lambda () (setq shown (1+ shown)))))
          (shape-artist-scratch)
          (setq scratch (current-buffer))
          (should (string-prefix-p "*shape-artist*" (buffer-name scratch)))
          (should shape-artist-core-mode)
          (should (eq shape-artist--mode 'move))
          (should (= shown 1)))
      (set-buffer original)
      (when (buffer-live-p scratch)
        (with-current-buffer scratch (ignore-errors (shape-artist-core-mode -1)))
        (kill-buffer scratch)))))

;; Each scenario runs in a fresh buffer: re-enabling within one buffer is a known
;; bug covered by the expected-failure tests below.
(ert-deftest test-shape-artist-keys-mode-command-toggle-enables-then-disables ()
  "`shape-artist-mode' with `toggle' turns the mode on and then off."
  (with-temp-buffer
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-mode 'toggle)
      (should shape-artist-core-mode)
      (shape-artist-mode 'toggle)
      (should-not shape-artist-core-mode))))

(ert-deftest test-shape-artist-keys-mode-command-numeric-args-enable-or-disable ()
  "A positive argument enables; zero, negative, and nil disable."
  (let ((shape-artist--update-sidebar-fn nil))
    (dolist (case '((1 . t) (5 . t) (-1 . nil) (0 . nil) (nil . nil)))
      (ert-info ((format "(shape-artist-mode %S) from off" (car case)))
        (with-temp-buffer
          (shape-artist-mode (car case))
          (should (eq (and shape-artist-core-mode t) (cdr case))))))
    (dolist (arg '(-1 0 nil))
      (ert-info ((format "from on, (shape-artist-mode %S)" arg))
        (with-temp-buffer
          (shape-artist-mode 1)
          (shape-artist-mode arg)
          (should-not shape-artist-core-mode))))))

(ert-deftest test-shape-artist-keys-mode-command-reenabling-enabled-buffer-is-idempotent ()
  "Enabling artist mode in a buffer where it is already on keeps it on."
  (with-temp-buffer
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-mode 1)
      (shape-artist-mode 1)
      (should shape-artist-core-mode))))

(ert-deftest test-shape-artist-keys-mode-command-toggle-twice-reenables ()
  "Toggling artist mode on, off, and on again in one buffer works."
  (with-temp-buffer
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-mode 'toggle)
      (shape-artist-mode 'toggle)
      (shape-artist-mode 'toggle)
      (should shape-artist-core-mode))))

(ert-deftest test-shape-artist-keys-mode-command-prefix-arg-defaults-to-toggle ()
  "Called interactively without a prefix argument, the command toggles."
  (with-temp-buffer
    (let ((shape-artist--update-sidebar-fn nil)
          (current-prefix-arg nil))
      (call-interactively #'shape-artist-mode)
      (should shape-artist-core-mode)
      (call-interactively #'shape-artist-mode)
      (should-not shape-artist-core-mode))))

;;; --- Menu Bar Tests ---

(defun test-shape-artist-keys--menu-commands (keymap)
  "Return every command symbol bound in menu KEYMAP, descending into submenus."
  (let (commands)
    (map-keymap
     (lambda (_event binding)
       (cond
        ((and (consp binding) (eq (car binding) 'menu-item))
         (let ((target (nth 2 binding)))
           (cond ((and (symbolp target) target (not (keymapp target)))
                  (push target commands))
                 ((keymapp target)
                  (setq commands (append (test-shape-artist-keys--menu-commands target)
                                         commands))))))
        ((and (consp binding) (stringp (car binding)))
         (let ((target (cdr binding)))
           (when (and (symbolp target) target (not (keymapp target)))
             (push target commands))))
        ((keymapp binding)
         (setq commands (append (test-shape-artist-keys--menu-commands binding)
                                commands)))))
     keymap)
    commands))

(defun test-shape-artist-keys--find-menu (title)
  "Return the menu keymap titled TITLE in the mode's menu bar, or nil.
Emacs versions differ in the key and binding format they use for a menu, so
search by title instead of looking up a fixed key."
  (let ((bar (lookup-key shape-artist-core-mode-map [menu-bar]))
        (found nil))
    (when (keymapp bar)
      (map-keymap
       (lambda (_event binding)
         (let ((candidate
                (cond ((and (consp binding) (eq (car binding) 'menu-item))
                       (cons (nth 1 binding) (nth 2 binding)))
                      ((and (consp binding) (stringp (car binding)))
                       (cons (car binding) (cdr binding))))))
           (when (and candidate (equal (car candidate) title)
                      (keymapp (cdr candidate)))
             (setq found (cdr candidate)))))
       bar))
    found))

(ert-deftest test-shape-artist-keys-menu-bar-entry-offers-only-real-commands ()
  "The Artist menu exists, covers the main features, and every item is a command."
  (let ((menu (test-shape-artist-keys--find-menu "Artist")))
    (should (keymapp menu))
    (let ((commands (test-shape-artist-keys--menu-commands menu)))
      (should (> (length commands) 8))
      (dolist (command commands)
        (ert-info ((format "menu item %s" command))
          (should (commandp command))))
      (dolist (expected '(shape-artist-help-hint shape-artist-help-cheatsheet
                          shape-artist-tutorial shape-artist-ui-toggle
                          shape-artist-undo shape-artist-explain-at-point
                          shape-artist-export shape-artist-quit))
        (should (memq expected commands))))))

(provide 'test-shape-artist-keys)
;;; test-shape-artist-keys.el ends here
