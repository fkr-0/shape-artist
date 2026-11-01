;;; test-shape-artist-embed.el --- Tests for editing diagrams in place -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'org)
(require 'shape-artist)
(require 'shape-artist-embed)

;;; --- Helpers ---------------------------------------------------------------

(defmacro test-shape-artist-embed--with-source (mode content marker &rest body)
  "Run BODY in a temp buffer in MODE holding CONTENT, point on line with MARKER."
  (declare (indent 3))
  `(with-temp-buffer
     (let ((shape-artist--update-sidebar-fn nil))
       (funcall ,mode)
       (insert ,content)
       (goto-char (point-min))
       (search-forward ,marker)
       (beginning-of-line)
       ,@body)))

(defmacro test-shape-artist-embed--with-windowless-sessions (&rest body)
  "Run BODY with `pop-to-buffer' reduced to `set-buffer' and cleanup afterwards."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'pop-to-buffer)
              (lambda (buffer &rest _) (set-buffer buffer))))
     (unwind-protect (progn ,@body)
       (dolist (buf (buffer-list))
         (when (string-prefix-p "*shape-artist: " (buffer-name buf))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf))))))

(defmacro test-shape-artist-embed--messages (&rest body)
  "Run BODY and return the `message' strings it produced, oldest first."
  (declare (indent 0))
  `(let ((captured nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format fmt args) captured)))))
       ,@body)
     (nreverse captured)))

(defun test-shape-artist-embed--detect (&rest args)
  "Return the detection plist for point, minus the buffer positions."
  (let ((found (apply #'shape-artist-embed-detect args)))
    (list :kind (plist-get found :kind)
          :prefix (plist-get found :prefix)
          :lines (plist-get found :lines))))

(defconst test-shape-artist-embed--box '("+--+" "|  |" "+--+")
  "The three-line box used by most fixtures.")

;;; --- Detection ----------------------------------------------------------------

(ert-deftest test-shape-artist-embed-detects-lisp-comment-diagram-and-excludes-prose ()
  "A diagram inside a `;;' comment is found without the surrounding prose."
  (test-shape-artist-embed--with-source #'emacs-lisp-mode
      ";; Overview of the system.\n;;\n;; +-----+    +-----+\n;; | api |--->| db  |\n;; +-----+    +-----+\n;;\n;; More prose here.\n(defun x ())\n"
      "| api |"
    (let ((found (shape-artist-embed-detect)))
      (should (equal (plist-get found :kind) 'comment))
      (should (equal (plist-get found :prefix) ";; "))
      (should (equal (plist-get found :lines)
                     '("+-----+    +-----+" "| api |--->| db  |" "+-----+    +-----+")))
      (should (equal (buffer-substring (plist-get found :beg) (plist-get found :end))
                     ";; +-----+    +-----+\n;; | api |--->| db  |\n;; +-----+    +-----+")))))

(ert-deftest test-shape-artist-embed-detects-each-comment-style ()
  "Common comment leaders, with indentation, are recognised and preserved."
  (dolist (case '(("    // +--+\n    // |  |\n    // +--+" "    // ")
                  ("# +--+\n# |  |\n# +--+" "# ")
                  ("-- +--+\n-- |  |\n-- +--+" "-- ")
                  (" * +--+\n * |  |\n * +--+" " * ")
                  ("% +--+\n% |  |\n% +--+" "% ")
                  ("> +--+\n> |  |\n> +--+" "> ")))
    (pcase-let ((`(,text ,prefix) case))
      (ert-info ((format "prefix %S" prefix))
        (test-shape-artist-embed--with-source #'fundamental-mode text "|  |"
          (should (equal (test-shape-artist-embed--detect)
                         (list :kind 'comment :prefix prefix
                               :lines test-shape-artist-embed--box))))))))

(ert-deftest test-shape-artist-embed-detects-plain-indented-diagram ()
  "An indented block in prose is found, keeping its indentation as the prefix."
  (test-shape-artist-embed--with-source #'text-mode
      "Some text.\n\n    +--+\n    |  |\n    +--+\n\nMore text.\n" "|  |"
    (should (equal (test-shape-artist-embed--detect)
                   (list :kind 'plain :prefix "    "
                         :lines test-shape-artist-embed--box)))))

(ert-deftest test-shape-artist-embed-treats-connectors-and-arrows-as-diagram-lines ()
  "Single-glyph connector rows stay part of the block."
  (test-shape-artist-embed--with-source #'fundamental-mode
      ";; +--+\n;; |  |\n;; +--+\n;;   |\n;;   v\n;; +--+\n;; |  |\n;; +--+\n" "|  |"
    (should (= (length (plist-get (shape-artist-embed-detect) :lines)) 8))))

(ert-deftest test-shape-artist-embed-detects-fenced-markdown-blocks ()
  "The interior of a Markdown code fence is the diagram, whatever it contains."
  (dolist (fence '("```" "```text" "~~~"))
    (ert-info ((format "fence %s" fence))
      (test-shape-artist-embed--with-source #'text-mode
          (format "Intro\n%s\n+--+\n|  |\n+--+\n%s\nAfter\n"
                  fence (substring fence 0 3))
          "|  |"
        (should (equal (test-shape-artist-embed--detect)
                       (list :kind 'fenced :prefix ""
                             :lines test-shape-artist-embed--box)))))))

(ert-deftest test-shape-artist-embed-detects-org-blocks ()
  "Org example and source blocks, including indented ones, are recognised."
  (dolist (case '(("#+begin_example\n+--+\n|  |\n+--+\n#+end_example\n" "")
                  ("#+begin_src ascii\n+--+\n|  |\n+--+\n#+end_src\n" "")
                  ("- item\n  #+begin_example\n  +--+\n  |  |\n  +--+\n  #+end_example\n" "  ")))
    (pcase-let ((`(,text ,prefix) case))
      (ert-info ((format "%S" text))
        (test-shape-artist-embed--with-source #'org-mode text "|  |"
          (should (equal (test-shape-artist-embed--detect)
                         (list :kind 'fenced :prefix prefix
                               :lines test-shape-artist-embed--box))))))))

(ert-deftest test-shape-artist-embed-detects-org-fixed-width-lines ()
  "In Org, colon-prefixed fixed-width lines form a diagram."
  (test-shape-artist-embed--with-source #'org-mode
      "Text\n\n: +--+\n: |  |\n: +--+\n\nMore\n" "|  |"
    (should (equal (test-shape-artist-embed--detect)
                   (list :kind 'comment :prefix ": "
                         :lines test-shape-artist-embed--box)))))

(ert-deftest test-shape-artist-embed-active-region-overrides-detection ()
  "An explicit region is edited exactly, prose lines included."
  (test-shape-artist-embed--with-source #'fundamental-mode
      ";; intro\n;; +--+\n;; |  |\n;; +--+\n;; outro\n" "intro"
    (let ((beg (point)))
      (goto-char (point-max))
      (let ((found (shape-artist-embed-detect beg (point))))
        (should (equal (plist-get found :prefix) ";; "))
        (should (equal (plist-get found :lines)
                       '("intro" "+--+" "|  |" "+--+" "outro")))))))

(ert-deftest test-shape-artist-embed-reports-when-there-is-no-diagram ()
  "Point on prose or a blank line is a clear user error, not a guess."
  (test-shape-artist-embed--with-source #'fundamental-mode
      ";; Just a sentence about boxes.\n\n" "sentence"
    (let ((text (error-message-string
                 (should-error (shape-artist-embed-detect) :type 'user-error))))
      (should (string-match-p "No diagram" text))
      (should (string-match-p "select" text)))))

;;; --- Rendering back ---------------------------------------------------------------

(ert-deftest test-shape-artist-embed-render-restores-prefix-and-trims-trailing-space ()
  "Lines get their prefix back, with no trailing whitespace anywhere."
  (should (equal (shape-artist-embed--render '("+--+  " "" "|  |") ";; ")
                 ";; +--+\n;;\n;; |  |"))
  (should (equal (shape-artist-embed--render '("+--+") "    // ") "    // +--+"))
  (should (equal (shape-artist-embed--render '("a" "b") "") "a\nb")))

(ert-deftest test-shape-artist-embed-render-drops-leading-and-trailing-blank-rows ()
  "Blank canvas rows added while drawing do not leak into the source."
  (should (equal (shape-artist-embed--render '("" "" "+--+" "|  |" "" "") "# ")
                 "# +--+\n# |  |")))

;;; --- Sessions -----------------------------------------------------------------------

(defconst test-shape-artist-embed--elisp-source
  ";; Intro.\n;; +--+\n;; |  |\n;; +--+\n;; Outro.\n(defun x ())\n"
  "A tiny source file with a commented diagram.")

(ert-deftest test-shape-artist-embed-opens-an-isolated-padded-session ()
  "Editing in place opens a separate buffer holding the stripped, padded text."
  (test-shape-artist-embed--with-windowless-sessions
    (test-shape-artist-embed--with-source #'emacs-lisp-mode
        test-shape-artist-embed--elisp-source "|  |"
      (let ((source (current-buffer))
            (before (buffer-string)))
        (shape-artist-edit-at-point)
        (let ((session (current-buffer)))
          (should-not (eq session source))
          (should shape-artist-core-mode)
          (should shape-artist-embed-session-mode)
          (should (equal (buffer-string) "+--+\n|  |\n+--+"))
          (should (eq (plist-get shape-artist-embed--session :source) source))
          ;; The source is untouched until commit.
          (with-current-buffer source
            (should (equal (buffer-string) before))))))))

(ert-deftest test-shape-artist-embed-commit-writes-back-and-touches-nothing-else ()
  "Committing replaces only the diagram lines, keeping the comment prefix."
  (test-shape-artist-embed--with-windowless-sessions
    (test-shape-artist-embed--with-source #'emacs-lisp-mode
        test-shape-artist-embed--elisp-source "|  |"
      (let ((source (current-buffer)))
        (shape-artist-edit-at-point)
        ;; Edit the diagram: move the box two columns right.
        (shape-artist-shapes-parse)
        (shape-artist-shapes-move
         (cl-find-if #'shape-artist-diagram-rectangle-p shape-artist--last-shapes-list)
         2 0)
        (shape-artist-embed-commit)
        (should (eq (current-buffer) source))
        (should (equal (buffer-string)
                       ";; Intro.\n;;   +--+\n;;   |  |\n;;   +--+\n;; Outro.\n(defun x ())\n"))
        (should-not (cl-find-if (lambda (b) (string-prefix-p "*shape-artist: " (buffer-name b)))
                                (buffer-list)))))))

(ert-deftest test-shape-artist-embed-commit-trims-canvas-growth ()
  "Rows and columns the canvas grew while editing leave no trailing blanks behind."
  (test-shape-artist-embed--with-windowless-sessions
    (test-shape-artist-embed--with-source #'emacs-lisp-mode
        test-shape-artist-embed--elisp-source "|  |"
      (shape-artist-edit-at-point)
      (shape-artist--expand-bottom)
      (shape-artist--expand-bottom)
      (shape-artist--expand-right)
      (let ((source (plist-get shape-artist-embed--session :source)))
        (shape-artist-embed-commit)
        (with-current-buffer source
          (should (equal (buffer-string) test-shape-artist-embed--elisp-source)))))))

(ert-deftest test-shape-artist-embed-commit-is-a-single-undo-step-in-the-source ()
  "One `undo' in the source buffer reverts the whole committed edit."
  (test-shape-artist-embed--with-windowless-sessions
    (test-shape-artist-embed--with-source #'emacs-lisp-mode
        test-shape-artist-embed--elisp-source "|  |"
      (buffer-enable-undo)
      (setq buffer-undo-list nil)
      (let ((source (current-buffer)))
        (shape-artist-edit-at-point)
        (shape-artist-shapes-parse)
        (shape-artist-shapes-move
         (cl-find-if #'shape-artist-diagram-rectangle-p shape-artist--last-shapes-list)
         3 1)
        (shape-artist-embed-commit)
        (with-current-buffer source
          (should-not (equal (buffer-string) test-shape-artist-embed--elisp-source))
          (undo-boundary)
          (let ((last-command nil)) (undo))
          (should (equal (buffer-string) test-shape-artist-embed--elisp-source)))))))

(ert-deftest test-shape-artist-embed-abort-leaves-the-source-alone ()
  "Aborting discards the session without touching the source."
  (test-shape-artist-embed--with-windowless-sessions
    (test-shape-artist-embed--with-source #'emacs-lisp-mode
        test-shape-artist-embed--elisp-source "|  |"
      (set-buffer-modified-p nil)
      (let ((source (current-buffer)))
        (shape-artist-edit-at-point)
        (shape-artist-shapes-parse)
        (shape-artist-shapes-move
         (cl-find-if #'shape-artist-diagram-rectangle-p shape-artist--last-shapes-list)
         5 0)
        (shape-artist-embed-abort)
        (should (eq (current-buffer) source))
        (should (equal (buffer-string) test-shape-artist-embed--elisp-source))
        (should-not (buffer-modified-p))))))

(ert-deftest test-shape-artist-embed-commit-without-changes-does-not-modify-the-source ()
  "An unchanged diagram leaves the source buffer unmodified."
  (test-shape-artist-embed--with-windowless-sessions
    (test-shape-artist-embed--with-source #'emacs-lisp-mode
        test-shape-artist-embed--elisp-source "|  |"
      (set-buffer-modified-p nil)
      (shape-artist-edit-at-point)
      (let ((source (plist-get shape-artist-embed--session :source)))
        (should (string-match-p "No changes"
                                (car (test-shape-artist-embed--messages
                                       (shape-artist-embed-commit)))))
        (with-current-buffer source
          (should-not (buffer-modified-p)))))))

(ert-deftest test-shape-artist-embed-commit-protects-concurrent-source-edits ()
  "If the source lines changed during the session, committing asks first."
  (test-shape-artist-embed--with-windowless-sessions
    (test-shape-artist-embed--with-source #'emacs-lisp-mode
        test-shape-artist-embed--elisp-source "|  |"
      (shape-artist-edit-at-point)
      (let ((source (plist-get shape-artist-embed--session :source)))
        (with-current-buffer source
          (goto-char (point-min))
          (search-forward "|  |")
          (replace-match "|ab|"))
        (shape-artist-shapes-parse)
        (shape-artist-shapes-move
         (cl-find-if #'shape-artist-diagram-rectangle-p shape-artist--last-shapes-list)
         1 0)
        ;; Declining keeps the session open and the source as the user left it.
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
          (should-error (shape-artist-embed-commit) :type 'user-error))
        (with-current-buffer source
          (should (string-match-p "|ab|" (buffer-string))))
        (should shape-artist-embed-session-mode)
        ;; Agreeing overwrites.
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (shape-artist-embed-commit))
        (with-current-buffer source
          (should-not (string-match-p "|ab|" (buffer-string)))
          (should (string-match-p ";;  \\+--\\+" (buffer-string))))))))

(ert-deftest test-shape-artist-embed-edits-org-example-blocks-in-place ()
  "Org example blocks round-trip, keeping their delimiters and surroundings."
  (test-shape-artist-embed--with-windowless-sessions
    (test-shape-artist-embed--with-source #'org-mode
        "* Heading\n#+begin_example\n+--+\n|  |\n+--+\n#+end_example\nAfter.\n" "|  |"
      (let ((original (buffer-string)))
        (shape-artist-edit-at-point)
        (shape-artist-shapes-parse)
        (shape-artist-shapes-move
         (cl-find-if #'shape-artist-diagram-rectangle-p shape-artist--last-shapes-list)
         1 0)
        (shape-artist-embed-commit)
        (should (equal (buffer-string)
                       "* Heading\n#+begin_example\n +--+\n |  |\n +--+\n#+end_example\nAfter.\n"))
        (should-not (equal (buffer-string) original))))))

;;; --- Inserting a new diagram ---------------------------------------------------------

(ert-deftest test-shape-artist-embed-insert-diagram-adds-a-prefixed-block ()
  "A new diagram drawn in a session lands below the current comment line."
  (test-shape-artist-embed--with-windowless-sessions
    (test-shape-artist-embed--with-source #'emacs-lisp-mode
        ";; Architecture:\n(defun x ())\n" "Architecture"
      (shape-artist-insert-diagram 4 12)
      (should (equal (plist-get shape-artist-embed--session :kind) 'insert))
      (should (= (count-lines (point-min) (point-max)) 4))
      ;; Draw a box by writing it straight into the canvas.
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (insert "+--+\n|  |\n+--+")
      (let ((source (plist-get shape-artist-embed--session :source)))
        (shape-artist-embed-commit)
        (with-current-buffer source
          (should (equal (buffer-string)
                         ";; Architecture:\n;; +--+\n;; |  |\n;; +--+\n(defun x ())\n")))))))

(ert-deftest test-shape-artist-embed-insert-diagram-with-nothing-drawn-inserts-nothing ()
  "Committing an untouched blank canvas leaves the source unchanged."
  (test-shape-artist-embed--with-windowless-sessions
    (test-shape-artist-embed--with-source #'emacs-lisp-mode
        ";; Architecture:\n(defun x ())\n" "Architecture"
      (let ((original (buffer-string)))
        (shape-artist-insert-diagram 4 12)
        (let ((source (plist-get shape-artist-embed--session :source)))
          (should (string-match-p "Nothing to insert"
                                  (car (test-shape-artist-embed--messages
                                         (shape-artist-embed-commit)))))
          (with-current-buffer source
            (should (equal (buffer-string) original))))))))

(ert-deftest test-shape-artist-embed-detects-diagrams-at-the-buffer-edges ()
  "A diagram at the start or end of the buffer is found once, newline or not."
  (dolist (text '(";; +--+\n;; |  |\n;; +--+"      ; no trailing newline
                  ";; +--+\n;; |  |\n;; +--+\n"   ; trailing newline
                  "+--+\n|  |\n+--+"))             ; no prefix at all
    (ert-info ((format "%S" text))
      (test-shape-artist-embed--with-source #'fundamental-mode text "|  |"
        (let ((found (shape-artist-embed-detect)))
          (should (equal (plist-get found :lines) test-shape-artist-embed--box))
          (should (= (plist-get found :beg) (point-min)))
          (should (equal (buffer-substring (plist-get found :beg) (plist-get found :end))
                         (string-trim-right text "\n"))))))))

(ert-deftest test-shape-artist-embed-ignores-lookalike-comment-leaders ()
  "Connector runs and other glyph soup are not mistaken for comment prefixes."
  (test-shape-artist-embed--with-source #'fundamental-mode
      "+--+---+\n|  |   |\n+--+---+\n" "|  |"
    (should (equal (plist-get (shape-artist-embed-detect) :kind) 'plain)))
  (test-shape-artist-embed--with-source #'fundamental-mode
      "--- +--+\n    |  |\n    +--+\n" "|  |"
    ;; "---" is a connector, not the `--' comment leader.
    (should (equal (plist-get (shape-artist-embed-detect) :prefix) ""))))

(ert-deftest test-shape-artist-embed-detects-unicode-box-diagrams ()
  "Single, rounded, and double Unicode boxes are diagram lines too."
  (dolist (box '(("┌──┐" "│  │" "└──┘")
                 ("╭──╮" "│  │" "╰──╯")
                 ("╔══╗" "║  ║" "╚══╝")))
    (ert-info ((format "%S" box))
      (test-shape-artist-embed--with-source #'fundamental-mode
          (concat "// prose before\n"
                  (mapconcat (lambda (l) (concat "// " l)) box "\n")
                  "\n// prose after\n")
          (nth 1 box)
        (should (equal (plist-get (shape-artist-embed-detect) :lines) box))))))

(provide 'test-shape-artist-embed)
;;; test-shape-artist-embed.el ends here
