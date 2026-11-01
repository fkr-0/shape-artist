;;; shape-artist-embed.el --- Edit ASCII diagrams in place, in any buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Shape Artist Contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Developers keep ASCII diagrams inside source comments, READMEs, Org and
;; Markdown documents.  `shape-artist-edit-at-point' finds the diagram around
;; point, strips its comment prefix and indentation, opens it in an isolated
;; Shape Artist session buffer, and writes it back (prefix restored, everything
;; around it byte-identical) when you commit with `C-c C-c'.  `C-c C-k' aborts.
;; `shape-artist-insert-diagram' starts a new diagram below the current line,
;; inside the same comment style.
;;
;; Recognised homes for a diagram:
;;
;;  - runs of comment lines (`;;', `//', `#', `--', `%', `*', `>', and `:' in
;;    Org), with or without indentation;
;;  - Markdown code fences and Org example/source blocks (the whole interior);
;;  - indented blocks in plain text.
;;
;; In the first and last case only diagram-looking lines are taken, so adjacent
;; prose is left alone.  To edit exactly some lines, select them first: an
;; active region always wins.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'shape-artist-core)

(declare-function shape-artist-ui-show "shape-artist-ui")

;;; --- Recognising diagram lines --------------------------------------------------

(defconst shape-artist-embed--leader-regexp
  (concat "\\`\\([ \t]*\\)"
          "\\(;\\{1,3\\}\\|//[/!]?\\|#+\\|--\\|%+\\|\\*\\|>+\\)"
          "\\(?:[ \t]\\|\\'\\)")
  "Match a comment leader at the start of a line.
Group 1 is the indentation, group 2 the leader.  The leader must be followed by
a blank or the end of the line, so connector runs such as `--->' are not taken
for comments.")

(defconst shape-artist-embed--org-leader-regexp
  "\\`\\([ \t]*\\)\\(:\\)\\(?:[ \t]\\|\\'\\)"
  "Match an Org fixed-width line, which is how Org stores literal text.")

(defun shape-artist-embed--leader (line)
  "Return the comment token (indentation plus leader) opening LINE, or nil."
  (when (or (string-match shape-artist-embed--leader-regexp line)
            (and (derived-mode-p 'org-mode)
                 (string-match shape-artist-embed--org-leader-regexp line)))
    (concat (match-string 1 line) (match-string 2 line))))

(defun shape-artist-embed--strip-token (line token)
  "Return LINE without TOKEN and one following blank, or nil if it lacks TOKEN."
  (when (string-prefix-p token line)
    (let ((rest (substring line (length token))))
      (cond ((string-empty-p rest) "")
            ((memq (aref rest 0) '(?\s ?\t)) (substring rest 1))))))

(defun shape-artist-embed--diagram-line-p (content)
  "Return non-nil when CONTENT (a line without its prefix) is diagram art.
That means box or connector glyphs (`+', `|', box-drawing characters), or a run
made only of line and arrow characters.  Ordinary prose does not qualify."
  (let ((trimmed (string-trim content)))
    (and (not (string-empty-p trimmed))
         (or (string-match-p "[+|─-╿]" trimmed)
             (and (string-match-p "\\`[-=<>^v. ]+\\'" trimmed)
                  (string-match-p "[-=<>^v]" trimmed))))))

;;; --- Locating the block ------------------------------------------------------------

(defun shape-artist-embed--line-bounds ()
  "Return (BOL . EOL) of the current line."
  (cons (line-beginning-position) (line-end-position)))

(defun shape-artist-embed--line-text ()
  "Return the text of the current line."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun shape-artist-embed--indentation (line)
  "Return the leading whitespace of LINE."
  (if (string-match "\\`[ \t]*" line) (match-string 0 line) ""))

(defun shape-artist-embed--common-indentation (lines)
  "Return the longest leading whitespace shared by every non-blank of LINES."
  (let ((common nil))
    (dolist (line lines)
      (unless (string-blank-p line)
        (let ((indent (shape-artist-embed--indentation line)))
          (setq common
                (if common
                    (let ((i 0) (n (min (length common) (length indent))))
                      (while (and (< i n) (eq (aref common i) (aref indent i)))
                        (setq i (1+ i)))
                      (substring common 0 i))
                  indent)))))
    (or common "")))

(defun shape-artist-embed--collect (direction accept)
  "Collect consecutive lines from point in DIRECTION (1 or -1) that ACCEPT.
ACCEPT is called with the line text and returns the stripped content or nil to
stop.  Point must be on a line already accepted; that line is not included.
Return a list of (BOL EOL STRIPPED) in buffer order."
  (let (found)
    (save-excursion
      (let ((previous (line-beginning-position)))
        ;; `forward-line' reports success on a final line without a newline
        ;; even though it stays on that line, so also stop when we did not move.
        (while (and (zerop (forward-line direction))
                    (/= (line-beginning-position) previous)
                    (let* ((bounds (shape-artist-embed--line-bounds))
                           (stripped (funcall accept (shape-artist-embed--line-text))))
                      (setq previous (car bounds))
                      (when stripped
                        (push (list (car bounds) (cdr bounds) stripped) found)
                        t))))))
    (if (> direction 0) (nreverse found) found)))

(defun shape-artist-embed--result (kind prefix entries)
  "Build the detection plist for KIND, PREFIX, and ENTRIES (BOL EOL STRIPPED)."
  (list :kind kind :prefix prefix
        :beg (car (car entries))
        :end (cadr (car (last entries)))
        :lines (mapcar (lambda (e) (nth 2 e)) entries)))

(defun shape-artist-embed--detect-comment-or-plain ()
  "Detect a diagram of comment or plain-indented lines around point."
  (let* ((here (shape-artist-embed--line-text))
         (token (shape-artist-embed--leader here))
         (accept (if token
                     (lambda (line)
                       (let ((stripped (shape-artist-embed--strip-token line token)))
                         (and stripped (shape-artist-embed--diagram-line-p stripped)
                              stripped)))
                   (lambda (line)
                     (and (shape-artist-embed--diagram-line-p line) line))))
         (current (funcall accept here)))
    (unless current
      (user-error "No diagram at point: put point on a box or line, or select the lines to edit"))
    (let* ((bounds (shape-artist-embed--line-bounds))
           (entries (append (shape-artist-embed--collect -1 accept)
                            (list (list (car bounds) (cdr bounds) current))
                            (shape-artist-embed--collect 1 accept))))
      (if token
          (shape-artist-embed--result
           'comment
           (concat token (if (cl-some (lambda (e)
                                        (let ((raw (buffer-substring-no-properties
                                                    (nth 0 e) (nth 1 e))))
                                          (and (> (length raw) (length token))
                                               (memq (aref raw (length token))
                                                     '(?\s ?\t)))))
                                      entries)
                             " " ""))
           entries)
        (let* ((raw (mapcar (lambda (e)
                              (buffer-substring-no-properties (nth 0 e) (nth 1 e)))
                            entries))
               (prefix (shape-artist-embed--common-indentation raw)))
          (shape-artist-embed--result
           'plain prefix
           (cl-mapcar (lambda (e line)
                        (list (nth 0 e) (nth 1 e) (substring line (length prefix))))
                      entries raw)))))))

(defun shape-artist-embed--block-interior (open-line close-line prefix)
  "Return the detection plist for the lines strictly between two delimiters.
OPEN-LINE and CLOSE-LINE are line numbers; PREFIX is the indentation to strip."
  (save-excursion
    (let (entries)
      (goto-char (point-min))
      (forward-line open-line)
      (dotimes (_ (- close-line open-line 1))
        (let* ((bounds (shape-artist-embed--line-bounds))
               (line (shape-artist-embed--line-text)))
          (push (list (car bounds) (cdr bounds)
                      (if (string-prefix-p prefix line)
                          (substring line (length prefix))
                        line))
                entries))
        (forward-line 1))
      (when entries
        (shape-artist-embed--result 'fenced prefix (nreverse entries))))))

(defun shape-artist-embed--detect-org-block ()
  "Detect an Org example, source, or similar block containing point."
  (save-excursion
    (let ((case-fold-search t)
          (point-line (line-number-at-pos)))
      (end-of-line)
      (when (re-search-backward "^\\([ \t]*\\)#\\+begin_\\([[:alnum:]_]+\\)" nil t)
        (let ((indent (match-string 1))
              (name (match-string 2))
              (open-line (1- (line-number-at-pos))))
          (when (re-search-forward
                 (format "^[ \t]*#\\+end_%s\\b" (regexp-quote name)) nil t)
            (let ((close-line (1- (line-number-at-pos))))
              (when (and (< (1+ open-line) point-line)
                         (<= point-line close-line)
                         (< (1- point-line) close-line))
                (shape-artist-embed--block-interior
                 (1+ open-line) (1+ close-line) indent)))))))))

(defun shape-artist-embed--detect-markdown-fence ()
  "Detect a Markdown code fence containing point."
  (save-excursion
    (let ((point-line (line-number-at-pos))
          (open nil) (open-char nil) (open-indent nil)
          (found nil))
      (goto-char (point-min))
      (while (and (not found) (not (eobp)))
        (let ((line (shape-artist-embed--line-text)))
          (when (string-match "\\`\\([ \t]*\\)\\(```\\|~~~\\)" line)
            (let ((fence (match-string 2 line))
                  (number (line-number-at-pos)))
              (cond ((null open)
                     (setq open number open-char fence
                           open-indent (match-string 1 line)))
                    ((equal fence open-char)
                     (when (and (< open point-line) (< point-line number))
                       (setq found (list open number open-indent)))
                     (setq open nil))))))
        (forward-line 1))
      (when found
        (shape-artist-embed--block-interior
         (nth 0 found) (nth 1 found) (nth 2 found))))))

(defun shape-artist-embed--detect-region (beg end)
  "Detect the lines covered by the region from BEG to END."
  (save-excursion
    (let* ((end (progn (goto-char end)
                       (if (and (bolp) (> end beg)) (1- end) end)))
           (first-bol (progn (goto-char beg) (line-beginning-position)))
           (last-eol (progn (goto-char end) (line-end-position)))
           (raw (split-string (buffer-substring-no-properties first-bol last-eol)
                              "\n"))
           (first (cl-find-if-not #'string-blank-p raw))
           (token (and first (shape-artist-embed--leader first)))
           (prefix (if token
                       (concat token
                               (if (string-match-p "\\`[ \t]" (substring first (length token)))
                                   " " ""))
                     (shape-artist-embed--common-indentation raw)))
           (lines (mapcar (lambda (line)
                            (cond ((string-blank-p line) "")
                                  (token (or (shape-artist-embed--strip-token line token)
                                             line))
                                  (t (substring line (min (length prefix)
                                                          (length line))))))
                          raw)))
      (unless (cl-some #'identity (mapcar (lambda (l) (not (string-blank-p l))) lines))
        (user-error "The selected region is empty"))
      (list :kind (if token 'comment 'plain) :prefix prefix
            :beg first-bol :end last-eol :lines lines))))

(defun shape-artist-embed-detect (&optional beg end)
  "Return a plist describing the diagram at point, or in BEG..END.
The plist has `:kind' (`comment', `plain', or `fenced'), `:prefix' (text that
each line starts with and which is stripped), `:beg' and `:end' (buffer
positions of the first line's start and the last line's end), and `:lines',
the stripped lines.  An explicit region, or an active one, wins over
detection.  Signal a `user-error' when there is no diagram."
  (cond
   ((and beg end) (shape-artist-embed--detect-region beg end))
   ((use-region-p)
    (shape-artist-embed--detect-region (region-beginning) (region-end)))
   (t
    (or (if (derived-mode-p 'org-mode)
            (shape-artist-embed--detect-org-block)
          (shape-artist-embed--detect-markdown-fence))
        (shape-artist-embed--detect-comment-or-plain)))))

;;; --- Rendering back ------------------------------------------------------------------

(defun shape-artist-embed--render (lines prefix)
  "Return LINES as text with PREFIX restored on every line.
Trailing whitespace is removed, blank lines get the prefix without its trailing
blanks, and blank rows at the start and end are dropped so canvas padding added
while drawing never leaks into the source."
  (let ((rows (mapcar #'string-trim-right lines)))
    (while (and rows (string-empty-p (car rows))) (setq rows (cdr rows)))
    (setq rows (nreverse rows))
    (while (and rows (string-empty-p (car rows))) (setq rows (cdr rows)))
    (setq rows (nreverse rows))
    (mapconcat (lambda (row)
                 (if (string-empty-p row)
                     (string-trim-right prefix)
                   (concat prefix row)))
               rows "\n")))

;;; --- Sessions -----------------------------------------------------------------------------

(defvar-local shape-artist-embed--session nil
  "Plist describing the editing session this buffer belongs to.
Keys: `:source' (buffer), `:beg' and `:end' (markers around the original lines),
`:prefix', `:kind' (`comment', `plain', `fenced', or `insert'), `:original',
the source text being replaced, and `:at', the insertion marker for new
diagrams.")

(defvar shape-artist-embed-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'shape-artist-embed-commit)
    (define-key map (kbd "C-c C-k") #'shape-artist-embed-abort)
    map)
  "Keymap for `shape-artist-embed-session-mode'.")

(define-minor-mode shape-artist-embed-session-mode
  "Minor mode for a buffer that edits a diagram belonging to another buffer.
\\<shape-artist-embed-session-mode-map>\\[shape-artist-embed-commit] writes the
diagram back; \\[shape-artist-embed-abort] discards the session."
  :lighter " Embed"
  :keymap shape-artist-embed-session-mode-map)

(defun shape-artist-embed--pad (lines)
  "Return LINES right-padded with spaces to equal width."
  (let ((width (apply #'max 0 (mapcar #'length lines))))
    (mapcar (lambda (line) (concat line (make-string (- width (length line)) ?\s)))
            lines)))

(defun shape-artist-embed--open (source lines session)
  "Open an editing buffer for SOURCE showing LINES, with SESSION plist stored."
  (let ((buffer (generate-new-buffer (format "*shape-artist: %s*" (buffer-name source)))))
    (with-current-buffer buffer
      (insert (mapconcat #'identity (shape-artist-embed--pad lines) "\n"))
      (goto-char (point-min))
      (setq shape-artist-embed--session session)
      (shape-artist-embed-session-mode 1)
      (shape-artist-core-mode 1)
      (set-buffer-modified-p nil)
      (setq buffer-undo-list nil))
    (pop-to-buffer buffer)
    (message "Editing diagram: C-c C-c applies it, C-c C-k cancels")
    buffer))

;;;###autoload
(defun shape-artist-edit-at-point ()
  "Edit the ASCII diagram at point (or in the region) in an isolated session.
The diagram's comment prefix and indentation are stripped for editing and
restored by `shape-artist-embed-commit'; nothing else in the buffer is touched."
  (interactive)
  (let* ((found (shape-artist-embed-detect))
         (source (current-buffer))
         (beg (copy-marker (plist-get found :beg)))
         (end (copy-marker (plist-get found :end) t)))
    (shape-artist-embed--open
     source (plist-get found :lines)
     (list :source source :beg beg :end end
           :prefix (plist-get found :prefix) :kind (plist-get found :kind)
           :original (buffer-substring-no-properties beg end)))))

;;;###autoload
(defun shape-artist-insert-diagram (&optional rows cols)
  "Start a new diagram below the current line, in the same comment style.
ROWS by COLS (default 8 by 40) sets the blank canvas.  Nothing is inserted
unless you draw something and commit."
  (interactive (when current-prefix-arg
                 (list (read-number "Rows: " 8) (read-number "Columns: " 40))))
  (let* ((line (shape-artist-embed--line-text))
         (token (shape-artist-embed--leader line))
         (prefix (if token
                     (concat token " ")
                   (shape-artist-embed--indentation line)))
         (source (current-buffer))
         (at (copy-marker (line-end-position))))
    (shape-artist-embed--open
     source
     (make-list (or rows 8) (make-string (or cols 40) ?\s))
     (list :source source :at at :prefix prefix :kind 'insert))))

(defun shape-artist-embed--session-lines ()
  "Return the lines of the current session buffer."
  (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))

(defun shape-artist-embed--finish (source)
  "Kill the current session buffer and return to SOURCE."
  (let ((session (current-buffer)))
    (when (buffer-live-p source) (pop-to-buffer source))
    (with-current-buffer session (set-buffer-modified-p nil))
    (kill-buffer session)))

(defun shape-artist-embed-commit ()
  "Write the edited diagram back into its source buffer and end the session."
  (interactive)
  (unless shape-artist-embed--session
    (user-error "This buffer is not a Shape Artist editing session"))
  (let* ((session shape-artist-embed--session)
         (source (plist-get session :source))
         (kind (plist-get session :kind))
         (text (shape-artist-embed--render (shape-artist-embed--session-lines)
                                           (plist-get session :prefix))))
    (unless (buffer-live-p source)
      (user-error "The source buffer is gone; kill this session with C-c C-k"))
    (if (eq kind 'insert)
        (shape-artist-embed--commit-insert session source text)
      (shape-artist-embed--commit-replace session source text))))

(defun shape-artist-embed--commit-insert (session source text)
  "Insert TEXT below the line recorded in SESSION in SOURCE."
  (if (string-empty-p text)
      (progn (shape-artist-embed--finish source)
             (message "Nothing to insert"))
    (with-current-buffer source
      (goto-char (plist-get session :at))
      (insert "\n" text))
    (shape-artist-embed--finish source)
    (message "Diagram inserted")))

(defun shape-artist-embed--commit-replace (session source text)
  "Replace the diagram lines recorded in SESSION in SOURCE with TEXT."
  (let ((beg (plist-get session :beg))
        (end (plist-get session :end))
        (original (plist-get session :original)))
    (cond
     ((equal text original)
      (shape-artist-embed--finish source)
      (message "No changes to the diagram"))
     (t
      (with-current-buffer source
        (unless (or (equal (buffer-substring-no-properties beg end) original)
                    (y-or-n-p "The source text changed while you were editing; overwrite it? "))
          (user-error "Commit cancelled: the source changed; copy your diagram or abort with C-c C-k"))
        (goto-char beg)
        (delete-region beg end)
        (insert text))
      (shape-artist-embed--finish source)
      (message "Diagram updated")))))

(defun shape-artist-embed-abort ()
  "Discard the editing session without changing the source buffer."
  (interactive)
  (unless shape-artist-embed--session
    (user-error "This buffer is not a Shape Artist editing session"))
  (shape-artist-embed--finish (plist-get shape-artist-embed--session :source))
  (message "Diagram edit cancelled"))

(provide 'shape-artist-embed)
;;; shape-artist-embed.el ends here
