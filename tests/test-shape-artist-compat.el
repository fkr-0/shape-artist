;;; test-shape-artist-compat.el --- Guard against post-27.1 Emacs functions -*- lexical-binding: t; -*-

;; The package declares Emacs 27.1 as its minimum, but development happens on
;; newer Emacs versions where a newer function can slip in unnoticed.  These
;; tests scan the sources for functions and idioms that do not exist in 27.1.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst test-shape-artist-compat--forbidden
  '(("(setf (plist-get" . "has no setter in Emacs 27; use `plist-put'")
    ("(string-split" . "added in Emacs 29; use `split-string'")
    ("(string-search" . "added in Emacs 28; use `string-match-p' with `regexp-quote'")
    ("(string-replace" . "added in Emacs 28; use `replace-regexp-in-string'")
    ("(string-pad" . "added in Emacs 29")
    ("(length=" . "added in Emacs 28")
    ("(length<" . "added in Emacs 28")
    ("(length>" . "added in Emacs 28")
    ("(ensure-list" . "added in Emacs 28; use `ensure-list' from `subr-x' or `if'")
    ("(named-let" . "added in Emacs 28")
    ("(file-name-concat" . "added in Emacs 28; use `expand-file-name'")
    ("(plistp" . "added in Emacs 29")
    ("(keymap-set" . "added in Emacs 29; use `define-key'")
    ("(keymap-lookup" . "added in Emacs 29; use `lookup-key'")
    ("(define-keymap" . "added in Emacs 29")
    ("(key-valid-p" . "added in Emacs 29")
    ("(seq-keep" . "added in Emacs 29")
    ("(take " . "added in Emacs 29; use `seq-take'")
    ("(ntake " . "added in Emacs 29"))
  "Source fragments that do not exist in Emacs 27.1, with the reason and the fix.")

(defun test-shape-artist-compat--violations (text)
  "Return a list of (LINE FRAGMENT REASON) for each forbidden fragment in TEXT."
  (let ((violations nil))
    (with-temp-buffer
      (insert text)
      ;; Comment detection below needs the Emacs Lisp syntax table.
      (delay-mode-hooks (emacs-lisp-mode))
      (dolist (entry test-shape-artist-compat--forbidden)
        (goto-char (point-min))
        (while (search-forward (car entry) nil t)
          ;; Skip occurrences inside comments and docstrings' prose by requiring
          ;; the fragment to start a form: the preceding character is
          ;; whitespace, an opening paren, a quote, or the line start.
          (let ((start (match-beginning 0)))
            (unless (save-excursion
                      (goto-char start)
                      (nth 4 (syntax-ppss)))
              (push (list (line-number-at-pos start) (car entry) (cdr entry))
                    violations))))))
    (sort violations (lambda (a b) (< (car a) (car b))))))

(defun test-shape-artist-compat--source-files (directory patterns)
  "Return the files in DIRECTORY matching any of the glob PATTERNS."
  (cl-loop for pattern in patterns
           append (directory-files directory t pattern)))

(ert-deftest test-shape-artist-compat-scanner-flags-known-offenders ()
  "The scanner reports forbidden idioms, with their line and the advice."
  (let ((found (test-shape-artist-compat--violations
                "(defun f (plan)\n  (setf (plist-get plan :x) 1)\n  (string-split s \",\"))\n")))
    (should (equal (mapcar #'car found) '(2 3)))
    (should (string-match-p "plist-put" (nth 2 (car found))))
    (should (string-match-p "split-string" (nth 2 (cadr found))))))

(ert-deftest test-shape-artist-compat-scanner-ignores-comments ()
  "A forbidden name mentioned in a comment is not a violation."
  (should-not (test-shape-artist-compat--violations
               ";; avoid (string-split) here\n(defun f () nil)\n")))

(ert-deftest test-shape-artist-compat-package-sources-avoid-post-27-functions ()
  "No package source uses a function newer than the declared Emacs 27.1."
  (let* ((root (file-name-directory (locate-library "shape-artist.el")))
         (files (test-shape-artist-compat--source-files
                 root '("\\`shape-artist.*\\.el\\'" "\\`artist-diagram-.*\\.el\\'")))
         (problems nil))
    (should (> (length files) 15))
    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (emacs-lisp-mode)
        (dolist (v (test-shape-artist-compat--violations (buffer-string)))
          (push (format "%s:%d: %s -- %s" (file-name-nondirectory file)
                        (nth 0 v) (nth 1 v) (nth 2 v))
                problems))))
    (should-not problems)))

(ert-deftest test-shape-artist-compat-test-sources-avoid-post-27-functions ()
  "The tests must run on Emacs 27.1 as well."
  (let* ((root (file-name-directory (locate-library "shape-artist.el")))
         (files (test-shape-artist-compat--source-files
                 (expand-file-name "tests" root) '("\\`test-.*\\.el\\'")))
         (problems nil))
    (dolist (file files)
      ;; This file lists the forbidden fragments as data.
      (unless (equal (file-name-nondirectory file) "test-shape-artist-compat.el")
        (with-temp-buffer
          (insert-file-contents file)
          (emacs-lisp-mode)
          (dolist (v (test-shape-artist-compat--violations (buffer-string)))
            (push (format "%s:%d: %s -- %s" (file-name-nondirectory file)
                          (nth 0 v) (nth 1 v) (nth 2 v))
                  problems)))))
    (should-not problems)))

(provide 'test-shape-artist-compat)
;;; test-shape-artist-compat.el ends here
