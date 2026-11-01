;;; test-shape-artist-persistence.el --- Persistence root tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'shape-artist-persistence)

(ert-deftest test-shape-artist-persistence-ignores-ambient-temp-project ()
  "A project marker at the temp root must not capture unrelated diagrams."
  (let* ((ambient (make-temp-file "shape-artist-ambient" t))
         (diagram-dir (expand-file-name "diagram" ambient))
         (temporary-file-directory (file-name-as-directory ambient))
         (default-directory (file-name-as-directory diagram-dir))
         (buffer-file-name (expand-file-name "drawing.txt" diagram-dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" ambient))
          (make-directory diagram-dir)
          (should (equal (shape-artist-persistence-default-root)
                         (file-name-as-directory diagram-dir))))
      (delete-directory ambient t))))

(ert-deftest test-shape-artist-persistence-uses-nested-real-project ()
  "A nested project keeps project-local sidecars even under the temp root."
  (let* ((ambient (make-temp-file "shape-artist-ambient" t))
         (project-dir (expand-file-name "project" ambient))
         (diagram-dir (expand-file-name "doc" project-dir))
         (temporary-file-directory (file-name-as-directory ambient))
         (default-directory (file-name-as-directory diagram-dir))
         (buffer-file-name (expand-file-name "drawing.txt" diagram-dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" ambient))
          (make-directory (expand-file-name ".git" project-dir) t)
          (make-directory diagram-dir t)
          (should (equal (shape-artist-persistence-default-root)
                         (file-name-as-directory project-dir))))
      (delete-directory ambient t))))

(ert-deftest test-shape-artist-persistence-root-is-normalized-directory ()
  "The configured root function's result becomes an absolute directory name."
  (let ((shape-artist-persistence-root-function (lambda () "/tmp/shape-artist-root")))
    (should (equal (shape-artist-persistence-root) "/tmp/shape-artist-root/")))
  (let ((default-directory "/tmp/")
        (shape-artist-persistence-root-function (lambda () "nested/dir/")))
    (should (equal (shape-artist-persistence-root) "/tmp/nested/dir/"))))

(ert-deftest test-shape-artist-persistence-root-rejects-invalid-results ()
  "A root function returning nil, an empty string, or a non-string is an error."
  (dolist (bad '(nil "" 42 (a list)))
    (ert-info ((format "root function returned %S" bad))
      (let ((shape-artist-persistence-root-function (lambda () bad)))
        (should-error (shape-artist-persistence-root) :type 'user-error)))))

(ert-deftest test-shape-artist-persistence-root-error-names-the-option ()
  "An invalid root names the option to configure."
  (let ((shape-artist-persistence-root-function (lambda () nil)))
    (should (string-match-p
             "shape-artist-persistence-root-function"
             (error-message-string
              (should-error (shape-artist-persistence-root) :type 'user-error))))))

(ert-deftest test-shape-artist-persistence-default-root-expands-abbreviated-project-roots ()
  "A project root that project.el reports as ~/... comes back absolute."
  (let* ((home (file-name-as-directory (make-temp-file "shape-artist-home" t)))
         (process-environment (cons (concat "HOME=" home) process-environment))
         (project-dir (expand-file-name "work/proj/" home))
         (buffer-file-name (expand-file-name "doc/drawing.txt" project-dir))
         (default-directory (file-name-directory buffer-file-name)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory buffer-file-name) t)
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&rest _) (list 'fake)))
                    ((symbol-function 'project-root)
                     (lambda (_project) "~/work/proj/")))
            (should (equal (shape-artist-persistence-default-root) project-dir))))
      (delete-directory home t))))

(provide 'test-shape-artist-persistence)
;;; test-shape-artist-persistence.el ends here
