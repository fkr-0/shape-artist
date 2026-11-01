;;; test-shape-artist-persistence.el --- Persistence root tests -*- lexical-binding: t; -*-

(require 'ert)
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

(provide 'test-shape-artist-persistence)
;;; test-shape-artist-persistence.el ends here
