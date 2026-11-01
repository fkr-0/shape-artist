;;; shape-artist-persistence.el --- Shared sidecar root resolution -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Shape Artist Contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Central project-root policy for Shape Artist sidecar files.  Keeping this in
;; one module prevents guides and metadata from choosing different locations.

;;; Code:

(require 'project)

(defgroup shape-artist-persistence nil
  "Project-local sidecar persistence for Shape Artist."
  :group 'shape-artist)

(defcustom shape-artist-persistence-root-function
  #'shape-artist-persistence-default-root
  "Function returning the directory used for Shape Artist sidecars.

The function is called without arguments in the diagram buffer and must return
an existing or creatable directory name."
  :type 'function
  :group 'shape-artist-persistence)

(defun shape-artist-persistence--anchor-directory ()
  "Return the directory from which project discovery should start."
  (file-name-as-directory
   (expand-file-name
    (if buffer-file-name
        (file-name-directory buffer-file-name)
      default-directory))))

(defun shape-artist-persistence--temporary-root-p (directory)
  "Return non-nil when DIRECTORY is the process temporary-file root."
  (let ((directory (file-name-as-directory (expand-file-name directory)))
        (temporary-root
         (file-name-as-directory (expand-file-name temporary-file-directory))))
    (or (equal directory temporary-root)
        (and (file-exists-p directory)
             (file-exists-p temporary-root)
             (file-equal-p directory temporary-root)))))

(defun shape-artist-persistence--project-primary-root (project)
  "Return PROJECT's primary root across supported Emacs versions."
  (if (fboundp 'project-root)
      ;; Avoid a direct call so package-lint does not raise the package minimum
      ;; above Emacs 27.1 merely for this runtime-selected compatibility path.
      (funcall 'project-root project)
    (with-suppressed-warnings ((obsolete project-roots))
      (car (project-roots project)))))

(defun shape-artist-persistence-default-root ()
  "Return a safe project root for the current diagram buffer.

Use Emacs' public project API when it discovers a meaningful project.  Ignore
an ambient project marker placed directly at variable
`temporary-file-directory', since that would make unrelated temporary diagrams
share and overwrite one global sidecar.  A real nested project inside the
temporary directory is still used."
  (let* ((anchor (shape-artist-persistence--anchor-directory))
         (project (project-current nil anchor))
         (candidate
          (and project
               (shape-artist-persistence--project-primary-root project))))
    ;; `project-root' may return an abbreviated name such as ~/work/..., so
    ;; always hand back an absolute, expanded directory.
    (file-name-as-directory
     (expand-file-name
      (if (and candidate
               (not (shape-artist-persistence--temporary-root-p candidate)))
          candidate
        anchor)))))

(defun shape-artist-persistence-root ()
  "Return the normalized persistence root for the current buffer."
  (let ((root (funcall shape-artist-persistence-root-function)))
    (unless (and (stringp root) (not (string-empty-p root)))
      (user-error "Shape Artist persistence root function returned %S; set `shape-artist-persistence-root-function' to a function that returns a directory name" root))
    (file-name-as-directory (expand-file-name root))))

(provide 'shape-artist-persistence)
;;; shape-artist-persistence.el ends here
