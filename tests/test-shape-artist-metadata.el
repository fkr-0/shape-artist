;;; test-shape-artist-metadata.el --- ERT tests for shape metadata -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-metadata)

(ert-deftest test-shape-artist-metadata-assigns-ids ()
  "Parsed shapes receive non-nil metadata IDs."
  (test-artist-with-buffer
      "+---+\n| a |\n+---+"
    (let* ((result (shape-artist-diagram-parse-buffer))
           (shapes (cdr result))
           (ids (mapcar #'shape-artist-metadata-shape-id shapes)))
      (should shapes)
      (should-not (cl-find nil ids))
      (should (cl-every (lambda (id) (string-prefix-p "sa-" id)) ids)))))

(ert-deftest test-shape-artist-metadata-preserves-id-across-unchanged-reparse ()
  "An unchanged shape keeps its ID across reparses."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let* ((first (cdr (shape-artist-diagram-parse-buffer)))
           (first-ids (mapcar #'shape-artist-metadata-shape-id first))
           (second (cdr (shape-artist-diagram-parse-buffer)))
           (second-ids (mapcar #'shape-artist-metadata-shape-id second)))
      (should (equal first-ids second-ids)))))

(ert-deftest test-shape-artist-metadata-preserves-existing-id-when-shape-added ()
  "Adding a new shape preserves the ID for unchanged existing shapes."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (let* ((first-shapes (cdr (shape-artist-diagram-parse-buffer)))
           (first-id (shape-artist-metadata-shape-id (car first-shapes))))
      (goto-char (point-max))
      (insert "\n\n+---+\n|   |\n+---+")
      (let* ((second-shapes (cdr (shape-artist-diagram-parse-buffer)))
             (second-ids (mapcar #'shape-artist-metadata-shape-id second-shapes)))
        (should (member first-id second-ids))
        (should (= (length second-ids)
                   (length (delete-dups (copy-sequence second-ids)))))))))

(ert-deftest test-shape-artist-metadata-reset-restarts-id-sequence ()
  "Resetting metadata clears the ID map and restarts generated IDs."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-diagram-parse-buffer)
    (shape-artist-metadata-reset t)
    (let* ((shapes (cdr (shape-artist-diagram-parse-buffer)))
           (id (shape-artist-metadata-shape-id (car shapes))))
      (should (equal id "sa-1")))))

(ert-deftest test-shape-artist-metadata-snapshot-includes-kind-signature-and-bounds ()
  "Metadata snapshots expose the essential shape identity fields."
  (test-artist-with-buffer
      "+---+\n| x |\n+---+"
    (shape-artist-diagram-parse-buffer)
    (let* ((snapshot (shape-artist-metadata-snapshot))
           (first (car snapshot)))
      (should (alist-get 'id first))
      (should (alist-get 'kind first))
      (should (alist-get 'signature first))
      (should (alist-get 'bounds first)))))

(ert-deftest test-shape-artist-metadata-save-load-roundtrip ()
  "Saved metadata restores IDs across reset and reparsing."
  (let ((temp-dir (make-temp-file "shape-artist-metadata" t)))
    (unwind-protect
        (let* ((file (expand-file-name "diagram.txt" temp-dir))
               (default-directory temp-dir)
               (shape-artist-metadata-id-prefix "persist"))
          (with-temp-file file
            (insert "+---+\n| x |\n+---+"))
          (test-artist-with-buffer
              "+---+\n| x |\n+---+"
            (setq buffer-file-name file)
            (let* ((first (cdr (shape-artist-diagram-parse-buffer)))
                   (saved-id (shape-artist-metadata-shape-id (car first))))
              (shape-artist-metadata-save)
              (shape-artist-metadata-reset t)
              (let ((shape-artist-metadata-id-prefix "fresh"))
                (shape-artist-metadata-load)
                (let* ((loaded shape-artist--last-shapes-list)
                       (loaded-id (shape-artist-metadata-shape-id (car loaded))))
                  (should (equal loaded-id saved-id))
                  (should (string-prefix-p "persist-" loaded-id)))))))
      (delete-directory temp-dir t))))

(ert-deftest test-shape-artist-metadata-delete-saved-removes-current-buffer-entry ()
  "Deleting saved metadata removes only the current buffer entry."
  (let ((temp-dir (make-temp-file "shape-artist-metadata" t)))
    (unwind-protect
        (let* ((file (expand-file-name "diagram.txt" temp-dir))
               (default-directory temp-dir))
          (with-temp-file file
            (insert "+---+\n|   |\n+---+"))
          (test-artist-with-buffer
              "+---+\n|   |\n+---+"
            (setq buffer-file-name file)
            (shape-artist-diagram-parse-buffer)
            (shape-artist-metadata-save)
            (shape-artist-metadata-delete-saved)
            (let* ((db (shape-artist-metadata--read-db
                        (shape-artist-metadata--persistence-path)))
                   (key (shape-artist-metadata--buffer-key)))
              (should-not (assoc key (plist-get db :buffers))))))
      (delete-directory temp-dir t))))

(ert-deftest test-shape-artist-metadata-save-requires-file-backed-buffer ()
  "Saving metadata reports a user error without `buffer-file-name'."
  (test-artist-with-buffer
      "+---+\n|   |\n+---+"
    (shape-artist-diagram-parse-buffer)
    (let ((buffer-file-name nil))
      (should-error (shape-artist-metadata-save) :type 'user-error))))

(provide 'test-shape-artist-metadata)
;;; test-shape-artist-metadata.el ends here
