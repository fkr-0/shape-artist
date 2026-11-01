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

;;; --- Signature and Snapshot Tests ---

(defun test-shape-artist-metadata--shape-of (kind)
  "Return the first parsed shape of KIND (a predicate symbol) in the current buffer."
  (cl-find-if kind (cdr (shape-artist-diagram-parse-buffer))))

(ert-deftest test-shape-artist-metadata-signatures-describe-each-shape-kind ()
  "Every shape kind has a signature naming its kind and key geometry."
  (test-artist-with-buffer "+---+\n| a |\n+---+\n\n---"
    (let ((rect (test-shape-artist-metadata--shape-of #'shape-artist-diagram-rectangle-p))
          (edge (test-shape-artist-metadata--shape-of #'shape-artist-diagram-edge-p))
          (label (test-shape-artist-metadata--shape-of #'shape-artist-diagram-label-p)))
      (let ((sig (shape-artist-metadata-shape-signature rect)))
        (should (eq (plist-get sig :kind) 'rectangle))
        (should (equal (list (plist-get sig :x) (plist-get sig :y)
                             (plist-get sig :width) (plist-get sig :height))
                       '(0 0 4 2))))
      (let ((sig (shape-artist-metadata-shape-signature edge)))
        (should (eq (plist-get sig :kind) 'edge))
        (should (equal (list (plist-get sig :x) (plist-get sig :y)
                             (plist-get sig :x2) (plist-get sig :y2))
                       '(0 4 2 4))))
      (let ((sig (shape-artist-metadata-shape-signature label)))
        (should (eq (plist-get sig :kind) 'label))
        (should (equal (string-trim (plist-get sig :text)) "a"))))
    (should (equal (shape-artist-metadata-shape-signature
                    (make-instance 'shape-artist-diagram-root))
                   '(:kind root)))
    (let ((sig (shape-artist-metadata-shape-signature
                (make-instance 'shape-artist-diagram-node
                               :x 1 :y 2 :width 3 :height 4))))
      (should (equal (list (plist-get sig :x) (plist-get sig :y)
                           (plist-get sig :width) (plist-get sig :height))
                     '(1 2 3 4))))))

(ert-deftest test-shape-artist-metadata-signature-ignores-id-but-tracks-geometry ()
  "Signatures exclude the ID, and change when geometry changes."
  (let ((a (make-instance 'shape-artist-diagram-rectangle
                          :x 0 :y 0 :width 3 :height 2 :id "one"))
        (b (make-instance 'shape-artist-diagram-rectangle
                          :x 0 :y 0 :width 3 :height 2 :id "two"))
        (c (make-instance 'shape-artist-diagram-rectangle
                          :x 1 :y 0 :width 3 :height 2)))
    (should (equal (shape-artist-metadata-shape-signature a)
                   (shape-artist-metadata-shape-signature b)))
    (should-not (equal (shape-artist-metadata-shape-signature a)
                       (shape-artist-metadata-shape-signature c)))))

(ert-deftest test-shape-artist-metadata-snapshot-reports-id-kind-and-bounds ()
  "Snapshots expose ID, kind, signature, and normalized bounds for serialization."
  (test-artist-with-buffer "+---+\n|   |\n+---+\n\n---"
    (shape-artist-diagram-parse-buffer)
    (let* ((snapshots (shape-artist-metadata-snapshot))
           (rect (cl-find 'rectangle snapshots :key (lambda (s) (alist-get 'kind s))))
           (edge (cl-find 'edge snapshots :key (lambda (s) (alist-get 'kind s)))))
      (should (stringp (alist-get 'id rect)))
      (should (equal (alist-get 'bounds rect) '(0 0 4 2)))
      (should (equal (alist-get 'bounds edge) '(0 4 2 4)))
      (should (eq (plist-get (alist-get 'signature rect) :kind) 'rectangle)))
    ;; An explicit list of shapes is honored instead of the current parse.
    (should (equal (shape-artist-metadata-snapshot
                    (list (make-instance 'shape-artist-diagram-rectangle
                                         :x 2 :y 3 :width 4 :height 5)))
                   `(((id . nil) (kind . rectangle)
                      (signature . ,(shape-artist-metadata-shape-signature
                                     (make-instance 'shape-artist-diagram-rectangle
                                                    :x 2 :y 3 :width 4 :height 5)))
                      (bounds . (2 3 6 8))))))))

(ert-deftest test-shape-artist-metadata-edge-bounds-are-normalized ()
  "Edge snapshot bounds are ordered min-to-max regardless of direction."
  (let ((edge (make-instance 'shape-artist-diagram-edge :x 5 :y 4 :x2 1 :y2 2)))
    (should (equal (alist-get 'bounds (shape-artist-metadata-shape-snapshot edge))
                   '(1 2 5 4)))))

(ert-deftest test-shape-artist-metadata-errors-say-what-to-do ()
  "Metadata persistence errors point at the remedy."
  (test-artist-with-buffer "+---+\n|   |\n+---+"
    (should (string-match-p
             "save this buffer"
             (error-message-string
              (should-error (shape-artist-metadata-save) :type 'user-error)))))
  (should (string-match-p
           "shape-artist-metadata-delete-saved"
           (error-message-string
            (should-error (shape-artist-metadata--apply-identity-state '(:bogus t))
                          :type 'user-error)))))

(provide 'test-shape-artist-metadata)
;;; test-shape-artist-metadata.el ends here
