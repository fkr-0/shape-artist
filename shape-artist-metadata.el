;;; shape-artist-metadata.el --- Stable IDs and metadata snapshots for parsed shapes -*- lexical-binding: t; -*-

;;; Commentary:

;;; Shape Artist intentionally keeps diagrams as plain ASCII text.  This module
;;; adds a small metadata layer on top of parse results: parsed shape objects get
;;; best-effort, buffer-local IDs that remain stable across unchanged reparses.
;;; IDs can also be saved to a project-local sidecar so references survive Emacs
;;; restarts without modifying the ASCII diagram text.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'shape-artist-persistence)

(defgroup shape-artist-metadata nil
  "Metadata and identity support for parsed Shape Artist diagrams."
  :group 'shape-artist
  :prefix "shape-artist-metadata-")

(defcustom shape-artist-metadata-id-prefix "sa"
  "Prefix used when generating buffer-local shape identifiers."
  :type 'string
  :group 'shape-artist-metadata)

(defcustom shape-artist-metadata-persistence-file ".shape-artist/metadata.el"
  "Project-relative sidecar path used for persisted shape metadata IDs."
  :type 'string
  :group 'shape-artist-metadata)

(defvar-local shape-artist-metadata--shape-id-map nil
  "Hash table mapping normalized shape signatures to stable shape IDs.

Keys are of the form (SIGNATURE . OCCURRENCE), where OCCURRENCE is a
1-based duplicate counter for shapes with identical signatures.")

(defvar-local shape-artist-metadata--next-id 1
  "Next numeric suffix used by `shape-artist-metadata--new-id'.")

(declare-function shape-artist-diagram-parse-buffer "shape-artist-diagram-parse")
(declare-function shape-artist--maybe-update-sidebar "shape-artist-core")

(defun shape-artist-metadata-reset (&optional quiet)
  "Clear buffer-local Shape Artist metadata identity state.

When QUIET is non-nil, do not show a status message."
  (interactive)
  (setq shape-artist-metadata--shape-id-map nil)
  (setq shape-artist-metadata--next-id 1)
  (unless quiet
    (message "Shape Artist metadata reset.")))

(defun shape-artist-metadata--new-id ()
  "Return a fresh buffer-local shape ID."
  (prog1 (format "%s-%d" shape-artist-metadata-id-prefix
                 shape-artist-metadata--next-id)
    (cl-incf shape-artist-metadata--next-id)))

(defun shape-artist-metadata-shape-id (shape)
  "Return the assigned metadata ID for SHAPE, or nil."
  (ignore-errors (oref shape id)))

(defun shape-artist-metadata--slot (shape slot)
  "Return SHAPE's SLOT value without byte-compiler slot validation."
  (eieio-oref shape slot))

(defun shape-artist-metadata--shape-kind (shape)
  "Return a metadata kind symbol for SHAPE."
  (cond
   ((object-of-class-p shape 'shape-artist-diagram-root) 'root)
   ((object-of-class-p shape 'shape-artist-diagram-rectangle) 'rectangle)
   ((object-of-class-p shape 'shape-artist-diagram-edge) 'edge)
   ((object-of-class-p shape 'shape-artist-diagram-label) 'label)
   (t 'node)))

(defun shape-artist-metadata-shape-signature (shape)
  "Return a normalized geometry/content signature for SHAPE.

The signature deliberately excludes the metadata ID.  It is stable for an
unchanged parse result and changes when the shape geometry or label content
changes.  This keeps identity best-effort and predictable until persisted
metadata sidecars are implemented."
  (let ((kind (shape-artist-metadata--shape-kind shape)))
    (pcase kind
      ('root (list :kind 'root))
      ('rectangle
       (let ((x (shape-artist-metadata--slot shape 'x))
             (y (shape-artist-metadata--slot shape 'y))
             (width (shape-artist-metadata--slot shape 'width))
             (height (shape-artist-metadata--slot shape 'height))
             (rect-kind (shape-artist-metadata--slot shape 'kind)))
         (list :kind kind
               :rect-kind rect-kind
               :x x
               :y y
               :width width
               :height height)))
      ('edge
       (let ((x (shape-artist-metadata--slot shape 'x))
             (y (shape-artist-metadata--slot shape 'y))
             (x2 (shape-artist-metadata--slot shape 'x2))
             (y2 (shape-artist-metadata--slot shape 'y2))
             (edge-type (shape-artist-metadata--slot shape 'edge-type))
             (arrow-start (shape-artist-metadata--slot shape 'arrow-start))
             (arrow-end (shape-artist-metadata--slot shape 'arrow-end))
             (start-attachment (shape-artist-metadata--slot shape 'start-attachment))
             (end-attachment (shape-artist-metadata--slot shape 'end-attachment)))
         (list :kind kind
               :x x
               :y y
               :x2 x2
               :y2 y2
               :edge-type edge-type
               :arrow-start arrow-start
               :arrow-end arrow-end
               :start-attachment start-attachment
               :end-attachment end-attachment)))
      ('label
       (let ((x (shape-artist-metadata--slot shape 'x))
             (y (shape-artist-metadata--slot shape 'y))
             (width (shape-artist-metadata--slot shape 'width))
             (height (shape-artist-metadata--slot shape 'height))
             (text (shape-artist-metadata--slot shape 'text))
             (align (shape-artist-metadata--slot shape 'align))
             (role (shape-artist-metadata--slot shape 'role)))
         (list :kind kind
               :x x
               :y y
               :width width
               :height height
               :text text
               :align align
               :role role)))
      (_
       (let ((x (shape-artist-metadata--slot shape 'x))
             (y (shape-artist-metadata--slot shape 'y))
             (width (shape-artist-metadata--slot shape 'width))
             (height (shape-artist-metadata--slot shape 'height)))
         (list :kind kind
               :x x
               :y y
               :width width
               :height height))))))

(defun shape-artist-metadata--signature-key (signature seen)
  "Return occurrence-qualified key for SIGNATURE and update SEEN table."
  (let ((occurrence (1+ (gethash signature seen 0))))
    (puthash signature occurrence seen)
    (cons signature occurrence)))

(defun shape-artist-metadata-reconcile-shapes (shapes)
  "Assign stable best-effort metadata IDs to SHAPES.

IDs are preserved across reparses when a shape's normalized signature and
duplicate occurrence remain unchanged.  New or changed shapes receive fresh IDs.
Return SHAPES."
  (let ((old shape-artist-metadata--shape-id-map)
        (new (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal)))
    (dolist (shape shapes)
      (let* ((signature (shape-artist-metadata-shape-signature shape))
             (key (shape-artist-metadata--signature-key signature seen))
             (id (and (hash-table-p old) (gethash key old))))
        (unless id
          (setq id (shape-artist-metadata--new-id)))
        (ignore-errors (oset shape id id))
        (puthash key id new)))
    (setq shape-artist-metadata--shape-id-map new)
    shapes))

(defun shape-artist-metadata-shape-snapshot (shape)
  "Return an alist snapshot for SHAPE suitable for later serialization."
  (let ((kind (shape-artist-metadata--shape-kind shape)))
    `((id . ,(shape-artist-metadata-shape-id shape))
      (kind . ,kind)
      (signature . ,(shape-artist-metadata-shape-signature shape))
      (bounds . ,(if (eq kind 'edge)
                     (let ((x (shape-artist-metadata--slot shape 'x))
                           (y (shape-artist-metadata--slot shape 'y))
                           (x2 (shape-artist-metadata--slot shape 'x2))
                           (y2 (shape-artist-metadata--slot shape 'y2)))
                       (list (min x x2) (min y y2) (max x x2) (max y y2)))
                   (let ((x (shape-artist-metadata--slot shape 'x))
                         (y (shape-artist-metadata--slot shape 'y))
                         (width (shape-artist-metadata--slot shape 'width))
                         (height (shape-artist-metadata--slot shape 'height)))
                     (list x y (+ x width) (+ y height))))))))

(defun shape-artist-metadata-snapshot (&optional shapes)
  "Return metadata snapshots for SHAPES or the current parse result."
  (let ((items (or shapes (and (boundp 'shape-artist--last-shapes-list)
                               shape-artist--last-shapes-list))))
    (mapcar #'shape-artist-metadata-shape-snapshot items)))

(defun shape-artist-metadata--map-entries (&optional map)
  "Return a deterministic entry list for identity MAP."
  (let ((entries nil)
        (source (or map shape-artist-metadata--shape-id-map)))
    (when (hash-table-p source)
      (maphash (lambda (key id)
                 (push (list :key key :id id) entries))
               source))
    (sort entries
          (lambda (a b)
            (string< (or (plist-get a :id) "")
                     (or (plist-get b :id) ""))))))

(defun shape-artist-metadata--id-suffix (id)
  "Return numeric suffix from ID when it matches the current ID prefix."
  (when (and (stringp id)
             (string-match
              (concat "\\`" (regexp-quote shape-artist-metadata-id-prefix)
                      "-\\([0-9]+\\)\\'")
              id))
    (string-to-number (match-string 1 id))))

(defun shape-artist-metadata--minimum-next-id (entries)
  "Return the minimum collision-free next ID counter for ENTRIES."
  (let ((max-seen 0))
    (dolist (entry entries)
      (let ((suffix (shape-artist-metadata--id-suffix (plist-get entry :id))))
        (when suffix
          (setq max-seen (max max-seen suffix)))))
    (1+ max-seen)))

(defun shape-artist-metadata--identity-state ()
  "Return serializable identity state for the current buffer."
  (list :version 1
        :next-id shape-artist-metadata--next-id
        :entries (shape-artist-metadata--map-entries)))

(defun shape-artist-metadata--apply-identity-state (state)
  "Restore buffer-local identity STATE."
  (unless (and (listp state) (plist-member state :entries))
    (user-error "Invalid Shape Artist metadata state"))
  (let ((map (make-hash-table :test 'equal))
        (entries (plist-get state :entries)))
    (dolist (entry entries)
      (let ((key (plist-get entry :key))
            (id (plist-get entry :id)))
        (when (and key id)
          (puthash key id map))))
    (setq shape-artist-metadata--shape-id-map map)
    (setq shape-artist-metadata--next-id
          (max (or (plist-get state :next-id) 1)
               (shape-artist-metadata--minimum-next-id entries)))))

(defun shape-artist-metadata--project-root ()
  "Return the current project root for metadata persistence."
  (shape-artist-persistence-root))

(defun shape-artist-metadata--buffer-key ()
  "Return stable persistence key for the current buffer."
  (unless buffer-file-name
    (user-error "Metadata persistence requires a file-backed buffer"))
  (file-relative-name (expand-file-name buffer-file-name)
                      (shape-artist-metadata--project-root)))

(defun shape-artist-metadata--persistence-path ()
  "Return absolute path of the metadata persistence sidecar file."
  (expand-file-name shape-artist-metadata-persistence-file
                    (shape-artist-metadata--project-root)))

(defun shape-artist-metadata--read-db (file)
  "Read metadata persistence database from FILE."
  (if (not (file-exists-p file))
      (list :version 1 :buffers nil)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((data (read (current-buffer))))
        (unless (and (listp data) (plist-member data :buffers))
          (user-error "Invalid Shape Artist metadata persistence file: %s" file))
        data))))

(defun shape-artist-metadata--write-db (file db)
  "Write metadata persistence database DB to FILE."
  (make-directory (file-name-directory file) t)
  (let ((print-length nil)
        (print-level nil))
    (with-temp-file file
      (insert ";;; Shape Artist metadata state.  Generated file; edit with care.\n")
      (prin1 db (current-buffer))
      (insert "\n"))))

(defun shape-artist-metadata--ensure-parsed ()
  "Parse the current buffer when no identity map is available yet."
  (when (and (fboundp 'shape-artist-diagram-parse-buffer)
             (or (not (hash-table-p shape-artist-metadata--shape-id-map))
                 (= (hash-table-count shape-artist-metadata--shape-id-map) 0)))
    (shape-artist-diagram-parse-buffer (current-buffer))))

(defun shape-artist-metadata--refresh-after-load ()
  "Refresh parse results and sidebar after loading identity state."
  (when (fboundp 'shape-artist-diagram-parse-buffer)
    (shape-artist-diagram-parse-buffer (current-buffer)))
  (when (fboundp 'shape-artist--maybe-update-sidebar)
    (shape-artist--maybe-update-sidebar)))

(defun shape-artist-metadata-save ()
  "Save current buffer metadata IDs to the project sidecar file."
  (interactive)
  (shape-artist-metadata--ensure-parsed)
  (let* ((file (shape-artist-metadata--persistence-path))
         (key (shape-artist-metadata--buffer-key))
         (db (shape-artist-metadata--read-db file))
         (buffers (plist-get db :buffers))
         (state (shape-artist-metadata--identity-state))
         (cell (assoc key buffers)))
    (if cell
        (setcdr cell state)
      (push (cons key state) buffers))
    (setq db (plist-put db :buffers buffers))
    (shape-artist-metadata--write-db file db)
    (message "Saved Shape Artist metadata for %s" key)))

(defun shape-artist-metadata-load ()
  "Load current buffer metadata IDs from the project sidecar file."
  (interactive)
  (let* ((file (shape-artist-metadata--persistence-path))
         (key (shape-artist-metadata--buffer-key))
         (db (shape-artist-metadata--read-db file))
         (state (cdr (assoc key (plist-get db :buffers)))))
    (if (not state)
        (message "No saved Shape Artist metadata for %s" key)
      (shape-artist-metadata--apply-identity-state state)
      (shape-artist-metadata--refresh-after-load)
      (message "Loaded Shape Artist metadata for %s" key))))

(defun shape-artist-metadata-delete-saved ()
  "Delete current buffer metadata IDs from the project sidecar file."
  (interactive)
  (let* ((file (shape-artist-metadata--persistence-path))
         (key (shape-artist-metadata--buffer-key))
         (db (shape-artist-metadata--read-db file))
         (buffers (cl-remove key (plist-get db :buffers)
                             :key #'car :test #'equal)))
    (setq db (plist-put db :buffers buffers))
    (shape-artist-metadata--write-db file db)
    (message "Deleted saved Shape Artist metadata for %s" key)))

(provide 'shape-artist-metadata)
;;; shape-artist-metadata.el ends here
