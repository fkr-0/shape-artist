;;; shape-artist-export.el --- Export diagrams as Mermaid, DOT and SVG -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Shape Artist Contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Turn the parsed ASCII diagram into text other tools understand:
;;
;;   mermaid -- a `flowchart' for Mermaid renderers
;;   dot     -- a `digraph' for Graphviz
;;   svg     -- a standalone, well-formed SVG drawing of the character grid
;;
;; The buffer is parsed fresh in a scratch copy, so exporting never disturbs
;; the live parse state, shape IDs or point.  Output is deterministic: nodes
;; are the rectangles sorted by row, then column, and are named n1, n2, ...
;; in that order.
;;
;; A node label is the text inside its rectangle (rows joined with a space,
;; blanks collapsed); an empty rectangle is labelled with its shape ID.  For a
;; rectangle that contains other rectangles, those rectangles and the
;; connectors between them are left out of the label.  Text that is not inside
;; a rectangle is not part of the diagram model and is not exported.
;;
;; Edges come from the parser's edge segments, grouped into connector routes
;; (segments that meet end to end, or where a stem ends on the middle of a bar
;; as in a fan-out; the parser's own connector graph does not join the latter).
;; An arrowhead on a segment end
;; that touches a rectangle points at that rectangle.  A connector linking two
;; rectangles is directed (`A --> B'), bidirectional (`A <--> B') or plain
;; (`A --- B') accordingly.  A connector touching more rectangles is expanded
;; from the pointed-to ones: every non-pointed rectangle gets an edge to each
;; pointed one (a fan-out), or, with no direction at all, the first rectangle
;; is joined to each of the others.  Connectors touching fewer than two
;; rectangles are skipped.
;;
;; Options (a plist, all optional):
;;   :direction    one of "LR" (default), "TB", "RL", "BT"; Mermaid and DOT
;;   :cell-width   SVG pixels per character column (default 9)
;;   :cell-height  SVG pixels per character row (default 18)
;;
;; Entry points:
;;   `shape-artist-export-string'   -- return the export as a string
;;   `shape-artist-export'          -- interactive; show, copy, optionally save
;;   `shape-artist-export-formats'  -- list the supported formats

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'shape-artist-core)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-metadata)

(defgroup shape-artist-export nil
  "Export Shape Artist diagrams to Mermaid, Graphviz DOT and SVG."
  :group 'shape-artist
  :prefix "shape-artist-export-")

(defcustom shape-artist-export-display-function #'display-buffer
  "Function called with the export buffer to show it to the user."
  :type 'function
  :group 'shape-artist-export)

(defconst shape-artist-export--formats '(mermaid dot svg)
  "Export formats supported by `shape-artist-export-string'.")

(defconst shape-artist-export--directions '("LR" "TB" "RL" "BT")
  "Layout directions accepted for the :direction option.")

(defconst shape-artist-export--buffer-name "*shape-artist-export*"
  "Name of the buffer that shows an export.")

(defun shape-artist-export-formats ()
  "Return the list of supported export format symbols."
  (copy-sequence shape-artist-export--formats))

;;; --- Model ----------------------------------------------------------------

(cl-defstruct (shape-artist-export--model
               (:constructor shape-artist-export--make-model))
  "Everything an exporter needs, derived from one fresh parse."
  grid nodes edges segments)

(cl-defstruct (shape-artist-export--node
               (:constructor shape-artist-export--make-node))
  "One exported rectangle."
  index id rect label lines rounded style)

(defun shape-artist-export--rect-less (a b)
  "Return non-nil when rectangle A precedes B by row, then column."
  (let ((ya (oref a y)) (yb (oref b y)) (xa (oref a x)) (xb (oref b x)))
    (or (< ya yb)
        (and (= ya yb)
             (or (< xa xb)
                 (and (= xa xb)
                      (> (* (oref a width) (oref a height))
                         (* (oref b width) (oref b height)))))))))

(defun shape-artist-export--inside-p (inner outer)
  "Return non-nil when rectangle INNER lies strictly inside OUTER."
  (and (not (eq inner outer))
       (> (oref inner x) (oref outer x))
       (> (oref inner y) (oref outer y))
       (< (+ (oref inner x) (oref inner width))
          (+ (oref outer x) (oref outer width)))
       (< (+ (oref inner y) (oref inner height))
          (+ (oref outer y) (oref outer height)))))

(defun shape-artist-export--interior-lines (grid rect rects segments)
  "Return the text rows inside RECT as a list of (ROW COL TEXT).

GRID is the character grid, RECTS all rectangles and SEGMENTS all edge
segments.  Nested rectangles and the segments inside RECT are blanked out when
RECT contains other rectangles; ROW and COL locate the first character."
  (let* ((x (oref rect x)) (y (oref rect y))
         (nested (cl-remove-if-not
                  (lambda (r) (shape-artist-export--inside-p r rect)) rects))
         (rows nil))
    (dotimes (dr (max 0 (1- (oref rect height))))
      (let* ((row (+ y 1 dr))
             (chars (make-string (max 0 (1- (oref rect width))) ?\s)))
        (dotimes (dc (length chars))
          (let ((col (+ x 1 dc)))
            (aset chars dc
                  (if (or (cl-some
                           (lambda (n)
                             (and (<= (oref n x) col (+ (oref n x) (oref n width)))
                                  (<= (oref n y) row (+ (oref n y) (oref n height)))))
                           nested)
                          (and nested
                               (cl-some
                                (lambda (s)
                                  (and (<= (min (oref s x) (oref s x2)) col
                                           (max (oref s x) (oref s x2)))
                                       (<= (min (oref s y) (oref s y2)) row
                                           (max (oref s y) (oref s y2)))))
                                segments)))
                      ?\s
                    (or (shape-artist-diagram--grid-char grid row col) ?\s)))))
        (let ((text (string-trim-right chars)))
          (unless (string-empty-p text)
            (let ((lead (- (length text) (length (string-trim-left text)))))
              (push (list row (+ x 1 lead) (string-trim-left text)) rows))))))
    (nreverse rows)))

(defun shape-artist-export--collapse (text)
  "Return TEXT with every run of whitespace turned into one space."
  (string-join (split-string text) " "))

(defun shape-artist-export--build-nodes (grid rects segments)
  "Return export nodes for RECTS, numbered in order, using GRID and SEGMENTS."
  (let ((index 0))
    (mapcar
     (lambda (rect)
       (setq index (1+ index))
       (let* ((lines (shape-artist-export--interior-lines
                      grid rect rects segments))
              (text (shape-artist-export--collapse
                     (string-join (mapcar (lambda (l) (nth 2 l)) lines) " ")))
              (id (shape-artist-metadata-shape-id rect)))
         (shape-artist-export--make-node
          :index index
          :id (format "n%d" index)
          :rect rect
          :label (if (string-empty-p text) (or id (format "n%d" index)) text)
          :lines lines
          :rounded (eq (oref rect kind) 'rounded-rectangle)
          :style (oref rect detected-style))))
     rects)))

(defun shape-artist-export--connector-edges (indices pointed)
  "Return (FROM TO DIRECTION) edges for node INDICES with POINTED ones.

DIRECTION is `forward', `both' or `none'; FROM and TO are node indices."
  (let ((sources (cl-set-difference indices pointed)))
    (cond
     ((< (length indices) 2) nil)
     ((null pointed)
      (mapcar (lambda (to) (list (car indices) to 'none)) (cdr indices)))
     ((null sources)
      (mapcar (lambda (to) (list (car indices) to 'both)) (cdr indices)))
     (t
      (cl-loop for from in sources
               nconc (mapcar (lambda (to) (list from to 'forward))
                             pointed))))))

(defun shape-artist-export--segment-cell-p (segment col row)
  "Return non-nil when the cell at COL and ROW lies on edge SEGMENT."
  (and (<= (min (oref segment x) (oref segment x2)) col
           (max (oref segment x) (oref segment x2)))
       (<= (min (oref segment y) (oref segment y2)) row
           (max (oref segment y) (oref segment y2)))))

(defun shape-artist-export--segments-joined-p (a b)
  "Return non-nil when edge segments A and B touch end to end or in a T."
  (or (shape-artist-export--segment-cell-p a (oref b x) (oref b y))
      (shape-artist-export--segment-cell-p a (oref b x2) (oref b y2))
      (shape-artist-export--segment-cell-p b (oref a x) (oref a y))
      (shape-artist-export--segment-cell-p b (oref a x2) (oref a y2))))

(defun shape-artist-export--segment-groups (segments)
  "Group SEGMENTS into connected connector routes.

Unlike the parser's connector graph this also joins a stem that ends on the
middle of a bar (a T-junction), which is how a fan-out is drawn."
  (let* ((vec (vconcat segments))
         (count (length vec))
         (parent (make-vector count 0)))
    (dotimes (i count) (aset parent i i))
    (cl-labels ((root (i) (if (= (aref parent i) i) i
                            (aset parent i (root (aref parent i)))
                            (aref parent i))))
      (dotimes (i count)
        (cl-loop for j from (1+ i) below count
                 when (shape-artist-export--segments-joined-p
                       (aref vec i) (aref vec j))
                 do (aset parent (root j) (root i))))
      (let ((groups (make-hash-table :test 'eql)) (order nil))
        (dotimes (i count)
          (let ((r (root i)))
            (unless (gethash r groups) (push r order))
            (puthash r (cons (aref vec i) (gethash r groups)) groups)))
        (mapcar (lambda (r) (nreverse (gethash r groups))) (nreverse order))))))

(defun shape-artist-export--build-edges (segments signature-index)
  "Return sorted, unique (FROM TO DIRECTION) edges for SEGMENTS.

SIGNATURE-INDEX maps rectangle signatures to node indices."
  (let ((edges nil))
    (dolist (group (shape-artist-export--segment-groups segments))
      (let ((indices nil) (pointed nil))
        (dolist (segment group)
          (dolist (end '(start end))
            (let* ((arrow (if (eq end 'start)
                              (oref segment arrow-start)
                            (oref segment arrow-end)))
                   (attachment (if (eq end 'start)
                                   (oref segment start-attachment)
                                 (oref segment end-attachment)))
                   (index (and attachment
                               (gethash (plist-get attachment :rect)
                                        signature-index))))
              (when index
                (cl-pushnew index indices)
                (when arrow (cl-pushnew index pointed))))))
        (dolist (edge (shape-artist-export--connector-edges
                       (sort indices #'<) pointed))
          (cl-pushnew edge edges :test #'equal))))
    (sort edges
          (lambda (a b)
            (or (< (nth 0 a) (nth 0 b))
                (and (= (nth 0 a) (nth 0 b))
                     (or (< (nth 1 a) (nth 1 b))
                         (and (= (nth 1 a) (nth 1 b))
                              (string< (symbol-name (nth 2 a))
                                       (symbol-name (nth 2 b)))))))))))

(defun shape-artist-export--segment-less (a b)
  "Return non-nil when edge segment A precedes B."
  (let ((ka (list (oref a y) (oref a x) (oref a y2) (oref a x2)))
        (kb (list (oref b y) (oref b x) (oref b y2) (oref b x2))))
    (cl-loop for x in ka for y in kb
             unless (= x y) return (< x y)
             finally return nil)))

(defun shape-artist-export--model-from-buffer ()
  "Parse the accessible text of the current buffer into an export model.

Parsing happens in a scratch buffer, leaving this buffer's state alone."
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert text)
      (let* ((shapes (cdr (shape-artist-diagram-parse-buffer)))
             (grid (shape-artist-diagram--extract-grid (current-buffer)))
             (rects (sort (cl-remove-if-not #'shape-artist-diagram-rectangle-p
                                            shapes)
                          #'shape-artist-export--rect-less))
             (segments (sort (cl-remove-if-not #'shape-artist-diagram-edge-p
                                               shapes)
                             #'shape-artist-export--segment-less))
             (nodes (shape-artist-export--build-nodes grid rects segments))
             (signatures (make-hash-table :test 'equal)))
        (dolist (node nodes)
          (puthash (shape-artist-diagram--rect-attachment-signature
                    (shape-artist-export--node-rect node))
                   (shape-artist-export--node-index node)
                   signatures))
        (shape-artist-export--make-model
         :grid grid
         :nodes nodes
         :edges (shape-artist-export--build-edges segments signatures)
         :segments segments)))))

;;; --- Options --------------------------------------------------------------

(defun shape-artist-export--direction (options)
  "Return the validated :direction from OPTIONS, defaulting to \"LR\"."
  (let ((direction (upcase (format "%s" (or (plist-get options :direction)
                                            "LR")))))
    (unless (member direction shape-artist-export--directions)
      (user-error "Unsupported direction %s (use one of %s)" direction
                  (string-join shape-artist-export--directions ", ")))
    direction))

(defun shape-artist-export--size (options key default)
  "Return the positive number under KEY in OPTIONS, or DEFAULT."
  (let ((value (or (plist-get options key) default)))
    (unless (and (numberp value) (> value 0))
      (user-error "Option %s must be a positive number, not %S" key value))
    value))

(defun shape-artist-export--node-at (model index)
  "Return the node numbered INDEX in MODEL."
  (nth (1- index) (shape-artist-export--model-nodes model)))

;;; --- Mermaid --------------------------------------------------------------

(defun shape-artist-export--replace-all (string replacements)
  "Return STRING with each (FROM . TO) in REPLACEMENTS applied in one pass."
  (let ((out nil))
    (dolist (char (append string nil))
      (push (or (cdr (assq char replacements)) (string char)) out))
    (apply #'concat (nreverse out))))

(defun shape-artist-export--mermaid-escape (text)
  "Return TEXT made safe inside a double-quoted Mermaid label."
  (shape-artist-export--replace-all
   text '((?# . "#35;") (?\" . "#quot;") (?& . "#amp;")
          (?< . "#lt;") (?> . "#gt;"))))

(defun shape-artist-export--mermaid (model options)
  "Return MODEL as a Mermaid flowchart string, honouring OPTIONS."
  (let ((lines (list (format "flowchart %s"
                             (shape-artist-export--direction options)))))
    (dolist (node (shape-artist-export--model-nodes model))
      (let ((label (shape-artist-export--mermaid-escape
                    (shape-artist-export--node-label node))))
        (push (format (if (shape-artist-export--node-rounded node)
                          "    %s(\"%s\")" "    %s[\"%s\"]")
                      (shape-artist-export--node-id node) label)
              lines)))
    (dolist (edge (shape-artist-export--model-edges model))
      (push (format "    %s %s %s"
                    (shape-artist-export--node-id
                     (shape-artist-export--node-at model (nth 0 edge)))
                    (pcase (nth 2 edge)
                      ('forward "-->") ('both "<-->") (_ "---"))
                    (shape-artist-export--node-id
                     (shape-artist-export--node-at model (nth 1 edge))))
            lines))
    (concat (string-join (nreverse lines) "\n") "\n")))

;;; --- Graphviz DOT ---------------------------------------------------------

(defun shape-artist-export--dot-escape (text)
  "Return TEXT made safe inside a double-quoted DOT string."
  (shape-artist-export--replace-all text '((?\\ . "\\\\") (?\" . "\\\""))))

(defun shape-artist-export--dot (model options)
  "Return MODEL as a Graphviz digraph string, honouring OPTIONS."
  (let ((lines (list "    node [shape=box];"
                     (format "    rankdir=%s;"
                             (shape-artist-export--direction options))
                     "digraph shape_artist {")))
    (dolist (node (shape-artist-export--model-nodes model))
      (push (format "    \"%s\" [label=\"%s\"%s];"
                    (shape-artist-export--dot-escape
                     (shape-artist-export--node-id node))
                    (shape-artist-export--dot-escape
                     (shape-artist-export--node-label node))
                    (if (shape-artist-export--node-rounded node)
                        ", style=rounded" ""))
            lines))
    (dolist (edge (shape-artist-export--model-edges model))
      (push (format "    \"%s\" -> \"%s\"%s;"
                    (shape-artist-export--node-id
                     (shape-artist-export--node-at model (nth 0 edge)))
                    (shape-artist-export--node-id
                     (shape-artist-export--node-at model (nth 1 edge)))
                    (pcase (nth 2 edge)
                      ('both " [dir=both]") ('none " [dir=none]") (_ "")))
            lines))
    (push "}" lines)
    (concat (string-join (nreverse lines) "\n") "\n")))

;;; --- SVG ------------------------------------------------------------------

(defun shape-artist-export--xml-escape (text)
  "Return TEXT escaped for XML text and attribute values.

Control characters that XML 1.0 forbids are replaced by spaces."
  (shape-artist-export--replace-all
   (apply #'string
          (mapcar (lambda (c) (if (and (< c 32) (not (memq c '(?\t ?\n)))) ?\s c))
                  (append text nil)))
   '((?& . "&amp;") (?< . "&lt;") (?> . "&gt;") (?\" . "&quot;"))))

(defun shape-artist-export--num (number)
  "Format NUMBER compactly for an SVG attribute, e.g. 4.5 or 9."
  (let ((text (format "%.2f" number)))
    (if (string-match-p "\\." text)
        (replace-regexp-in-string "\\.?0+\\'" "" text)
      text)))

(defun shape-artist-export--attached-point (segment end cw ch)
  "Return (X . Y) pixels for END (`start' or `end') of SEGMENT.

CW and CH are the cell size.  An end touching a rectangle is extended onto the
rectangle's border so the drawn line connects."
  (let* ((col (if (eq end 'start) (oref segment x) (oref segment x2)))
         (row (if (eq end 'start) (oref segment y) (oref segment y2)))
         (attachment (if (eq end 'start)
                         (oref segment start-attachment)
                       (oref segment end-attachment)))
         (rect (plist-get attachment :rect))
         (px (* (+ col 0.5) cw))
         (py (* (+ row 0.5) ch)))
    (when attachment
      (pcase (plist-get attachment :side)
        ('left (setq px (* (+ (nth 0 rect) 0.5) cw)))
        ('right (setq px (* (+ (nth 0 rect) (nth 2 rect) 0.5) cw)))
        ('top (setq py (* (+ (nth 1 rect) 0.5) ch)))
        ('bottom (setq py (* (+ (nth 1 rect) (nth 3 rect) 0.5) ch)))))
    (cons px py)))

(defun shape-artist-export--svg-rect (rect cw ch)
  "Return SVG element strings for RECT drawn on a CW x CH cell grid."
  (let* ((x (* (+ (oref rect x) 0.5) cw))
         (y (* (+ (oref rect y) 0.5) ch))
         (w (* (oref rect width) cw))
         (h (* (oref rect height) ch))
         (style (oref rect detected-style))
         (rounded (eq (oref rect kind) 'rounded-rectangle))
         (inset (* 0.2 (min cw ch)))
         (elements
          (list (format "  <rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\"%s fill=\"none\" stroke=\"black\"%s/>"
                        (shape-artist-export--num x) (shape-artist-export--num y)
                        (shape-artist-export--num w) (shape-artist-export--num h)
                        (if rounded
                            (format " rx=\"%s\"" (shape-artist-export--num cw))
                          "")
                        (if (eq style 'heavy) " stroke-width=\"2\"" "")))))
    (when (and (eq style 'unicode-double) (> w (* 2 inset)) (> h (* 2 inset)))
      (setq elements
            (append elements
                    (list (format "  <rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" fill=\"none\" stroke=\"black\"/>"
                                  (shape-artist-export--num (+ x inset))
                                  (shape-artist-export--num (+ y inset))
                                  (shape-artist-export--num (- w (* 2 inset)))
                                  (shape-artist-export--num (- h (* 2 inset))))))))
    elements))

(defun shape-artist-export--svg-segment (segment cw ch)
  "Return the SVG line element for edge SEGMENT on a CW x CH cell grid."
  (let ((from (shape-artist-export--attached-point segment 'start cw ch))
        (to (shape-artist-export--attached-point segment 'end cw ch)))
    (format "  <line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" stroke=\"black\"%s%s/>"
            (shape-artist-export--num (car from))
            (shape-artist-export--num (cdr from))
            (shape-artist-export--num (car to))
            (shape-artist-export--num (cdr to))
            (if (oref segment arrow-start)
                " marker-start=\"url(#shape-artist-arrow)\"" "")
            (if (oref segment arrow-end)
                " marker-end=\"url(#shape-artist-arrow)\"" ""))))

(defun shape-artist-export--svg (model options)
  "Return MODEL as a standalone SVG document string, honouring OPTIONS."
  (let* ((cw (shape-artist-export--size options :cell-width 9))
         (ch (shape-artist-export--size options :cell-height 18))
         (grid (shape-artist-export--model-grid model))
         (cols (if (> (length grid) 0) (length (aref grid 0)) 0))
         (width (* (max 1 cols) cw))
         (height (* (max 1 (length grid)) ch))
         (segments (shape-artist-export--model-segments model))
         (arrows (cl-some (lambda (s) (or (oref s arrow-start) (oref s arrow-end)))
                          segments))
         (font-size (max 1 (round (* ch 0.78))))
         (lines (list (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%s\" height=\"%s\" viewBox=\"0 0 %s %s\" font-family=\"monospace\" font-size=\"%d\">"
                              (shape-artist-export--num width)
                              (shape-artist-export--num height)
                              (shape-artist-export--num width)
                              (shape-artist-export--num height)
                              font-size))))
    (when arrows
      (push "  <defs>" lines)
      (push "    <marker id=\"shape-artist-arrow\" viewBox=\"0 0 10 10\" refX=\"10\" refY=\"5\" markerWidth=\"8\" markerHeight=\"8\" orient=\"auto-start-reverse\">" lines)
      (push "      <path d=\"M 0 0 L 10 5 L 0 10 z\" fill=\"black\"/>" lines)
      (push "    </marker>" lines)
      (push "  </defs>" lines))
    (dolist (node (shape-artist-export--model-nodes model))
      (dolist (element (shape-artist-export--svg-rect
                        (shape-artist-export--node-rect node) cw ch))
        (push element lines)))
    (dolist (segment segments)
      (push (shape-artist-export--svg-segment segment cw ch) lines))
    (dolist (node (shape-artist-export--model-nodes model))
      (dolist (line (shape-artist-export--node-lines node))
        (push (format "  <text x=\"%s\" y=\"%s\" xml:space=\"preserve\">%s</text>"
                      (shape-artist-export--num (* (nth 1 line) cw))
                      (shape-artist-export--num (* (+ (nth 0 line) 0.75) ch))
                      (shape-artist-export--xml-escape (nth 2 line)))
              lines)))
    (push "</svg>" lines)
    (concat (string-join (nreverse lines) "\n") "\n")))

;;; --- Public API -----------------------------------------------------------

(defun shape-artist-export--format-symbol (format)
  "Return FORMAT (a symbol or string) as a supported format symbol."
  (let ((symbol (if (symbolp format) format (intern (downcase format)))))
    (unless (memq symbol shape-artist-export--formats)
      (user-error "Unsupported export format %s (use one of %s)" format
                  (mapconcat #'symbol-name shape-artist-export--formats ", ")))
    symbol))

;;;###autoload
(defun shape-artist-export-string (format &optional options)
  "Return the current buffer's diagram exported as FORMAT.

FORMAT is `mermaid', `dot' or `svg'.  OPTIONS is a plist; see the
Commentary for :direction, :cell-width and :cell-height.  The buffer is parsed
fresh, and the result is deterministic."
  (let ((symbol (shape-artist-export--format-symbol format))
        (model (shape-artist-export--model-from-buffer)))
    (pcase symbol
      ('mermaid (shape-artist-export--mermaid model options))
      ('dot (shape-artist-export--dot model options))
      ('svg (shape-artist-export--svg model options)))))

(defun shape-artist-export--read-format ()
  "Prompt for an export format and return it as a symbol."
  (intern (completing-read "Export format: "
                           (mapcar #'symbol-name shape-artist-export--formats)
                           nil t nil nil "mermaid")))

(defun shape-artist-export--write-file (string file confirm)
  "Write STRING to FILE as UTF-8; with CONFIRM, ask before overwriting."
  (when (and confirm (file-exists-p file)
             (not (y-or-n-p (format "Overwrite %s? " file))))
    (user-error "Export not written"))
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region string nil file nil 'silent)))

;;;###autoload
(defun shape-artist-export (format &optional file)
  "Export the diagram as FORMAT, show it, and copy it to the kill ring.

Interactively, prompt for FORMAT (default mermaid); with a prefix argument also
prompt for a FILE to write.  The result appears in `*shape-artist-export*'.
Return the exported string."
  (interactive
   (list (shape-artist-export--read-format)
         (and current-prefix-arg (read-file-name "Write export to: "))))
  (let ((string (shape-artist-export-string format))
        (interactive-p (called-interactively-p 'any))
        (buffer (get-buffer-create shape-artist-export--buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (fundamental-mode)
        (erase-buffer)
        (insert string)
        (goto-char (point-min))))
    (kill-new string)
    (when file
      (shape-artist-export--write-file string file interactive-p))
    (funcall shape-artist-export-display-function buffer)
    (message "Exported %s%s (copied to the kill ring)"
             (shape-artist-export--format-symbol format)
             (if file (format ", written to %s" file) ""))
    string))

(provide 'shape-artist-export)
;;; shape-artist-export.el ends here
