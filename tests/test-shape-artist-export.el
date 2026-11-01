;;; test-shape-artist-export.el --- ERT tests for Mermaid, DOT and SVG export -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'xml)
(require 'test-helper)
(require 'shape-artist-diagram-parse)
(require 'shape-artist-core)
(require 'shape-artist-export)

;;; --- Helpers ---

(defun test-export--lines (&rest lines)
  "Join LINES with newlines and end with one, as the exporters do."
  (concat (mapconcat #'identity lines "\n") "\n"))

(defun test-export--diagram (&rest lines)
  "Join diagram LINES with newlines, without a trailing newline."
  (mapconcat #'identity lines "\n"))

(defun test-export--box (label &optional unicode)
  "Return a one-line box around LABEL, in UNICODE glyphs when non-nil."
  (let ((bar (make-string (+ 2 (length label)) (if unicode ?─ ?-))))
    (mapconcat #'identity
               (if unicode
                   (list (concat "┌" bar "┐") (concat "│ " label " │")
                         (concat "└" bar "┘"))
                 (list (concat "+" bar "+") (concat "| " label " |")
                       (concat "+" bar "+")))
               "\n")))

(defun test-export--run (format text &optional options)
  "Return TEXT exported as FORMAT with OPTIONS, from a temp buffer."
  (test-artist-with-buffer
      text
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-export-string format options))))

(defun test-export--xml (string)
  "Parse the XML STRING and return its root element, failing when malformed."
  (with-temp-buffer
    (insert string)
    (let ((tree (if (fboundp 'libxml-parse-xml-region)
                    (libxml-parse-xml-region (point-min) (point-max))
                  (car (xml-parse-region (point-min) (point-max))))))
      (should tree)
      tree)))

(defun test-export--strict-xml-p (string)
  "Return non-nil when STRING is well-formed XML according to `xml.el'.

`xml-parse-region' signals on mismatched tags, bad entities and the like."
  (with-temp-buffer
    (insert string)
    (condition-case nil
        (progn (xml-parse-region (point-min) (point-max)) t)
      (error nil))))

(defun test-export--elements (tree name)
  "Return every element called NAME anywhere inside TREE."
  (let ((found nil))
    (cl-labels ((walk (node)
                  (when (consp node)
                    (when (eq (car node) name) (push node found))
                    (dolist (child (cddr node)) (walk child)))))
      (walk tree))
    (nreverse found)))

(defun test-export--attr (element name)
  "Return attribute NAME of ELEMENT as a string."
  (cdr (assq name (cadr element))))

;;; --- Formats ---

(ert-deftest test-shape-artist-export-formats ()
  "The supported formats are mermaid, dot and svg."
  (should (equal (shape-artist-export-formats) '(mermaid dot svg)))
  (let ((formats (shape-artist-export-formats)))
    (setcar formats 'nope)
    (should (equal (shape-artist-export-formats) '(mermaid dot svg)))))

(ert-deftest test-shape-artist-export-unknown-format ()
  "An unsupported format is a `user-error' naming the choices."
  (let ((err (should-error (test-export--run 'pdf "+--+\n|  |\n+--+")
                           :type 'user-error)))
    (should (string-match-p "mermaid, dot, svg" (cadr err))))
  (should-error (test-export--run "png" "") :type 'user-error))

(ert-deftest test-shape-artist-export-format-may-be-a-string ()
  "A format given as a string is accepted case-insensitively."
  (let ((text "+---+\n| A |\n+---+"))
    (should (equal (test-export--run "DOT" text) (test-export--run 'dot text)))))

;;; --- Mermaid ---

(ert-deftest test-shape-artist-export-mermaid-arrow ()
  "Two boxes joined by an arrow become a directed edge."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--diagram "+-------+     +-----+"
                                        "| Start |---->| End |"
                                        "+-------+     +-----+"))
                 (test-export--lines "flowchart LR"
                                     "    n1[\"Start\"]"
                                     "    n2[\"End\"]"
                                     "    n1 --> n2"))))

(ert-deftest test-shape-artist-export-mermaid-reverse-arrow ()
  "An arrowhead at the left box points the edge from right to left."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--diagram "+---+     +---+"
                                        "| A |<----| B |"
                                        "+---+     +---+"))
                 (test-export--lines "flowchart LR"
                                     "    n1[\"A\"]"
                                     "    n2[\"B\"]"
                                     "    n2 --> n1"))))

(ert-deftest test-shape-artist-export-mermaid-bidirectional ()
  "Arrowheads at both ends make a bidirectional edge."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--diagram "+---+     +---+"
                                        "| A |<--->| B |"
                                        "+---+     +---+"))
                 (test-export--lines "flowchart LR"
                                     "    n1[\"A\"]"
                                     "    n2[\"B\"]"
                                     "    n1 <--> n2"))))

(ert-deftest test-shape-artist-export-mermaid-undirected ()
  "A line without arrowheads makes an undirected edge."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--diagram "+---+     +---+"
                                        "| A |-----| B |"
                                        "+---+     +---+"))
                 (test-export--lines "flowchart LR"
                                     "    n1[\"A\"]"
                                     "    n2[\"B\"]"
                                     "    n1 --- n2"))))

(ert-deftest test-shape-artist-export-mermaid-vertical-arrow ()
  "A vertical arrow points from the upper box to the lower one."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--diagram "+-----+"
                                        "| Top |"
                                        "+-----+"
                                        "   |"
                                        "   v"
                                        "+-----+"
                                        "| Low |"
                                        "+-----+"))
                 (test-export--lines "flowchart LR"
                                     "    n1[\"Top\"]"
                                     "    n2[\"Low\"]"
                                     "    n1 --> n2"))))

(ert-deftest test-shape-artist-export-mermaid-elbow-connector ()
  "A connector with a bend still links its two boxes."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--diagram "+---+"
                                        "| A |"
                                        "+---+"
                                        "  |"
                                        "  +-----+"
                                        "        |"
                                        "        v"
                                        "      +---+"
                                        "      | B |"
                                        "      +---+"))
                 (test-export--lines "flowchart LR"
                                     "    n1[\"A\"]"
                                     "    n2[\"B\"]"
                                     "    n1 --> n2"))))

(ert-deftest test-shape-artist-export-mermaid-no-label-uses-shape-id ()
  "A box without text is labelled with its shape ID."
  (should (equal (test-export--run 'mermaid "+--+\n|  |\n+--+")
                 (test-export--lines "flowchart LR"
                                     "    n1[\"sa-1\"]"))))

(ert-deftest test-shape-artist-export-mermaid-rounded ()
  "Rounded boxes use the rounded node shape."
  (should (equal (test-export--run 'mermaid "/---\\\n| A |\n\\---/")
                 (test-export--lines "flowchart LR" "    n1(\"A\")")))
  (should (equal (test-export--run 'mermaid "╭───╮\n│ A │\n╰───╯")
                 (test-export--lines "flowchart LR" "    n1(\"A\")"))))

(ert-deftest test-shape-artist-export-mermaid-fan-out ()
  "A stem ending on a bar fans out to every box the bar reaches."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--diagram "  +-----+"
                                        "  | Top |"
                                        "  +-----+"
                                        "     |"
                                        " +---+-----+"
                                        " |         |"
                                        " v         v"
                                        "+---+     +---+"
                                        "| L |     | R |"
                                        "+---+     +---+"))
                 (test-export--lines "flowchart LR"
                                     "    n1[\"Top\"]"
                                     "    n2[\"L\"]"
                                     "    n3[\"R\"]"
                                     "    n1 --> n2"
                                     "    n1 --> n3"))))

(ert-deftest test-shape-artist-export-mermaid-undirected-hub ()
  "Without arrowheads a multi-way connector is a star from the first box."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--diagram "  +-----+"
                                        "  | Hub |"
                                        "  +-----+"
                                        "     |"
                                        " +---+-----+"
                                        " |         |"
                                        "+---+     +---+"
                                        "| L |     | R |"
                                        "+---+     +---+"))
                 (test-export--lines "flowchart LR"
                                     "    n1[\"Hub\"]"
                                     "    n2[\"L\"]"
                                     "    n3[\"R\"]"
                                     "    n1 --- n2"
                                     "    n1 --- n3"))))

(ert-deftest test-shape-artist-export-mermaid-isolated-box ()
  "A box with no connector is just a node."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--diagram "+---+     +---+"
                                        "| A |---->| B |"
                                        "+---+     +---+"
                                        ""
                                        "+------+"
                                        "| Solo |"
                                        "+------+"))
                 (test-export--lines "flowchart LR"
                                     "    n1[\"A\"]"
                                     "    n2[\"B\"]"
                                     "    n3[\"Solo\"]"
                                     "    n1 --> n2"))))

(ert-deftest test-shape-artist-export-mermaid-skips-dangling-connectors ()
  "Connectors touching fewer than two boxes are skipped without error."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--diagram "+---+"
                                        "| A |----->"
                                        "+---+"
                                        ""
                                        "-----"
                                        "  |"
                                        "  |"))
                 (test-export--lines "flowchart LR" "    n1[\"A\"]"))))

(ert-deftest test-shape-artist-export-mermaid-duplicate-edges-collapse ()
  "Two parallel arrows between the same boxes give one edge."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--diagram "+---+  +---+"
                                        "| A |->| B |"
                                        "|   |->|   |"
                                        "+---+  +---+"))
                 (test-export--lines "flowchart LR"
                                     "    n1[\"A\"]"
                                     "    n2[\"B\"]"
                                     "    n1 --> n2"))))

(ert-deftest test-shape-artist-export-mermaid-multi-line-and-spaced-labels ()
  "Label rows join with a space and blanks collapse."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--diagram "+----------+"
                                        "| Hello    |"
                                        "|   big    |"
                                        "| a    b   |"
                                        "+----------+"))
                 (test-export--lines "flowchart LR"
                                     "    n1[\"Hello big a b\"]"))))

(ert-deftest test-shape-artist-export-mermaid-container-label ()
  "A container is labelled by its own text, not by the boxes inside it."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--diagram "+-------------+"
                                        "| Outer       |"
                                        "|  +---+      |"
                                        "|  | a |-->   |"
                                        "|  +---+      |"
                                        "+-------------+"))
                 (test-export--lines "flowchart LR"
                                     "    n1[\"Outer\"]"
                                     "    n2[\"a\"]"))))

(ert-deftest test-shape-artist-export-mermaid-empty-buffer ()
  "An empty buffer exports a header only."
  (should (equal (test-export--run 'mermaid "")
                 (test-export--lines "flowchart LR")))
  (should (equal (test-export--run 'mermaid "just some prose\nno boxes")
                 (test-export--lines "flowchart LR"))))

(ert-deftest test-shape-artist-export-mermaid-escaping ()
  "Quotes, ampersands, angle brackets and hashes are entity-escaped."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--box "say \"hi\" & <b> \\ #1"))
                 (test-export--lines
                  "flowchart LR"
                  "    n1[\"say #quot;hi#quot; #amp; #lt;b#gt; \\ #35;1\"]"))))

(ert-deftest test-shape-artist-export-mermaid-direction ()
  "The :direction option selects the flow direction."
  (dolist (direction '("LR" "TB" "RL" "BT"))
    (should (equal (car (split-string
                         (test-export--run 'mermaid "+---+\n| A |\n+---+"
                                           (list :direction direction))
                         "\n"))
                   (concat "flowchart " direction))))
  (should (string-prefix-p "flowchart TB"
                           (test-export--run 'mermaid ""
                                             '(:direction "tb"))))
  (should (string-prefix-p "flowchart BT"
                           (test-export--run 'mermaid "" '(:direction BT)))))

(ert-deftest test-shape-artist-export-invalid-direction ()
  "An unknown direction is a `user-error' for Mermaid and DOT."
  (dolist (format '(mermaid dot))
    (should-error (test-export--run format "" '(:direction "UP"))
                  :type 'user-error)))

;;; --- Ordering ---

(ert-deftest test-shape-artist-export-nodes-sorted-by-row-then-column ()
  "Nodes are numbered by row, then column, whatever the drawing order."
  (should (equal (test-export--run
                  'mermaid
                  (test-export--diagram "        +---+"
                                        "        | C |"
                                        "        +---+"
                                        "+---+"
                                        "| A |   +---+"
                                        "+---+   | B |"
                                        "        +---+"))
                 (test-export--lines "flowchart LR"
                                     "    n1[\"C\"]"
                                     "    n2[\"A\"]"
                                     "    n3[\"B\"]"))))

(ert-deftest test-shape-artist-export-deterministic ()
  "Exporting twice, or after other parses, gives identical output."
  (let ((text (test-export--diagram "+---+     +---+"
                                    "| A |---->| B |"
                                    "+---+     +---+"
                                    "   |"
                                    "   v"
                                    "+---+"
                                    "| C |"
                                    "+---+")))
    (dolist (format '(mermaid dot svg))
      (should (equal (test-export--run format text)
                     (test-export--run format text))))
    (test-artist-with-buffer
        text
      (let ((first (shape-artist-export-string 'dot)))
        (shape-artist-diagram-parse-buffer)
        (should (equal (shape-artist-export-string 'dot) first))))))

(ert-deftest test-shape-artist-export-does-not-touch-live-parse-state ()
  "Exporting leaves the buffer text, point and parse state untouched."
  (test-artist-with-buffer
      "+---+\n| A |\n+---+"
    (goto-char 8)
    (let ((shape-artist--update-sidebar-fn nil))
      (shape-artist-export-string 'mermaid)
      (shape-artist-export-string 'svg))
    (should (= (point) 8))
    (should (equal (buffer-string) "+---+\n| A |\n+---+"))
    (should-not shape-artist--last-shapes-list)
    (should-not shape-artist--last-root)))

(ert-deftest test-shape-artist-export-respects-narrowing ()
  "Only the accessible text is exported."
  (test-artist-with-buffer
      "+---+\n| A |\n+---+\n\n+---+\n| B |\n+---+"
    (narrow-to-region 1 18)
    (should (equal (shape-artist-export-string 'mermaid)
                   (test-export--lines "flowchart LR" "    n1[\"A\"]")))))

;;; --- DOT ---

(ert-deftest test-shape-artist-export-dot-arrow ()
  "A directed edge becomes a plain DOT edge."
  (should (equal (test-export--run
                  'dot
                  (test-export--diagram "+-------+     +-----+"
                                        "| Start |---->| End |"
                                        "+-------+     +-----+"))
                 (test-export--lines "digraph shape_artist {"
                                     "    rankdir=LR;"
                                     "    node [shape=box];"
                                     "    \"n1\" [label=\"Start\"];"
                                     "    \"n2\" [label=\"End\"];"
                                     "    \"n1\" -> \"n2\";"
                                     "}"))))

(ert-deftest test-shape-artist-export-dot-bidirectional-and-undirected ()
  "Bidirectional edges use dir=both and plain lines use dir=none."
  (should (equal (test-export--run
                  'dot
                  (test-export--diagram "+---+     +---+"
                                        "| A |<--->| B |"
                                        "+---+     +---+"
                                        ""
                                        "+---+     +---+"
                                        "| C |-----| D |"
                                        "+---+     +---+"))
                 (test-export--lines "digraph shape_artist {"
                                     "    rankdir=LR;"
                                     "    node [shape=box];"
                                     "    \"n1\" [label=\"A\"];"
                                     "    \"n2\" [label=\"B\"];"
                                     "    \"n3\" [label=\"C\"];"
                                     "    \"n4\" [label=\"D\"];"
                                     "    \"n1\" -> \"n2\" [dir=both];"
                                     "    \"n3\" -> \"n4\" [dir=none];"
                                     "}"))))

(ert-deftest test-shape-artist-export-dot-rounded-and-unlabelled ()
  "Rounded boxes get style=rounded; empty boxes use their shape ID.

The ID (`sa-1' here) is the parser's per-parse ID, which is deterministic."
  (should (equal (test-export--run 'dot "/---\\\n| A |\n\\---/\n\n+--+\n|  |\n+--+")
                 (test-export--lines "digraph shape_artist {"
                                     "    rankdir=LR;"
                                     "    node [shape=box];"
                                     "    \"n1\" [label=\"A\", style=rounded];"
                                     "    \"n2\" [label=\"sa-1\"];"
                                     "}"))))

(ert-deftest test-shape-artist-export-dot-fan-out ()
  "A fan-out produces one DOT edge per target."
  (should (equal (test-export--run
                  'dot
                  (test-export--diagram "  +-----+"
                                        "  | Top |"
                                        "  +-----+"
                                        "     |"
                                        " +---+-----+"
                                        " |         |"
                                        " v         v"
                                        "+---+     +---+"
                                        "| L |     | R |"
                                        "+---+     +---+"))
                 (test-export--lines "digraph shape_artist {"
                                     "    rankdir=LR;"
                                     "    node [shape=box];"
                                     "    \"n1\" [label=\"Top\"];"
                                     "    \"n2\" [label=\"L\"];"
                                     "    \"n3\" [label=\"R\"];"
                                     "    \"n1\" -> \"n2\";"
                                     "    \"n1\" -> \"n3\";"
                                     "}"))))

(ert-deftest test-shape-artist-export-dot-escaping ()
  "Backslashes and quotes are escaped in DOT labels; other characters are not."
  (should (equal (test-export--run
                  'dot
                  (test-export--box "say \"hi\" & <b> \\ #1"))
                 (test-export--lines "digraph shape_artist {"
                                     "    rankdir=LR;"
                                     "    node [shape=box];"
                                     "    \"n1\" [label=\"say \\\"hi\\\" & <b> \\\\ #1\"];"
                                     "}"))))

(ert-deftest test-shape-artist-export-dot-empty-and-direction ()
  "An empty buffer is an empty digraph; :direction sets rankdir."
  (should (equal (test-export--run 'dot "")
                 (test-export--lines "digraph shape_artist {"
                                     "    rankdir=LR;"
                                     "    node [shape=box];"
                                     "}")))
  (should (string-match-p "^    rankdir=TB;$"
                          (test-export--run 'dot "" '(:direction "TB")))))

;;; --- SVG ---

(ert-deftest test-shape-artist-export-svg-single-box-golden ()
  "A single labelled box has a readable, exact SVG form."
  (should (equal (test-export--run 'svg "+---+\n| A |\n+---+")
                 (test-export--lines
                  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"45\" height=\"54\" viewBox=\"0 0 45 54\" font-family=\"monospace\" font-size=\"14\">"
                  "  <rect x=\"4.5\" y=\"9\" width=\"36\" height=\"36\" fill=\"none\" stroke=\"black\"/>"
                  "  <text x=\"18\" y=\"31.5\" xml:space=\"preserve\">A</text>"
                  "</svg>"))))

(ert-deftest test-shape-artist-export-svg-arrow-golden ()
  "Two boxes joined by an arrow draw a connected line with a marker."
  (should (equal (test-export--run
                  'svg
                  (test-export--diagram "+---+  +---+"
                                        "| A |->| B |"
                                        "+---+  +---+"))
                 (test-export--lines
                  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"108\" height=\"54\" viewBox=\"0 0 108 54\" font-family=\"monospace\" font-size=\"14\">"
                  "  <defs>"
                  "    <marker id=\"shape-artist-arrow\" viewBox=\"0 0 10 10\" refX=\"10\" refY=\"5\" markerWidth=\"8\" markerHeight=\"8\" orient=\"auto-start-reverse\">"
                  "      <path d=\"M 0 0 L 10 5 L 0 10 z\" fill=\"black\"/>"
                  "    </marker>"
                  "  </defs>"
                  "  <rect x=\"4.5\" y=\"9\" width=\"36\" height=\"36\" fill=\"none\" stroke=\"black\"/>"
                  "  <rect x=\"67.5\" y=\"9\" width=\"36\" height=\"36\" fill=\"none\" stroke=\"black\"/>"
                  "  <line x1=\"40.5\" y1=\"27\" x2=\"67.5\" y2=\"27\" stroke=\"black\" marker-end=\"url(#shape-artist-arrow)\"/>"
                  "  <text x=\"18\" y=\"31.5\" xml:space=\"preserve\">A</text>"
                  "  <text x=\"81\" y=\"31.5\" xml:space=\"preserve\">B</text>"
                  "</svg>"))))

(ert-deftest test-shape-artist-export-svg-empty-buffer ()
  "An empty buffer is a minimal one-cell, well-formed document."
  (let ((svg (test-export--run 'svg "")))
    (should (equal svg
                   (test-export--lines
                    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"9\" height=\"18\" viewBox=\"0 0 9 18\" font-family=\"monospace\" font-size=\"14\">"
                    "</svg>")))
    (should (test-export--strict-xml-p svg))))

(ert-deftest test-shape-artist-export-svg-reverse-and-both-markers ()
  "Arrowheads at the start and at both ends use marker-start and marker-end."
  (let ((reverse (test-export--run
                  'svg (test-export--diagram "+---+  +---+"
                                             "| A |<-| B |"
                                             "+---+  +---+")))
        (both (test-export--run
               'svg (test-export--diagram "+---+  +---+"
                                          "| A |<>| B |"
                                          "+---+  +---+")))
        (plain (test-export--run
                'svg (test-export--diagram "+---+  +---+"
                                           "| A |--| B |"
                                           "+---+  +---+"))))
    (should (string-match-p "marker-start=" reverse))
    (should-not (string-match-p "marker-end=" reverse))
    (should (string-match-p "<marker " reverse))
    (should-not (string-match-p "<marker \\|marker-start\\|marker-end" plain))
    (should-not (string-match-p "<defs>" plain))
    (should (string-match-p "<line [^>]*stroke" both))))

(ert-deftest test-shape-artist-export-svg-vertical-arrow-extends-to-borders ()
  "A vertical connector is drawn from border to border."
  (let ((svg (test-export--run
              'svg (test-export--diagram "+---+"
                                         "| A |"
                                         "+---+"
                                         "  |"
                                         "  v"
                                         "+---+"
                                         "| B |"
                                         "+---+"))))
    (should (string-match-p
             (regexp-quote
              "<line x1=\"22.5\" y1=\"45\" x2=\"22.5\" y2=\"99\" stroke=\"black\" marker-end=\"url(#shape-artist-arrow)\"/>")
             svg))))

(ert-deftest test-shape-artist-export-svg-rounded-heavy-and-double ()
  "Rounded boxes get rx, heavy boxes a thick stroke, double boxes an inner rect."
  (let ((rounded (test-export--run 'svg "/---\\\n| A |\n\\---/"))
        (heavy (test-export--run 'svg "+===+\n| A |\n+===+"))
        (double (test-export--run 'svg "╔═══╗\n║ A ║\n╚═══╝"))
        (single (test-export--run 'svg "┌───┐\n│ A │\n└───┘")))
    (should (string-match-p "<rect [^>]*rx=\"9\"" rounded))
    (should-not (string-match-p "rx=" single))
    (should (string-match-p "stroke-width=\"2\"" heavy))
    (should-not (string-match-p "stroke-width" single))
    (should (= (length (test-export--elements (test-export--xml single) 'rect))
               1))
    (let ((rects (test-export--elements (test-export--xml double) 'rect)))
      (should (= (length rects) 2))
      ;; The inner rectangle is strictly inset on every side.
      (let* ((outer (car rects)) (inner (cadr rects))
             (ox (string-to-number (test-export--attr outer 'x)))
             (iw (string-to-number (test-export--attr inner 'width)))
             (ow (string-to-number (test-export--attr outer 'width))))
        (should (> (string-to-number (test-export--attr inner 'x)) ox))
        (should (< iw ow))))))

(ert-deftest test-shape-artist-export-svg-cell-size-options ()
  "The :cell-width and :cell-height options scale the whole drawing."
  (let* ((svg (test-export--run 'svg "+---+\n| A |\n+---+"
                                '(:cell-width 10 :cell-height 20)))
         (root (test-export--xml svg)))
    (should (equal (test-export--attr root 'width) "50"))
    (should (equal (test-export--attr root 'height) "60"))
    (should (equal (test-export--attr root 'viewBox) "0 0 50 60"))
    (let ((rect (car (test-export--elements root 'rect))))
      (should (equal (test-export--attr rect 'x) "5"))
      (should (equal (test-export--attr rect 'y) "10"))
      (should (equal (test-export--attr rect 'width) "40"))
      (should (equal (test-export--attr rect 'height) "40")))
    (should (equal (test-export--attr (car (test-export--elements root 'text))
                                      'x)
                   "20")))
  (let ((root (test-export--xml (test-export--run
                                 'svg "+---+\n| A |\n+---+"
                                 '(:cell-width 4.5)))))
    (should (equal (test-export--attr root 'width) "22.5"))))

(ert-deftest test-shape-artist-export-svg-invalid-cell-size ()
  "A non-positive or non-numeric cell size is a `user-error'."
  (dolist (options '((:cell-width 0) (:cell-height -3) (:cell-width "9")))
    (should-error (test-export--run 'svg "+---+\n| A |\n+---+" options)
                  :type 'user-error)))

(ert-deftest test-shape-artist-export-svg-escaping-and-well-formed ()
  "Markup characters in labels are escaped and the document stays valid XML."
  (let* ((svg (test-export--run
               'svg
               (test-export--box "a & b <c> \"d\" 'e' \\")))
         (root (test-export--xml svg)))
    (should (test-export--strict-xml-p svg))
    (should (string-match-p
             (regexp-quote ">a &amp; b &lt;c&gt; &quot;d&quot; 'e' \\</text>")
             svg))
    (should-not (string-match-p "<c>" svg))
    (should (string-match-p
             "a & b <c>"
             (mapconcat (lambda (el) (format "%s" (car (last el))))
                        (test-export--elements root 'text) " ")))))

(ert-deftest test-shape-artist-export-svg-control-characters-removed ()
  "Characters XML forbids never reach the document."
  (let ((svg (test-export--run
              'svg (test-export--box (concat "a" (string 1 2) "b")))))
    (should-not (string-match-p "[\u0001\u0002]" svg))
    (should (test-export--strict-xml-p svg))
    (should (string-match-p ">a  b</text>" svg))))

(ert-deftest test-shape-artist-export-svg-parses-with-structure ()
  "A busy diagram parses and has the expected element counts."
  (let* ((svg (test-export--run
               'svg
               (test-export--diagram "+-------+      +-------+"
                                     "| Start |----->|  Work |"
                                     "+-------+      +-------+"
                                     "    |              |"
                                     "    v              v"
                                     "+-------+      /-------\\"
                                     "| Queue |      |  End  |"
                                     "+-------+      \\-------/")))
         (root (test-export--xml svg)))
    (should (test-export--strict-xml-p svg))
    (should (eq (car root) 'svg))
    (should (string-match-p "<svg xmlns=\"http://www.w3.org/2000/svg\" " svg))
    (should (= (length (test-export--elements root 'rect)) 4))
    (should (= (length (test-export--elements root 'line)) 3))
    (should (= (length (test-export--elements root 'text)) 4))
    (should (= (length (test-export--elements root 'marker)) 1))))

(ert-deftest test-shape-artist-export-svg-multi-line-label-rows ()
  "Each text row of a box becomes its own positioned text element."
  (let ((root (test-export--xml
               (test-export--run
                'svg (test-export--diagram "+-----+"
                                           "| one |"
                                           "|     |"
                                           "| two |"
                                           "+-----+")))))
    (should (equal (mapcar (lambda (el) (test-export--attr el 'y))
                           (test-export--elements root 'text))
                   '("31.5" "67.5")))))

(ert-deftest test-shape-artist-export-svg-unicode-text ()
  "Non-ASCII labels survive in the SVG."
  (let ((svg (test-export--run 'svg (test-export--box "Größe" t))))
    (should (string-match-p ">Größe</text>" svg))
    (should (test-export--strict-xml-p svg))))

;;; --- Interactive command ---

(defmacro test-export--with-ui (&rest body)
  "Run BODY with the export UI isolated from windows, prompts and the kill ring."
  (declare (indent 0))
  `(let ((kill-ring nil)
         (kill-ring-yank-pointer nil)
         (shown nil)
         (shape-artist--update-sidebar-fn nil))
     (when (get-buffer shape-artist-export--buffer-name)
       (kill-buffer shape-artist-export--buffer-name))
     (unwind-protect
         (let ((shape-artist-export-display-function
                (lambda (buffer) (push buffer shown) nil)))
           (cl-letf (((symbol-function 'message) #'ignore))
             ,@body))
       (when (get-buffer shape-artist-export--buffer-name)
         (kill-buffer shape-artist-export--buffer-name)))))

(ert-deftest test-shape-artist-export-command-is-interactive ()
  "The command and its prompt are wired for interactive use."
  (should (commandp 'shape-artist-export))
  (should (eq (car (interactive-form 'shape-artist-export)) 'interactive)))

(ert-deftest test-shape-artist-export-command-shows-and-copies ()
  "The command shows the export in its buffer and copies it."
  (test-export--with-ui
    (test-artist-with-buffer
        "+---+     +---+\n| A |---->| B |\n+---+     +---+"
      (let ((result (shape-artist-export 'dot)))
        (should (equal result (shape-artist-export-string 'dot)))
        (should (equal (car kill-ring) result))
        (let ((buffer (get-buffer shape-artist-export--buffer-name)))
          (should buffer)
          (should (equal shown (list buffer)))
          (with-current-buffer buffer
            (should (equal (buffer-string) result))
            (should (eq major-mode 'fundamental-mode))
            (should (= (point) (point-min)))))))))

(ert-deftest test-shape-artist-export-command-reuses-buffer ()
  "A second export replaces the first instead of appending."
  (test-export--with-ui
    (test-artist-with-buffer
        "+---+\n| A |\n+---+"
      (shape-artist-export 'dot)
      (let ((second (shape-artist-export 'mermaid)))
        (with-current-buffer shape-artist-export--buffer-name
          (should (equal (buffer-string) second)))
        (should (= (length kill-ring) 2))))))

(ert-deftest test-shape-artist-export-command-prompts-for-format ()
  "Interactively the format comes from `completing-read', default mermaid."
  (test-export--with-ui
    (test-artist-with-buffer
        "+---+\n| A |\n+---+"
      (let ((calls nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest args) (push args calls) "svg")))
          (let ((result (call-interactively #'shape-artist-export)))
            (should (string-prefix-p "<svg " result))))
        (should (= (length calls) 1))
        (let ((args (car calls)))
          (should (equal (nth 1 args) '("mermaid" "dot" "svg")))
          (should (nth 3 args))
          (should (equal (nth 6 args) "mermaid")))))))

(ert-deftest test-shape-artist-export-command-default-format ()
  "Accepting the default choice exports Mermaid."
  (test-export--with-ui
    (test-artist-with-buffer
        "+---+\n| A |\n+---+"
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll _pred _req _init _hist def) def)))
        (should (string-prefix-p "flowchart LR"
                                 (call-interactively #'shape-artist-export)))))))

(ert-deftest test-shape-artist-export-command-writes-file-with-prefix ()
  "With a prefix argument the export is also written to the chosen file."
  (test-export--with-ui
    (let ((file (make-temp-file "shape-artist-export-" nil ".mmd")))
      (unwind-protect
          (test-artist-with-buffer
              (test-export--box "Größe" t)
            (delete-file file)
            (let ((current-prefix-arg '(4))
                  (asked nil))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "mermaid"))
                        ((symbol-function 'read-file-name)
                         (lambda (&rest args) (push args asked) file))
                        ((symbol-function 'y-or-n-p)
                         (lambda (&rest _) (ert-fail "unexpected overwrite prompt"))))
                (let ((result (call-interactively #'shape-artist-export)))
                  (should (= (length asked) 1))
                  (should (file-exists-p file))
                  (should (equal (with-temp-buffer
                                   (let ((coding-system-for-read 'utf-8))
                                     (insert-file-contents file))
                                   (buffer-string))
                                 result))
                  (should (string-match-p "Größe" result))))))
        (when (file-exists-p file) (delete-file file))))))

(ert-deftest test-shape-artist-export-command-no-prefix-writes-no-file ()
  "Without a prefix argument no file is requested."
  (test-export--with-ui
    (test-artist-with-buffer
        "+---+\n| A |\n+---+"
      (let ((current-prefix-arg nil))
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "dot"))
                  ((symbol-function 'read-file-name)
                   (lambda (&rest _) (ert-fail "must not ask for a file"))))
          (call-interactively #'shape-artist-export))))))

(ert-deftest test-shape-artist-export-command-overwrite-confirmation ()
  "Interactively, an existing file is only replaced after confirmation."
  (test-export--with-ui
    (let ((file (make-temp-file "shape-artist-export-")))
      (unwind-protect
          (test-artist-with-buffer
              "+---+\n| A |\n+---+"
            (with-temp-file file (insert "precious"))
            (let ((current-prefix-arg '(4)))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "dot"))
                        ((symbol-function 'read-file-name) (lambda (&rest _) file))
                        ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
                (should-error (call-interactively #'shape-artist-export)
                              :type 'user-error)
                (should (equal (with-temp-buffer (insert-file-contents file)
                                                 (buffer-string))
                               "precious")))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "dot"))
                        ((symbol-function 'read-file-name) (lambda (&rest _) file))
                        ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                (call-interactively #'shape-artist-export)
                (should (string-prefix-p
                         "digraph"
                         (with-temp-buffer (insert-file-contents file)
                                           (buffer-string)))))))
        (delete-file file)))))

(ert-deftest test-shape-artist-export-programmatic-file-argument ()
  "Calling from Lisp with a file writes it without prompting."
  (test-export--with-ui
    (let ((file (make-temp-file "shape-artist-export-" nil ".svg")))
      (unwind-protect
          (test-artist-with-buffer
              "+---+\n| A |\n+---+"
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (&rest _) (ert-fail "unexpected prompt"))))
              (let ((result (shape-artist-export 'svg file)))
                (should (equal (with-temp-buffer
                                 (insert-file-contents file)
                                 (buffer-string))
                               result)))))
        (delete-file file)))))

(ert-deftest test-shape-artist-export-command-unknown-format ()
  "An unknown format from Lisp fails before any buffer or file is touched."
  (test-export--with-ui
    (test-artist-with-buffer
        "+---+\n| A |\n+---+"
      (should-error (shape-artist-export 'pdf) :type 'user-error)
      (should-not (get-buffer shape-artist-export--buffer-name))
      (should-not kill-ring))))

;;; test-shape-artist-export.el ends here
