# Changelog

All notable changes to Shape Artist are documented in this file.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and
the project follows [Semantic Versioning](https://semver.org/). Versions
`0.1.0` to `0.2.1` were recorded in the history before the first public release
and are tagged retroactively on the commits that introduced them.

## [Unreleased]

## [0.3.0] - 2026-11-01

A user-experience release: trust, editing where diagrams live, learnability,
direct manipulation, and diagnosis. See `doc/ux-roadmap.org`.

### Added

- **In-place editing.** `shape-artist-edit-at-point` finds the diagram around
  point (or the active region) in any buffer: comment runs (`;;`, `//`, `#`,
  `--`, `%`, `*`, `>`, Org `:`), Markdown fences, Org example/source blocks, and
  indented text. It opens an isolated session with the prefix removed and
  writes back on `C-c C-c` (`C-c C-k` aborts) with everything else
  byte-identical, as one undo step, and with a guard if the source changed.
  `shape-artist-insert-diagram` starts a new diagram in the same comment style.
- **Mouse support.** Press to select, drag to move with a ghost preview, drag a
  corner handle to resize, drag across blank canvas to draw, shift-press for a
  group, double-press to edit a label, right-press for a context menu. Every
  drag is one undo step.
- **Interactive sidebar.** `RET` or a click on a shape row selects that shape;
  `shape-artist-shape-edit-focus` is the public entry point.
- **Learnability.** `?` shows a mode-aware key hint and, pressed again, a grouped
  and annotated command palette; `shape-artist-help-cheatsheet` is generated from
  the keymaps; an Artist menu-bar entry; `shape-artist-tutorial` with a live
  checklist and persisted progress; a one-time first-run hint.
- **Diagnose and export.** `E e` explains why a box is not detected, `E r` /
  `E R` repair near-miss boxes, and `E x` exports Mermaid, Graphviz DOT, or SVG.
- **Complete styles.** All seven border families are selectable and drawable;
  the `shape-artist-default-style` option controls DRAW; style commands apply
  only to the shapes that can take them; cycling stays within a box's family.
- **Development.** `scripts/coverage.sh` with a CI coverage floor of 95%, and
  seeded property tests (random moves, resizes, and restyles) asserting that
  edits never corrupt a diagram. 253 tests became 700 and line coverage went
  from 78% to 97%, with the new tests mutation-checked.

### Changed

- **Breaking:** `u` is always undo (it toggled the sidebar outside DRAW mode);
  `a` now toggles the sidebar (it only showed it). The documentation keymaps now
  list every live key, and the key audit is enforced by a test.
- `?` opens the key hint instead of going straight to the palette; the palette
  is the second press.
- Guide changes are part of ordinary Emacs undo: one `undo` reverts text and
  guides together. `G u` / `G R` still work and stay in step.
- `shape-artist-quit` and turning the mode off return the buffer to its previous
  major mode, without altering the text.
- Error messages state the cause, the offending value, and the remedy.
- Evil states are driven only when Evil is active in the buffer.

### Fixed

- Enabling the mode twice, or toggling `shape-artist-mode` off and on in one
  buffer, no longer signals "You are already editing a picture".
- Moving a box along a short attached connector no longer collapses it to a
  stray `-`, and moving a box past the far end of an end-attached connector no
  longer reverses it and corrupts the box. Connectors stay at least two cells
  long.
- Dragging past the top or left of the canvas no longer errors during a preview.
- Loading Evil without enabling it no longer half-initialises Evil in artist
  buffers.

### Known limitations

- Not yet done from the roadmap: typing a label directly inside a box, a
  "connect two boxes" tool with automatic routing, DRAW tool selection and key
  presets, a one-key Org `C-c '` integration (the commands work in Org blocks
  but have no default binding), and importing Mermaid or DOT.
- A connector whose stem ends on the middle of a bar (a T-junction) is not
  joined by the parser's own graph, so the exporter groups segments itself.
- Boxes that share a border row or column with an earlier box are detected only
  once.
- Emacs 27.1 is the declared minimum; the CI matrix covers 27.1 to snapshot.

## [0.2.1] - 2026-11-01

### Fixed

- Disabling `shape-artist-core-mode` now restores the buffer's prior cursor
  type, mode-line format, and `whitespace-style`. The saved state is
  buffer-local, so enabling the mode in a second buffer no longer clobbers what
  the first buffer restores, and `whitespace-style` is no longer left forced to
  `(spaces space-mark)` after the mode is turned off.

## [0.2.0] - 2026-11-01

### Added

- Stable best-effort metadata IDs for parsed shapes, plus project-local metadata
  sidecar persistence that never alters diagram text.
- A shared persistence module so guide and metadata sidecars resolve their
  project root through one policy.
- Coherent ASCII and Unicode rectangle-family detection (standard, heavy,
  light, rounded, Unicode single, Unicode rounded, and Unicode double), with the
  detected border family recorded on each rectangle.
- A derived connector/graph analysis layer: logical orthogonal route grouping,
  rectangle adjacency, connected components, and breadth-first shortest-path
  queries.
- A centralized modal presentation contract with theme-aware mode/header/sidebar
  badges, distinct cursor forms, explicit SELECT candidate/marked overlays,
  persistent SHAPE-EDIT target highlighting, and `shape-artist-mode-changed-hook`.
- Sidebar graph health (nodes, connections, components) and shape IDs.
- End-to-end coverage for the key-driven mode visual contract.

### Changed

- Replaced hard-coded mode/preview colors with semantic theme-derived faces and
  made mode transitions the single owner of Evil state and visual feedback.
- Orthogonal `+` bends now participate in both axis scans, so elbow routes form
  one connected component instead of leaving an undetected gap.
- Tightened rectangle detection to validate complete border families and reject
  mixed-family near-boxes; move/redraw operations preserve detected Unicode box
  families.
- Standardized command discovery on the built-in `completing-read` UI and
  removed the unused optional Transient variables.
- Unified project-root selection for guide and metadata sidecars.

### Known limitations

- Multi-segment orthogonal routes are detected and analyzed, but automatic
  obstacle-aware rerouting and junction editing remain future work.
- Batch alignment/distribution commands and freeform non-rectangular shapes such
  as diamonds are not yet supported.

## [0.1.0] - 2026-07-08

Baseline recorded by the committed history prior to `0.2.0`; not tagged.

### Added

- Modal MOVE, DRAW, SELECT, and SHAPE-EDIT workflows with an Evil-compatible
  integration and a dependency-free non-Evil dispatch path.
- Rectangle detection, straight-edge/arrow parsing, labels, and a hierarchical
  EIEIO diagram model.
- Shape selection, multi-selection, label editing, movement, anchored resize,
  border styling, arrows, and grouped movement.
- Straight connector attachment metadata and connector sticking when attached
  rectangles or selected groups move.
- Persistent sidebar, outline tree, diagram centering, and a mode-aware
  `completing-read` quick-action interface.
- Horizontal and vertical alignment guides, non-destructive guide/grid overlays,
  snapping, guide adoption, per-guide properties, guide metadata undo/redo, and
  project-local guide persistence.
- Eldev-based lint, compile, test, package, and doctor commands.
- End-to-end regression coverage for key-driven draw/select/edit workflows,
  arrows, group movement, connector sticking, anchored resize, and border
  parsing.

### Changed

- Migrated parser and outline implementation to the `shape-artist-*` namespace,
  retaining compatibility shims for historical feature names.

[Unreleased]: https://github.com/fkr-0/shape-artist/compare/v0.3.0...HEAD
[0.3.0]: https://github.com/fkr-0/shape-artist/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/fkr-0/shape-artist/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/fkr-0/shape-artist/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/fkr-0/shape-artist/releases/tag/v0.1.0
