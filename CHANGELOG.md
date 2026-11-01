# Changelog

All notable changes to Shape Artist are documented in this file.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/) and
the project follows [Semantic Versioning](https://semver.org/). No release tags
exist yet; `0.1.0` is the baseline that was recorded by the committed history
before the `0.2.0` work and was never tagged.

## [Unreleased]

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
