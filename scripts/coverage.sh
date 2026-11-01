#!/usr/bin/env bash
# Run the ERT suite under undercover and print a per-file line-coverage summary.
#
# Usage: scripts/coverage.sh [--min PERCENT]
#   --min PERCENT   exit non-zero when total line coverage is below PERCENT
#                   (default: $COVERAGE_MIN, or no threshold).
#
# Output: coverage/lcov.info (git-ignored) plus a summary on stdout.
#
# Known limitation: coverage is edebug-based (undercover).  Functions with a
# computed `(interactive (list ...))' spec are reported as uncovered unless they
# are invoked through `call-interactively', even when their bodies run, so the
# total is a slight under-estimate.  Test such commands interactively.
set -euo pipefail

min="${COVERAGE_MIN:-}"
while [ $# -gt 0 ]; do
  case "$1" in
    --min) min="$2"; shift 2 ;;
    -h|--help) sed -n '2,10p' "$0"; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

cd "$(dirname "$0")/.."
root="$(pwd)"
report="$root/coverage/lcov.info"
mkdir -p coverage
rm -f "$report"

# Let Eldev install and resolve dependencies (Evil via the test set, undercover
# via the coverage set declared in the Eldev file), then run plain Emacs with
# those packages on the load path.  Eldev does not drive the run itself:
# `eldev exec' pre-requires the project, uninstrumented, and the Eldev
# undercover plugin writes no report.
eldev prepare test coverage >/dev/null

emacs_version="$(emacs --batch --eval '(princ (format "%d.%d" emacs-major-version emacs-minor-version))')"
load_args=()
for dir in "$root"/.eldev/"$emacs_version"/packages/*/; do
  load_args+=(-L "$dir")
done

# Run from a scratch copy holding only sources and tests: byte-compiled files are
# never instrumented, so a stale .elc beside a source would hide it from the report.
scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT
cp ./*.el "$scratch"/
cp -R tests scripts "$scratch"/
find "$scratch" -name '*.elc' -delete

# UNDERCOVER_FORCE: undercover is otherwise inert outside recognised CI services.
# stdin is closed so that nothing in the instrumented run can block waiting for
# input, which would hang a CI job silently.
run_instrumented() {
  (cd "$scratch" &&
   UNDERCOVER_FORCE=true SHAPE_ARTIST_COVERAGE_REPORT="$report" \
     SHAPE_ARTIST_ROUNDTRIP_DIAGRAMS="${SHAPE_ARTIST_ROUNDTRIP_DIAGRAMS:-2}" \
     emacs --batch -L "$scratch" -L "$scratch/tests" "${load_args[@]}" \
           -l scripts/coverage.el) 2>&1 < /dev/null
}

status=0
if [ -n "${CI:-}" ]; then
  # Stream the output so a stalled run can be diagnosed from the job log.
  run_instrumented | tee coverage/test-output.log || status=$?
else
  run_instrumented > coverage/test-output.log || status=$?
fi
if [ "$status" -ne 0 ]; then
  echo "coverage run failed (status $status); tail of coverage/test-output.log:" >&2
  tail -25 coverage/test-output.log >&2
  exit 1
fi

# Map scratch-copy paths back to the real checkout.
sed -i "s#^SF:$scratch/#SF:$root/#" "$report"

grep -E '^Ran [0-9]+ tests' coverage/test-output.log || true
[ -s "$report" ] || { echo "no lcov report produced" >&2; exit 1; }

awk -v root="$root/" -v min="$min" '
  /^SF:/ { f = substr($0, 4); sub(root, "", f) }
  /^DA:/ { split(substr($0, 4), a, ","); t[f]++; if (a[2] > 0) h[f]++ }
  END {
    printf "%-38s %9s %7s\n", "file", "hit/total", "cover"
    for (f in t) {
      th += h[f]; tt += t[f]
      printf "%-38s %4d/%-4d %6.1f%%\n", f, h[f], t[f], 100 * h[f] / t[f] | "sort -k3 -n"
    }
    close("sort -k3 -n")
    pct = 100 * th / tt
    printf "%-38s %4d/%-4d %6.1f%%\n", "TOTAL", th, tt, pct
    if (min != "" && pct < min) {
      printf "FAIL: total coverage %.1f%% is below the required %s%%\n", pct, min
      exit 1
    }
  }' "$report"
