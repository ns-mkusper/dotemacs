#!/usr/bin/env bash
# Install the tangled config into a fresh Emacs directory and check it
# against timing budgets (perf-check.el) and window layout (window-check.el).
#
# Packages are fetched by straight on the first boot, which is slow; point
# PERF_EMACS_DIR's straight/ at a cache to reuse them (CI caches it).
#
#   PERF_EMACS_DIR  config directory to install into (default ~/.emacs.d)
#   PERF_SCALE      multiplier for every timing budget (default 1)
#   PERF_OUT_DIR    where results are written (default ./perf-results)
#   PERF_AFTER_INIT file loaded after the config, e.g. to confirm a check
#                   fails when a regression is reintroduced
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TESTS_DIR="${ROOT_DIR}/tests/perf"
EMACS_DIR="${PERF_EMACS_DIR:-${HOME}/.emacs.d}"
OUT_DIR="${PERF_OUT_DIR:-${PWD}/perf-results}"
mkdir -p "${EMACS_DIR}" "${OUT_DIR}"

boot=(-l "${TESTS_DIR}/no-prompts.el" -l "${EMACS_DIR}/early-init.el" -l "${EMACS_DIR}/init.el")
if [[ -n "${PERF_AFTER_INIT:-}" ]]; then
  boot+=(-l "${PERF_AFTER_INIT}")
fi

echo "== Installing config into ${EMACS_DIR} =="
"${ROOT_DIR}/tests/tangle-config.sh" "${EMACS_DIR}" >/dev/null

echo "== First boot: fetch and build packages (untimed) =="
emacs --batch "${boot[@]}" </dev/null >"${OUT_DIR}/first-boot.log" 2>&1 || true
grep -E '^(Error|Warning) \(use-package\)|^ERR' "${OUT_DIR}/first-boot.log" || true

status=0

echo "== Timing checks =="
PERF_OUT="${OUT_DIR}/perf.json" \
  emacs --batch "${boot[@]}" -l "${TESTS_DIR}/perf-check.el" </dev/null 2>"${OUT_DIR}/perf-check.log" \
  || status=1

echo "== Window checks (emacs -nw in a 60x200 terminal) =="
# -q: load the config only through the -l chain, after no-prompts.el.
# Without it emacs -nw loads init.el itself first and can block on a prompt.
rm -f "${OUT_DIR}/window-check.txt"
quoted=$(printf ' %q' emacs -nw -q "${boot[@]}" -l "${TESTS_DIR}/window-check.el")
if WINDOW_CHECK_OUT="${OUT_DIR}/window-check.txt" TERM=xterm-256color \
     timeout 300 script -qec "stty rows 60 cols 200;${quoted}" /dev/null >/dev/null 2>&1; then
  cat "${OUT_DIR}/window-check.txt"
else
  cat "${OUT_DIR}/window-check.txt" 2>/dev/null || echo "window-check produced no output (timed out or crashed)"
  status=1
fi

exit "${status}"
