#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TARGET_DIR="${1:-${HOME}/.emacs.d}"

mkdir -p "${TARGET_DIR}"
"${ROOT_DIR}/tests/tangle-config.sh" "${TARGET_DIR}"

# Keep the source org with deployed config for easy retangling in-place.
cp "${ROOT_DIR}/dotemacs.org" "${TARGET_DIR}/dotemacs.org"

echo "Deployed literate config to: ${TARGET_DIR}"
echo "Launch Emacs using that directory, e.g.:"
echo "  emacs --init-directory \"${TARGET_DIR}\""
