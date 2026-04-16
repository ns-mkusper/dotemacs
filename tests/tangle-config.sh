#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TARGET_DIR="${1:-${ROOT_DIR}}"
ORG_FILE="${ROOT_DIR}/dotemacs.org"

if [[ ! -f "${ORG_FILE}" ]]; then
  echo "Missing literate config: ${ORG_FILE}" >&2
  exit 1
fi

mkdir -p "${TARGET_DIR}"
if [[ ! -d "${TARGET_DIR}/inits" ]]; then
  mkdir -p "${TARGET_DIR}/inits"
fi

TMP_ORG="$(mktemp "${TARGET_DIR}/.dotemacs-tangle-XXXXXX.org")"
cp "${ORG_FILE}" "${TMP_ORG}"
cleanup() {
  rm -f "${TMP_ORG}"
}
trap cleanup EXIT

EMACS_ORG_PATH="${TMP_ORG}"
if command -v cygpath >/dev/null 2>&1; then
  EMACS_ORG_PATH="$(cygpath -m "${TMP_ORG}")"
fi

emacs --batch -Q \
  --eval "(require 'org)" \
  --eval "(require 'ob-tangle)" \
  --eval "(let ((org-confirm-babel-evaluate nil)) (org-babel-tangle-file \"${EMACS_ORG_PATH}\"))"
