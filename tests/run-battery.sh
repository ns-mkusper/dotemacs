#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SUITE="${TEST_SUITE:-core-static}"

run_core_static() {
  echo "== Shell syntax checks =="
  bash -n "${ROOT_DIR}/tramp/"*.sh

  echo "== Emacs Lisp syntax checks =="
  local elisp_files=()
  while IFS= read -r -d '' file; do
    elisp_files+=("${file}")
  done < <(git -C "${ROOT_DIR}/.." ls-files -z '*.el')
  if [[ "${#elisp_files[@]}" -eq 0 ]]; then
    echo "No elisp files found."
    return
  fi
  emacs --batch -Q -l "${ROOT_DIR}/elisp-syntax-check.el" -- "${elisp_files[@]}"
}

case "${SUITE}" in
  core-static)
    run_core_static
    ;;
  tramp-ci-direct)
    TRAMP_TEST_CASE=ci-direct "${ROOT_DIR}/tramp/test-battery.sh"
    ;;
  tramp-ci-bastion)
    TRAMP_TEST_CASE=ci-bastion "${ROOT_DIR}/tramp/test-battery.sh"
    ;;
  *)
    echo "Unknown TEST_SUITE: ${SUITE}" >&2
    exit 2
    ;;
esac
