#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SUITE="${TEST_SUITE:-core-static}"

run_core_static() {
  echo "== Literate config tangle check =="
  "${ROOT_DIR}/tangle-config.sh"

  echo "== Shell syntax checks =="
  local shell_files=()
  if git -C "${ROOT_DIR}/.." rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    while IFS= read -r -d '' file; do
      shell_files+=("${ROOT_DIR}/../${file}")
    done < <(git -C "${ROOT_DIR}/.." ls-files -z '*.sh')
  else
    while IFS= read -r -d '' file; do
      shell_files+=("${file}")
    done < <(find "${ROOT_DIR}/.." -type f -name '*.sh' -print0)
  fi
  if [[ "${#shell_files[@]}" -eq 0 ]]; then
    echo "No shell scripts found."
  else
    bash -n "${shell_files[@]}"
  fi

  echo "== Emacs Lisp syntax checks =="
  local elisp_files=()
  if git -C "${ROOT_DIR}/.." rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    while IFS= read -r -d '' file; do
      elisp_files+=("${file}")
    done < <(git -C "${ROOT_DIR}/.." ls-files -z '*.el')
  else
    while IFS= read -r -d '' file; do
      elisp_files+=("${file#${ROOT_DIR}/../}")
    done < <(find "${ROOT_DIR}/.." -type f -name '*.el' -print0)
  fi
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
