#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CASE_NAME="${TRAMP_TEST_CASE:-ci-direct}"

run_case() {
  local scenarios="$1"
  local cycles="$2"
  local enforce_abs="$3"
  local timeout_secs="$4"

  TRAMP_TEST_SCENARIOS="${TRAMP_TEST_SCENARIOS:-${scenarios}}" \
  TRAMP_TEST_CYCLES="${TRAMP_TEST_CYCLES:-${cycles}}" \
  TRAMP_TEST_ENFORCE_ABS_PERF="${TRAMP_TEST_ENFORCE_ABS_PERF:-${enforce_abs}}" \
  TRAMP_TEST_TIMEOUT_SECS="${TRAMP_TEST_TIMEOUT_SECS:-${timeout_secs}}" \
  "${ROOT_DIR}/run-tramp-bench.sh"
}

case "${CASE_NAME}" in
  ci-direct)
    run_case "direct" "3" "1" "300"
    ;;
  ci-bastion)
    run_case "bastion" "3" "1" "300"
    ;;
  smoke-all)
    run_case "direct,bastion" "1" "0" "240"
    ;;
  real-smoke)
    run_case "real" "1" "0" "300"
    ;;
  *)
    echo "Unknown TRAMP_TEST_CASE: ${CASE_NAME}" >&2
    exit 2
    ;;
esac
