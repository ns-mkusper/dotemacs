#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DOCKERFILE="${ROOT_DIR}/tests/Dockerfile.emacs30-sid"
IMAGE_TAG="${EMACS30_IMAGE_TAG:-emacs30-sid:local}"
SUITE="${TEST_SUITE:-core-static}"
RUN_TRAMP_SMOKE="${RUN_TRAMP_SMOKE:-1}"

if ! command -v docker >/dev/null 2>&1; then
  echo "docker is required but was not found in PATH" >&2
  exit 1
fi

echo "== Building ${IMAGE_TAG} from ${DOCKERFILE} =="
docker build -f "${DOCKERFILE}" -t "${IMAGE_TAG}" "${ROOT_DIR}/tests"

container_cmd='
set -euo pipefail
mkdir -p /repo
tar -C /repo -xf -
cd /repo
git config --global --add safe.directory /repo
chmod +x tests/run-battery.sh tests/tramp/*.sh
TEST_SUITE="${TEST_SUITE:-core-static}" ./tests/run-battery.sh
if [[ "${RUN_TRAMP_SMOKE:-1}" == "1" ]]; then
  emacs --batch -l tests/tramp/smoke-platform.el
fi
'

echo "== Running suite ${SUITE} inside ${IMAGE_TAG} =="
tar -C "${ROOT_DIR}" -cf - . \
  | docker run --rm -i \
      -e TEST_SUITE="${SUITE}" \
      -e RUN_TRAMP_SMOKE="${RUN_TRAMP_SMOKE}" \
      "${IMAGE_TAG}" \
      bash -lc "${container_cmd}"
