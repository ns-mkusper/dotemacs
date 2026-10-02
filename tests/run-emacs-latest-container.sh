#!/usr/bin/env bash
# Run the test battery inside a container with the latest released Emacs.
#
# Uses the prebuilt silex/emacs images (https://hub.docker.com/r/silex/emacs),
# so there is nothing to build locally.  Override EMACS_LATEST_IMAGE to try a
# different release, e.g. EMACS_LATEST_IMAGE=silex/emacs:master-ci for the
# nightly development snapshot.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IMAGE="${EMACS_LATEST_IMAGE:-silex/emacs:31-ci}"
SUITE="${TEST_SUITE:-core-static}"
RUN_TRAMP_SMOKE="${RUN_TRAMP_SMOKE:-1}"

if ! command -v docker >/dev/null 2>&1; then
  echo "docker is required but was not found in PATH" >&2
  exit 1
fi

container_cmd='
set -euo pipefail
mkdir -p /repo
tar -C /repo -xf -
cd /repo
git config --global --add safe.directory /repo
chmod +x tests/run-battery.sh tests/tangle-config.sh tests/tramp/*.sh
emacs --version | head -n 1
TEST_SUITE="${TEST_SUITE:-core-static}" ./tests/run-battery.sh
if [[ "${RUN_TRAMP_SMOKE:-1}" == "1" ]]; then
  emacs --batch -l tests/tramp/smoke-platform.el
fi
'

echo "== Running suite ${SUITE} inside ${IMAGE} =="
tar -C "${ROOT_DIR}" --exclude=./.git -cf - . \
  | docker run --rm -i \
      -e TEST_SUITE="${SUITE}" \
      -e RUN_TRAMP_SMOKE="${RUN_TRAMP_SMOKE}" \
      "${IMAGE}" \
      bash -c "${container_cmd}"
