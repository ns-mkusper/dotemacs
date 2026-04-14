#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SSH_IMAGE="tramp-sshd-test:latest"
RUNNER_IMAGE="tramp-emacs-bench:latest"
OUT_DIR="${TRAMP_TEST_OUTPUT_DIR:-${ROOT_DIR}/out}"
CYCLES="${TRAMP_TEST_CYCLES:-4}"
VERBOSE="${TRAMP_TEST_VERBOSE:-10}"
EMACS_TIMEOUT="${TRAMP_TEST_TIMEOUT_SECS:-180}"
SCENARIOS_CSV="${TRAMP_TEST_SCENARIOS:-direct,bastion}"
REAL_TARGET="${TRAMP_TEST_REAL_TARGET:-}"
REAL_SSH_DIR="${TRAMP_TEST_REAL_SSH_DIR:-${HOME}/.ssh}"
ENFORCE_ABS_PERF="${TRAMP_TEST_ENFORCE_ABS_PERF:-1}"
SKIP_BUILD="${TRAMP_TEST_SKIP_BUILD:-0}"
TEST_KEY_FILE="${TRAMP_TEST_KEY_FILE:-}"
TEST_KEY_PUB_FILE="${TRAMP_TEST_KEY_PUB_FILE:-}"
SSH_TEST_USER="${TRAMP_TEST_USER:-mkusper}"
DIRECT_HOST="${TRAMP_TEST_DIRECT_HOST:-127.0.0.1}"
DIRECT_PORT="${TRAMP_TEST_DIRECT_PORT:-2204}"
BASTION_HOST="${TRAMP_TEST_BASTION_HOST:-127.0.0.1}"
BASTION_PORT="${TRAMP_TEST_BASTION_PORT:-2224}"
BASTION_TARGET_HOST="${TRAMP_TEST_BASTION_TARGET_HOST:-tramp-sshd-target}"

DIRECT_CONTAINER="tramp-sshd-direct"
BASTION_CONTAINER="tramp-sshd-bastion"
TARGET_CONTAINER="tramp-sshd-target"
BASTION_NETWORK="tramp-bastion-net"

TMP_DIR="$(mktemp -d)"

cleanup() {
  docker rm -f "${DIRECT_CONTAINER}" "${BASTION_CONTAINER}" "${TARGET_CONTAINER}" >/dev/null 2>&1 || true
  docker network rm "${BASTION_NETWORK}" >/dev/null 2>&1 || true
  rm -rf "${TMP_DIR}"
}
trap cleanup EXIT

ssh_wait() {
  local port="$1"
  local key="$2"
  local userhost="$3"
  local ok=0
  for _ in $(seq 1 60); do
    if ssh -i "${key}" -p "${port}" \
      -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
      "${userhost}" "echo up" >/dev/null 2>&1; then
      ok=1
      break
    fi
    sleep 0.5
  done
  [[ "${ok}" -eq 1 ]]
}

build_images() {
  docker build -f "${ROOT_DIR}/Dockerfile.sshd" -t "${SSH_IMAGE}" "${ROOT_DIR}" >/dev/null
  docker build -f "${ROOT_DIR}/Dockerfile.runner" -t "${RUNNER_IMAGE}" "${ROOT_DIR}" >/dev/null
  echo "== Emacs version =="
  docker run --rm "${RUNNER_IMAGE}" "emacs --version | head -n 1"
}

prepare_key_material() {
  if [[ -n "${TEST_KEY_FILE}" ]]; then
    if [[ ! -f "${TEST_KEY_FILE}" ]]; then
      echo "TRAMP_TEST_KEY_FILE not found: ${TEST_KEY_FILE}" >&2
      exit 1
    fi
    cp "${TEST_KEY_FILE}" "${TMP_DIR}/id_ed25519"
    chmod 600 "${TMP_DIR}/id_ed25519"
    if [[ -n "${TEST_KEY_PUB_FILE}" ]]; then
      if [[ ! -f "${TEST_KEY_PUB_FILE}" ]]; then
        echo "TRAMP_TEST_KEY_PUB_FILE not found: ${TEST_KEY_PUB_FILE}" >&2
        exit 1
      fi
      cp "${TEST_KEY_PUB_FILE}" "${TMP_DIR}/id_ed25519.pub"
    else
      ssh-keygen -y -f "${TMP_DIR}/id_ed25519" > "${TMP_DIR}/id_ed25519.pub"
    fi
  else
    ssh-keygen -t ed25519 -N "" -f "${TMP_DIR}/id_ed25519" >/dev/null
  fi
  cp "${TMP_DIR}/id_ed25519.pub" "${ROOT_DIR}/authorized_keys"
}

prepare_ssh_dir_direct() {
  local dir="${TMP_DIR}/ssh-direct"
  mkdir -p "${dir}"
  cp "${TMP_DIR}/id_ed25519" "${dir}/id_ed25519"
  chmod 600 "${dir}/id_ed25519"
  cat > "${dir}/config" <<EOF
Host tramp-direct
  HostName ${DIRECT_HOST}
  Port ${DIRECT_PORT}
  User ${SSH_TEST_USER}
  IdentityFile /root/.ssh/id_ed25519
  StrictHostKeyChecking no
  UserKnownHostsFile /dev/null
EOF
  chmod 600 "${dir}/config"
  echo "${dir}"
}

prepare_ssh_dir_bastion() {
  local dir="${TMP_DIR}/ssh-bastion"
  mkdir -p "${dir}"
  cp "${TMP_DIR}/id_ed25519" "${dir}/id_ed25519"
  chmod 600 "${dir}/id_ed25519"
  cat > "${dir}/config" <<EOF
Host tramp-bastion
  HostName ${BASTION_HOST}
  Port ${BASTION_PORT}
  User ${SSH_TEST_USER}
  IdentityFile /root/.ssh/id_ed25519
  StrictHostKeyChecking no
  UserKnownHostsFile /dev/null

Host tramp-target-via-bastion
  HostName ${BASTION_TARGET_HOST}
  User ${SSH_TEST_USER}
  IdentityFile /root/.ssh/id_ed25519
  ProxyJump tramp-bastion
  StrictHostKeyChecking no
  UserKnownHostsFile /dev/null
EOF
  chmod 600 "${dir}/config"
  echo "${dir}"
}

prepare_ssh_dir_real() {
  local src="$1"
  local dir="${TMP_DIR}/ssh-real"
  mkdir -p "${dir}"
  chmod 700 "${dir}"

  # Some ~/.ssh trees include ControlMaster sockets. Copy best-effort and drop sockets.
  set +e
  cp -a "${src}/." "${dir}/" 2>/dev/null
  cp_rc=$?
  set -e
  if [[ "${cp_rc}" -ne 0 ]]; then
    echo "Warning: partial ~/.ssh copy (likely due to sockets). Continuing with copied files." >&2
  fi
  find "${dir}" -type s -delete 2>/dev/null || true
  if [[ -f "${dir}/config" ]]; then
    chmod 600 "${dir}/config"
  fi
  find "${dir}" -maxdepth 1 -type f -name "id_*" -exec chmod 600 {} \;

  echo "${dir}"
}

start_direct_stack() {
  docker rm -f "${DIRECT_CONTAINER}" >/dev/null 2>&1 || true
  docker run -d --name "${DIRECT_CONTAINER}" -p "${DIRECT_PORT}:22" "${SSH_IMAGE}" >/dev/null
  if ! ssh_wait "${DIRECT_PORT}" "${TMP_DIR}/id_ed25519" "${SSH_TEST_USER}@${DIRECT_HOST}"; then
    echo "Direct SSHD did not become reachable on ${DIRECT_HOST}:${DIRECT_PORT}" >&2
    exit 1
  fi
}

start_bastion_stack() {
  docker rm -f "${BASTION_CONTAINER}" "${TARGET_CONTAINER}" >/dev/null 2>&1 || true
  docker network rm "${BASTION_NETWORK}" >/dev/null 2>&1 || true
  docker network create "${BASTION_NETWORK}" >/dev/null

  docker run -d --name "${TARGET_CONTAINER}" --network "${BASTION_NETWORK}" "${SSH_IMAGE}" >/dev/null
  docker run -d --name "${BASTION_CONTAINER}" --network "${BASTION_NETWORK}" -p "${BASTION_PORT}:22" "${SSH_IMAGE}" >/dev/null

  if ! ssh_wait "${BASTION_PORT}" "${TMP_DIR}/id_ed25519" "${SSH_TEST_USER}@${BASTION_HOST}"; then
    echo "Bastion SSHD did not become reachable on ${BASTION_HOST}:${BASTION_PORT}" >&2
    exit 1
  fi
}

run_profile() {
  local scenario="$1"
  local target="$2"
  local ssh_dir="$3"
  local profile="$4"
  local scenario_out="${OUT_DIR}/${scenario}"
  local runner_container="tramp-emacs-bench-${scenario}-${profile}-$$"

  docker rm -f "${runner_container}" >/dev/null 2>&1 || true
  docker create --name "${runner_container}" --network host \
    -e TRAMP_TEST_PROFILE="${profile}" \
    -e TRAMP_TEST_TARGET="${target}" \
    -e TRAMP_TEST_CYCLES="${CYCLES}" \
    -e TRAMP_TEST_VERBOSE="${VERBOSE}" \
    -e TRAMP_TEST_OUTPUT_DIR="/bench/out" \
    "${RUNNER_IMAGE}" \
    "mkdir -p /bench/out /root/.ssh \
      && chown -R root:root /root/.ssh \
      && find /root/.ssh -type d -exec chmod 700 {} + \
      && find /root/.ssh -type f -exec chmod 600 {} + \
      && emacs --batch -l /bench/tramp-benchmark.el" >/dev/null

  docker cp "${ssh_dir}/." "${runner_container}:/root/.ssh/"

  set +e
  timeout "${EMACS_TIMEOUT}s" docker start -a "${runner_container}" | tee "${scenario_out}/${profile}-stdout.txt"
  rc="${PIPESTATUS[0]}"
  set -e
  docker cp "${runner_container}:/bench/out/." "${scenario_out}/"
  docker rm -f "${runner_container}" >/dev/null 2>&1 || true

  if [[ "${rc}" -ne 0 ]]; then
    echo "Scenario ${scenario}, profile ${profile} failed with exit code ${rc}" >&2
    exit "${rc}"
  fi
}

run_scenario() {
  local scenario="$1"
  local target="$2"
  local ssh_dir="$3"
  local scenario_out="${OUT_DIR}/${scenario}"

  mkdir -p "${scenario_out}"
  rm -rf "${scenario_out:?}/"*
  echo
  echo "== Scenario: ${scenario} =="
  echo "Target: ${target}"

  run_profile "${scenario}" "${target}" "${ssh_dir}" current
  run_profile "${scenario}" "${target}" "${ssh_dir}" optimized

  echo
  TRAMP_TEST_OUTPUT_DIR="${scenario_out}" "${ROOT_DIR}/analyze-results.sh"
  if [[ "${ENFORCE_ABS_PERF}" == "1" ]]; then
    echo
    "${ROOT_DIR}/perf-gate.sh" "${scenario}" "${scenario_out}"
  fi
}

main() {
  cd "${ROOT_DIR}"
  mkdir -p "${OUT_DIR}"
  IFS=',' read -r -a scenarios <<< "${SCENARIOS_CSV}"
  needs_sshd=0
  for scenario in "${scenarios[@]}"; do
    if [[ "${scenario}" == "direct" || "${scenario}" == "bastion" ]]; then
      needs_sshd=1
      break
    fi
  done
  if [[ "${needs_sshd}" == "1" ]]; then
    prepare_key_material
  fi
  if [[ "${SKIP_BUILD}" == "1" ]]; then
    if [[ "${needs_sshd}" == "1" ]]; then
      docker image inspect "${SSH_IMAGE}" >/dev/null
    fi
    docker image inspect "${RUNNER_IMAGE}" >/dev/null
    echo "== Using prebuilt images =="
    docker run --rm "${RUNNER_IMAGE}" "emacs --version | head -n 1"
  else
    build_images
  fi

  for scenario in "${scenarios[@]}"; do
    case "${scenario}" in
      direct)
        start_direct_stack
        run_scenario "direct" "/ssh:tramp-direct:/tmp/" "$(prepare_ssh_dir_direct)"
        ;;
      bastion)
        start_bastion_stack
        run_scenario "bastion" "/ssh:tramp-target-via-bastion:/tmp/" "$(prepare_ssh_dir_bastion)"
        ;;
      real)
        if [[ -z "${REAL_TARGET}" ]]; then
          echo "TRAMP_TEST_REAL_TARGET is required for scenario=real" >&2
          exit 1
        fi
        if [[ ! -d "${REAL_SSH_DIR}" ]]; then
          echo "TRAMP_TEST_REAL_SSH_DIR not found: ${REAL_SSH_DIR}" >&2
          exit 1
        fi
        run_scenario "real" "${REAL_TARGET}" "$(prepare_ssh_dir_real "${REAL_SSH_DIR}")"
        ;;
      *)
        echo "Unknown scenario: ${scenario}" >&2
        exit 1
        ;;
    esac
  done

  if [[ "${needs_sshd}" == "1" ]]; then
    rm -f "${ROOT_DIR}/authorized_keys"
  fi
  echo
  echo "Artifacts written to: ${OUT_DIR}"
}

main "$@"
