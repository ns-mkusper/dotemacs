#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUT_DIR="${TRAMP_TEST_OUTPUT_DIR:-${ROOT_DIR}/out}"
MAX_CONNECT_RATIO="${MAX_CONNECT_RATIO:-1.25}"
MAX_LIST_RATIO="${MAX_LIST_RATIO:-1.25}"

CUR_TXT="${OUT_DIR}/current-summary.txt"
OPT_TXT="${OUT_DIR}/optimized-summary.txt"

if [[ ! -f "${CUR_TXT}" || ! -f "${OPT_TXT}" ]]; then
  echo "Missing summary files in ${OUT_DIR}" >&2
  exit 1
fi

extract_val() {
  local file="$1"
  local key="$2"
  awk -F= -v k="${key}" '$1==k {gsub(/s$/,"",$2); print $2; exit}' "${file}"
}

cur_connect="$(extract_val "${CUR_TXT}" "AVG_CONNECT")"
cur_list="$(extract_val "${CUR_TXT}" "AVG_LIST")"
opt_connect="$(extract_val "${OPT_TXT}" "AVG_CONNECT")"
opt_list="$(extract_val "${OPT_TXT}" "AVG_LIST")"

echo "== TRAMP regression analysis =="
echo "Current   connect=${cur_connect}s list=${cur_list}s"
echo "Optimized connect=${opt_connect}s list=${opt_list}s"

for p in current optimized; do
  debug_file="$(find "${OUT_DIR}/${p}/debug" -type f -name "*tramp ssh*" | head -1 || true)"
  if [[ -n "${debug_file}" ]]; then
    ssh_cmd="$(grep -a -m1 "exec ssh " "${debug_file}" || true)"
    cache_hits="$(grep -a -c "cache used: t" "${debug_file}" || true)"
    cache_miss="$(grep -a -c "cache used: nil" "${debug_file}" || true)"
    echo "${p} ssh command: ${ssh_cmd}"
    echo "${p} cache stats: hits=${cache_hits} miss=${cache_miss}"
  fi
done

python3 - "$cur_connect" "$cur_list" "$opt_connect" "$opt_list" "$MAX_CONNECT_RATIO" "$MAX_LIST_RATIO" <<'PY'
import sys
cur_c, cur_l, opt_c, opt_l, max_cr, max_lr = map(float, sys.argv[1:])
errors = []
if opt_c > cur_c * max_cr:
    errors.append(f"optimized connect regression: {opt_c:.4f}s > {cur_c*max_cr:.4f}s")
if opt_l > cur_l * max_lr:
    errors.append(f"optimized list regression: {opt_l:.4f}s > {cur_l*max_lr:.4f}s")
if errors:
    print("FAILED:")
    for e in errors:
        print(f"- {e}")
    sys.exit(1)
print("PASS: optimized profile is within regression thresholds.")
PY

error_hits="$(
  grep -R -a -E -n "Connection timed out|Permission denied|Authentication failed|Entering debugger|Tramp: Opening connection .* failed|Tramp failed to connect|Connection refused" "${OUT_DIR}" \
    | grep -v "Looking for regexp" \
    || true
)"
if [[ -n "${error_hits}" ]]; then
  echo "FAILED: found critical errors in TRAMP logs." >&2
  echo "${error_hits}" >&2
  exit 1
fi

echo "PASS: no critical error signatures found in verbose TRAMP logs."
