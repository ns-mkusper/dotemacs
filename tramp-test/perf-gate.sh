#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <scenario> <scenario_out_dir>" >&2
  exit 2
fi

SCENARIO="$1"
OUT_DIR="$2"
OPT_TXT="${OUT_DIR}/optimized-summary.txt"

if [[ ! -f "${OPT_TXT}" ]]; then
  echo "Missing optimized summary: ${OPT_TXT}" >&2
  exit 1
fi

extract_val() {
  local file="$1"
  local key="$2"
  awk -F= -v k="${key}" '$1==k {gsub(/s$/,"",$2); print $2; exit}' "${file}"
}

opt_connect="$(extract_val "${OPT_TXT}" "AVG_CONNECT")"
opt_list="$(extract_val "${OPT_TXT}" "AVG_LIST")"

case "${SCENARIO}" in
  direct)
    max_connect="${TRAMP_PERF_DIRECT_MAX_CONNECT:-1.20}"
    max_list="${TRAMP_PERF_DIRECT_MAX_LIST:-0.40}"
    ;;
  bastion)
    max_connect="${TRAMP_PERF_BASTION_MAX_CONNECT:-3.50}"
    max_list="${TRAMP_PERF_BASTION_MAX_LIST:-1.20}"
    ;;
  real)
    max_connect="${TRAMP_PERF_REAL_MAX_CONNECT:-5.00}"
    max_list="${TRAMP_PERF_REAL_MAX_LIST:-2.00}"
    ;;
  *)
    echo "Unknown scenario for perf gate: ${SCENARIO}" >&2
    exit 1
    ;;
esac

echo "== TRAMP absolute perf gate (${SCENARIO}) =="
echo "Optimized connect=${opt_connect}s (max ${max_connect}s)"
echo "Optimized list=${opt_list}s (max ${max_list}s)"

python3 - "$opt_connect" "$opt_list" "$max_connect" "$max_list" <<'PY'
import sys
opt_c, opt_l, max_c, max_l = map(float, sys.argv[1:])
errs = []
if opt_c > max_c:
    errs.append(f"connect too slow: {opt_c:.4f}s > {max_c:.4f}s")
if opt_l > max_l:
    errs.append(f"list too slow: {opt_l:.4f}s > {max_l:.4f}s")
if errs:
    print("FAILED:")
    for e in errs:
        print(f"- {e}")
    sys.exit(1)
print("PASS: absolute performance gate met.")
PY
