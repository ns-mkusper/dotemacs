# TRAMP Test Battery

## What gets saved
- Local runs write artifacts to `tramp-test/out/` by default.
- CI runs upload a tarball artifact per case (for example `tramp-ci-direct-artifacts`).
- Each scenario includes:
  - `current-summary.txt` / `optimized-summary.txt`
  - `current-summary.sexp` / `optimized-summary.sexp`
  - per-cycle logs under `current/logs/` and `optimized/logs/`
  - TRAMP debug files under `current/debug/` and `optimized/debug/`

## Battery entrypoint
- `tramp-test/test-battery.sh`
- Cases:
  - `ci-direct`
  - `ci-bastion`
  - `smoke-all`
  - `real-smoke`

Example:

```bash
TRAMP_TEST_CASE=smoke-all ./tramp-test/test-battery.sh
```

## Useful env vars
- `TRAMP_TEST_OUTPUT_DIR`
- `TRAMP_TEST_CYCLES`
- `TRAMP_TEST_VERBOSE`
- `TRAMP_TEST_TIMEOUT_SECS`
- `TRAMP_TEST_ENFORCE_ABS_PERF`
- `TRAMP_TEST_SKIP_BUILD=1` (use prebuilt images)
