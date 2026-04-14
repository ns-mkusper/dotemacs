# Test Batteries

This repository uses test batteries under `tests/`.

## Layout
- `tests/tramp/`: TRAMP performance and regression battery.
- `tests/run-battery.sh`: top-level battery entrypoint.

## Top-level suites
- `core-static`: shell syntax + Emacs Lisp parse checks.
- `tramp-ci-direct`: run TRAMP direct scenario battery.
- `tramp-ci-bastion`: run TRAMP bastion scenario battery.

## Real host integration (manual CI)
- Workflow: `.github/workflows/tramp-real-integration.yml`
- Runs a matrix across remote target OS labels:
  - `linux`
  - `macos`
  - `windows`
- Requires workflow inputs:
  - `target_linux`
  - `target_macos`
  - `target_windows`
- Uses repository secrets:
  - `TRAMP_TEST_REAL_SSH_PRIVATE_KEY`
  - `TRAMP_TEST_REAL_SSH_CONFIG` (optional)

Example:

```bash
TEST_SUITE=core-static ./tests/run-battery.sh
```
