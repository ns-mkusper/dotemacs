# Test Batteries

This repository uses test batteries under `tests/`.

## Layout
- `tests/tramp/`: TRAMP performance and regression battery.
- `tests/run-battery.sh`: top-level battery entrypoint.

## Top-level suites
- `core-static`: shell syntax + Emacs Lisp parse checks.
- `tramp-ci-direct`: run TRAMP direct scenario battery.
- `tramp-ci-bastion`: run TRAMP bastion scenario battery.

Example:

```bash
TEST_SUITE=core-static ./tests/run-battery.sh
```
