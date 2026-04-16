# Test Batteries

This repository uses test batteries under `tests/`.

## Layout
- `tests/tramp/`: TRAMP performance and regression battery.
- `tests/run-battery.sh`: top-level battery entrypoint.
- `tests/tangle-config.sh`: tangles `dotemacs.org` and fails if generated config files drift.

## Top-level suites
- `core-static`: literate tangle consistency check, shell syntax checks for tracked `*.sh`, plus Emacs Lisp parse checks for tracked `*.el`.
- `tramp-ci-direct`: run TRAMP direct scenario battery.
- `tramp-ci-bastion`: run TRAMP bastion scenario battery.
- `tests/run-emacs30-container.sh`: build a Debian sid container with Emacs 30.2 and run batteries inside it.

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

Containerized Emacs 30 run (includes TRAMP platform smoke by default):

```bash
TEST_SUITE=core-static ./tests/run-emacs30-container.sh
```

Optional env vars:
- `EMACS30_IMAGE_TAG` (default: `emacs30-sid:local`)
- `RUN_TRAMP_SMOKE` (`1` by default, set `0` to skip `tests/tramp/smoke-platform.el`)
- `EMACS30_SKIP_BUILD` (`0` by default, set `1` to use a prebuilt image tag)
