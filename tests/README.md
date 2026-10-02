# Test Batteries

This repository uses test batteries under `tests/`.

## Layout
- `tests/tramp/`: TRAMP performance and regression battery.
- `tests/run-battery.sh`: top-level battery entrypoint.
- `tests/tangle-config.sh`: tangles `dotemacs.org` into a target directory (repo root by default).

## Top-level suites
- `core-static`: literate tangle step, shell syntax checks for tracked `*.sh`, Emacs Lisp parse checks over generated and tracked `*.el`, and the Emacs compatibility check below.
- `tramp-ci-direct`: run TRAMP direct scenario battery.
- `tramp-ci-bastion`: run TRAMP bastion scenario battery.
- `tests/run-emacs30-container.sh`: build a Debian sid container with Emacs 30.2 and run batteries inside it.
- `tests/run-emacs-latest-container.sh`: run batteries inside the prebuilt `silex/emacs:31-ci` image (latest Emacs release).

## Emacs compatibility check
`tests/emacs-compat-check.el` reads every tangled config file and fails when the *running* Emacs
marks a referenced function or variable obsolete, or when a `use-package` block declared
`:straight (:type built-in)` names a library that Emacs does not ship. It loads no third-party
packages, so it runs offline in a bare Emacs. Running it under several Emacs versions (the OS
matrix, the Emacs 30 container, the latest-release container) is what keeps the config portable.

```bash
emacs --batch -Q -l tests/emacs-compat-check.el -- early-init.el init.el inits/*.el
```

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

Tangle into a specific Emacs directory:

```bash
./tests/tangle-config.sh ~/.emacs.d
```

Containerized Emacs 30 run (includes TRAMP platform smoke by default):

```bash
TEST_SUITE=core-static ./tests/run-emacs30-container.sh
```

Containerized latest-release run (Emacs 31.x; set `EMACS_LATEST_IMAGE=silex/emacs:master-ci` for the nightly snapshot):

```bash
TEST_SUITE=core-static ./tests/run-emacs-latest-container.sh
```

Optional env vars:
- `EMACS30_IMAGE_TAG` (default: `emacs30-sid:local`)
- `RUN_TRAMP_SMOKE` (`1` by default, set `0` to skip `tests/tramp/smoke-platform.el`)
- `EMACS30_SKIP_BUILD` (`0` by default, set `1` to use a prebuilt image tag)
