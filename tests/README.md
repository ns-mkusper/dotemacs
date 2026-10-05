# Test Batteries

This repository uses test batteries under `tests/`. The config targets the
latest Emacs release (31.x); the container run below is the reference environment.

## Layout
- `tests/tramp/`: TRAMP performance and regression battery.
- `tests/run-battery.sh`: top-level battery entrypoint.
- `tests/tangle-config.sh`: tangles `dotemacs.org` into a target directory (repo root by default).
- `tests/emacs-compat-check.el`: checks the tangled config against the running Emacs (see below).
- `tests/run-emacs-latest-container.sh`: runs the batteries inside the prebuilt `silex/emacs:31-ci` image.

## Top-level suites
- `core-static`: literate tangle step, shell syntax checks for tracked `*.sh`, plus Emacs Lisp parse checks over generated and tracked `*.el`. Runs on the Linux/macOS/Windows matrix with whatever Emacs the OS ships, so it only exercises tangling and syntax.
- `emacs-compat`: tangle, then run `tests/emacs-compat-check.el` under the current Emacs. Meant to run under Emacs 31 (the container job); older Emacsen will report options that have since been renamed.
- `tramp-ci-direct`: run TRAMP direct scenario battery.
- `tramp-ci-bastion`: run TRAMP bastion scenario battery.

## Emacs compatibility check
`tests/emacs-compat-check.el` reads every tangled config file and fails when the *running* Emacs
marks a referenced function or variable obsolete, or when a `use-package` block declared
`:straight (:type built-in)` names a library that Emacs does not ship. `:if`/`:when`/`:unless`
guards on `use-package` blocks are honoured. It loads no third-party packages, so it runs
offline in a bare Emacs.

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
TEST_SUITE=emacs-compat ./tests/run-battery.sh
```

Tangle into a specific Emacs directory:

```bash
./tests/tangle-config.sh ~/.emacs.d
```

Containerized Emacs 31 run (core-static, emacs-compat and the TRAMP platform smoke):

```bash
./tests/run-emacs-latest-container.sh
```

Optional env vars:
- `EMACS_LATEST_IMAGE` (default: `silex/emacs:31-ci`; use `silex/emacs:master-ci` for the nightly snapshot)
- `TEST_SUITES` (default: `core-static emacs-compat`)
- `RUN_TRAMP_SMOKE` (`1` by default, set `0` to skip `tests/tramp/smoke-platform.el`)
