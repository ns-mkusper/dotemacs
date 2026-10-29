# Test Batteries

This repository uses test batteries under `tests/`. The config targets the
latest Emacs release (31.x); the container run below is the reference environment.

## Layout
- `tests/tramp/`: TRAMP performance and regression battery.
- `tests/run-battery.sh`: top-level battery entrypoint.
- `tests/tangle-config.sh`: tangles `dotemacs.org` into a target directory (repo root by default).
- `tests/emacs-compat-check.el`: checks the tangled config against the running Emacs (see below).
- `tests/run-emacs-latest-container.sh`: runs the batteries inside the prebuilt `silex/emacs:31-ci` image.
- `tests/ghostel-interrupt-test.el`: opens a ghostel terminal with the config's shell settings and checks that C-c interrupts a running program without killing the shell (see below).

## Top-level suites
- `core-static`: literate tangle step, shell syntax checks for tracked `*.sh`, plus Emacs Lisp parse checks over generated and tracked `*.el`. Runs on the Linux/macOS/Windows matrix with whatever Emacs the OS ships, so it only exercises tangling and syntax.
- `emacs-compat`: tangle, then run `tests/emacs-compat-check.el` under the current Emacs. Meant to run under Emacs 31 (the container job); older Emacsen will report options that have since been renamed.
- `perf`: install the full config (packages and all) and check it against timing budgets and window layout; see below.
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

## Performance regressions
`tests/perf/run-perf.sh` tangles the config into a fresh Emacs directory, boots it once to fetch and
build packages, then runs two checks against the loaded config:

- `tests/perf/perf-check.el` (batch): startup time, org-roam staying unloaded at startup, opening empty
  and 1 MB JSON files, and opening Python files in a project that holds a 15,000-file venv. Each has a
  budget a few times above current timings, so noise passes and 10x regressions fail.
- `tests/perf/window-check.el` (`emacs -nw` in a 60x200 terminal): compilation output opens at about 30%
  of the frame without taking focus, shrinks when short, and stays capped when long.

Fixtures are generated on the fly. `tests/perf/no-prompts.el` declines every prompt (vterm's module
build, grammar installs) so unattended runs never block. `PERF_SCALE` multiplies all budgets (CI uses 2),
and `PERF_AFTER_INIT` loads a file after the config, which is how to confirm a check fails when a
regression is reintroduced. The `perf-ci` workflow runs this in the Emacs 31 container on every PR that
touches `dotemacs.org`, caching straight's packages between runs (a cold run takes about 8 minutes, a
warm one under a minute).

```bash
docker run --rm -v "$PWD:/repo:ro" silex/emacs:31-ci bash /repo/tests/perf/run-perf.sh
```

## Ghostel C-c check
`.github/workflows/ghostel-ci.yml` runs `tests/ghostel-interrupt-test.el` on Ubuntu and Windows.
On Windows it uses the runner's MSYS2 install with zsh, the same `ghostel-shell` the config sets,
and interrupts both an MSYS2 program (`sleep`) and a native console program (`ping.exe`).
It needs a ghostel checkout; the native module is downloaded on first run.

```bash
GHOSTEL_DIR=~/.emacs.d/straight/repos/ghostel TANGLE_DIR=/tmp/tangled \
  emacs -Q --batch -l tests/ghostel-interrupt-test.el
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
