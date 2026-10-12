#!/usr/bin/env bash
# verify.sh — Definition-of-Done gate for emburdenlca.
#
# Called by the Ralph Loop driver after each task iteration to decide
# whether to mark the task done. Exits 0 iff the package installs and
# every testthat test passes. --full adds R CMD check (slow); default
# (`--quick`) is fast enough for the Loop's per-iteration cycle.
#
# This file lives at the worktree root because the Loop's driver
# refuses to mark a task complete without it (`verify.sh not found at
# <worktree>/verify.sh; refusing to mark work done`). Keep it at the
# repo root; downstream packages that depend on emburdenlca can
# source it to reproduce the same gate.
#
# Usage:
#   ./verify.sh            # quick: install + testthat (default)
#   ./verify.sh --quick    # same as default
#   ./verify.sh --full     # quick + R CMD check (10-30 min)
set -euo pipefail

cd "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

MODE="${1:---quick}"
FAIL=0
step() { printf "\n== %s ==\n" "$1"; }

step "install (devtools::install, quiet)"
Rscript -e 'devtools::install(upgrade = "never", quiet = TRUE)' \
    || { echo "install failed"; FAIL=1; }

if [[ $FAIL -eq 0 ]]; then
    step "testthat suite (via devtools::test — handles cwd)"
    # devtools::test() resolves the test directory from the package root
    # without requiring us to cd into tests/. test_check() does the opposite:
    # it expects cwd = tests/ and fails silently from the worktree root.
    Rscript -e 'devtools::test(reporter = "summary", stop_on_failure = TRUE)' \
        || { echo "testthat failures"; FAIL=1; }
fi

if [[ "$MODE" == "--full" && $FAIL -eq 0 ]]; then
    step "R CMD check (slow — gate on errors only, not warnings)"
    # devtools::check() defaults error_on="warning" which fails on the
    # 4 pre-existing WARNINGs + 6 NOTEs on main (confirmed by the Loop's
    # agent on 2026-10-12). Those warnings predate the LCA-DEEPENING
    # work and are a separate hardening ticket. For the per-task DoD
    # gate we only fail on actual R CMD check ERRORs.
    Rscript -e 'devtools::check(args = c("--no-manual"), error_on = "error", quiet = TRUE)' \
        || { echo "R CMD check errors"; FAIL=1; }
fi

echo
if [[ $FAIL -eq 0 ]]; then
    echo "verify: OK ($MODE)"
    exit 0
else
    echo "verify: FAIL ($MODE)"
    exit 1
fi
