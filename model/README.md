# Frozen model files

This directory is the complete input recipe for the BET 2026 **Diagnostic
model**. Do not run it in place. Use `../doitall-seed23.sh`, which copies these
files to a fresh `run/` directory and reproduces the diagnostic-model
initialization.

`doitall.sh` starts with `bet.ini -makepar`, runs all eleven estimation phases,
and applies the archived seed-23 checkpoints at Phases 1, 2 and 5 only after
their input hashes match the reference fit. The fitted output is `11.par`; the
root runner also saves it as `final.par`.

Run `../verify` before fitting to validate `MANIFEST.sha256`.

The frequency input explicitly declares that no weight-frequency data are
present (`WFIntervals`, `WFFirst`, and both `WFWidth` fields are zero). The
obsolete trailing WF missing-value field was removed from every fishery
record; all catch and length-frequency values are unchanged.

The repository's official `doitall-seed23.sh` runner applies the archived
seed-23 initialization. The standalone Release also supplies a separate
`doitall.sh` entry point for an ordinary `bet.ini -makepar` initialization
without the seed-23 checkpoints.
