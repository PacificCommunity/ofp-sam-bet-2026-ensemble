# Saved native inputs

Download the [ensemble standalone ZIP](standalone.zip) for all 80 retained fits, or the [RR standalone ZIP](../rr-test/standalone.zip) for 30 completed pairs. Each includes MFCL, final.par, matching FRQ/INI/TAG and other inputs, the original doitall and model settings. No Kflow access is needed.

```sh
make verify
make rerun CASE=ensemble-005 OUT=/tmp/bet-005
make rerun CASE=rrtest-005-rr1 OUT=/tmp/bet-rr005
```

Use R, Make and Linux x86-64 for MFCL execution. `make prepare` (also `make restore`) copies one case into a new folder without running it. `make refit` uses its original doitall; `make plan-refit` shows the command. Add `ANCHOR=1` to select an RR1 case’s RR0 arm. Full refits take longer and are separate from saved-PAR evaluation.

The ZIP works independently: unzip it, then run the same Make commands inside its directory. `make unpack` exposes ordinary `models/<case>/` files; `make list` shows the available cases. The evaluator writes evaluated.par, plot-evaluated.par.rep, other native outputs and small CSV checks. It uses a ceiling of one evaluation and checks that the reported iteration and function counters remain zero. Original saved PARs and published results are preserved.

[Original ensemble PARs and whole REPs](../final-par/) · [RR results](../rr-test/results.md) · [exact refit settings](refit-configs.tar.gz) and [manifest](refit-configs.json). Failed RR1 attempts have no final PAR. Where the historical engine identity was not recorded, compatibility of the supplied executable is checked separately.

The earlier [native.tar.gz](https://raw.githubusercontent.com/PacificCommunity/ofp-sam-bet-2026-ensemble/main/reproduce/native.tar.gz) and [files.json](files.json) remain available at their original paths.

## Original Hessians

The [Hessian index](hessian-index.csv) links the original matrices for all 80
retained fits: 62 PDH and 18 Near-PDH. It maps source model IDs to the report's
E001–E080 labels. Each archive includes the matching final PAR, original Hessian
metadata and calculation log. Files are stored as optional release downloads
to keep clones small.

```sh
make hessian CASE=ensemble-001 OUT=/absolute/bet-hessian
```

This downloads and checks one saved archive; it does not run MFCL. Choose the
source model ID from the index and a new folder outside the repository.
[The manifest](hessians.json) pins every archive and file by SHA256.

The central REP comparison uses the scaled bound `1e-10 * max(1, abs(reference))`; the objective uses an absolute bound of `1e-6`.
