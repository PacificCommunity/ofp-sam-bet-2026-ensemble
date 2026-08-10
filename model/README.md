# Frozen model files

This directory is the complete frozen input recipe derived from BET 2026
Diagnostic Job 21641. Do not run it in place; the ensemble runner
copies these files into a fresh model directory before fitting.

`doitall.sh` starts with `bet.ini -makepar`, runs all eleven estimation phases,
and applies no seed, jitter or fitted checkpoint. It retains Job 21641 Diagnostic
selectivity and uses the direct negative-binomial parameterization
`tau = 1 + exp(fish_pars(4))`. Each ensemble fit fixes tau at its selected value
(`1.2`, `1.3` or `1.4`) with fish flags 43/44 fixed at zero. The ensemble
preparation writes each design row's steepness directly into both the
run-directory `bet.ini` and selected model configuration before MFCL starts;
`doitall.sh` rejects any mismatch and passes a byte-identical INI copy to
`-makepar`. It writes tau directly into the selected model configuration while
keeping Diagnostic selectivity fixed. The fitted output is `11.par`; the root
runner also saves it as `final.par`.

The runner audits the selected fixed tau, steepness, Lorenzen M, DM concentration and the
complete Diagnostic selectivity flags after every fitted phase. Any phase that changes
one of these required settings stops before the next phase is run.

The ensemble preparation validates `MANIFEST.sha256` before each fit; from the
repository root, `Rscript scripts/validate-all-model-inputs.R` preflights all
100 model configurations.

The frequency input explicitly declares that no weight-frequency data are
present (`WFIntervals`, `WFFirst`, and both `WFWidth` fields are zero). The
obsolete trailing WF missing-value field was removed from every fishery
record; all catch and length-frequency values are unchanged.

The source archive is Kflow Job 21641, fitted from diagnostic repository commit
`3abf0c64fb9b0c2d70b9c672dc7d9a655d3060d6`. The frozen data files are
byte-for-byte identical to the previous ensemble base; the fitting controls and
provenance use the Job 21641 Diagnostic model as the tau=2 reference before the
declared tau axis is applied.
