# Frozen model files

This directory is the complete frozen input recipe derived from BET 2026
Diagnostic Job 21641. Do not run it in place; the ensemble runner
copies these files into a fresh model directory before fitting.

`doitall.sh` starts with `bet.ini -makepar`, runs all eleven estimation phases,
and applies no seed, jitter or fitted checkpoint. It retains Job 21641 Diagnostic
selectivity and fixes the direct negative-binomial tag parameter at `tau=2`
(`parest 305=1`, all `fish_pars(4)=0`, fish flags 43/44 fixed at zero). The
ensemble preparation changes steepness in both the INI and the selected model
configuration while keeping Diagnostic selectivity fixed. The fitted output is
`11.par`; the root runner also saves it as `final.par`.

The runner audits fixed tau, steepness, Lorenzen M, DM concentration and the
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
provenance are updated to the Job 21641 Diagnostic model with tau=2 fixed.
