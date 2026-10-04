# Saved model files

The [run index](runs.csv) connects original job numbers to saved archives and
source commits. [Native files](native-files.csv) lists exact member paths and
hashes. A published result is associated with a run only when its source or
stored bytes support that association.

| Result | Native files and source evidence |
|---|---|
| Diagnostic | Job 21641 final PAR, REP, inputs and twelve-phase `doitall`; merged Hessian from Job 22196 |
| Stepwise | 23 published last PARs; 22 retained as original raw bytes inside RDS, plus Diagnostic |
| Sensitivities | 17 final PAR/REP pairs match published hashes |
| Retrospective | Seven peels; native PAR/REP and other retained files recovered from RDS raw bytes |
| Jitter | 30 runs; completed final PARs and failed checkpoints retain their original status |
| Ensemble | All 80 retained final PARs match published hashes; native Hessians and source-parent PARs preserved |
| Fishery impacts | Eight native REPs match published hashes; original local inputs, scripts and logs retained |
| Strict-tag selftest | Exact merged result and 50 pseudo-data tables; per-case native fit files remain unavailable |

Recovered files use stored raw byte vectors, with the original RDS selector,
compression and hashes recorded. They are not rebuilt from numeric model values.
Failed checkpoints are not labelled successful final fits.
The [RDS recovery example](recovery.md) checks a stored native file independently.

The [ensemble dependency proof](evidence/retained-ensemble-native-dependency-PAR-proof.json)
identifies each retained fit's PAR and native Hessian in its original dependency
snapshot. Those parent input PARs are not assigned as the Hessian job's own fit.
[Local fishery runs](local-runs.json) have separate provenance; their Kflow IDs
remain unknown.

See [gaps](gaps.json) for missing files, truncated originals, source identities
and external runtime dependencies. [Refit instructions](refit.md) use saved inputs
and actual execution controls. The published report, figures and live HTML have
not been regenerated.
