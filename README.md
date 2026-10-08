[![Preservation checks](https://github.com/PacificCommunity/ofp-sam-bet-2026-ensemble/actions/workflows/verify-preserved-results.yml/badge.svg?branch=main)](https://github.com/PacificCommunity/ofp-sam-bet-2026-ensemble/actions/workflows/verify-preserved-results.yml?query=branch%3Amain) [![Design checks](https://github.com/PacificCommunity/ofp-sam-bet-2026-ensemble/actions/workflows/validate.yml/badge.svg?branch=main)](https://github.com/PacificCommunity/ofp-sam-bet-2026-ensemble/actions/workflows/validate.yml?query=branch%3Amain) [![RR standalone](https://github.com/PacificCommunity/ofp-sam-bet-2026-ensemble/actions/workflows/check-rr-standalone.yml/badge.svg?branch=main)](https://github.com/PacificCommunity/ofp-sam-bet-2026-ensemble/actions/workflows/check-rr-standalone.yml?query=branch%3Amain)
 [![Saved ensemble](https://github.com/PacificCommunity/ofp-sam-bet-2026-ensemble/actions/workflows/check-ensemble-standalone.yml/badge.svg?branch=main)](https://github.com/PacificCommunity/ofp-sam-bet-2026-ensemble/actions/workflows/check-ensemble-standalone.yml?query=branch%3Amain)

# BET 2026 Diagnostic ensemble

Frozen inputs, 80 retained fits and cached uncertainty/projection results for the [SC22 assessment](https://meetings.wcpfc.int/node/33590).

Read the [ensemble report](https://pacificcommunity.github.io/ofp-sam-bet-2026-ensemble/bet-2026-ensemble-report.html), [interactive viewer](https://pacificcommunity.github.io/ofp-sam-bet-2026-ensemble/bet-2026-ensemble-interactive-viewer.html) or [assessment report repository](https://github.com/PacificCommunity/ofp-sam-bet-2026-report).

The [paired reporting-rate comparison](rr-test/results.md) includes 30 completed pairs and four failed attempts. See the [reference document](https://meetings.wcpfc.int/node/32932) and [rerun guide](rr-test/README.md).

```sh
make verify
make prepare CASE=ensemble-005 OUT=/tmp/bet-inputs
make rerun CASE=ensemble-005 OUT=/tmp/bet-005
make rerun CASE=rrtest-005-rr1 OUT=/tmp/bet-rr005
make refit CASE=ensemble-005 OUT=/tmp/bet-refit
```

R and Make are required; MFCL runs on Linux x86-64. The [ensemble ZIP](reproduce/standalone.zip) and [RR ZIP](rr-test/standalone.zip) include final PARs, matching inputs, MFCL and original doitall scripts. See the [rerun guide](reproduce/README.md).

`prepare` copies inputs without executing MFCL; `rerun` evaluates the saved
final PAR and checks the objective and selected REP values. `refit` runs the
original complete fitting recipe. Choose a fresh OUT for each command.

<a name="recreate-and-validate"></a><a name="run-a-model"></a>
[Design and model commands](docs/reproducibility.md#recreate-and-validate) · [scientific basis](docs/scientific-basis.md).

<a name="retained-final-par-and-viewer-rep-files"></a><a name="outputs"></a><a name="distribution-figure"></a>
[Retained PAR/REP files](final-par/) · [design](design/model-draws.csv) · distribution [PNG](design/distributions.png) / [PDF](design/distributions.pdf).

<a name="reusable-hessian-uncertainty"></a><a name="stochastic-projections-and-reusable-caches"></a><a name="reproducible-report"></a>
[Original Hessians](reproduce/README.md#original-hessians) · [Uncertainty and projection caches](docs/reproducibility.md#reusable-hessian-uncertainty) · [report rebuilding](docs/reproducibility.md#reproducible-report).
