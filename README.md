# BET 2026 Diagnostic ensemble

This repository preserves the BET 2026 assessment ensemble and its historical paired reporting-rate follow-up: frozen inputs, 80 retained PAR/REP files, 30 completed RR1 counterparts, four failure records, and cached uncertainty and projection results.

<a name="published-report-links"></a>

Read the [SC22 assessment](https://meetings.wcpfc.int/node/33590) and [report repository](https://github.com/PacificCommunity/ofp-sam-bet-2026-report), then open the [ensemble report](https://pacificcommunity.github.io/ofp-sam-bet-2026-ensemble/bet-2026-ensemble-report.html) or [viewer](https://pacificcommunity.github.io/ofp-sam-bet-2026-ensemble/bet-2026-ensemble-interactive-viewer.html). Saved [RR0](results/bet-2026-ensemble-report-rr0-inclusion.html), [RR1](results/bet-2026-ensemble-report-rr1-exclusion.html) and [subset comparison](results/bet-2026-ensemble-report-rr-comparison.html) reports remain available.

<a name="bet-2026-exact-paired-rr-comparison"></a>
<a name="read-the-results"></a>

The [paired results and original figures](rr-test/results.md) support the [SC22 reporting-rate follow-up](https://meetings.wcpfc.int/node/32932). These historical v2.5 fits use the pre-fix executable; their scope and limitations are in the [review and rerun guide](rr-test/README.md).

<a name="verify-or-rerun"></a>

```sh
python3 rr-test/verify-results.py
./rr-test/rerun rrtest-005-rr1 --dry-run
```

The guide gives both-arm Docker commands and summary rebuilding instructions. The [run archive](preservation/README.md) indexes original Kflow outputs, source snapshots and coverage gaps.

- <a name="recreate-and-validate"></a>[Design validation](docs/reproducibility.md#recreate-and-validate) · <a name="run-a-model"></a>[Model fitting](docs/reproducibility.md#run-a-model).
- <a name="retained-final-par-and-viewer-rep-files"></a>[Retained fits](docs/retained-final-pars.md) · <a name="outputs"></a>[Design](design/model-draws.csv) and [outputs](docs/reproducibility.md#outputs).
- <a name="distribution-figure"></a>Distribution [PNG](design/distributions.png) / [PDF](design/distributions.pdf) · [scientific basis](docs/scientific-basis.md).
- <a name="reusable-hessian-uncertainty"></a>[Hessian caches](docs/reproducibility.md#reusable-hessian-uncertainty) · <a name="stochastic-projections-and-reusable-caches"></a>[Projections](docs/reproducibility.md#stochastic-projections-and-reusable-caches).
- <a name="reproducible-report"></a>[Report rebuilding](docs/reproducibility.md#reproducible-report) · <a name="reporting-rate-retained-subset-sensitivity"></a>[Subset interpretation](docs/reproducibility.md#reporting-rate-retained-subset-sensitivity).
