# BET 2026 ensemble

This repository defines a reproducible 100-model structural ensemble for the
BET 2026 Diagnostic model. Each row of
[`design/model-draws.csv`](design/model-draws.csv) is one model configuration.
The ensemble changes only the five uncertainty axes listed below; all other
inputs and the Diagnostic seed-23 fitting path remain unchanged.

| Axis | 100-model representation |
|---|---|
| Steepness | 100 stratified quantiles from the 2024 South Pacific albacore censored beta prior: mean 0.87, SD 0.063, bounded by 0.2 and 1.0 |
| Tag mixing period | 0.05–0.35, with 0.20 most frequent: 6, 12, 19, 26, 19, 12 and 6 models |
| Tag reporting | MFCL tag flag column 2: 50 inclusion (`0`) and 50 exclusion (`1`) models |
| Natural mortality | Quarterly M at age 40 from a truncated lognormal on 0.050–0.165, mode 0.0702 and median 0.078136 |
| Effort creep | Five official BET/YFT scenarios with 20 models each |

The albacore steepness distribution is used as a transparent working prior
because a current BET-specific probability distribution is unavailable. It is
not presented as evidence that albacore and bigeye have identical stock–recruit
dynamics. The full rationale and sources are in
[`docs/scientific-basis.md`](docs/scientific-basis.md).

## Recreate and validate

Only base R is required.

```sh
Rscript scripts/create-ensemble-design.R
Rscript scripts/validate-ensemble-design.R
```

The design is deterministic (`design_seed = 20260802`). Continuous axes use
fixed quantiles and discrete margins use exact counts. The script searches
20,000 independent permutations and retains the design with the lowest maximum
association among axes.

## Outputs

- `design/model-draws.csv` — machine-readable source of truth
- `design/distribution-parameters.csv` — exact distribution parameters
- `design/continuous-summary.csv` and `design/discrete-summary.csv` — marginal summaries
- `design/effort-creep-sources.csv` — official source files and SHA-256 hashes
- `design/rank-correlation.csv` — cross-axis balance audit
- `design/distributions.png` — visual summary

These are structural ensemble draws, not optimizer jitters.
