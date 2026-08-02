# BET 2026 ensemble

This repository defines a reproducible 100-model structural ensemble for the
BET 2026 Diagnostic model. Each row of
[`design/model-draws.csv`](design/model-draws.csv) is one model configuration.
The ensemble changes only the five uncertainty axes listed below; all other
inputs and the Diagnostic seed-23 fitting path remain unchanged.

| Axis | 100-model representation |
|---|---|
| Steepness | 100 stratified quantiles from the 2024 South Pacific albacore censored beta prior: mean 0.87, SD 0.063, bounded by 0.2 and 1.0 |
| Tag mixing periods (`K` cutoff) | Release-group mixing periods derived at Kolmogorov dissimilarity cutoffs 0.05–0.35, with 0.20 most frequent: 6, 12, 19, 26, 19, 12 and 6 models |
| Tag reporting | MFCL tag flag column 2: 50 inclusion (`0`) and 50 exclusion (`1`) models |
| Natural mortality | Quarterly Lorenzen `M0` at `L(40.5)`: bounded logit-normal on 0.050–0.165, with elicited mode 0.0702 and median 0.078136 |
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
Rscript scripts/validate-all-model-inputs.R
```

The design uses no random-number generator or seed. Continuous axes use fixed
quantiles, discrete margins use exact counts, and fixed modular permutations
pair the five margins. The assignments therefore do not depend on R's
random-number implementation; only negligible numerical-library rounding in
distribution quantiles can vary across platforms. `design/model-draws.csv` is
the committed source of truth. `design/rank-correlation.csv` reports the actual
pairwise rank correlations; no composite balance score is used.

## Run a model

Every run starts from the frozen Diagnostic inputs and the documented seed-23
phase checkpoints. The preparation step changes only the selected five axes,
then compares the resulting INI, three checkpoint PARs, FRQ and `doitall.sh`
against both the design row and authoritative source files before MFCL starts.

```sh
./run.sh ensemble-001
```

The output folder contains `ensemble-metadata.csv` and
`input-change-audit.csv` with full-precision values. Display labels are concise
but self-describing; for example:
`E001 | h=0.669 | K=0.20 | RR=include | M0=0.0663/qtr | creep=1.0/0.50%`.
The exact values remain in the metadata rather than the rounded label. Kflow
uses the same command with one independent Suva job per design row and a phase
10/11 convergence criterion of `1e-4`.

## Distribution figure

![BET 2026 ensemble marginal distributions](design/distributions.png)

**Figure.** Marginal distributions of the 100-model BET 2026 structural
ensemble. Continuous curves show the distributions represented by deterministic
quantiles; rugs show the retained values. Natural mortality is defined at the
reference length `L(40.5 quarters)`. The selected ensemble distribution is
shown against the Hamel–Cope adult-mortality prior after model-specific scaling
to `M0`, the tag-based estimate and its 90% confidence interval, and the 2023
assessment value. The model reference age (10.125 years) is not an assumed
maximum age: age class 40 is a plus group, while the longevity calculation uses
`Amax = 15 years`. The selected `M0` curve approaches zero density smoothly at
both design limits; the limits are ensemble controls, not observed biological
bounds. Bar labels give the exact number of models at each discrete level. A
vector version is available as
[`design/distributions.pdf`](design/distributions.pdf).

## Outputs

- `design/model-draws.csv` — machine-readable source of truth
- `design/distribution-parameters.csv` — exact distribution parameters
- `design/continuous-summary.csv` and `design/discrete-summary.csv` — marginal summaries
- `design/m-evidence.csv` — natural-mortality evidence and interval definitions
- `design/hamel-cope-amax-sensitivity.csv` — `Amax` 13, 15 and 16-year implications on the adult-`M` and MFCL-`M0` scales
- `design/effort-creep-sources.csv` — official source files and SHA-256 hashes
- `design/mixing-sources.csv` — official `SC22-IP10-regionMean` mixing-scenario files, source commit and SHA-256 hashes
- `design/input-validation-summary.csv` — exact preflight result for all 100 frozen inputs
- `design/rank-correlation.csv` — pairwise cross-axis association audit
- `design/distributions.png` and `design/distributions.pdf` — publication-ready figure

These are structural ensemble draws, not optimizer jitters.
