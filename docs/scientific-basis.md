# Scientific basis

## Design principle

The ensemble represents uncertainty that is not captured by estimation
uncertainty within one fitted model. Marginal distributions are specified
first, then coupled by fixed modular permutations modulo the prime number 101.
The multipliers are recorded in `design/distribution-parameters.csv`. This
requires no random-number generator or seed and gives identical pairings
across R versions, apart from possible negligible numerical-library rounding
when evaluating distribution quantiles. The committed `model-draws.csv` is the
source of truth. The 100 configurations preserve the requested marginal
distributions without asserting biological correlations among the five axes.
Pairwise rank correlations are reported directly; no composite balance score
is defined.

## Steepness

Steepness is sampled from the censored beta distribution used in the 2024
South Pacific albacore assessment:

\[
h = 0.2 + 0.8Y, \qquad
Y \sim \mathrm{Beta}(17.541500, 3.403575).
\]

This gives mean 0.87 and SD 0.063 on the 0.2–1.0 steepness scale. The finite
ensemble uses the midpoints of 100 equal-probability intervals rather than
unrestricted random draws, so the distribution is represented reproducibly
across the full probability range.

This is a working distribution borrowed from the latest South Pacific
albacore ensemble because no current BET-specific probability distribution is
available. It should not be interpreted as evidence that albacore and bigeye
have the same steepness. Existing WCPO BET assessments instead represent this
uncertainty using fixed grid levels, most recently 0.65, 0.80 and 0.95.

Sources:

- [2024 South Pacific albacore stock assessment](https://meetings.wcpfc.int/system/files/2024-08/SC20-SA-WP-02%20Sth_Pacific_albacore_assessment2024_rev3.pdf)
- [2023 WCPO bigeye stock assessment](https://meetings.wcpfc.int/system/files/2023-09/SC19-SA-WP-05_BET_2023_Rev2%20%28Posted%20on%2015Sep2023%29.pdf)
- [Review of recruitment assumptions in tuna RFMO assessments](https://doi.org/10.1016/j.fishres.2018.11.031)

## Tag mixing period

The seven allowed values are 0.05, 0.10, 0.15, 0.20, 0.25, 0.30 and 0.35.
The exact counts are 6, 12, 19, 26, 19, 12 and 6. This symmetric discrete
distribution makes 0.20 the mode while retaining both requested tails.

The draw is written directly to the first MFCL tag-flag column used for the
mixing-period setting.

## Tag reporting

Tag reporting is a two-level uncertainty axis with equal probability:

- `0`: include reporting-rate information during the pre-mixing period;
- `1`: exclude it, matching the current Diagnostic configuration.

The value is written to column 2 of the relevant MFCL tag flag. The design
contains exactly 50 models of each type.

## Natural mortality

The sampled parameter is quarterly `M0` at the Lorenzen reference length
`Lref = L(40.5 quarters)`. It controls the level of the mortality-at-age
schedule; it does not change the fixed Lorenzen exponent.

### Tag-based estimate

Ducharme-Barth et al. (2026) fitted a joint reverse-time cohort analysis to
Coral Sea archival and conventional tags and Region 4 conventional tags. The
selected random-effects model estimated `M0 = 0.0624` per quarter, SE 0.0076,
with a delta-method 90% confidence interval of 0.0500–0.0749. `M0` is mortality
at `Lref = L(40.5)`, exactly the reference scale used here. The baseline
Lorenzen exponent was fixed at -1.

This is informative but not treated as an exact prior. The authors identify
important confounding between M and reporting rates: lowering estimated PTTP
and RTTP reporting rates substantially lowered M, and unmodelled emigration,
tag loss or reporting heterogeneity can act like mortality. The paper therefore
interprets the result as supporting somewhat lower M while broadly
corroborating the previous order of magnitude.

### Hamel–Cope longevity prior

Hamel and Cope (2022) define a lognormal prior with

\[
\mathrm{median}(M_{annual}) = 5.40/A_{max}, \qquad
\mathrm{SD}[\log(M)] = 0.31.
\]

For `Amax = 15 years`, the annual median is 0.36. Instantaneous rates scale
linearly with the time unit, so the equivalent quarterly prior is

\[
M_{quarter} \sim \mathrm{Lognormal}(\log(0.09), 0.31).
\]

Its quarterly mean is 0.09443 and its 2.5th and 97.5th percentiles are 0.04902
and 0.16524. These are probability limits, not hard biological bounds. The
coefficient 5.40/Amax is explicitly the **median**, not the arithmetic mean, so
that half the prior probability lies on each side of the point estimate.
Hamel and Cope also caution that Amax should be supported by adequate,
representative and validated age sampling.

### Selected ensemble distribution

The evidence is synthesised as an explicitly elicited truncated lognormal
distribution on rounded limits 0.050–0.165. Its mode is 0.0702, the midpoint of
the tag estimate 0.0624 and the previous-assessment value 0.078. Its conditional
median is 0.078136, matching `exp(-2.54930339768360)` in the Diagnostic
Lorenzen intercept. The fitted log-SD is 0.287645, close to but not identical
to the Hamel–Cope value 0.31.

This selected distribution is therefore **not** described as the Hamel–Cope
prior itself. It combines the tag result, the previous assessment and the
longevity prior while retaining the requested mode and finite ensemble range.
The original untruncated Hamel–Cope curve is displayed separately in the
distribution figure.

MFCL uses quarterly time periods in this model. Each draw is entered as
`log(M0_quarterly)` in the Lorenzen intercept; no additional factor of four is
applied at model input.

Sources:

- [Ducharme-Barth et al. (2026), WCPFC-SC22-2026-SA-IP14](https://meetings.wcpfc.int/node/32286)
- [Hamel and Cope (2022), longevity-based prior for natural mortality](https://doi.org/10.1016/j.fishres.2022.106477)

## Effort creep

The five official BET/YFT regional-CPUE scenarios are sampled with equal
weight. The primary and secondary rates and exact counts are:

| Primary | Secondary | Models |
|---:|---:|---:|
| 0.5% | 0.25% | 20 |
| 1.0% | 0.50% | 20 |
| 1.5% | 0.75% | 20 |
| 2.0% | 1.00% | 20 |
| 2.5% | 1.25% | 20 |

The exact upstream filenames and SHA-256 hashes are recorded in
`design/effort-creep-sources.csv`. When model directories are materialised,
only the regional CPUE field for fisheries 29–33 should be transferred from
the selected source file into the cleaned Diagnostic FRQ; other FRQ fields
remain unchanged.

Source: [BET/YFT FRQ-build repository](https://github.com/PacificCommunity/ofp-sam-2026-BET-YFT-frq-build/tree/main/BET),
commit `26a4d6e3b41066c6d3b32dd4a38d381f616a0cff`.
