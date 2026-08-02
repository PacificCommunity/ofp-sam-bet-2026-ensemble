# Scientific basis

## Design principle

The ensemble represents uncertainty that is not captured by estimation
uncertainty within one fitted model. Marginal distributions are specified
first, then independently permuted to reduce accidental association among the
five axes. The 100 retained configurations therefore preserve the requested
marginal distributions without imposing biological correlations that have not
been justified.

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

The natural-mortality draw is quarterly M at age 40. The evidence supplied for
the ensemble was:

- tag-based analysis: mean 0.0624 and 90% interval 0.0500–0.0745;
- previous assessment: 0.078;
- Hamel–Cope longevity prior for a 15-year maximum age: median 0.090,
  log-scale SD 0.31 and approximate 95% interval 0.049–0.165.

These are synthesised as a truncated lognormal distribution on 0.050–0.165.
Its mode is 0.0702, the midpoint of 0.0624 and 0.078. Its conditional median is
0.078136, matching `exp(-2.54930339768360)` in the Diagnostic Lorenzen
intercept. The calibrated log-scale SD is 0.287645, close to the Hamel–Cope
value of 0.31.

MFCL uses quarterly time periods in this model. Each M draw is therefore
entered as `log(M_quarterly)` in the Lorenzen intercept; it is not divided or
multiplied by four.

Sources:

- WCPFC-SC22-2026-SA-IP14, available from the [SC22 meeting page](https://meetings.wcpfc.int/meetings/sc22?order=changed&sort=asc)
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
