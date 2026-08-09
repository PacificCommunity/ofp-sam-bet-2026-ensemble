# Scientific basis

## Design principle

The ensemble represents uncertainty that is not captured by estimation
uncertainty within one fitted model. Marginal distributions are specified
first, then coupled by six separately generated random permutations. This randomization
is performed once without a fixed RNG seed and accepted only when every
pairwise absolute Spearman correlation is at most 0.10. The resulting rank
assignments are frozen in `design/pairing-map.csv`; normal recreation and model
runs read that file and never redraw it. This removes a deterministic modular
lattice while retaining exact reproducibility of the committed ensemble. The
100 configurations preserve the requested marginal distributions without
asserting biological correlations among the six axes. Pairwise rank
correlations are reported directly; no composite balance score is defined.

## Tag overdispersion

The direct negative-binomial tag parameter is included as a three-level
structural axis. Tau is fixed, not estimated, at `1.2`, `1.3` or `1.4` in
33, 34 and 33 models, respectively. MFCL uses

\[
\tau = 1 + \exp\{\mathrm{fish\_pars}(4)\},
\]

so the corresponding fixed direct parameters are `log(0.2)`, `log(0.3)` and
`log(0.4)`. The tag likelihood remains negative binomial (`parest 111=4`),
the direct parameterization remains active (`parest 305=1`), and fish flags
43/44 remain zero in every fit. Runtime audits verify the selected value after
makepar and after every estimation phase.

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

## Tag mixing periods and K cutoff

The seven ensemble values 0.05, 0.10, 0.15, 0.20, 0.25, 0.30 and 0.35 are
Kolmogorov dissimilarity (`K`) cutoffs used to determine tag-mixing periods;
they are not mixing periods themselves. For each cutoff, SC22-IP10 selected
the shortest simulated period at which `K` fell below the cutoff, assigning
four quarters when the cutoff was not reached within the three simulated
quarters. The resulting release-group-specific periods are integers from zero
to four and are already stored in the corresponding upstream INI.

The exact counts are 6, 12, 19, 26, 19, 12 and 6. This symmetric discrete
distribution makes 0.20 the mode while retaining both requested tails.

Each draw selects the corresponding authoritative `bet.2026.mix-*.ini`
scenario. The complete release-group-specific first column of the MFCL tag-flag
block is transferred from that source to the frozen Job 21641 Diagnostic INI.
Thus `K = 0.15`, for example, selects
`bet.2026.mix-0.15.ini`; it does not write `0.15` into the MFCL tag flags.

All seven files are frozen from the upstream `SC22-IP10-regionMean` branch at
commit `efe3107c72774ee73b5e6dc45e44cf51f0fc20e8`. Preflight validation checks
the complete-file SHA-256 and also requires all 98 transferred first-column
values to be finite integers in the MFCL-supported range 0–4. This prevents an
invalid generated source (for example, a literal `NA`) from reaching MFCL.

## Tag reporting

Tag reporting is a two-level uncertainty axis with equal probability:

- `0`: include reporting-rate information during the pre-mixing period;
- `1`: exclude it, matching the current Diagnostic configuration.

The value is written to column 2 of the relevant MFCL tag flag. The design
contains exactly 50 models of each type. For an inclusion model, release events
whose selected mixing period is zero retain flag 2 = 1 because current MFCL
does not permit reporting-rate application with a zero mixing period. All
positive-mixing events receive flag 2 = 0. The source mixing periods themselves
are not altered, and the number of compatibility exclusions is recorded for
every design row.

The planned design was balanced (50 inclusion and 50 exclusion models), but
only 34 inclusion models were retained compared with 46 exclusion models. This
imbalance reflects differential numerical stability, not unequal design
weights. In the MFCL pre-mixing calculation, inclusion reconstructs tag catches
by dividing by the estimated reporting rate (with a small numerical offset).
Very small reporting rates can therefore generate large intermediate values
and numerical overflow. The MFCL manual similarly cautions that reporting rates
may be poorly determined during mixing and recommends excluding them from this
calculation. The reporting-rate treatment remains an uncertainty axis, while
the unequal retention is reported explicitly rather than reweighted away.

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

### Longevity prior and the MFCL reference age

The longevity-based meta-analytical framework originates with Hamel (2015).
Hamel and Cope (2022) subsequently re-evaluated the relationship and developed
the practical longevity-based formulation used here. Their updated formulation
defines a lognormal prior with

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
Hamel and Cope (2022) also caution that Amax should be supported by adequate,
representative and validated age sampling.

`Amax = 15 years` and the MFCL reference `L(40.5 quarters)` have different
roles and are not contradictory. `Amax` is a stock-level longevity proxy. In
the 40-age-class quarterly MFCL model, age class 40 is an aggregate plus group;
`L(40.5)` is the length used to anchor the declining Lorenzen curve, not a claim
that fish cannot live beyond 10.125 years. The source implements

\[
\log M_a = \log M_0 - \log\{L_a/L(40.5)\},
\]

because the fixed exponent is -1 and scaled length is exactly one in the last
age class. Thus the first coefficient in row 5 of `age_pars` is `log(M0)` and
the second is the length exponent. The Diagnostic values `-2.54930339768360`
and `-1` produce `M0 = 0.078136` per quarter at age class 40.

The Hamel (2015) longevity framework, as updated for practical application by
Hamel and Cope (2022), derives an approximately age-invariant adult mortality
from longevity. It is therefore useful as external adult-M information but is
not mathematically identical to MFCL `M0`. Following the published practice of
scaling a Lorenzen curve so that its maturity-weighted adult mean equals the
longevity estimate, the Diagnostic growth curve, maturity ogive and exponent
-1 give

\[
\overline{M}_{adult}=1.113626\,M_0.
\]

The `Amax = 15` prior consequently has a model-aligned `M0` median of 0.08082
per quarter and a 95% interval of 0.04402–0.14838. This is close to the
Diagnostic `M0 = 0.07814`. Ducharme-Barth et al. (2026) displayed the
longevity-prior curve directly against `M0`; the age-specific comparison in the
report instead applies the above scale conversion and the same Diagnostic
growth schedule to all three `M0` values, so the curves use the same parameter
definition. The tag-analysis 90% interval is propagated over age using that
same fixed Lorenzen shape.

The WCPO evidence supports 15 years as a plausible working longevity, but the
strength of that evidence should be stated precisely. Bomb-radiocarbon work
validated the annual-zone ageing method for BET samples through age 13, while
the same validated annual-zone method has produced WCPO BET estimates up to
15 years. The 2023 assessment also notes tagging evidence consistent with still
older attained ages. Therefore 15 years is defensible as a working value, not
as an exactly observed upper biological limit. The implications of `Amax` 13,
15 and 16 years are reported in
`design/hamel-cope-amax-sensitivity.csv`.

### Selected ensemble distribution

The evidence is synthesised as a bounded logit-normal distribution on the
elicited design range 0.050–0.165. Its mode is 0.0702, the midpoint of the tag
estimate 0.0624 and the previous-assessment value 0.078. Its median is
0.078136, matching `exp(-2.54930339768360)` in the Diagnostic Lorenzen
intercept. These two constraints imply a logit-scale mean of -1.127290 and SD
of 0.803491. The resulting 95% distribution interval is 0.05723–0.12016. The
density approaches zero smoothly at both limits, avoiding the vertical edge
created by truncating a distribution with positive boundary density.

This selected distribution is therefore **not** the longevity prior of Hamel
(2015), as updated by Hamel and Cope (2022). It combines the tag result and the
previous assessment, while the model-aligned longevity prior is used as an
external plausibility check. The
mode and median are elicited synthesis choices rather than statistics reported
by a single paper: 0.0702 is the requested midpoint of two assessment-relevant
estimates, and 0.078136 retains the Diagnostic centre. The range endpoints were
requested from the approximate longevity-prior adult-M 95% limits, but they are used
here only as finite ensemble controls; they are not asserted to be biological
confidence bounds for MFCL `M0`. The 100 retained values are deterministic
midpoints of equal-probability intervals (`p = 0.005, 0.015, ..., 0.995`). The
finite ensemble therefore represents both tails without assigning the elicited
controls themselves as model values; its realised range is 0.05452–0.13275 per
quarter.

MFCL uses quarterly time periods in this model. Each draw is entered as
`log(M0_quarterly)` in the Lorenzen intercept; no additional factor of four is
applied at model input.

Sources:

- [Ducharme-Barth et al. (2026), WCPFC-SC22-2026-SA-IP14](https://meetings.wcpfc.int/node/32286)
- [Hamel (2015), meta-analytical prior for natural mortality using life-history correlates](https://doi.org/10.1093/icesjms/fsu131)
- [Hamel and Cope (2022), updated development and application of the longevity-based prior](https://doi.org/10.1016/j.fishres.2022.106477)
- [Hoyle (2021), scaling longevity-based adult M to a Lorenzen schedule](https://iotc.org/sites/default/files/documents/2021/10/IOTC-2021-WPTT23-08_Rev1_0.pdf)
- [Andrews et al. (2024), WCPO tuna age validation](https://doi.org/10.1093/icesjms/fsae074)
- [2023 WCPO bigeye stock assessment](https://meetings.wcpfc.int/node/19353)
- [MFCL Lorenzen implementation](https://github.com/PacificCommunity/ofp-sam-mfcl/blob/de4abeca920063bf234ce66ec3a0f043c56e885f/src/natural_mortality_spline.cpp)

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
