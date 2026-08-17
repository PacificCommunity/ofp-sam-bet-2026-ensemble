# Retained final PARs: RR exclusion (`tag_reporting_flag2=1`)

This directory contains 46 exact retained native MFCL model pairs in the
**exclusion** reporting-rate group. Original source IDs and filenames are
preserved as `ensemble-NNN/final.par` and `ensemble-NNN/bet.ini`. The companion
group is **inclusion** (`tag_reporting_flag2=0`). Authoritative
PARs under `../final-par/` are not moved, renamed, or modified.

Each `bet.ini` is the model-specific Kflow input, not the generic base INI. Its
size and SHA-256 are taken from the checksum-verified source archive. The run
copied this validated `bet.ini` to `bet.model.ini`, asserted byte identity, and
passed `bet.model.ini` to MFCL `-makepar`; the archive audit independently
confirms that both archived files are byte-identical for all 80 retained fits.
The tracked materializer reapplies steepness, natural mortality, mixing period,
and requested RR controls and must reproduce each archived INI hash exactly.

Membership is the requested model-design axis in
`../design/model-draws.csv::tag_reporting_flag2`, after independently deriving
the 80 retained IDs from
`../data/ensemble/fit-diagnostics.csv::maximum_gradient <= 1e-4` and checking
them against `../data/ensemble/retained-final-par-manifest.csv`.

Important zero-mixing caveat: `tag_reporting_flag2=0` means requested RR
inclusion. At an event with mixing period greater than zero its effective flag
2 is 0, but at a zero-mixing event the implementation forces effective flag 2
to 1. Therefore the split records the requested design treatment, not a claim
that every event-level flag in an inclusion PAR is zero. In this group,
39 models have at least one zero-mixing event. Requested
exclusion (`tag_reporting_flag2=1`) has effective flag 2 equal to 1 for every
event.

`SHA256SUMS` covers the 46 PARs and 46 matching INIs. The complete
mapping, MGC values, archive members, source/destination paths, sizes, hashes,
and zero-mixing fields are in
`../data/ensemble/retained-final-par-rr-split-manifest.csv`. Exact archived INI
provenance is independently locked in
`../data/ensemble/retained-final-ini-manifest.csv`.

Reproduce the split or verify an existing split from the repository root:

```sh
./scripts/split-retained-final-pars-by-reporting.py
./scripts/split-retained-final-pars-by-reporting.py --check
```
