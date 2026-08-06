# Assessment-model projection cache

This folder contains the standalone, resumable BET 2026 ensemble projection
workflow. The locked scenario is:

- fitted data through 2024;
- projection years 2025–2054;
- 10 stochastic recruitment simulations per ensemble model;
- every fishery fixed at its exact 2022–2024 mean annual catch, with the
  observed quarterly pattern retained and an absent fishery-quarter treated as
  zero catch;
- each fishery retains the number or weight unit defined by the model input
  data flag; unlike units are never summed for scientific reporting;
- future recruitment sampled from each model's estimated 1972–2023
  recruitment deviations (the first 20 assessment years and terminal 2024
  are excluded);
- no future catch or effort randomisation; and
- each model's fixed tag-overdispersion value (`tau = 1.2`, `1.3` or `1.4`)
  preserved in the projection parameter file.

`cache-native-projection` is the public entry point. It validates and hashes
the fitted model, common inputs, executable, scripts and scenario before doing
any work. A matching compressed cache is returned without a model run. On a
cache miss, model options 7 and 8 and the stochastic projection are run in
an isolated temporary directory; `parse-native-projection.R` then retains only
the compact report quantities, provenance and checksums. The temporary raw
files are removed only after the RDS and its SHA-256 sidecar are complete.

`build-fishery-conditioning-audit.R` independently reconstructs the 132
fishery-quarter means from the public assessment input. Its checksum-locked
CSV verifies the 16 number-based and 17 weight-based fisheries, the treatment
of absent observations as zero and the 25 exact-zero fishery-quarter means.

`aggregate-native-projections.R` creates the checksum-auditable ensemble
payload used by the report from complete ten-simulation caches. It records the
full 88-model assessment set and every model without a complete native
projection; it never fills, regularises or silently substitutes an incomplete
projection. The aggregate also writes `catch-conditioning.csv`, which records
the unit and exact conditioning value for every fishery. The legacy numeric
sum retained in the compact caches is labelled as a mixed-unit audit only.

`run-native-projection-kflow` is the thin Kflow adapter. It reads `final.par`
from an attached completed-model archive and writes only the compact cache,
its SHA-256 sidecar and a success marker. The scientific workflow remains in
the same standalone cache entry point used locally.
