# Native MFCL projection cache

This folder contains the standalone, resumable BET 2026 ensemble projection
workflow. The locked scenario is:

- fitted data through 2024;
- projection years 2025–2054;
- 10 stochastic recruitment simulations per ensemble model;
- every fishery fixed at its exact 2022–2024 mean catch;
- future recruitment sampled natively from the fitted 1972–2023 deviates;
- no future catch or effort randomisation; and
- each model's fixed tag-overdispersion value (`tau = 1.2`, `1.3` or `1.4`)
  preserved in the projection parameter file.

`cache-native-projection` is the public entry point. It validates and hashes
the fitted model, common inputs, executable, scripts and scenario before doing
any work. A matching compressed cache is returned without an MFCL run. On a
cache miss, native option 7, option 8 and the stochastic projection are run in
an isolated temporary directory; `parse-native-projection.R` then retains only
the compact report quantities, provenance and checksums. The temporary raw
files are removed only after the RDS and its SHA-256 sidecar are complete.

`aggregate-native-projections.R` requires all 88 completed model caches and
creates the checksum-auditable ensemble payload used by the report. It never
silently substitutes a missing model.

`run-native-projection-kflow` is the thin Kflow adapter. It reads `final.par`
from an attached completed-model archive and writes only the compact cache,
its SHA-256 sidecar and a success marker. The scientific workflow remains in
the same standalone cache entry point used locally.
