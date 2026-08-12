# `more_tau` results are pending

This branch changes only the fixed tag-overdispersion axis in the planned
100-model ensemble. The committed files under `final-par/`, `data/ensemble/`,
`data/estimation/`, `data/projection/`, `results/` and the report-facing data
were produced by the preceding `main` design with tau 1.2, 1.3 and 1.4. They
are retained byte-for-byte for provenance only and are **not** results of the
4.96, 5.14 and 5.20 design.

`run-report` and `projection/cache-native-projection` intentionally refuse to
run while this marker exists. Remove it only after all new fits have been
collected, convergence-retained outputs and manifests have replaced the legacy
payloads, and fitted tau values have been checked against
`design/model-draws.csv`.
