# Exact RR paired reruns

This pre-release experiment reruns the 34 retained RR0 (`tag_reporting_flag2 = 0`) ensemble fits as exact RR1 (`tag_reporting_flag2 = 1`) counterparts. It is isolated on the `RR_test` branch and does not change the 80-model assessment ensemble or its reports.

## Pairing contract

Each `rrtest-…-rr1` row is anchored to one retained `ensemble-…` RR0 fit. Steepness, natural mortality, mixing-period source and K cutoff, tag likelihood weight (tau), effort-creep inputs, selectivity, initialization, executable, Docker image, and all other model inputs are held fixed. The only requested model change is tag-reporting flag column 2.

For current-MFCL compatibility, a tag event with mixing period zero is stored with the column-2 sentinel value `1` in both arms. Therefore:

- positive-mixing rows are `0` in the RR0 anchor and `1` in its RR1 counterpart;
- zero-mixing rows remain `1` in both arms;
- zero-mixing events are not counted as RR0 reporting-rate exclusions.

`validate.R` materializes all 34 RR0 anchors and all 34 RR1 counterparts. Within every pair it checks that every prepared file is byte-identical except `bet.ini`, metadata, and their manifests; inside `bet.ini`, it proves that only the permitted positive-mixing column-2 fields change.

## Reproduce and validate

```sh
Rscript rr-test/create-design.R
Rscript rr-test/validate.R
python3 rr-test/register-kflow-task.py --dry-run --with-hessian > /tmp/rr-test-kflow-plan.json
```

The fit task is pinned to the same `tuna-flow:v2.5` image digest, image MFCL executable (SHA-256 `f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0`), ordinary makepar/no seed initialization, and Suva execution requirements used by the source ensemble. Fits use the original `/home/mfcl/mfclo64` path. Hessian workers use the equivalent normalized image path `/home/mfcl/./mfclo64`; this preserves that same image executable while preventing the checks adapter from substituting a bundled executable.

## Submit

Commit and push `RR_test` before submission, then provide the Kflow API token in the environment:

```sh
python3 rr-test/register-kflow-task.py --submit --with-hessian
```

This submits 34 independent RR1 fits. For each accepted fit, the registrar invokes the authoritative `ofp-sam-bet-2026-checks/scripts/submit_kflow_checks.py` workflow with three Hessian partitions and retains the merged Hessian matrix. Each partition declares its paired fit as an input dependency; the authoritative workflow then merges the three partitions and attaches the Hessian result to that fit. The diagnostic source fallback is explicitly `PacificCommunity/ofp-sam-bet-2026-ensemble@RR_test`.

Fit and diagnostic outputs remain in Kflow/Suva and are not committed to this repository.
