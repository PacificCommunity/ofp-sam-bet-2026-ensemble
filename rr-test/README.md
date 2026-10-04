<a name="exact-rr-paired-reruns"></a>

# Exact paired RR fits

This folder preserves the 34-model Kflow campaign used for the [SC22 reporting-rate follow-up](https://meetings.wcpfc.int/node/32932). Fits were submitted on 18 August 2026 from commit [`2ffa434`](https://github.com/PacificCommunity/ofp-sam-bet-2026-ensemble/commit/2ffa4347760459ad20562ef93f92a70861cee420), task `bet-2026-rr-paired-test`.

Thirty counterparts completed and passed MGC ≤ 1e-4. Four failed with exit code 134 and a `choleski_exception`: anchors **001, 022, 025 and 080**. Their original status files and stderr excerpts are in [failures/](failures/). They are excluded from the paired summaries.

<a name="reproduce-and-validate"></a>

## Review the saved results

See the [paired results and original SC22 figures](results.md) for the comparison overview.

```sh
git clone --branch RR_test --single-branch https://github.com/PacificCommunity/ofp-sam-bet-2026-ensemble.git
cd ofp-sam-bet-2026-ensemble
python3 rr-test/verify-results.py
Rscript rr-test/summarize.R /tmp/bet-rr-review
python3 rr-test/verify-results.py --rebuilt-root /tmp/bet-rr-review
```

Python 3.9 or later verifies the saved file checksums, source identities, both-arm MGC and the actual archived input differences. Base R rebuilds the paired tables and plots from the native REP/PAR files and compares them with the original 30-pair table. It writes to the chosen output root; omit the last argument to rebuild the committed summary in `rr-test/`.

The final command also compares all three rebuilt tables with the published tables. Columns, row order, IDs, periods and counts must match exactly. Calculated real values allow absolute error `1e-10` or relative error `1e-12` for platform roundoff; saved original files still require exact checksums.

| Location | Contents |
|---|---|
| [results/run-manifest.csv](results/run-manifest.csv) | All 34 anchor/counterpart IDs, original Kflow jobs, outcomes, MGC, grid settings, source commits and archive hashes |
| [results/file-manifest.csv](results/file-manifest.csv) | Source archive members, file hashes and byte counts for both arms |
| [results/paired-quantities.csv](results/paired-quantities.csv) | Both-arm quantities and RR1 − RR0 differences for the 30 completed pairs |
| [results/paired-timeseries.csv](results/paired-timeseries.csv) | Annual values from each native REP |
| [results/summary.csv](results/summary.csv) | Paired summary statistics |
| [fits/](fits/) | Original RR1 final PAR, Phase 11 REP, INI, gradients and input audits |
| [../final-par-rr-inclusion-flag2-0/](../final-par-rr-inclusion-flag2-0/) | Original RR0 PAR, REP and INI files |
| [reference/](reference/) | Original analysis CSVs and SC22 figures, with their own checksum manifest |

Only the selected review files are copied from each Kflow archive. Full fitting archives and Hessian attachments remain at their original Kflow locations. `INPUTS.sha256` inside each fit is the original complete run inventory; use `verify-results.py` to verify this published subset.

## Rerun one pair

Use Python 3.9 or later, Docker and enough resources for 2 CPUs and 8 GiB. The image is pinned to Linux amd64; other architectures need Docker emulation.

```sh
# Inspect the command; no model is run.
./rr-test/rerun rrtest-005-rr1 --dry-run

# Refit under RR1, then refit its original RR0 anchor.
./rr-test/rerun rrtest-005-rr1
./rr-test/rerun rrtest-005-rr1 --anchor
```

To use the preserved Docker image, follow the [saved runtime instructions](../preservation/runtime.md), then add `--saved-runtime`. This selects the exact loaded image ID, refuses image pulls, and records that selection in the command receipt.

```sh
./rr-test/rerun rrtest-005-rr1 --saved-runtime --dry-run
./rr-test/rerun rrtest-005-rr1 --anchor --saved-runtime --dry-run
```

Each arm starts from ordinary `-makepar`, without a fitted checkpoint or jitter. The helper verifies the image executable hash before preparing inputs, validates the selected pair against the historical RR0 input hash, and uses the original phase 10/11 convergence criterion (`-4`). It refuses existing output directories.

Results go to `outputs/rr-test-local/rrtest-005-rr1/run/` and `outputs/rr-test-local/ensemble-005/run/`. Each parent folder also contains the command receipt and run log. To repeat an attempt, choose a fresh folder with `--output-dir /new/path`. Numerical convergence and elapsed time can differ across reruns; inspect final MGC and the stock-status quantities.

<a name="pairing-contract"></a>

## Pairing and interpretation

Steepness, M, mixing cutoff K, tau, effort creep, selectivity and initialization are fixed within each pair. In `bet.ini`, positive-mixing tag rows change column 2 from 0 to 1. Zero-mixing rows retain sentinel 1 in both arms. The saved RR1 INIs are checked against the archived RR0 INIs; `validate.R` also checks prepared inputs against the historical archive hashes.

The summary uses mean SB in **2021–2024** divided by mean SB_F=0 in **2014–2023**. Annual plotted depletion uses each year's mean SB divided by the same year's mean SB_F=0. F/F_MSY is the reciprocal of the native REP's F multiplier at MSY. These are central estimates; the script does not use Hessian uncertainty.

The historical image is `tuna-flow:v2.5@sha256:c87f1f6d9d4f62dc447844b58afe35f96af175bf933cb6cffbbbe39a59172360`, with `/home/mfcl/mfclo64` and required SHA-256 `f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0`. The repository's bundled executable has a different hash and is not used by the rerun helper. The old RR0 PAR manifest labels that bundled hash as the runtime; the later runtime audit identifies the pre-fix image executable. Those provenance records are preserved; the new manifest distinguishes the declared fit executable from the saved PAR/REP file identities.

The pre-fix implementation handled RR exclusion inconsistently between tag dynamics and likelihood. These preserved fits do not demonstrate the corrected v2.6 response or establish an unbiased setting. The successful pairs are also a selected subset of the 34 attempted counterparts.

<a name="submit"></a>

The original Kflow registrar remains available for maintainers. `--with-hessian` needs PyYAML and a sibling `ofp-sam-bet-2026-checks` checkout. It submits a new campaign and is not needed to read, verify or rerun an individual saved pair.
