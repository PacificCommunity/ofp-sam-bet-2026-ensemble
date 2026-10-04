<a name="exact-rr-paired-reruns"></a>

# Paired reporting-rate fits

Saved results for the [SC22 reporting-rate follow-up](https://meetings.wcpfc.int/node/32932): **30 completed pairs from 34 attempts**. RR0 includes reporting rates; RR1 excludes them. Anchors 001, 022, 025 and 080 failed under RR1 and are excluded from the summaries.

<a name="reproduce-and-validate"></a>

## Read and check

Start with the [results and SC22 figures](results.md). The [run manifest](results/run-manifest.csv) records both original job IDs, settings and outcomes; the [file manifest](results/file-manifest.csv) identifies every retained source file.

```sh
git clone --branch main --single-branch https://github.com/PacificCommunity/ofp-sam-bet-2026-ensemble.git
cd ofp-sam-bet-2026-ensemble
python3 rr-test/verify-results.py
```

To rebuild the paired tables and plots with base R:

```sh
Rscript rr-test/summarize.R /tmp/bet-rr-review
python3 rr-test/verify-results.py --rebuilt-root /tmp/bet-rr-review
```

Original final PARs, REPs, INIs and input audits are in [fits/](fits/) for RR1 and [the retained anchor folder](../final-par-rr-inclusion-flag2-0/) for RR0. [Failures](failures/) retain the original error evidence. Saved results require exact file checksums; rebuilt numeric tables allow only documented platform roundoff.

## Regenerate central outputs

Use Python 3.9+, Docker, 2 CPUs and 8 GiB. The pinned image runs Linux amd64; other architectures require emulation.

```sh
./rr-test/rerun rrtest-005-rr1 --outputs-only
./rr-test/rerun rrtest-005-rr1 --anchor --outputs-only
```

This loads the original final PAR with one function evaluation using the original complete-output controls, checks the executable and native input hashes, requires objective agreement within `1e-6`, and compares the central REP biomass and MSY values with the original. It writes `evaluated.par`, `plot-evaluated.par.rep`, a log and a verification receipt beneath `outputs/rr-test-evaluated/`. It does not recreate Hessians, optimiser history or stochastic projections. Failed RR1 fits have no final PAR to evaluate.

Add `--dry-run` to inspect the command. For the [preserved runtime](runtime.md), add `--saved-runtime`; the helper then refuses image pulls. Use `--output-dir /new/path` for another attempt. Existing directories are refused.

## Refit a pair

```sh
./rr-test/rerun rrtest-005-rr1
./rr-test/rerun rrtest-005-rr1 --anchor
```

Both arms start from `-makepar` and use the original phase 10/11 convergence criterion (`-4`). Outputs go beneath `outputs/rr-test-local/`. Review MGC and stock-status quantities after each refit.

<a name="pairing-contract"></a>

## Interpretation

Within each pair, all inputs except reporting-rate inclusion are fixed. Positive-mixing tag rows change INI column 2 from 0 to 1; zero-mixing rows retain sentinel 1. `validate.R` checks prepared inputs against the archived RR0 INIs.

The summary uses mean SB in 2021–2024 divided by mean SB_F=0 in 2014–2023; annual depletion uses the matching year. F/F_MSY is the reciprocal of the native REP's F multiplier at MSY. These are historical pre-fix central estimates. The rerun selects the preserved v2.5 executable (`f5bc…`); the original external RR0 executable hash remains unknown. They do not establish the corrected v2.6 response or an unbiased setting.

<a name="submit"></a>

The original registrar is retained for maintainers. Reading results and regenerating a saved pair require no scheduler records.
