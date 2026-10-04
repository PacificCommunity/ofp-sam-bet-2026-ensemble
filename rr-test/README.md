<a name="exact-rr-paired-reruns"></a>

# Paired reporting-rate fits

[SC22 follow-up](https://meetings.wcpfc.int/node/32932): **30/34 completed pairs**. RR0 includes reporting rates; RR1 excludes them. Failed RR1 anchors: 001, 022, 025 and 080.

<a name="reproduce-and-validate"></a>

## Read and check

Start with [results and figures](results.md). The [file manifest](results/file-manifest.csv) identifies delivered files and their sources.

```sh
python3 rr-test/verify-results.py
Rscript rr-test/summarize.R /tmp/bet-rr-review
python3 rr-test/verify-results.py --rebuilt-root /tmp/bet-rr-review
```

Original anchor PARs/REPs stay in [final-par/](../final-par/). The [compact package](../reproduce/README.md) preserves native inputs, scripts, RR1 PARs and central REP sections.

## Standalone ZIP: saved-PAR outputs without optimisation

[standalone.zip](standalone.zip) contains the saved last PARs, exact inputs and one static MFCL engine for both arms of all 30 completed pairs. Unzip it, then regenerate one saved case into a fresh directory:

```sh
cd bet-2026-rr-standalone
./run-final --verify
./run-final rrtest-005-rr1 /tmp/bet-rrtest-005-evaluation
./run-final ensemble-005 /tmp/bet-ensemble-005-evaluation
```

The original controller sets a function-evaluation ceiling of **1** (`1 1 1`). The wrapper and CI require actual native **1,997 variables, iteration 0, function evaluation 0** records; missing or nonzero counters fail. Ceiling 1 is separate from reported counter 0. Earlier literal-zero tests omitted `Adult biomass in absence of fishing` in all three tested cases, so that mode is excluded. Those runs also emitted no native iteration/function-evaluation counter records, so they do not establish reported counter zero.

Use `./run-final --list` to see all 60 saved cases. The ZIP's README explains the separate full-refit working-copy command. Four failed RR1 fits have no saved PAR and remain excluded. Native evaluation requires Linux x86-64 and Python 3; it uses no network, R, Docker or job history.

## Regenerate central outputs

On Linux x86_64, Python and the pinned 34.55 MB executable are sufficient:

```sh
./rr-test/rerun rrtest-005-rr1 --outputs-only
./rr-test/rerun rrtest-005-rr1 --anchor --outputs-only
```

This existing runner uses a ceiling of 1 to check input hashes, objective and central REP values. New outputs go under `outputs/rr-test-evaluated/`. Add `--dry-run` to inspect; [runtime options and limits](runtime.md) cover Docker, saved executables and full outputs.

## Refit a pair

```sh
./rr-test/rerun rrtest-005-rr1
./rr-test/rerun rrtest-005-rr1 --anchor
```

Full refits use original preparation and phase 10/11 criterion `-4`.

<a name="pairing-contract"></a>

## Interpretation

Within each pair, other inputs are fixed. These are historical central estimates; [interpretation](results.md) and [runtime limits](runtime.md) apply. Original external anchor executable identity remains unknown.

<a name="submit"></a>

The original registrar remains available to maintainers.
