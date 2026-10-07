<a name="exact-rr-paired-reruns"></a>

# Paired reporting-rate fits

[SC22 follow-up](https://meetings.wcpfc.int/node/32932): **30/34 completed pairs**. RR0 includes tag reporting rates; RR1 excludes them. Failed RR1 attempts: 001, 022, 025 and 080.

<a name="reproduce-and-validate"></a>

## Read the results

[Results and figures](results.md) · [file manifest](results/file-manifest.csv).
Rebuild the paired summaries from saved native PAR/REP files with base R:

```sh
Rscript rr-test/summarize.R /tmp/bet-rr-review
```

## Run the saved PAR

[standalone.zip](standalone.zip) includes both arms of the 30 completed pairs: last PARs, exact inputs, original doitall scripts and one MFCL executable. Unzip it, then:

```sh
cd bet-2026-rr-standalone
make unpack
make verify
make rerun CASE=rrtest-005-rr1 OUT=/tmp/bet-rr1-005
make rerun CASE=ensemble-005 OUT=/tmp/bet-rr0-005
```

`models/<case>/` contains ordinary MFCL files. R calls MFCL directly and checks the native outputs against the saved objective and central REP. New outputs include `evaluated.par`, `plot-evaluated.par.rep`, annual `central-results.csv` and recent `management-quantities.csv`.

Native execution needs Linux x86-64, R, Make, tar with XZ support and SHA-256 tools. No Python, R packages, downloads or Kflow records are needed. The ZIP also supports `make list`, `make prepare` and `make refit`; its README gives the commands.

The original evaluation ceiling is **1**, with required native iteration and function counter **0**. A literal ceiling of 0 omits required biomass output. The check covers ten central REP sections; it does not establish whole-output or fresh-refit equality.

Original RR0 PARs and whole REPs remain in [final-par/](../final-par/). The [native package](../reproduce/README.md) also preserves the other retained ensemble fits. Figure 2B–C uses the repository's planned-100 and retained-80 tables.

<a name="pairing-contract"></a>

## Interpretation

These are historical pre-fix central estimates. Other scientific inputs are fixed within each pair; the tag flag changes only for positive-mixing groups. The scripts retain their original preparation differences. Original external RR0 executable identity remains unknown. See [results](results.md) and [runtime details](runtime.md).

<a name="submit"></a>

The historical registrar remains available to maintainers.

The central REP comparison uses the scaled bound `1e-10 * max(1, abs(reference))`; the objective uses an absolute bound of `1e-6`.
