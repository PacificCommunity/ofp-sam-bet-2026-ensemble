# Retained native MFCL final PARs and Viewer REPs

> On the `more_tau` branch these are byte-preserved legacy `main` outputs from
> the tau 1.2/1.3/1.4 design. They are verified for provenance only and are not
> fitted results for tau 4.96/5.14/5.20.

This repository directly preserves the exact final PAR files for the 80
ensemble fits retained by the public maximum-gradient-component criterion
(`MGC <= 1e-4`) together with their final Viewer outputs:

```text
final-par/<source-id>/final.par
final-par/<source-id>/plot-11.par.rep
```

`plot-11.par.rep` is the canonical REP produced from the retained fit with the
final Phase 11 settings and can be supplied directly to mfclshiny or another
MFCL REP reader. A clone also contains the runnable inputs, scripts and actual
native MFCL executable, so ordinary users do not need Kflow, access to Suva or
a separate release download. The bundled executable is statically linked for
x86-64 Linux.

The source audit covers all 100 Kflow archives. Ninety archives contain a
completed `final.par`. Ten configurations have no completed PAR:
`ensemble-013`, `ensemble-026`, `ensemble-028`, `ensemble-058`,
`ensemble-060`, `ensemble-061`, `ensemble-064`, `ensemble-067`,
`ensemble-077` and `ensemble-097`. Ten completed fits are excluded by the MGC
criterion: `ensemble-003`, `ensemble-016`, `ensemble-017`, `ensemble-020`,
`ensemble-037`, `ensemble-048`, `ensemble-059`, `ensemble-083`,
`ensemble-096` and `ensemble-100`. The remaining exact 80 source IDs and every
archive/PAR checksum are listed in
`data/ensemble/retained-final-par-manifest.csv`.
These directory names are the original 100-design source IDs, not the
sequential 1--80 display labels used after report filtering.

The authoritative execution provenance is:

- Kflow task: `bet-2026-ensemble-tau`
- source commit: `24483e3c3a36cddb511fc85454b81a218e1c46e7`
- image: `ghcr.io/pacificcommunity/tuna-flow:v2.5@sha256:c87f1f6d9d4f62dc447844b58afe35f96af175bf933cb6cffbbbe39a59172360`
- native MFCL: version `2.2.7.9`, SHA-256 `8995f72019869863c1d1c0b4f44fc6a6268d1f79031f5bc79dc354ee10f0a63e`

The maintainer-only recovery step checks each source archive SHA before
extracting its declared member. For each committed, recovered or downloaded
PAR, the public verifier checks its PAR SHA/size, raw objective, raw MGC,
active-parameter count and MFCL compilation version. It then checks the fitted
steepness, fixed tau, natural mortality, DM concentration and exact 33-fishery
Diagnostic selectivity against the design row. Native PAR mode materializes
the model-specific inputs and asks MFCL to load each PAR with zero function
evaluations; this is an objective/model parity check, not REP reproduction.

The committed `plot-11.par.rep` files are generated separately by running the
same native executable from each exact `final.par` with the final Phase 11
controls and one function evaluation. This preserves the committed PAR while
reproducing the canonical final Viewer output. All 80 regenerated files are
byte-identical to the corresponding Kflow archive outputs. Their exact
identities are locked in `data/ensemble/retained-final-rep-manifest.csv`.

From a normal repository clone:

```sh
./scripts/verify-retained-final-pars final-par - 2 fast
./scripts/verify-retained-final-pars final-par - 2 native
./scripts/verify-retained-final-reps final-par
./run.sh ensemble-001
```

The first command quickly verifies all hashes, archived metadata and fitted
model controls. The second additionally performs native load/evaluation for
all 80 PARs with zero function evaluations. The third verifies that the 80
committed REPs have the exact recorded SHA/size, Viewer header and required
sections. The fourth independently refits a selected configuration from its
ordinary `bet.ini -makepar` start; it does not start from the retained PAR.
If FLR4MFCL is installed, its semantic REP parser can also be exercised with:

```sh
./scripts/verify-retained-final-reps \
  final-par data/ensemble/retained-final-rep-manifest.csv parse
```

To reproduce all REPs without overwriting the committed files, generate them
in a new directory and verify that output against its generated manifest:

```sh
./scripts/generate-retained-final-reps \
  final-par /tmp/bet-reps /tmp/retained-final-rep-manifest.csv 2 all
./scripts/verify-retained-final-reps \
  /tmp/bet-reps /tmp/retained-final-rep-manifest.csv fast
```

An optional archive of the retained PAR material is published from the
non-version prerelease tag
`retained-final-pars-2026.08.11` as
[`bet-2026-ensemble-retained-final-pars-2026.08.11.tar.gz`](https://github.com/PacificCommunity/ofp-sam-bet-2026-ensemble/releases/download/retained-final-pars-2026.08.11/bet-2026-ensemble-retained-final-pars-2026.08.11.tar.gz).
This separate prerelease does not replace the existing report release or alter
any report, PDF, viewer or GitHub Pages target.

The Suva recovery helper is maintainer-only and read-only. It writes nothing
on the submit host:

```sh
./scripts/fetch-retained-final-pars /tmp/bet-ensemble-retained-final-pars
```

After recovery, a maintainer can build the release asset locally:

```sh
./scripts/build-ensemble-reproducibility-bundle \
  final-par \
  bet-2026-ensemble-retained-final-pars-2026.08.11.tar.gz 2
```
