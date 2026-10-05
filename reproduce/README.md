# Saved native inputs

[Download native.tar.gz](https://raw.githubusercontent.com/PacificCommunity/ofp-sam-bet-2026-ensemble/main/reproduce/native.tar.gz). It is included in a normal clone;
[files.json](files.json) lists the archived files and checksums.

This compact package preserves exact native inputs and original scripts for 80 retained ensemble models and 30 completed reporting-rate reruns. Original ensemble PARs and whole REPs remain in [final-par/](../final-par/); the package contains the 30 RR1 final PARs and exact central REP sections. [RR results](../rr-test/results.md) can be read immediately.

Check archived bytes without downloading or running a model:

```sh
make verify
```

Restore one case to a new folder, including the checksum-checked preserved executable:

```sh
make restore CASE=rrtest-005-rr1 OUT=/tmp/bet-rr005
```

Use `make rerun CASE=rrtest-005-rr1 OUT=/tmp/bet-rr005-evaluated` to
regenerate central outputs. `CASE=ensemble-005` selects a retained ensemble
fit. Make and Python 3 are required; the runner fetches the pinned executable.
See [the RR helper](../rr-test/README.md). Failed RR1 fits have no saved final PAR. Original external engine identity remains unknown where it was not recorded; compatibility of the selected executable is checked separately.

Restoration provides evaluation inputs. `make refit CASE=rrtest-005-rr1
OUT=/tmp/bet-rr005-refit` uses the original preparation and fitting runner
in the pinned Docker image; add `ANCHOR=1` for its RR0 arm. Use
`make plan-refit` with the same arguments to inspect the command first.

The original per-model configurations are in [refit-configs.tar.gz](refit-configs.tar.gz)
(2.6 kB): 80 retained ensemble fits and 30 completed RR1 fits. Extract into an
empty directory to inspect the original settings; paths follow the original
`outputs/models/` and `outputs/rr-test/` layout. `make verify` checks every
member against [refit-configs.json](refit-configs.json).

## Original Hessians

The [Hessian index](hessian-index.csv) links the original matrices for all 80
retained fits: 62 PDH and 18 Near-PDH. It maps source model IDs to the report's
E001–E080 labels. Each archive includes the matching final PAR, original Hessian
metadata and calculation log. Files are stored as optional release downloads
to keep clones small.

```sh
make hessian CASE=ensemble-001 OUT=/absolute/bet-hessian
```

This downloads and checks one saved archive; it does not run MFCL. Choose the
source model ID from the index and a new folder outside the repository.
[The manifest](hessians.json) pins every archive and file by SHA256.
