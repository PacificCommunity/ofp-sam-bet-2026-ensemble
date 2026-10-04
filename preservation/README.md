# Original Kflow results

Start with the [saved models](models.md), [run index](runs.csv), [published model sources](published-models.json)
and [coverage gaps](gaps.json). Original job numbers identify the runs;
`kflow-021641` is Diagnostic Job 21641. The catalog records source commits, actual
scripts and verified final PARs. Failed-run checkpoints remain separately identified.
Scheduler status and the native model's status are recorded separately.

Large files are Release downloads. Select a job to fetch its files.
Python 3.10 or later is sufficient.

From the repository root:

```sh
python3 preservation/tools.py list --job kflow-021641
python3 preservation/tools.py download --job kflow-021641
python3 preservation/tools.py verify-members --job kflow-021641
python3 preservation/tools.py inspect --job kflow-021641 --ordinal 89
python3 preservation/tools.py inspect --job kflow-021641 --ordinal 95 --content
python3 preservation/tools.py restore --job kflow-021641 --output diagnostic-original
```

This example inspects the original final PAR and `doitall.sh`, then restores
the job's saved files. Commands verify hashes before use and never execute scripts.
Restore requires a new directory and refuses links, unsafe paths and overwrites.
For recovered retrospective/jitter files, use the auxiliary `archive_id` recorded
in the catalog with `--archive`.

[Offline Git bundles](sources.md) preserve available source commits; the Pages snapshot preserves
the published HTML and linked assets. [Execution settings](execution-parameters-preservation-v4.jsonl.gz)
record the original commands and runtime controls. The [refit example](refit.md)
uses the original Diagnostic inputs and [saved runtime](runtime.md). [Download verification](evidence/archive-download-verification.json) records the fresh GitHub readbacks.
Additional historical archives are still being preserved; check the gaps before cleaning source jobs.
