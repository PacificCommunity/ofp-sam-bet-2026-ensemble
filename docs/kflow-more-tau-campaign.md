# `more_tau` Kflow campaign

This branch runs 100 independent native-MFCL fits under the single Kflow task
`bet-2026-ensemble-more-tau`. The scientific model rows come from
`design/model-draws.csv`; execution site is a scheduling attribute only and is
never written into prepared MFCL inputs.

The frozen `design/submission-sites.csv` assigns exactly 50 fits to Noumea and
50 to Suva. It was selected independently of fitting with seed label
`more_tau-site-split-v1`, accepted attempt 2242, and CPython's
`random.Random(int(sha256("more_tau-site-split-v1:2242"), 16)).shuffle()`.
Its SHA-256 is
`28ea8e11866cfa591f7ed1f7ad3a66b2832c4350eb230be35a4dceb028574454`.
The split has 25 reporting-rate inclusion and 25 exclusion rows per site; the
tau allocations are 17/17/16 at 4.96/5.14/5.20 in Noumea and 16/17/17 in
Suva. Its maximum absolute Spearman correlation with the six design axes is
0.0246183.

Each job requests 2 CPUs, 8GB memory and 20GB disk. The image is pinned by
digest in `kflow.yaml`. The submitted jobs use `/home/mfcl/mfclo64`, whose
required SHA-256 is
`f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0`;
`run.sh` verifies both that binary and the submitted repository commit before
preparing a model. This is intentionally distinct from the bundled executable.

The registrar has four explicit modes:

```sh
python3 scripts/register-kflow-task.py --dry-run
python3 scripts/register-kflow-task.py --register
python3 scripts/register-kflow-task.py --submit
python3 scripts/register-kflow-task.py --audit
```

`--dry-run` is local and read-only. The other modes require `KFLOW_API_TOKEN`.
Registration and submission additionally require a clean local `more_tau`
branch whose HEAD exactly matches `origin/more_tau`. An existing task is never
updated: its full contract must match. Before submission, every API page is
read and every `JOB_KEY` must be unique. Only absent keys are posted. A failed
or timed-out POST is reconciled through read-only listing and is never retried
automatically. After submission, all 100 persisted job contracts are fetched
and checked, including branch, commit, executable hash, resources, host, slot
constraint, empty inputs, empty triggers and empty attachments/artifacts.
