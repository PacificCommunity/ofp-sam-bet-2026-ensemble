# Refit a saved model

Restore a job first, using the [archive guide](README.md). Keep that verified copy
and run in a new directory. The [run index](runs.csv) identifies the original
source commit, final PAR and `doitall`; execution settings retain the submitted
controls. Utility jobs and failed checkpoints require their own interpretation.

Diagnostic Job 21641 is a worked example. Its archived native folder contains
the original inputs, selected model controls and complete twelve-phase `doitall.sh`.
The recorded settings are `MODEL_ID=S0.90-F2`, convergence `-4`, and
`PROGRAM_PATH=/home/mfcl/mfclo64`.

After restoring it as `diagnostic-original`, [load the saved runtime](runtime.md).
The following command starts a new fit with the recorded image ID.

```sh
cp -R diagnostic-original/outputs/S0.90-F2-tau2-fixed diagnostic-refit
docker run --rm --pull=never --platform linux/amd64 \
  --mount "type=bind,src=$PWD/diagnostic-refit,dst=/work" \
  --workdir /work --entrypoint /bin/bash \
  --env MODEL_ID=S0.90-F2 \
  --env PROGRAM_PATH=/home/mfcl/mfclo64 \
  --env BET_PHASE10_11_CONVERGENCE=-4 \
  sha256:898925450ab7ec3f99cb9d8427cc37d24097cc2fe9f0468d74f70aaf35daec3a \
  doitall.sh
```

Compare the new `final.par`, REP, objective and gradient with the saved originals.
This documents the historical workflow; a new scientific fit has not been run
to validate these instructions. The archive contains the inputs and script;
the saved runtime retains the pinned image and its engine. These runs record the
external engine path, but do not record its executed-file hash. The bundled
repository executable has a separate identity; see the [runtime evidence](evidence/runtime.json) before
substituting it.

Other runs use their recorded settings and source versions. The [paired RR guide](../rr-test/README.md)
provides prepared both-arm refit commands and explains the historical v2.5
executable limitation. Check [remaining gaps](gaps.json) before deleting source jobs.
