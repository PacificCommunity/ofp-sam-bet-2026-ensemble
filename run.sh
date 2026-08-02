#!/bin/sh
set -eu

model_id=${ENSEMBLE_SELECT:-${STEP_SELECT:-${1:-}}}
if [ -z "$model_id" ]; then
  echo "Set ENSEMBLE_SELECT or run: ./run.sh ensemble-001" >&2
  exit 2
fi

repo_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
"$repo_dir/scripts/run-ensemble" "$model_id" "$repo_dir/outputs/models/$model_id"
