#!/bin/sh
set -eu

model_id=${RR_TEST_SELECT:-${ENSEMBLE_SELECT:-${1:-}}}
if [ -z "$model_id" ]; then
  echo "Set RR_TEST_SELECT or run: ./rr-test/run.sh rrtest-001-rr1" >&2
  exit 2
fi

repo_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
export ENSEMBLE_DESIGN_FILE=rr-test/model-draws.csv
exec "$repo_dir/scripts/run-ensemble" "$model_id" "$repo_dir/outputs/rr-test/$model_id"
