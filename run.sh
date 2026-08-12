#!/bin/sh
set -eu

repo_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
expected_repository_commit=${EXPECTED_REPOSITORY_COMMIT:-}
if [ -n "$expected_repository_commit" ]; then
  actual_repository_commit=$(git -C "$repo_dir" rev-parse HEAD)
  if [ "$actual_repository_commit" != "$expected_repository_commit" ]; then
    echo "Repository commit mismatch: expected $expected_repository_commit, got $actual_repository_commit" >&2
    exit 2
  fi
fi

program_path=${PROGRAM_PATH:-./mfclo64}
expected_program_sha256=${EXPECTED_PROGRAM_SHA256:-}
if [ -n "$expected_program_sha256" ]; then
  if [ ! -x "$program_path" ]; then
    echo "MFCL executable is missing or not executable: $program_path" >&2
    exit 2
  fi
  actual_program_sha256=$(sha256sum "$program_path" | awk '{print $1}')
  if [ "$actual_program_sha256" != "$expected_program_sha256" ]; then
    echo "MFCL executable SHA-256 mismatch: expected $expected_program_sha256, got $actual_program_sha256" >&2
    exit 2
  fi
fi

model_id=${ENSEMBLE_SELECT:-${STEP_SELECT:-${1:-}}}
if [ -z "$model_id" ]; then
  echo "Set ENSEMBLE_SELECT or run: ./run.sh ensemble-001" >&2
  exit 2
fi

PROGRAM_PATH="$program_path" "$repo_dir/scripts/run-ensemble" "$model_id" "$repo_dir/outputs/models/$model_id"
