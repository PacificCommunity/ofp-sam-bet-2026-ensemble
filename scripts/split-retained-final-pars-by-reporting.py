#!/usr/bin/env python3
"""Materialize and verify retained PAR/INI/REP reporting-rate splits.

The split is deliberately copy-only: ``final-par`` remains the authoritative
source and existing destination files are never overwritten or removed.
"""

from __future__ import annotations

import argparse
import csv
from decimal import Decimal, InvalidOperation
import hashlib
import io
import os
from pathlib import Path
import re
import subprocess
import tempfile


EXPECTED_RETAINED = 80
EXPECTED_GROUP_COUNTS = {0: 34, 1: 46}
MGC_LIMIT = Decimal("1e-4")
MODEL_ID_RE = re.compile(r"ensemble-[0-9]{3}\Z")
SHA256_RE = re.compile(r"[0-9a-f]{64}\Z")


def fail(message: str) -> None:
    raise SystemExit("ERROR: " + message)


def regular_unlinked_file(path: Path, description: str) -> None:
    if not path.is_file() or path.is_symlink():
        fail(f"{description} is missing, non-regular, or a symbolic link: {path}")


def sha256_path(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for block in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def sha256_bytes(payload: bytes) -> str:
    return hashlib.sha256(payload).hexdigest()


def parse_decimal(value: str, description: str) -> Decimal:
    try:
        result = Decimal(value)
    except InvalidOperation:
        fail(f"invalid decimal {description}: {value!r}")
    if not result.is_finite():
        fail(f"non-finite decimal {description}: {value!r}")
    return result


def parse_nonnegative_integer(value: str, description: str) -> int:
    if not re.fullmatch(r"0|[1-9][0-9]*", value):
        fail(f"invalid non-negative integer {description}: {value!r}")
    return int(value)


def read_csv(path: Path, required_columns: tuple[str, ...]) -> list[dict[str, str]]:
    regular_unlinked_file(path, "CSV input")
    with path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        if reader.fieldnames is None:
            fail(f"CSV has no header: {path}")
        missing = [name for name in required_columns if name not in reader.fieldnames]
        if missing:
            fail(f"CSV is missing columns {missing}: {path}")
        rows = list(reader)
    return rows


def rows_by_model(
    rows: list[dict[str, str]], description: str, expected_rows: int | None = None
) -> dict[str, dict[str, str]]:
    if expected_rows is not None and len(rows) != expected_rows:
        fail(f"{description} has {len(rows)} rows; expected {expected_rows}")
    answer: dict[str, dict[str, str]] = {}
    for row in rows:
        model_id = row.get("ensemble_id", "")
        if MODEL_ID_RE.fullmatch(model_id) is None:
            fail(f"{description} has invalid ensemble_id: {model_id!r}")
        if model_id in answer:
            fail(f"{description} has duplicate ensemble_id: {model_id}")
        answer[model_id] = row
    return answer


def csv_payload(fieldnames: tuple[str, ...], rows: list[dict[str, str]]) -> bytes:
    buffer = io.StringIO(newline="")
    writer = csv.DictWriter(buffer, fieldnames=fieldnames, lineterminator="\n")
    writer.writeheader()
    writer.writerows(rows)
    return buffer.getvalue().encode("utf-8")


def expected_inventory(root: Path, model_ids: list[str]) -> tuple[set[Path], set[Path]]:
    directories = {root / model_id for model_id in model_ids}
    files = {
        root / model_id / filename
        for model_id in model_ids
        for filename in ("bet.ini", "final.par", "plot-11.par.rep")
    }
    files.update({root / "README.md", root / "SHA256SUMS"})
    return directories, files


def inspect_output_inventory(root: Path, model_ids: list[str], require_complete: bool) -> None:
    expected_directories, expected_files = expected_inventory(root, model_ids)
    if not root.exists():
        if require_complete:
            fail(f"split output directory is missing: {root}")
        return
    if not root.is_dir() or root.is_symlink():
        fail(f"split output root is non-directory or a symbolic link: {root}")

    actual_directories: set[Path] = set()
    actual_files: set[Path] = set()
    for current, directory_names, file_names in os.walk(root, followlinks=False):
        current_path = Path(current)
        for name in directory_names:
            path = current_path / name
            if path.is_symlink():
                fail(f"symbolic links are forbidden in split output: {path}")
            actual_directories.add(path)
        for name in file_names:
            path = current_path / name
            if path.is_symlink() or not path.is_file():
                fail(f"non-regular files are forbidden in split output: {path}")
            actual_files.add(path)

    extra_directories = actual_directories - expected_directories
    extra_files = actual_files - expected_files
    if extra_directories or extra_files:
        extras = sorted(str(path.relative_to(root)) for path in extra_directories | extra_files)
        fail(f"unexpected split output entries under {root}: {', '.join(extras)}")
    if require_complete:
        missing_directories = expected_directories - actual_directories
        missing_files = expected_files - actual_files
        if missing_directories or missing_files:
            missing = sorted(str(path.relative_to(root)) for path in missing_directories | missing_files)
            fail(f"incomplete split output under {root}: {', '.join(missing)}")


def ensure_directory(path: Path) -> None:
    path.mkdir(mode=0o755, parents=False, exist_ok=True)
    if not path.is_dir() or path.is_symlink():
        fail(f"output path is non-directory or a symbolic link: {path}")


def install_bytes_without_overwrite(path: Path, payload: bytes) -> None:
    expected_sha = sha256_bytes(payload)
    if path.exists() or path.is_symlink():
        regular_unlinked_file(path, "generated artifact")
        if path.stat().st_size != len(payload) or sha256_path(path) != expected_sha:
            fail(f"refusing to overwrite changed generated artifact: {path}")
        return

    descriptor, temporary_name = tempfile.mkstemp(prefix=".split-artifact-", dir=path.parent)
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "wb") as handle:
            handle.write(payload)
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(temporary, 0o644)
        try:
            os.link(temporary, path)
        except FileExistsError:
            regular_unlinked_file(path, "concurrently created generated artifact")
            if path.stat().st_size != len(payload) or sha256_path(path) != expected_sha:
                fail(f"concurrently created artifact differs: {path}")
    finally:
        if temporary.exists():
            temporary.unlink()


def replace_generated_bytes(path: Path, payload: bytes) -> None:
    """Atomically refresh a known generated metadata file."""
    if path.exists() or path.is_symlink():
        regular_unlinked_file(path, "generated metadata")
        if path.read_bytes() == payload:
            return

    descriptor, temporary_name = tempfile.mkstemp(prefix=".split-metadata-", dir=path.parent)
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "wb") as handle:
            handle.write(payload)
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(temporary, 0o644)
        os.replace(temporary, path)
    finally:
        if temporary.exists():
            temporary.unlink()


def copy_without_overwrite(
    source: Path, destination: Path, expected_size: int, expected_sha: str
) -> None:
    if destination.exists() or destination.is_symlink():
        regular_unlinked_file(destination, "split artifact")
        if destination.stat().st_size != expected_size or sha256_path(destination) != expected_sha:
            fail(f"refusing to overwrite changed split artifact: {destination}")
        return

    descriptor, temporary_name = tempfile.mkstemp(prefix=".split-final-par-", dir=destination.parent)
    temporary = Path(temporary_name)
    digest = hashlib.sha256()
    copied = 0
    try:
        with source.open("rb") as input_handle, os.fdopen(descriptor, "wb") as output_handle:
            for block in iter(lambda: input_handle.read(1024 * 1024), b""):
                output_handle.write(block)
                digest.update(block)
                copied += len(block)
            output_handle.flush()
            os.fsync(output_handle.fileno())
        if copied != expected_size or digest.hexdigest() != expected_sha:
            fail(f"source changed while it was copied: {source}")
        os.chmod(temporary, 0o644)
        try:
            os.link(temporary, destination)
        except FileExistsError:
            regular_unlinked_file(destination, "concurrently created split artifact")
            if destination.stat().st_size != expected_size or sha256_path(destination) != expected_sha:
                fail(f"concurrently created split artifact differs: {destination}")
    finally:
        if temporary.exists():
            temporary.unlink()


def materialize_exact_model_inis(repo: Path, model_ids: list[str]) -> dict[str, bytes]:
    helper = repo / "scripts" / "materialize-retained-final-inis.R"
    regular_unlinked_file(helper, "model-specific INI materializer")
    with tempfile.TemporaryDirectory(prefix="bet-retained-final-inis-") as temporary_name:
        scratch = Path(temporary_name)
        ini_root = scratch / "rendered"
        ini_root.mkdir(mode=0o700)
        id_file = scratch / "model-ids.txt"
        id_file.write_text("".join(f"{model_id}\n" for model_id in model_ids), encoding="ascii")
        result = subprocess.run(
            ["Rscript", str(helper), str(ini_root), str(id_file)],
            cwd=repo,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
        )
        if result.returncode != 0:
            fail("model-specific INI materialization failed:\n" + result.stdout.rstrip())

        expected_files = {ini_root / model_id / "bet.ini" for model_id in model_ids}
        actual_files: set[Path] = set()
        actual_directories: set[Path] = set()
        for current, directory_names, file_names in os.walk(ini_root, followlinks=False):
            current_path = Path(current)
            for name in directory_names:
                path = current_path / name
                if path.is_symlink():
                    fail(f"INI materializer produced a symbolic-link directory: {path}")
                actual_directories.add(path)
            for name in file_names:
                path = current_path / name
                if path.is_symlink() or not path.is_file():
                    fail(f"INI materializer produced a non-regular file: {path}")
                actual_files.add(path)
        expected_directories = {ini_root / model_id for model_id in model_ids}
        if actual_directories != expected_directories or actual_files != expected_files:
            fail("INI materializer output is not the exact one-file-per-retained-model set")
        return {
            model_id: (ini_root / model_id / "bet.ini").read_bytes()
            for model_id in model_ids
        }


def readme_payload(group: str, flag: int, count: int, zero_mixing_models: int) -> bytes:
    other_group = "exclusion" if group == "inclusion" else "inclusion"
    other_flag = 1 - flag
    return f"""# Retained MFCL outputs: RR {group} (`tag_reporting_flag2={flag}`)

This directory contains {count} exact retained native MFCL model triplets in the
**{group}** reporting-rate group. Original source IDs and filenames are
preserved as `ensemble-NNN/final.par`, `ensemble-NNN/bet.ini`, and
`ensemble-NNN/plot-11.par.rep`. The companion
group is **{other_group}** (`tag_reporting_flag2={other_flag}`). Authoritative
PARs and Viewer-ready Phase 11 REPs under `../final-par/` are not moved,
renamed, or modified.

Each `bet.ini` is the model-specific Kflow input, not the generic base INI. Its
size and SHA-256 are taken from the checksum-verified source archive. The run
copied this validated `bet.ini` to `bet.model.ini`, asserted byte identity, and
passed `bet.model.ini` to MFCL `-makepar`; the archive audit independently
confirms that both archived files are byte-identical for all 80 retained fits.
The tracked materializer reapplies steepness, natural mortality, mixing period,
and requested RR controls and must reproduce each archived INI hash exactly.
Each `plot-11.par.rep` is the exact final Phase 11 Viewer output for that
retained fit. Its byte size, line count and SHA-256 are locked against the
checksum-verified Kflow archive by
`../data/ensemble/retained-final-rep-manifest.csv`.

Membership is the requested model-design axis in
`../design/model-draws.csv::tag_reporting_flag2`, after independently deriving
the 80 retained IDs from
`../data/ensemble/fit-diagnostics.csv::maximum_gradient <= 1e-4` and checking
them against `../data/ensemble/retained-final-par-manifest.csv`.

Important zero-mixing caveat: `tag_reporting_flag2=0` means requested RR
inclusion. At an event with mixing period greater than zero its effective flag
2 is 0, but at a zero-mixing event the implementation forces effective flag 2
to 1. Therefore the split records the requested design treatment, not a claim
that every event-level flag in an inclusion PAR is zero. In this group,
{zero_mixing_models} models have at least one zero-mixing event. Requested
exclusion (`tag_reporting_flag2=1`) has effective flag 2 equal to 1 for every
event.

`SHA256SUMS` covers the {count} PARs, {count} matching INIs and {count} matching
REPs. The complete
mapping, MGC values, archive members, source/destination paths, sizes, hashes,
and zero-mixing fields are in
`../data/ensemble/retained-final-par-rr-split-manifest.csv`. Exact archived INI
provenance is independently locked in
`../data/ensemble/retained-final-ini-manifest.csv`.

Reproduce the split or verify an existing split from the repository root:

```sh
./scripts/split-retained-final-pars-by-reporting.py
./scripts/split-retained-final-pars-by-reporting.py --check
```
""".encode("utf-8")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--check",
        action="store_true",
        help="verify the complete committed split without creating missing artifacts",
    )
    arguments = parser.parse_args()

    repo = Path(__file__).resolve().parent.parent
    source_root = repo / "final-par"
    design_path = repo / "design" / "model-draws.csv"
    fit_path = repo / "data" / "ensemble" / "fit-diagnostics.csv"
    retained_path = repo / "data" / "ensemble" / "retained-final-par-manifest.csv"
    retained_ini_path = repo / "data" / "ensemble" / "retained-final-ini-manifest.csv"
    retained_rep_path = repo / "data" / "ensemble" / "retained-final-rep-manifest.csv"
    mapping_path = repo / "data" / "ensemble" / "retained-final-par-rr-split-manifest.csv"
    output_roots = {
        0: repo / "final-par-rr-inclusion-flag2-0",
        1: repo / "final-par-rr-exclusion-flag2-1",
    }
    group_names = {0: "inclusion", 1: "exclusion"}

    if not source_root.is_dir() or source_root.is_symlink():
        fail(f"authoritative retained-PAR root is missing or linked: {source_root}")

    retained_rows = read_csv(
        retained_path,
        (
            "ensemble_id",
            "final_par_member",
            "final_par_sha256",
            "final_par_bytes",
            "maximum_gradient_component",
            "retention_criterion",
        ),
    )
    retained = rows_by_model(retained_rows, "retained final-PAR manifest", EXPECTED_RETAINED)

    retained_ini_rows = read_csv(
        retained_ini_path,
        (
            "ensemble_id",
            "kflow_job",
            "kflow_task",
            "source_archive",
            "archive_sha256",
            "bet_ini_member",
            "bet_ini_sha256",
            "bet_ini_bytes",
            "bet_model_ini_member",
            "bet_model_ini_sha256",
            "bet_model_ini_bytes",
            "bet_ini_model_ini_byte_identical",
            "source_commit",
        ),
    )
    retained_ini = rows_by_model(
        retained_ini_rows, "retained final-INI manifest", EXPECTED_RETAINED
    )
    if set(retained_ini) != set(retained):
        fail("retained final-PAR and final-INI manifest ensemble IDs differ")

    retained_rep_rows = read_csv(
        retained_rep_path,
        (
            "ensemble_id",
            "source_archive",
            "archive_sha256",
            "final_par_sha256",
            "plot_rep_file",
            "source_rep_member",
            "plot_rep_sha256",
            "plot_rep_bytes",
            "plot_rep_lines",
            "source_commit",
        ),
    )
    retained_rep = rows_by_model(
        retained_rep_rows, "retained final-REP manifest", EXPECTED_RETAINED
    )
    if set(retained_rep) != set(retained):
        fail("retained final-PAR and final-REP manifest ensemble IDs differ")

    fit_rows = read_csv(fit_path, ("ensemble_id", "maximum_gradient"))
    fit = rows_by_model(fit_rows, "fit diagnostics")
    retained_from_mgc = {
        model_id
        for model_id, row in fit.items()
        if parse_decimal(row["maximum_gradient"], f"MGC for {model_id}") <= MGC_LIMIT
    }
    retained_ids = set(retained)
    if len(retained_from_mgc) != EXPECTED_RETAINED or retained_from_mgc != retained_ids:
        fail("MGC <= 1e-4 fit set is not the exact 80-row retained final-PAR manifest set")

    design_rows = read_csv(
        design_path,
        (
            "ensemble_id",
            "tag_reporting_flag2",
            "tag_reporting",
            "tag_mixing_k_cutoff",
            "zero_mixing_events",
            "tag_reporting_zero_mixing_exclusions",
        ),
    )
    design = rows_by_model(design_rows, "authoritative model design", 100)
    if not retained_ids.issubset(design):
        fail("one or more retained ensemble IDs are absent from the authoritative design")

    source_model_ids: set[str] = set()
    for child in source_root.iterdir():
        if child.is_symlink() or not child.is_dir() or MODEL_ID_RE.fullmatch(child.name) is None:
            fail(f"unexpected entry in authoritative retained-PAR root: {child}")
        source_model_ids.add(child.name)
    if source_model_ids != retained_ids:
        fail("authoritative final-par directory IDs differ from the exact retained manifest IDs")

    sorted_retained_ids = sorted(retained_ids)
    materialized_inis = materialize_exact_model_inis(repo, sorted_retained_ids)
    if set(materialized_inis) != retained_ids:
        fail("materialized model-specific INI IDs differ from the exact retained set")

    groups: dict[int, list[dict[str, str | int]]] = {0: [], 1: []}
    for model_id in sorted_retained_ids:
        retained_row = retained[model_id]
        retained_ini_row = retained_ini[model_id]
        retained_rep_row = retained_rep[model_id]
        fit_row = fit[model_id]
        design_row = design[model_id]

        manifest_mgc = parse_decimal(
            retained_row["maximum_gradient_component"], f"manifest MGC for {model_id}"
        )
        fit_mgc = parse_decimal(fit_row["maximum_gradient"], f"fit MGC for {model_id}")
        if manifest_mgc > MGC_LIMIT or abs(manifest_mgc - fit_mgc) > Decimal("1e-15"):
            fail(f"retained manifest and fit-diagnostic MGC differ for {model_id}")
        if retained_row["retention_criterion"] != "public fit ledger maximum_gradient <= 1e-4":
            fail(f"unexpected retention criterion for {model_id}")

        requested_flag = parse_nonnegative_integer(
            design_row["tag_reporting_flag2"], f"tag_reporting_flag2 for {model_id}"
        )
        if requested_flag not in (0, 1):
            fail(f"tag_reporting_flag2 is not 0 or 1 for {model_id}")
        group = group_names[requested_flag]
        if design_row["tag_reporting"] != group:
            fail(f"tag_reporting label and tag_reporting_flag2 disagree for {model_id}")

        zero_mixing_events = parse_nonnegative_integer(
            design_row["zero_mixing_events"], f"zero_mixing_events for {model_id}"
        )
        zero_mixing_exclusions = parse_nonnegative_integer(
            design_row["tag_reporting_zero_mixing_exclusions"],
            f"tag_reporting_zero_mixing_exclusions for {model_id}",
        )
        expected_zero_exclusions = zero_mixing_events if requested_flag == 0 else 0
        if zero_mixing_exclusions != expected_zero_exclusions:
            fail(f"zero-mixing RR exclusions are inconsistent for {model_id}")

        expected_member = f"./outputs/models/{model_id}/final.par"
        if retained_row["final_par_member"] != expected_member:
            fail(f"unexpected archived final.par member for {model_id}")
        expected_size = parse_nonnegative_integer(
            retained_row["final_par_bytes"], f"final_par_bytes for {model_id}"
        )
        expected_sha = retained_row["final_par_sha256"]
        if SHA256_RE.fullmatch(expected_sha) is None:
            fail(f"invalid retained final-PAR SHA-256 for {model_id}")
        source = source_root / model_id / "final.par"
        regular_unlinked_file(source, "authoritative retained final PAR")
        if source.stat().st_size != expected_size or sha256_path(source) != expected_sha:
            fail(f"authoritative retained final PAR differs from its manifest: {model_id}")

        for field in (
            "kflow_job",
            "kflow_task",
            "source_archive",
            "archive_sha256",
            "source_commit",
        ):
            if retained_ini_row[field] != retained_row[field]:
                fail(f"retained PAR and INI archive provenance differ for {model_id}: {field}")
        expected_ini_member = f"./outputs/models/{model_id}/bet.ini"
        expected_model_ini_member = f"./outputs/models/{model_id}/bet.model.ini"
        if (
            retained_ini_row["bet_ini_member"] != expected_ini_member
            or retained_ini_row["bet_model_ini_member"] != expected_model_ini_member
        ):
            fail(f"unexpected archived model-specific INI member for {model_id}")
        ini_sha = retained_ini_row["bet_ini_sha256"]
        ini_size = parse_nonnegative_integer(
            retained_ini_row["bet_ini_bytes"], f"bet_ini_bytes for {model_id}"
        )
        model_ini_size = parse_nonnegative_integer(
            retained_ini_row["bet_model_ini_bytes"], f"bet_model_ini_bytes for {model_id}"
        )
        if (
            SHA256_RE.fullmatch(ini_sha) is None
            or retained_ini_row["bet_model_ini_sha256"] != ini_sha
            or model_ini_size != ini_size
            or retained_ini_row["bet_ini_model_ini_byte_identical"] != "TRUE"
        ):
            fail(f"archived bet.ini/bet.model.ini identity is invalid for {model_id}")
        ini_payload = materialized_inis[model_id]
        if len(ini_payload) != ini_size or sha256_bytes(ini_payload) != ini_sha:
            fail(
                f"scientifically materialized bet.ini differs from the authoritative "
                f"Kflow archive for {model_id}"
            )

        for field in ("source_archive", "archive_sha256", "source_commit"):
            if retained_rep_row[field] != retained_row[field]:
                fail(f"retained PAR and REP archive provenance differ for {model_id}: {field}")
        if retained_rep_row["final_par_sha256"] != expected_sha:
            fail(f"retained REP manifest references a different final PAR for {model_id}")
        expected_rep_file = f"{model_id}/plot-11.par.rep"
        expected_rep_member = f"./outputs/models/{model_id}/plot-11.par.rep"
        if (
            retained_rep_row["plot_rep_file"] != expected_rep_file
            or retained_rep_row["source_rep_member"] != expected_rep_member
        ):
            fail(f"unexpected retained REP path or archive member for {model_id}")
        rep_sha = retained_rep_row["plot_rep_sha256"]
        rep_size = parse_nonnegative_integer(
            retained_rep_row["plot_rep_bytes"], f"plot_rep_bytes for {model_id}"
        )
        rep_lines = parse_nonnegative_integer(
            retained_rep_row["plot_rep_lines"], f"plot_rep_lines for {model_id}"
        )
        if SHA256_RE.fullmatch(rep_sha) is None or rep_lines != 6349:
            fail(f"invalid retained REP hash or line count for {model_id}")
        rep_source = source_root / model_id / "plot-11.par.rep"
        regular_unlinked_file(rep_source, "authoritative retained Phase 11 REP")
        if rep_source.stat().st_size != rep_size or sha256_path(rep_source) != rep_sha:
            fail(f"authoritative retained REP differs from its manifest: {model_id}")

        groups[requested_flag].append(
            {
                "ensemble_id": model_id,
                "rr_group": group,
                "requested_tag_reporting_flag2": requested_flag,
                "effective_flag2_when_mixing_period_positive": requested_flag,
                "effective_flag2_when_mixing_period_zero": 1,
                "tag_mixing_k_cutoff": design_row["tag_mixing_k_cutoff"],
                "zero_mixing_events": zero_mixing_events,
                "tag_reporting_zero_mixing_exclusions": zero_mixing_exclusions,
                "maximum_gradient_component": retained_row["maximum_gradient_component"],
                "retention_criterion": retained_row["retention_criterion"],
                "source_archive": retained_ini_row["source_archive"],
                "archive_sha256": retained_ini_row["archive_sha256"],
                "final_par_source_path": f"final-par/{model_id}/final.par",
                "final_par_split_path": f"{output_roots[requested_flag].name}/{model_id}/final.par",
                "final_par_bytes": expected_size,
                "final_par_sha256": expected_sha,
                "bet_ini_archive_member": retained_ini_row["bet_ini_member"],
                "bet_model_ini_archive_member": retained_ini_row["bet_model_ini_member"],
                "archived_bet_ini_model_ini_byte_identical": "TRUE",
                "bet_ini_split_path": f"{output_roots[requested_flag].name}/{model_id}/bet.ini",
                "bet_ini_bytes": ini_size,
                "bet_ini_sha256": ini_sha,
                "plot_rep_source_path": f"final-par/{model_id}/plot-11.par.rep",
                "plot_rep_split_path": (
                    f"{output_roots[requested_flag].name}/{model_id}/plot-11.par.rep"
                ),
                "plot_rep_archive_member": retained_rep_row["source_rep_member"],
                "plot_rep_bytes": rep_size,
                "plot_rep_lines": rep_lines,
                "plot_rep_sha256": rep_sha,
            }
        )

    for flag, expected_count in EXPECTED_GROUP_COUNTS.items():
        if len(groups[flag]) != expected_count:
            fail(
                f"{group_names[flag]} flag2={flag} group has {len(groups[flag])} rows; "
                f"expected {expected_count}"
            )
    inclusion_ids = {str(row["ensemble_id"]) for row in groups[0]}
    exclusion_ids = {str(row["ensemble_id"]) for row in groups[1]}
    if inclusion_ids & exclusion_ids or inclusion_ids | exclusion_ids != retained_ids:
        fail("RR groups overlap or do not form the exact retained 80-model union")

    mapping_fields = (
        "ensemble_id",
        "rr_group",
        "requested_tag_reporting_flag2",
        "effective_flag2_when_mixing_period_positive",
        "effective_flag2_when_mixing_period_zero",
        "tag_mixing_k_cutoff",
        "zero_mixing_events",
        "tag_reporting_zero_mixing_exclusions",
        "maximum_gradient_component",
        "retention_criterion",
        "source_archive",
        "archive_sha256",
        "final_par_source_path",
        "final_par_split_path",
        "final_par_bytes",
        "final_par_sha256",
        "bet_ini_archive_member",
        "bet_model_ini_archive_member",
        "archived_bet_ini_model_ini_byte_identical",
        "bet_ini_split_path",
        "bet_ini_bytes",
        "bet_ini_sha256",
        "plot_rep_source_path",
        "plot_rep_split_path",
        "plot_rep_archive_member",
        "plot_rep_bytes",
        "plot_rep_lines",
        "plot_rep_sha256",
    )
    mapping_rows = [
        {field: str(row[field]) for field in mapping_fields}
        for flag in (0, 1)
        for row in groups[flag]
    ]
    mapping_bytes = csv_payload(mapping_fields, mapping_rows)

    for flag in (0, 1):
        model_ids = [str(row["ensemble_id"]) for row in groups[flag]]
        inspect_output_inventory(output_roots[flag], model_ids, require_complete=arguments.check)
    if arguments.check:
        regular_unlinked_file(mapping_path, "RR split mapping manifest")
        if mapping_path.read_bytes() != mapping_bytes:
            fail(f"RR split mapping manifest is not reproducible: {mapping_path}")
    else:
        if not mapping_path.parent.is_dir() or mapping_path.parent.is_symlink():
            fail(f"mapping manifest parent is missing or linked: {mapping_path.parent}")

    for flag in (0, 1):
        root = output_roots[flag]
        rows = groups[flag]
        model_ids = [str(row["ensemble_id"]) for row in rows]
        if not arguments.check:
            ensure_directory(root)
            for model_id in model_ids:
                ensure_directory(root / model_id)

        sha_lines: list[str] = []
        for row in rows:
            model_id = str(row["ensemble_id"])
            expected_size = int(row["final_par_bytes"])
            expected_sha = str(row["final_par_sha256"])
            source = source_root / model_id / "final.par"
            destination = root / model_id / "final.par"
            if arguments.check:
                regular_unlinked_file(destination, "split PAR")
                if destination.stat().st_size != expected_size or sha256_path(destination) != expected_sha:
                    fail(f"split PAR is not byte-identical to its source: {destination}")
            else:
                copy_without_overwrite(source, destination, expected_size, expected_sha)

            ini_payload = materialized_inis[model_id]
            ini_size = int(row["bet_ini_bytes"])
            ini_sha = str(row["bet_ini_sha256"])
            ini_destination = root / model_id / "bet.ini"
            if arguments.check:
                regular_unlinked_file(ini_destination, "split model-specific INI")
                if (
                    ini_destination.stat().st_size != ini_size
                    or sha256_path(ini_destination) != ini_sha
                    or ini_destination.read_bytes() != ini_payload
                ):
                    fail(
                        f"split bet.ini is not byte-identical to its archived and "
                        f"scientifically materialized source: {ini_destination}"
                    )
            else:
                install_bytes_without_overwrite(ini_destination, ini_payload)

            rep_size = int(row["plot_rep_bytes"])
            rep_sha = str(row["plot_rep_sha256"])
            rep_source = source_root / model_id / "plot-11.par.rep"
            rep_destination = root / model_id / "plot-11.par.rep"
            if arguments.check:
                regular_unlinked_file(rep_destination, "split Phase 11 REP")
                if (
                    rep_destination.stat().st_size != rep_size
                    or sha256_path(rep_destination) != rep_sha
                ):
                    fail(f"split REP is not byte-identical to its source: {rep_destination}")
            else:
                copy_without_overwrite(rep_source, rep_destination, rep_size, rep_sha)
            sha_lines.append(f"{ini_sha}  {model_id}/bet.ini\n")
            sha_lines.append(f"{expected_sha}  {model_id}/final.par\n")
            sha_lines.append(f"{rep_sha}  {model_id}/plot-11.par.rep\n")

        zero_mixing_models = sum(int(row["zero_mixing_events"]) > 0 for row in rows)
        readme = readme_payload(
            group_names[flag], flag, EXPECTED_GROUP_COUNTS[flag], zero_mixing_models
        )
        sums = "".join(sha_lines).encode("ascii")
        if arguments.check:
            for path, expected in ((root / "README.md", readme), (root / "SHA256SUMS", sums)):
                regular_unlinked_file(path, "generated split metadata")
                if path.read_bytes() != expected:
                    fail(f"generated split metadata is not reproducible: {path}")
        else:
            replace_generated_bytes(root / "README.md", readme)
            replace_generated_bytes(root / "SHA256SUMS", sums)

        inspect_output_inventory(root, model_ids, require_complete=True)

    if not arguments.check:
        replace_generated_bytes(mapping_path, mapping_bytes)
    regular_unlinked_file(mapping_path, "RR split mapping manifest")
    if mapping_path.read_bytes() != mapping_bytes:
        fail(f"generated mapping manifest verification failed: {mapping_path}")

    action = "Verified" if arguments.check else "Materialized and verified"
    print(f"{action} retained final-PAR RR split.")
    for flag in (0, 1):
        root = output_roots[flag]
        par_bytes = sum(int(row["final_par_bytes"]) for row in groups[flag])
        ini_bytes = sum(int(row["bet_ini_bytes"]) for row in groups[flag])
        rep_bytes = sum(int(row["plot_rep_bytes"]) for row in groups[flag])
        sums_sha = sha256_path(root / "SHA256SUMS")
        print(
            f"  {group_names[flag]} flag2={flag}: {len(groups[flag])} PAR+INI+REP triplets, "
            f"PAR bytes={par_bytes}, INI bytes={ini_bytes}, REP bytes={rep_bytes}, "
            f"SHA256SUMS sha256={sums_sha}, path={root.relative_to(repo)}"
        )
    print(
        "  overlap=0; union=80; PARs and REPs match final-par sources; INIs match "
        "archived bet.ini and bet.model.ini bytes"
    )
    print(
        "  mapping: 80 rows, "
        f"sha256={sha256_path(mapping_path)}, path={mapping_path.relative_to(repo)}"
    )


if __name__ == "__main__":
    main()
