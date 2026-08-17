#!/usr/bin/env python3
"""Audit retained model INIs in Kflow archives and emit a provenance CSV.

This program is sent to the Suva submit host. It only reads the declared
archives, verifies their SHA-256 identities, and writes CSV to stdout.
"""

import base64
import csv
import hashlib
import io
import os
import re
import sys
import tarfile


def fail(message):
    raise SystemExit(message)


def sha256_path(path):
    digest = hashlib.sha256()
    with open(path, "rb") as source:
        for block in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def regular_member_bytes(archive, member_name, model_id):
    try:
        member = archive.getmember(member_name)
    except KeyError:
        fail("Missing " + member_name + " for " + model_id)
    if not member.isfile() or member.issym() or member.islnk():
        fail(member_name + " is not a regular archive member for " + model_id)
    extracted = archive.extractfile(member)
    if extracted is None:
        fail("Could not read " + member_name + " for " + model_id)
    return extracted.read()


if len(sys.argv) != 3:
    fail("Usage: python3 - REMOTE_TASK_ROOT BASE64_RETAINED_PAR_MANIFEST")

task_root = os.path.realpath(sys.argv[1])
if not re.fullmatch(r"/home/[A-Za-z0-9_-]+/KflowOutput/bet-2026-ensemble-tau", task_root):
    fail("Refusing unexpected Kflow task root")
if not os.path.isdir(task_root) or os.path.islink(task_root):
    fail("Kflow task root is missing or is a symbolic link")

try:
    manifest_text = base64.b64decode(sys.argv[2], validate=True).decode("utf-8")
except Exception as error:
    fail("Invalid encoded manifest: " + str(error))

rows = list(csv.DictReader(io.StringIO(manifest_text)))
if len(rows) != 80 or len({row.get("ensemble_id") for row in rows}) != 80:
    fail("Expected exactly 80 unique retained manifest rows")
rows.sort(key=lambda row: row["ensemble_id"])

fieldnames = (
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
)
writer = csv.DictWriter(sys.stdout, fieldnames=fieldnames, lineterminator="\n")
writer.writeheader()

for row in rows:
    model_id = row.get("ensemble_id", "")
    source_archive = row.get("source_archive", "")
    if not re.fullmatch(r"ensemble-[0-9]{3}", model_id):
        fail("Invalid ensemble ID in retained manifest")
    if not re.fullmatch(r"job-[0-9]{6}/output_archive[.]tar[.]gz", source_archive):
        fail("Invalid source archive for " + model_id)
    expected_archive = "job-%06d/output_archive.tar.gz" % int(row["kflow_job"])
    if source_archive != expected_archive:
        fail("Archive/job mapping mismatch for " + model_id)

    archive_path = os.path.realpath(os.path.join(task_root, source_archive))
    if os.path.commonpath([task_root, archive_path]) != task_root:
        fail("Archive escapes task root for " + model_id)
    if not os.path.isfile(archive_path) or os.path.islink(archive_path):
        fail("Archive is missing or linked for " + model_id)
    if sha256_path(archive_path) != row["archive_sha256"]:
        fail("Archive SHA-256 mismatch for " + model_id)

    prefix = "./outputs/models/%s/" % model_id
    bet_ini_member = prefix + "bet.ini"
    bet_model_ini_member = prefix + "bet.model.ini"
    with tarfile.open(archive_path, mode="r:gz") as archive:
        bet_ini = regular_member_bytes(archive, bet_ini_member, model_id)
        bet_model_ini = regular_member_bytes(archive, bet_model_ini_member, model_id)
    if bet_ini != bet_model_ini:
        fail("Archived bet.ini and makepar bet.model.ini differ for " + model_id)

    ini_sha = hashlib.sha256(bet_ini).hexdigest()
    writer.writerow(
        {
            "ensemble_id": model_id,
            "kflow_job": row["kflow_job"],
            "kflow_task": row["kflow_task"],
            "source_archive": source_archive,
            "archive_sha256": row["archive_sha256"],
            "bet_ini_member": bet_ini_member,
            "bet_ini_sha256": ini_sha,
            "bet_ini_bytes": len(bet_ini),
            "bet_model_ini_member": bet_model_ini_member,
            "bet_model_ini_sha256": ini_sha,
            "bet_model_ini_bytes": len(bet_model_ini),
            "bet_ini_model_ini_byte_identical": "TRUE",
            "source_commit": row["source_commit"],
        }
    )
