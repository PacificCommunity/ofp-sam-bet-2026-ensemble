#!/usr/bin/env python3
"""Stream checksum-verified retained PARs from the authoritative Kflow task.

This program is sent to the Suva submit host by the maintainer-only fetch
wrapper. It reads archives and writes a deterministic tar.gz to stdout. It
does not create, update or remove any remote file.
"""

import base64
import csv
import gzip
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


if len(sys.argv) != 3:
    fail("Usage: python3 - REMOTE_TASK_ROOT BASE64_MANIFEST")

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

gzip_stream = gzip.GzipFile(
    filename="", mode="wb", compresslevel=6, fileobj=sys.stdout.buffer, mtime=0
)
with gzip_stream:
    with tarfile.open(fileobj=gzip_stream, mode="w|") as output_tar:
        for row in rows:
            model_id = row.get("ensemble_id", "")
            source_archive = row.get("source_archive", "")
            source_member = row.get("final_par_member", "")
            if not re.fullmatch(r"ensemble-[0-9]{3}", model_id):
                fail("Invalid ensemble ID in manifest")
            if not re.fullmatch(r"job-[0-9]{6}/output_archive[.]tar[.]gz", source_archive):
                fail("Invalid source archive in manifest for " + model_id)
            expected_archive = "job-%06d/output_archive.tar.gz" % int(row["kflow_job"])
            expected_member = "./outputs/models/%s/final.par" % model_id
            if source_archive != expected_archive or source_member != expected_member:
                fail("Archive/member mapping mismatch for " + model_id)

            archive = os.path.realpath(os.path.join(task_root, source_archive))
            if os.path.commonpath([task_root, archive]) != task_root:
                fail("Archive escapes task root for " + model_id)
            if not os.path.isfile(archive) or os.path.islink(archive):
                fail("Archive is missing or linked for " + model_id)
            if sha256_path(archive) != row["archive_sha256"]:
                fail("Archive SHA-256 mismatch for " + model_id)

            final_data = None
            with tarfile.open(archive, mode="r|gz") as source_tar:
                for member in source_tar:
                    if member.name != source_member:
                        continue
                    if not member.isfile() or member.issym() or member.islnk():
                        fail("final.par member is not a regular file for " + model_id)
                    extracted = source_tar.extractfile(member)
                    if extracted is None:
                        fail("Could not read final.par for " + model_id)
                    final_data = extracted.read()
                    break
            if final_data is None:
                fail("Missing final.par member for " + model_id)
            if len(final_data) != int(row["final_par_bytes"]):
                fail("final.par size mismatch for " + model_id)
            if hashlib.sha256(final_data).hexdigest() != row["final_par_sha256"]:
                fail("final.par SHA-256 mismatch for " + model_id)

            info = tarfile.TarInfo(name=model_id + "/final.par")
            info.size = len(final_data)
            info.mode = 0o644
            info.mtime = 0
            info.uid = 0
            info.gid = 0
            info.uname = "root"
            info.gname = "root"
            output_tar.addfile(info, io.BytesIO(final_data))
