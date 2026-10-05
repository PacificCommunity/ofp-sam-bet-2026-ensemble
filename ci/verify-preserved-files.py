#!/usr/bin/env python3
"""Check the exact files retained from the published assessment source."""
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import sys
import tarfile

root = Path(__file__).resolve().parent.parent
manifest = json.loads((root / "ci/preserved-files.json").read_text())
failures = []
for record in manifest["files"] + manifest.get("approved_additions", []):
    relative = PurePosixPath(record["path"])
    if relative.is_absolute() or ".." in relative.parts:
        failures.append(f"Invalid preserved path: {relative}")
        continue
    path = root.joinpath(*relative.parts)
    try:
        if record["mode"] == "120000":
            if not path.is_symlink():
                raise ValueError("expected a symbolic link")
            content = os.readlink(path).encode()
        else:
            if path.is_symlink() or not path.is_file():
                raise ValueError("expected a regular file")
            content = path.read_bytes()
            executable = bool(path.stat().st_mode & 0o111)
            if executable != (record["mode"] == "100755"):
                raise ValueError("executable mode changed")
        observed = hashlib.sha256(content).hexdigest()
        if observed != record["sha256"]:
            raise ValueError("SHA256 changed")
    except (OSError, ValueError) as error:
        failures.append(f"{relative}: {error}")
if failures:
    print("\n".join(failures), file=sys.stderr)
    raise SystemExit(1)
print(f'Preserved {len(manifest["files"])} files from {manifest["source_commit"]}.')

# Original model configurations for retained ensemble and RR1 fits.
saved = json.loads((root / "reproduce/refit-configs.json").read_text())
archive = root / "reproduce" / saved["archive"]["path"]
if archive.stat().st_size != saved["archive"]["bytes"] or hashlib.sha256(archive.read_bytes()).hexdigest() != saved["archive"]["sha256"]:
    raise SystemExit("Model configuration archive changed")
with tarfile.open(archive, "r:gz") as bundle:
    members = bundle.getmembers()
    expected = saved["files"]
    if len(members) != len(expected) or len({m.name for m in members}) != len(members):
        raise SystemExit("Model configuration member count changed")
    for member, record in zip(members, expected):
        p = PurePosixPath(member.name)
        if p.is_absolute() or ".." in p.parts or not member.isfile() or member.name != record["path"] or member.size != record["bytes"] or member.mode != record["mode"]:
            raise SystemExit("Invalid model configuration member")
        if hashlib.sha256(bundle.extractfile(member).read()).hexdigest() != record["sha256"]:
            raise SystemExit("Model configuration member changed")
print(f"Verified {len(expected)} original model configurations.")
