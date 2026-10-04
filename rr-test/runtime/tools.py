#!/usr/bin/env python3
"""Verify and inspect preserved archive bytes; never execute their contents.

objects.json: schema_version=1, bundles[{id,path,bytes,sha256,url?}],
archives[{id,bytes,sha256,members_manifest,parts[{bundle,ordinal,member,
offset,bytes,sha256}]}]. Parts are ordered, contiguous original-byte segments.
Member JSONL (optionally .gz): ordinal,name,type,bytes,sha256,mode,linkname;
mtime/uid/gid/pax_headers are compared when supplied. Catalog jobs use job_key,
archive_id, source_identity, status, gaps and optional terminal_par/scripts refs.
Each supplied ref may override archive_id; absence uses the job's primary archive.
"""
import argparse
import gzip
import hashlib
import io
import json
import os
from pathlib import Path, PurePosixPath
import re
import secrets
import stat
import sys
import tarfile
import tempfile
from urllib.parse import urlsplit
from urllib.request import urlopen


BLOCK = 1024 * 1024
UNKNOWN = {"", "UNKNOWN", "unknown", None}


def require(ok, message):
    if not ok:
        raise ValueError(message)


def integer(value, label):
    require(type(value) is int and value >= 0, f"invalid {label}")
    return value


def digest(value, label):
    require(isinstance(value, str) and re.fullmatch(r"[0-9a-f]{64}", value),
            f"missing/UNKNOWN or invalid SHA256: {label}")
    return value


def local(root, name):
    require(isinstance(name, str) and name and not Path(name).is_absolute(),
            "local path must be relative")
    require(".." not in Path(name).parts, "local path contains traversal")
    result = (root / name).resolve()
    require(result.is_relative_to(root.resolve()), "local path escapes root")
    return result


def read_json(path):
    with path.open(encoding="utf-8") as handle:
        return json.load(handle)


def unique(rows, key):
    result = {}
    for row in rows:
        value = row[key]
        require(isinstance(value, str) and value not in UNKNOWN and value not in result,
                f"missing/duplicate {key}")
        result[value] = row
    return result


def check_file(path, record):
    expected = digest(record.get("sha256"), str(path))
    size = integer(record.get("bytes"), "object bytes")
    require(path.is_file(), f"missing file: {path}")
    require(path.stat().st_size == size, f"size mismatch: {path}")
    sha = hashlib.sha256()
    with path.open("rb") as handle:
        while chunk := handle.read(BLOCK):
            sha.update(chunk)
    require(sha.hexdigest() == expected, f"SHA256 mismatch: {path}")


class ChunkReader(io.RawIOBase):
    """Expose a bounded chunk iterator as an unseekable tar input."""
    def __init__(self, chunks):
        self.chunks = iter(chunks)
        self.pending = memoryview(b"")

    def readable(self):
        return True

    def readinto(self, target):
        if not self.pending:
            self.pending = memoryview(next(self.chunks, b""))
        count = min(len(target), len(self.pending))
        target[:count] = self.pending[:count]
        self.pending = self.pending[count:]
        return count


class Store:
    def __init__(self, root, objects, catalog):
        self.root = root.resolve()
        data = read_json(objects)
        require(data.get("schema_version") == 1, "unsupported objects schema")
        self.bundles = unique(data["bundles"], "id")
        self.archives = unique(data["archives"], "id")
        self.checked = set()
        self.checked_manifests = set()
        self.jobs = {}
        if catalog.exists():
            data = read_json(catalog)
            require(data.get("schema_version") == 1, "unsupported catalog schema")
            self.jobs = unique(data["jobs"], "job_key")

    def bundle(self, name):
        require(name in self.bundles, f"missing/UNKNOWN bundle: {name}")
        row = self.bundles[name]
        path = local(self.root, row["path"])
        if name not in self.checked:
            check_file(path, row)
            self.checked.add(name)
        return path

    def select(self, job, archive):
        if job:
            require(job in self.jobs, f"unknown job selector: {job}")
            archive = self.jobs[job].get("archive_id")
        require(isinstance(archive, str) and archive not in UNKNOWN and archive in self.archives,
                f"missing/UNKNOWN archive reference: {job or archive}")
        return self.archives[archive]

    def chunks(self, archive):
        """Check part hashes and reconstructed original archive, including trailers."""
        expected = digest(archive.get("sha256"), archive["id"])
        size = integer(archive.get("bytes"), "archive bytes")
        whole, offset = hashlib.sha256(), 0
        require(archive.get("parts"), f"no archive parts: {archive['id']}")
        for part in archive["parts"]:
            require(integer(part.get("offset"), "part offset") == offset,
                    f"noncontiguous/reordered archive parts: {archive['id']}")
            expected_part = digest(part.get("sha256"), "archive part")
            count = integer(part.get("bytes"), "part bytes")
            ordinal = integer(part.get("ordinal"), "outer member ordinal")
            path = self.bundle(part.get("bundle"))
            found = False
            with tarfile.open(path, "r:*") as outer:
                for index, member in enumerate(outer):
                    if index != ordinal:
                        continue
                    require(member.name == part.get("member") and member.isfile(),
                            "outer member name/type mismatch")
                    require(member.size == count, "outer member size mismatch")
                    hashed, received = hashlib.sha256(), 0
                    with outer.extractfile(member) as handle:
                        while chunk := handle.read(BLOCK):
                            hashed.update(chunk)
                            whole.update(chunk)
                            received += len(chunk)
                            yield chunk
                    require(received == count and hashed.hexdigest() == expected_part,
                            "original archive part SHA256/size mismatch")
                    found = True
                    break
            require(found, "missing outer member ordinal")
            offset += count
        require(offset == size and whole.hexdigest() == expected,
                f"original archive SHA256/size mismatch: {archive['id']}")

    def member_rows(self, archive):
        path = local(self.root, archive.get("members_manifest"))
        if archive["id"] not in self.checked_manifests:
            if "members_manifest_sha256" in archive:
                check_file(path, {"sha256": archive["members_manifest_sha256"],
                                 "bytes": archive.get("members_manifest_bytes", path.stat().st_size)})
            elif "members_manifest_bytes" in archive:
                require(path.stat().st_size == integer(archive["members_manifest_bytes"], "manifest bytes"),
                        "member manifest size mismatch")
            self.checked_manifests.add(archive["id"])
        opener = gzip.open if path.suffix == ".gz" else open
        with opener(path, "rt", encoding="utf-8") as handle:
            for line in handle:
                require(line.strip(), "blank member manifest row")
                yield json.loads(line)

    def members(self, archive, visit=None):
        """Streaming member verification; no filesystem extraction by this method."""
        rows = iter(self.member_rows(archive))
        raw = io.BufferedReader(ChunkReader(self.chunks(archive)), buffer_size=64 * 1024)
        count = 0
        try:
            with tarfile.open(fileobj=raw, mode="r|*") as source:
                for ordinal, member in enumerate(source):
                    row = next(rows, None)
                    require(row is not None, "archive has extra member")
                    for key in ("ordinal", "bytes", "mode"):
                        integer(row.get(key), f"member {key}")
                    kind = ("file" if member.isfile() else "dir" if member.isdir()
                            else "symlink" if member.issym() else "hardlink" if member.islnk()
                            else "other")
                    actual = {"ordinal": ordinal, "name": member.name, "type": kind,
                              "bytes": member.size, "mode": member.mode,
                              "linkname": member.linkname}
                    for key, value in actual.items():
                        require(row.get(key) == value, f"member {ordinal} {key} mismatch")
                    for key in ("mtime", "uid", "gid", "pax_headers"):
                        if key in row:
                            require(row[key] == getattr(member, key), f"member {ordinal} {key} mismatch")
                    payload = source.extractfile(member) if member.isfile() else None
                    sha, received = hashlib.sha256(), 0
                    # The visitor consumes a wrapped payload so hashes still cover every byte.
                    def data_chunks():
                        nonlocal received
                        if payload is not None:
                            while chunk := payload.read(BLOCK):
                                sha.update(chunk)
                                received += len(chunk)
                                yield chunk
                    content = data_chunks()
                    if visit is not None:
                        visit(row, content)
                    for _ in content:
                        pass
                    if payload is not None:
                        payload.close()
                        require(received == member.size and sha.hexdigest() == digest(row.get("sha256"), "member"),
                                f"member {ordinal} SHA256/size mismatch")
                    else:
                        require(row.get("sha256") is None, "nonfile member has content hash")
                    count += 1
                    source.members.clear()  # Python 3.10 otherwise caches all streamed TarInfo objects.
            require(next(rows, None) is None, "manifest has omitted/missing archive member")
            # Tar readers may stop at their zero blocks before original compressed trailers.
            while raw.read(BLOCK):
                pass
        finally:
            raw.close()
        return count


def safe_name(row):
    name = row["name"]
    require(isinstance(name, str) and "\x00" not in name and "\\" not in name,
            "unsafe member name")
    path = PurePosixPath(name)
    require(not path.is_absolute() and ".." not in path.parts,
            f"unsafe archive traversal: ordinal {row['ordinal']}")
    require(not any(re.match(r"^[A-Za-z]:", part) for part in path.parts),
            "restore refuses Windows drive paths")
    require(row["type"] in {"file", "dir"}, "restore refuses links/special members")
    require(path.parts or row["type"] == "dir", "empty file path")
    return path


def restore(store, archive, output):
    require(not output.exists() and not output.is_symlink(), "restore output already exists")
    seen, parents = set(), set()
    def preflight(row, _):
        path = safe_name(row)
        require(path not in seen, "restore refuses duplicate output paths")
        require(row["type"] != "file" or path not in parents, "file/directory path conflict")
        for parent in path.parents:
            require(parent not in seen or parent in parents, "file parent path conflict")
            parents.add(parent)
        seen.add(path)
        if row["type"] == "dir":
            parents.add(path)
    store.members(archive, preflight)
    output.mkdir(parents=False, exist_ok=False)
    def write(row, content):
        path = safe_name(row)
        target = output.joinpath(*path.parts)
        if row["type"] == "dir":
            target.mkdir(parents=True, exist_ok=True)
        else:
            target.parent.mkdir(parents=True, exist_ok=True)
            with target.open("xb") as handle:
                for chunk in content:
                    handle.write(chunk)
            target.chmod(row["mode"] & 0o777)
    store.members(archive, write)
    print(f"RESTORED {archive['id']} -> {output}; contents were not executed")


def download(store, names):
    for name in names:
        row = store.bundles[name]
        path = local(store.root, row["path"])
        if path.exists():
            check_file(path, row)
            print(f"VERIFIED existing bundle {name}")
            continue
        url = row.get("url")
        parsed = urlsplit(url or "")
        require(parsed.scheme == "https" and parsed.netloc and not parsed.username and not parsed.password,
                "download requires an explicit HTTPS URL without credentials")
        size = integer(row.get("bytes"), "bundle bytes")
        digest(row.get("sha256"), "bundle")
        path.parent.mkdir(parents=True, exist_ok=True)
        temporary = None
        try:
            with tempfile.NamedTemporaryFile(dir=path.parent, prefix=".download-", delete=False) as out:
                temporary = Path(out.name)
                received = 0
                with urlopen(url, timeout=60) as source:
                    require(urlsplit(source.geturl()).scheme == "https", "download redirected outside HTTPS")
                    while chunk := source.read(BLOCK):
                        received += len(chunk)
                        require(received <= size, "download exceeds expected size")
                        out.write(chunk)
            check_file(temporary, row)
            os.link(temporary, path)  # Atomic, exclusive: never overwrite existing bytes.
            print(f"DOWNLOADED and VERIFIED bundle {name}")
        finally:
            if temporary is not None:
                temporary.unlink(missing_ok=True)


def archive_selection(store, key):
    archive = store.select(None, key)
    digest(archive.get("sha256"), archive["id"])
    size = integer(archive.get("bytes"), "archive bytes")
    require(isinstance(archive.get("parts"), list) and archive["parts"], "missing archive parts")
    names, seen, offset = [], set(), 0
    for part in archive["parts"]:
        require(isinstance(part, dict), "invalid archive part")
        require(integer(part.get("offset"), "part offset") == offset,
                "noncontiguous/reordered archive parts")
        offset += integer(part.get("bytes"), "part bytes")
        digest(part.get("sha256"), "archive part")
        ordinal = integer(part.get("ordinal"), "outer member ordinal")
        bundle = part.get("bundle")
        require(isinstance(bundle, str) and bundle in store.bundles, "missing/UNKNOWN selected bundle")
        require((bundle, ordinal) not in seen, "duplicate selected outer member ordinal")
        seen.add((bundle, ordinal))
        require(isinstance(part.get("member"), str) and part["member"], "missing part member name")
        digest(store.bundles[bundle].get("sha256"), "selected bundle")
        integer(store.bundles[bundle].get("bytes"), "selected bundle bytes")
        if bundle not in names:
            names.append(bundle)
    require(offset == size, "archive part byte coverage mismatch")
    return archive, names


def output_parent(output):
    name = os.fspath(output)
    require(name and "\x00" not in name and "\\" not in name, "unsafe output path")
    parts = name.split("/")
    require(".." not in parts and parts[-1] not in {"", "."}, "unsafe output traversal/filename")
    parts = [part for part in parts if part not in {"", "."}]
    require(parts and not any(re.match(r"^[A-Za-z]:", p) for p in parts), "unsafe output path")
    fd = os.open("/" if name.startswith("/") else ".", os.O_RDONLY | os.O_DIRECTORY)
    try:
        for part in parts[:-1]:
            child = os.open(part, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=fd)
            os.close(fd)
            fd = child
        return fd, parts[-1]
    except BaseException:
        os.close(fd)
        raise


def file_signature(info):
    return (info.st_dev, info.st_ino, info.st_mode, info.st_size, info.st_mtime_ns, info.st_ctime_ns)


def unlink_owned(parent, name, identity):
    try:
        current = os.stat(name, dir_fd=parent, follow_symlinks=False)
    except FileNotFoundError:
        return
    if (current.st_dev, current.st_ino) == identity:
        os.unlink(name, dir_fd=parent)


def save_archive(store, key, output):
    """Save exact original compressed bytes, without unpacking or executing them."""
    archive, names = archive_selection(store, key)
    parent, target = output_parent(output)
    temporary, owned = None, None
    try:
        try:
            os.stat(target, dir_fd=parent, follow_symlinks=False)
        except FileNotFoundError:
            pass
        else:
            raise ValueError("save-archive output already exists")
        sources = {}
        for name in names:
            path = local(store.root, store.bundles[name]["path"])
            before = file_signature(path.stat())
            check_file(path, store.bundles[name])
            require(file_signature(path.stat()) == before, "bundle changed during preflight")
            sources[name] = (path, before)
            store.checked.add(name)
        temporary = ".save-archive-" + secrets.token_hex(16)
        fd = os.open(temporary, os.O_RDWR | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                     0o600, dir_fd=parent)
        info = os.fstat(fd)
        owned = (info.st_dev, info.st_ino)
        with os.fdopen(fd, "w+b") as handle:
            for chunk in store.chunks(archive):
                handle.write(chunk)
            handle.flush()
            os.fsync(handle.fileno())
            handle.seek(0)
            saved, count = hashlib.sha256(), 0
            while chunk := handle.read(BLOCK):
                saved.update(chunk)
                count += len(chunk)
            require(count == archive["bytes"] and saved.hexdigest() == archive["sha256"],
                    "saved original archive SHA256/size mismatch")
            for name, (path, before) in sources.items():
                require(file_signature(path.stat()) == before, "bundle changed during assembly")
                check_file(path, store.bundles[name])
                require(file_signature(path.stat()) == before, "bundle changed during postcheck")
            os.link(temporary, target, src_dir_fd=parent, dst_dir_fd=parent, follow_symlinks=False)
            final = os.open(target, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK, dir_fd=parent)
            with os.fdopen(final, "rb") as result:
                before = os.fstat(result.fileno())
                require(stat.S_ISREG(before.st_mode) and (before.st_dev, before.st_ino) == owned,
                        "saved output inode changed")
                sha, received = hashlib.sha256(), 0
                while chunk := result.read(BLOCK):
                    sha.update(chunk)
                    received += len(chunk)
                require(file_signature(os.fstat(result.fileno())) == file_signature(before),
                        "saved output changed during verification")
                require(received == archive["bytes"] and sha.hexdigest() == archive["sha256"],
                        "final saved archive SHA256/size mismatch")
    except BaseException:
        if owned is not None:
            unlink_owned(parent, target, owned)
        raise
    finally:
        try:
            if temporary is not None and owned is not None:
                unlink_owned(parent, temporary, owned)
        finally:
            os.close(parent)
    print(f"SAVED exact archive {archive['id']} -> {output}; SHA256 {archive['sha256']}")


def job_selection(store, key):
    require(key in store.jobs, f"unknown job selector: {key}")
    job = store.jobs[key]
    require(isinstance(job.get("source_identity"), dict), "missing/UNKNOWN canonical source identity object")
    primary = store.select(key, None)
    check_references(store, job)
    archives = [primary["id"]]
    refs = ([job["terminal_par"]] if "terminal_par" in job else [])
    for field in ("scripts", "recovered_objects"):
        require(isinstance(job.get(field, []), list), f"invalid {field} references")
        if field == "recovered_objects":
            require(all(isinstance(ref, dict) and "archive_id" in ref for ref in job.get(field, [])),
                    "missing/UNKNOWN recovery archive reference")
        refs.extend(job.get(field, []))
    for ref in refs:
        require(isinstance(ref, dict), "invalid supplied auxiliary reference")
        archive = store.select(None, ref.get("archive_id", primary["id"]))
        if archive["id"] not in archives:
            archives.append(archive["id"])
    names = []
    for name in archives:
        require(store.archives[name].get("parts"), f"missing archive parts: {name}")
        for part in store.archives[name]["parts"]:
            bundle = part.get("bundle")
            require(bundle in store.bundles, f"missing/UNKNOWN selected bundle: {bundle}")
            if bundle not in names:
                names.append(bundle)
    return job, archives, names


def list_job(store, key):
    job, archives, names = job_selection(store, key)
    print(json.dumps(dict(job_key=key, source_identity=job.get("source_identity"), status=job.get("status"),
        primary_archive_id=job.get("archive_id"), archive_ids=archives,
        terminal_par=job.get("terminal_par"), terminal_par_evidence=job.get("terminal_par_evidence"),
        scripts=job.get("scripts", []), recovered_objects=job.get("recovered_objects", []),
        bundles=[store.bundles[name] for name in names], gaps=job.get("gaps", []), warnings=job.get("warnings", []),
        execution_parameters=dict(files=[p.name for p in sorted(store.root.glob("execution-parameters*.jsonl.gz"))],
                                  match_original_id=job.get("source_identity", {}).get("id"))), indent=2))


def has_unknown(value):
    if isinstance(value, dict):
        return not value or any(has_unknown(x) for x in value.values())
    if isinstance(value, list):
        return not value or any(has_unknown(x) for x in value)
    return value is None or (isinstance(value, str) and value.strip().lower() in {"", "unknown"})


def identity_incomplete(identity):
    if not isinstance(identity, dict):
        return True
    for key in ("system", "id", "task", "repository", "commit", "host"):
        value = identity.get(key)
        if not isinstance(value, str) or has_unknown(value):
            return True
    number = identity.get("number")
    return (type(number) is not int or number < 0
            or re.fullmatch(r"[0-9a-fA-F]{40}", identity["commit"]) is None)


def script_name(name):
    name = PurePosixPath(name).name.lower()
    return (name.endswith((".sh", ".bash", ".py", ".r"))
            or (not PurePosixPath(name).suffix
                and (name == "run" or name.startswith(("doitall", "run-", "phase-")))))


def check_references(store, job):
    refs = []
    if "terminal_par" in job:
        refs.append(("PAR", job["terminal_par"]))
    if "scripts" in job:
        require(isinstance(job["scripts"], list), "missing/UNKNOWN scripts references")
        refs.extend(("script", ref) for ref in job["scripts"])
    if not refs:
        return
    wanted = {}
    for role, ref in refs:
        require(isinstance(ref, dict), f"missing/UNKNOWN {role} reference")
        ordinal = integer(ref.get("ordinal"), f"{role} reference ordinal")
        require(isinstance(ref.get("name"), str) and not has_unknown(ref["name"]),
                f"missing/UNKNOWN {role} reference name")
        if "sha256" in ref:
            digest(ref["sha256"], f"{role} reference")
        if "bytes" in ref:
            integer(ref["bytes"], f"{role} reference bytes")
        archive_id = ref.get("archive_id", job.get("archive_id"))
        store.select(None, archive_id)  # Explicit UNKNOWN overrides must not fall back to primary.
        wanted.setdefault(archive_id, {}).setdefault(ordinal, []).append((role, ref))
    for archive_id, ordinals in wanted.items():
        archive = store.select(None, archive_id)
        for row in store.member_rows(archive):
            ordinal = integer(row.get("ordinal"), "manifest ordinal")
            for role, ref in ordinals.pop(ordinal, []):
                require(row.get("name") == ref["name"] and row.get("type") == "file",
                        f"{role} reference member name/type mismatch at ordinal {ordinal}")
                actual_hash = digest(row.get("sha256"), f"{role} member")
                require("sha256" not in ref or ref["sha256"] == actual_hash,
                        f"{role} reference SHA256 mismatch at ordinal {ordinal}")
                require("bytes" not in ref or ref["bytes"] == integer(row.get("bytes"), "member bytes"),
                        f"{role} reference byte count mismatch at ordinal {ordinal}")
                expected_role = (ref["name"].lower().endswith(".par") if role == "PAR"
                                 else script_name(ref["name"]))
                require(expected_role, f"{role} reference filename role mismatch at ordinal {ordinal}")
        require(not ordinals, f"missing referenced PAR/script member ordinal(s): {archive_id}")


def verify(store, include_members):
    errors = []
    for name in store.bundles:
        try:
            store.bundle(name)
            print(f"VERIFIED bundle {name}")
        except (ValueError, OSError, EOFError, tarfile.TarError) as error:
            errors.append(f"bundle {name}: {error}")
    for name, archive in store.archives.items():
        try:
            if include_members:
                count = store.members(archive)
                print(f"VERIFIED archive {name}, {count} members")
            else:
                for _ in store.chunks(archive):
                    pass
                print(f"VERIFIED archive object {name}")
        except (ValueError, OSError, EOFError, tarfile.TarError) as error:
            errors.append(f"archive {name}: {error}")
    incomplete = 0
    for key, job in store.jobs.items():
        archive = job.get("archive_id")
        job_incomplete = (not isinstance(archive, str) or archive not in store.archives
                          or bool(job.get("gaps")) or identity_incomplete(job.get("source_identity"))
                          or not isinstance(job.get("status"), str) or has_unknown(job.get("status")))
        try:
            check_references(store, job)
        except (ValueError, KeyError, OSError, EOFError, tarfile.TarError) as error:
            job_incomplete = True
            errors.append(f"catalog job {key}: {error}")
        if job_incomplete:
            incomplete += 1
            print(f"INCOMPLETE catalog job {key} (see catalog identities/gaps)")
    if not store.jobs:
        print("INCOMPLETE catalog coverage: no jobs supplied")
        incomplete += 1
    for error in errors:
        print(f"FAIL {error}", file=sys.stderr)
    print(f"Hash checks finished; {incomplete} incomplete catalog jobs/scopes. "
          "No full-runtime or deletion-readiness claim.")
    return 1 if errors or incomplete else 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parent)
    parser.add_argument("--objects", default="objects.json")
    parser.add_argument("--catalog", default="catalog.json")
    commands = parser.add_subparsers(dest="command", required=True)
    check = commands.add_parser("verify", help="Check bundles and original archive hashes offline")
    check.add_argument("--members", action="store_true", help="Also stream/check all member manifests")
    for command in ("verify-members", "inspect", "restore"):
        sub = commands.add_parser(command)
        selector = sub.add_mutually_exclusive_group(required=True)
        selector.add_argument("--job")
        selector.add_argument("--archive")
        if command == "inspect":
            sub.add_argument("--ordinal", type=int, required=True)
            sub.add_argument("--member", help="Also require this exact original member name")
            sub.add_argument("--content", action="store_true", help="Print requested PAR/script text")
            sub.add_argument("--max-bytes", type=int, default=262144)
        elif command == "restore":
            sub.add_argument("--output", type=Path, required=True)
    fetch = commands.add_parser("download", help="Explicit network action, then SHA/size verification")
    selection = fetch.add_mutually_exclusive_group()
    selection.add_argument("--bundle", action="append", help="Default: all bundles")
    selection.add_argument("--job", help="Fetch primary and supplied auxiliary/recovery archive bundles for one job")
    selection.add_argument("--archive", help="Fetch only the selected archive's part bundles")
    saving = commands.add_parser("save-archive", help="Save exact original compressed archive bytes offline")
    saving.add_argument("--archive", required=True)
    saving.add_argument("--output", required=True)
    listing = commands.add_parser("list", help="Show one catalog job and its required archive/bundle references offline")
    listing.add_argument("--job", required=True)
    args = parser.parse_args(argv)
    try:
        store = Store(args.root, local(args.root, args.objects), local(args.root, args.catalog))
        if args.command == "verify":
            return verify(store, args.members)
        if args.command == "download":
            if args.archive is not None:
                names = archive_selection(store, args.archive)[1]
            else:
                names = job_selection(store, args.job)[2] if args.job else (args.bundle or list(store.bundles))
            require(all(name in store.bundles for name in names), "unknown bundle selector")
            download(store, names)
            return 0
        if args.command == "list":
            list_job(store, args.job)
            return 0
        if args.command == "save-archive":
            save_archive(store, args.archive, args.output)
            return 0
        archive = store.select(args.job, args.archive)
        if args.command == "verify-members":
            print(f"VERIFIED {archive['id']}: {store.members(archive)} members")
        elif args.command == "restore":
            restore(store, archive, args.output)
        else:
            require(args.ordinal >= 0 and args.max_bytes >= 0, "invalid ordinal/byte limit")
            captured = []
            def inspect(row, content):
                if row["ordinal"] != args.ordinal:
                    return
                require(args.member is None or row["name"] == args.member, "inspect member name mismatch")
                require(row["type"] == "file", "inspect requires regular file")
                name = PurePosixPath(row["name"]).name
                declared = store.jobs.get(args.job, {}).get("scripts", [])
                allowed = (name.endswith(".par") or name.endswith(".sh") or name.startswith("doitall")
                           or any(x.get("ordinal") == args.ordinal and x.get("name") == row["name"] for x in declared))
                require(allowed, "inspect permits exact PAR/script members only")
                text = None
                if args.content:
                    require(row["bytes"] <= args.max_bytes, "inspect content exceeds byte limit")
                    text = b"".join(content).decode("utf-8")
                captured.append((row, text))
            store.members(archive, inspect)
            require(len(captured) == 1, "inspect ordinal not found")
            row, text = captured[0]
            print(json.dumps({"archive_id": archive["id"], "archive_sha256": archive["sha256"], **row}, ensure_ascii=False))
            if text is not None:
                print(text, end="" if text.endswith("\n") else "\n")
        return 0
    except (ValueError, KeyError, OSError, EOFError, tarfile.TarError) as error:
        print(f"FAIL {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
