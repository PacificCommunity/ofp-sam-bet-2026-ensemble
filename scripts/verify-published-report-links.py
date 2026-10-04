#!/usr/bin/env python3
"""Check preserved local report paths and README anchors, without network access."""
import argparse
from collections import Counter
import html
from html.parser import HTMLParser
from pathlib import Path
import re
from urllib.parse import quote, unquote, urljoin, urlsplit


ROOT = Path(__file__).resolve().parents[1]
BASELINE = "scripts/published-report-targets.txt"
SITE = "https://published.invalid/ofp-sam-bet-2026-ensemble/"
SITE_PREFIX = "/ofp-sam-bet-2026-ensemble/"
ENTRY_POINTS = (
    "bet-2026-ensemble-report.html",
    "bet-2026-ensemble-interactive-viewer.html",
    "bet-2026-ensemble-report-rr-comparison.html",
    "bet-2026-ensemble-report-rr0-inclusion.html",
    "bet-2026-ensemble-report-rr1-exclusion.html",
)
# Heading anchors from 2ffa4347760459ad20562ef93f92a70861cee420.
README_ANCHORS = {
    "README.md": {
        "bet-2026-diagnostic-ensemble", "recreate-and-validate", "run-a-model",
        "retained-final-par-and-viewer-rep-files", "distribution-figure", "outputs",
        "reusable-hessian-uncertainty", "stochastic-projections-and-reusable-caches",
        "reproducible-report", "reporting-rate-retained-subset-sensitivity",
        # Also retain the subsequently published paired overview's aliases.
        "bet-2026-exact-paired-rr-comparison", "read-the-results", "verify-or-rerun",
    },
    "rr-test/README.md": {
        "exact-rr-paired-reruns", "pairing-contract", "reproduce-and-validate", "submit",
    },
}


def require(condition, message):
    if not condition:
        raise ValueError(message)


class Links(HTMLParser):
    def __init__(self, text):
        super().__init__(convert_charrefs=True)
        self.anchors = set()
        self.references = set()
        self.base = None
        self.feed(text)
        self.close()

    def handle_starttag(self, tag, attrs):
        for key, value in attrs:
            if value is None:
                continue
            if key == "id" or (tag == "a" and key == "name"):
                self.anchors.add(value)
            if tag == "base" and key == "href":
                if self.base is None:
                    self.base = value
            elif key in {"href", "src"}:
                # Avoid retaining large embedded data:image payloads.
                local = value.strip()
                if not re.match(r"[A-Za-z][A-Za-z0-9+.-]*:", local) and not local.startswith("//"):
                    self.references.add(value)


def markdown_anchors(text):
    visible, headings = [], Counter()
    anchors, fence = set(), None
    for line in text.splitlines():
        marker = re.match(r"^ {0,3}(`{3,}|~{3,})", line)
        if marker:
            value = marker.group(1)
            if fence is None:
                fence = value
            elif value[0] == fence[0] and len(value) >= len(fence):
                fence = None
            continue
        if fence is not None or line.startswith("    "):
            continue
        visible.append(re.sub(r"(`+).*?\1", "", line))
        heading = re.match(r"^ {0,3}#{1,6}[ \t]+(.+?)(?:[ \t]+#+[ \t]*)?$", line)
        if heading:
            title = re.sub(r"!?\[([^]]*)\]\([^)]*\)", r"\1", heading.group(1))
            title = html.unescape(re.sub(r"<[^>]*>", "", title)).lower()
            slug = re.sub(r"[^\w -]", "", title).replace(" ", "-")
            index = headings[slug]
            headings[slug] += 1
            anchors.add(f"{slug}-{index}" if index else slug)
    return anchors | Links("\n".join(visible)).anchors


def resolve_local(report_root, source, reference, base=None):
    # Explicit URL schemes (including data/mailto/javascript) and //host URLs
    # are deliberately excluded. Queries do not change the local target path.
    reference = reference.strip()
    raw = urlsplit(reference)
    if raw.scheme or raw.netloc:
        return None
    source_url = urljoin(SITE, quote(source.relative_to(report_root).as_posix()))
    resolved = urlsplit(urljoin(urljoin(source_url, base or ""), reference))
    if resolved.scheme != "https" or resolved.netloc != "published.invalid":
        return None  # An external <base href> also makes relative URLs external.
    decoded = unquote(resolved.path, errors="strict")
    require(decoded.startswith(SITE_PREFIX), f"URL escapes the published report root: {source.name}: {reference}")
    target = (report_root / decoded[len(SITE_PREFIX):]).resolve()
    require(target.is_relative_to(report_root), f"URL escapes the published report root: {reference}")
    if decoded.endswith("/"):
        require(target.is_dir(), f"Missing directory target: {source.name}: {reference}")
    if target.is_dir():
        # GitHub Pages directory links need an actual index document.
        target = next((target / name for name in ("index.html", "index.htm") if (target / name).is_file()), None)
        require(target is not None, f"Directory target has no index HTML: {source.name}: {reference}")
    require(target.is_file(), f"Missing local target: {source.name}: {reference}")
    fragment = unquote(resolved.fragment, errors="strict").split(":~:text=", 1)[0]
    return target, fragment


def check(root):
    root = root.resolve()
    report_root = (root / "results").resolve()
    baseline = [line.strip() for line in (root / BASELINE).read_text().splitlines()
                if line.strip() and not line.lstrip().startswith("#")]
    require(len(baseline) == len(set(baseline)), "Repeated published baseline target.")
    require({"results/" + name for name in ENTRY_POINTS} <= set(baseline), "Baseline omits a report entry point.")
    for relative in baseline:
        target = (root / relative).resolve()
        require(target.is_relative_to(report_root) and target.is_file(), f"Missing preserved published target: {relative}")
    for relative, required in README_ANCHORS.items():
        actual = markdown_anchors((root / relative).read_text())
        require(required <= actual, f"Missing preserved README anchor(s) in {relative}: " + ", ".join(sorted(required - actual)))
    parsed, visited, references = {}, set(), 0

    def document(path):
        if path not in parsed:
            parsed[path] = Links(path.read_text(encoding="utf-8"))
        return parsed[path]

    pending = [report_root / name for name in ENTRY_POINTS]
    while pending:
        source = pending.pop().resolve()
        if source in visited:
            continue
        visited.add(source)
        scan = document(source)
        for reference in sorted(scan.references):
            resolved = resolve_local(report_root, source, reference, scan.base)
            if resolved is None:
                continue
            references += 1
            target, fragment = resolved
            if target.suffix.lower() in {".html", ".htm", ".svg"}:
                require(not fragment or fragment in document(target).anchors,
                        f"Missing target fragment: {source.name}: {reference}")
            if target.suffix.lower() in {".html", ".htm"}:
                pending.append(target)
    print(f"Verified {len(ENTRY_POINTS)} report entry points, {len(baseline)} preserved published paths, "
          f"{references} local HTML href/src references and {sum(map(len, README_ANCHORS.values()))} README anchors.")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT, help="repository root (default: this checkout)")
    args = parser.parse_args()
    try:
        check(args.root)
    except (OSError, ValueError) as error:
        parser.exit(1, f"Published link check failed: {error}\n")


if __name__ == "__main__":
    main()
