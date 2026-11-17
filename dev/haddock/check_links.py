#!/usr/bin/env python3
"""Check documentation fragments and instance permalinks in Haddock archives."""

import argparse
import json
import posixpath
import tarfile
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit


class Page(HTMLParser):
    """Read instance headers and their generated permalink targets."""

    def __init__(self, content: str) -> None:
        super().__init__()
        self.ids: set[str] = set()
        self.duplicates: set[str] = set()
        self.duplicate_count = 0
        self.instance_headers = 0
        self.method_headers = 0
        self.unscoped_methods: list[tuple[str, str]] = []
        self.links: list[str] = []
        self.permalinks: list[tuple[str, str]] = []
        self.header_links: list[tuple[str, list[str]]] = []
        self.instance_id: str | None = None
        self.method_id: str | None = None
        self.instance_links: list[str] | None = None
        self.method_links: list[str] | None = None
        self.details: list[str] = []
        self.div_depth = 0
        self.methods_depth: int | None = None
        self.feed(content)

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        attributes = dict(attrs)
        identifier = attributes.get("id")
        if identifier is not None:
            if identifier in self.ids:
                self.duplicates.add(identifier)
                self.duplicate_count += 1
            self.ids.add(identifier)
        if tag == "td":
            self.instance_id = None
            self.instance_links = None
        classes = (attributes.get("class") or "").split()
        if tag == "details":
            self.details.append(identifier or "")
        if tag == "div":
            self.div_depth += 1
            if "methods" in classes:
                self.methods_depth = self.div_depth
        if tag == "p" and "src" in classes and self.methods_depth is not None and any(value.startswith("i:") for value in self.details):
            self.method_id = identifier or "<missing method id>"
            owner = next(value for value in reversed(self.details) if value.startswith("i:"))
            if not self.method_id.startswith(owner + ":"):
                self.unscoped_methods.append((self.method_id, "<method id outside instance scope>"))
            self.method_links = []
            self.header_links.append((self.method_id, self.method_links))
            self.method_headers += 1
        if tag == "span" and "instance" in classes:
            self.instance_id = attributes.get("data-details-id") or "<missing instance id>"
            self.instance_links = []
            self.header_links.append((self.instance_id, self.instance_links))
            self.instance_headers += 1
        href = attributes.get("href")
        if tag == "a" and href is not None:
            self.links.append(href)
            target = self.method_id or self.instance_id
            if "selflink" in classes and target is not None:
                self.permalinks.append((target, href))
                links = self.method_links if self.method_id is not None else self.instance_links
                assert links is not None
                links.append(href)

    def handle_endtag(self, tag: str) -> None:
        if tag == "td":
            self.instance_id = None
            self.instance_links = None
        if tag == "p":
            self.method_id = None
            self.method_links = None
        if tag == "details":
            self.details.pop()
        if tag == "div":
            if self.methods_depth == self.div_depth:
                self.methods_depth = None
            self.div_depth -= 1

    def broken(self) -> list[tuple[str, str]]:
        """Require each instance permalink to name its own details element."""
        counts = [(target, f"<{len(links)} permalinks>") for target, links in self.header_links if len(links) != 1]
        return counts + self.unscoped_methods + [
            (target, href)
            for target, href in self.permalinks
            if unquote(href) != "#" + target or target not in self.ids
        ]


def check_archive(path: Path) -> tuple[dict[str, object], bool]:
    """Check the HTML that the documentation archive contains."""
    pages: dict[str, Page] = {}
    broken: list[dict[str, str]] = []
    source_links: list[dict[str, str]] = []
    duplicate_instance_ids: list[dict[str, str]] = []
    with tarfile.open(path) as archive:
        for member in archive.getmembers():
            if not member.isfile() or not member.name.endswith(".html"):
                continue
            source = archive.extractfile(member)
            assert source is not None
            pages[member.name] = Page(source.read().decode("utf-8"))
    checked_fragments = 0
    checked_permalinks = 0
    external_fragments = 0
    for name, page in pages.items():
        checked_permalinks += len(page.permalinks)
        broken.extend(
            {"page": name, "expected": "#" + target, "actual": href}
            for target, href in page.broken()
        )
        duplicate_instance_ids.extend(
            {"page": name, "id": identifier}
            for identifier in sorted(page.duplicates)
            if identifier.startswith("i:")
        )
        for href in page.links:
            url = urlsplit(href)
            if not url.fragment:
                continue
            target = posixpath.normpath(posixpath.join(posixpath.dirname(name), unquote(url.path))) if url.path else name
            archive_root = name.split("/", 1)[0] if "/" in name else ""
            inside_archive = target.startswith(archive_root + "/") if archive_root else not target.startswith(("/", "../"))
            if url.scheme or url.netloc or not inside_archive:
                external_fragments += 1
                continue
            if target not in pages:
                broken.append({"page": name, "target": target, "error": "missing local page"})
                continue
            checked_fragments += 1
            if unquote(url.fragment) not in pages[target].ids:
                issue = {"page": name, "target": target, "fragment": unquote(url.fragment)}
                (source_links if "/src/" in target else broken).append(issue)
    report: dict[str, object] = {
        "archive": str(path),
        "required_checks": ["all local documentation and source fragments", "one permalink per instance and method", "unique instance ids"],
        "checked_local_fragments": checked_fragments,
        "checked_instance_permalinks": checked_permalinks,
        "instance_headers": sum(page.instance_headers for page in pages.values()),
        "scoped_instance_methods": sum(page.method_headers for page in pages.values()),
        "unchecked_external_fragments": external_fragments,
        "broken": broken,
        "duplicate_instance_ids": duplicate_instance_ids,
        "all_duplicate_id_occurrences_audit_only": sum(page.duplicate_count for page in pages.values()),
        "broken_source_fragments": source_links,
    }
    return report, checked_permalinks == 0 or bool(broken) or bool(source_links) or bool(duplicate_instance_ids)


def main() -> None:
    """Report broken instance permalinks and fail if any exist."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("archives", type=Path, nargs="+")
    arguments = parser.parse_args()
    failed = False
    for path in arguments.archives:
        report, archive_failed = check_archive(path)
        print(json.dumps(report))
        failed = failed or archive_failed
    raise SystemExit(1 if failed else 0)


if __name__ == "__main__":
    main()
