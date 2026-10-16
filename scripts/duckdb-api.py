#!/usr/bin/env python3
# pyright: strict
"""Fetch a pinned DuckDB API snapshot and check binding coverage offline."""

from __future__ import annotations

import argparse
from concurrent.futures import ThreadPoolExecutor
import gzip
import hashlib
import io
import json
from pathlib import Path, PurePosixPath
import re
import sys
import tarfile
from typing import TypedDict, cast
from urllib.parse import quote
from urllib.request import Request, urlopen


ROOT = Path(__file__).resolve().parents[1]
VENDOR = ROOT / "duckdb-ffi/vendor"
ARCHIVE = VENDOR / "duckdb-api.tar.gz"
METADATA = VENDOR / "duckdb-api.json"
CHECKSUM = VENDOR / "duckdb-api.sha256"
PREFIX = "duckdb-2.0/"
HEADERS = ("duckdb.h", "duckdb_v2.h", "duckdb_extension.h", "duckdb_extension_v2.h")


class FileRecord(TypedDict):
    source: str
    sha256: str


class SourceArchive(TypedDict):
    url: str
    sha256: str


class Pin(TypedDict):
    repository: str
    ref: str
    commit: str
    commit_date: str
    source_archive: SourceArchive


class Manifest(Pin):
    files: dict[str, FileRecord]


class CommitInfo(TypedDict):
    sha: str
    commit: dict[str, dict[str, str]]


class TreeEntry(TypedDict):
    path: str
    type: str


class TreeInfo(TypedDict):
    truncated: bool
    tree: list[TreeEntry]


def download(url: str) -> bytes:
    """Read one upstream resource."""
    request = Request(url, headers={"User-Agent": "duckdb-haskell-api-snapshot"})
    with urlopen(request, timeout=60) as response:
        return cast(bytes, response.read())


def archive_digest(url: str) -> str:
    """Read a source archive and calculate its SHA256 digest."""
    digest = hashlib.sha256()
    request = Request(url, headers={"User-Agent": "duckdb-haskell-api-snapshot"})
    with urlopen(request, timeout=60) as response:
        while block := cast(bytes, response.read(1024 * 1024)):
            digest.update(block)
    return digest.hexdigest()


def refresh(ref: str) -> None:
    """Resolve an explicit ref and fetch its headers and API specification."""
    api = "https://api.github.com/repos/duckdb/duckdb"
    commit = cast(CommitInfo, json.loads(download(f"{api}/commits/{quote(ref, safe='')}")))
    sha = commit["sha"]
    if not re.fullmatch(r"[0-9a-f]{40}", sha):
        raise ValueError("Upstream did not return a full commit SHA")
    tree = cast(TreeInfo, json.loads(download(f"{api}/git/trees/{sha}?recursive=1")))
    if tree["truncated"]:
        raise ValueError("Upstream tree response is incomplete")
    sources = {f"src/include/{header}": header for header in HEADERS}
    sources["LICENSE"] = "LICENSE"
    for entry in tree["tree"]:
        path = entry["path"]
        if entry["type"] == "blob" and (
            re.fullmatch(r"api_spec/v[12]/.*\.yaml", path)
            or path in ("api_spec/README.md", "api_spec/VERSIONING.md")
        ):
            sources[path] = path

    def fetch(item: tuple[str, str]) -> tuple[str, str, bytes]:
        source, target = item
        data = download(f"https://raw.githubusercontent.com/duckdb/duckdb/{sha}/{source}")
        return source, target, data

    # Fetch all files before changing the snapshot.
    with ThreadPoolExecutor(max_workers=4) as pool:
        fetched = list(pool.map(fetch, sorted(sources.items())))
    archive_url = f"https://codeload.github.com/duckdb/duckdb/tar.gz/{sha}"
    manifest: Manifest = {
        "repository": "https://github.com/duckdb/duckdb",
        "ref": ref,
        "commit": sha,
        "commit_date": commit["commit"]["committer"]["date"],
        "source_archive": {"url": archive_url, "sha256": archive_digest(archive_url)},
        "files": {},
    }
    members: dict[str, bytes] = {}
    for source, target, data in fetched:
        members[PREFIX + target] = data
        manifest["files"][target] = {"source": source, "sha256": hashlib.sha256(data).hexdigest()}
    members[PREFIX + "provenance.json"] = (json.dumps(manifest, indent=2, sort_keys=True) + "\n").encode()
    metadata = snapshot_metadata(manifest)
    write_archive(metadata, members)
    print(f"Fetched {len(fetched)} files at {sha}")


def snapshot_metadata(manifest: Manifest) -> Pin:
    """Keep the pin metadata outside the archive."""
    return {
        "repository": manifest["repository"],
        "ref": manifest["ref"],
        "commit": manifest["commit"],
        "commit_date": manifest["commit_date"],
        "source_archive": manifest["source_archive"],
    }


def archive_bytes(members: dict[str, bytes]) -> bytes:
    """Build an archive with fixed order, modes, and timestamps."""
    output = io.BytesIO()
    with gzip.GzipFile(filename="", mode="wb", fileobj=output, mtime=0, compresslevel=9) as compressed:
        with tarfile.open(fileobj=compressed, mode="w", format=tarfile.USTAR_FORMAT) as archive:
            for name, data in sorted(members.items()):
                member = tarfile.TarInfo(name)
                member.size = len(data)
                member.mode = 0o644
                member.mtime = 0
                member.uid = member.gid = 0
                member.uname = member.gname = ""
                archive.addfile(member, io.BytesIO(data))
    return output.getvalue()


def verify_archive(data: bytes, metadata: Pin) -> dict[str, bytes]:
    """Verify member names, metadata, and every archived file digest."""
    if not re.fullmatch(r"[0-9a-f]{40}", metadata["commit"]):
        raise ValueError("Snapshot commit must be a full SHA")
    source = metadata["source_archive"]
    if source["url"] != f"https://codeload.github.com/duckdb/duckdb/tar.gz/{metadata['commit']}" or not re.fullmatch(r"[0-9a-f]{64}", source["sha256"]):
        raise ValueError("Source archive URL or SHA256 is invalid")
    members: dict[str, bytes] = {}
    with tarfile.open(fileobj=io.BytesIO(data), mode="r:gz") as archive:
        for member in archive.getmembers():
            name = member.name
            path = PurePosixPath(name)
            if path.is_absolute() or ".." in path.parts or "\\" in name or path.as_posix() != name:
                raise ValueError(f"Unsafe archive member: {name}")
            if not member.isfile():
                raise ValueError(f"Archive member must be a regular file: {name}")
            if name in members:
                raise ValueError(f"Duplicate archive member: {name}")
            handle = archive.extractfile(member)
            if handle is None:
                raise ValueError(f"Unreadable archive member: {name}")
            with handle:
                members[name] = handle.read()
    manifest = cast(Manifest, json.loads(members[PREFIX + "provenance.json"]))
    if snapshot_metadata(manifest) != metadata:
        raise ValueError("Inner provenance differs from external metadata")
    expected = {PREFIX + name for name in manifest["files"]} | {PREFIX + "provenance.json"}
    if expected != members.keys():
        raise ValueError(f"Archive file list differs: {sorted(expected ^ members.keys())}")
    for target, record in manifest["files"].items():
        if hashlib.sha256(members[PREFIX + target]).hexdigest() != record["sha256"]:
            raise ValueError(f"SHA256 mismatch: {target}")
    return members


def read_archive() -> tuple[Pin, dict[str, bytes]]:
    """Read the pinned archive after verifying its checksum and contents."""
    data = ARCHIVE.read_bytes()
    digest = CHECKSUM.read_text()
    if not re.fullmatch(r"[0-9a-f]{64}\n", digest):
        raise ValueError("Archive checksum must be one SHA256 digest and a newline")
    if hashlib.sha256(data).hexdigest() != digest.strip():
        raise ValueError("SHA256 mismatch: API archive")
    metadata = cast(Pin, json.loads(METADATA.read_text()))
    return metadata, verify_archive(data, metadata)


def write_archive(metadata: Pin, members: dict[str, bytes]) -> None:
    """Write the verified archive, compact metadata, and checksum."""
    data = archive_bytes(members)
    verify_archive(data, metadata)
    VENDOR.mkdir(parents=True, exist_ok=True)
    for path, contents in (
        (ARCHIVE, data),
        (METADATA, (json.dumps(metadata, indent=2, sort_keys=True) + "\n").encode()),
        (CHECKSUM, (hashlib.sha256(data).hexdigest() + "\n").encode()),
    ):
        temporary = path.with_suffix(path.suffix + ".tmp")
        temporary.write_bytes(contents)
        temporary.replace(path)


def clean_c(source: str) -> str:
    """Remove comments and preprocessor directives before reading declarations."""
    source = re.sub(r"/\*.*?\*/|//[^\n]*", "", source, flags=re.S)
    source = source.replace("\\\n", "")
    return re.sub(r"^[ \t]*#[^\n]*", "", source, flags=re.M)


def normalize(source: str) -> str:
    """Normalize declaration whitespace."""
    return re.sub(r"\s*([*(),;{}=])\s*", r"\1", " ".join(source.split()))


def parameter_type(parameter: str) -> str:
    """Remove a parameter name from a C declaration."""
    parameter = parameter.strip()
    if parameter == "void":
        return "void"
    return normalize(re.sub(r"\b[A-Za-z_]\w*\s*$", "", parameter))


def functions(header: Path | str) -> dict[str, str]:
    """Read every exported declaration, including gated declarations."""
    source = clean_c(header.read_text() if isinstance(header, Path) else header)
    pattern = r"DUCKDB_C_API\s+(?:DUCKDB_DEPRECATED\s+)?([^;()]*?\S)\s*(duckdb_\w+)\s*\(([^;]*?)\)\s*;"
    result: dict[str, str] = {}
    for match in re.finditer(pattern, source, re.S):
        returns, name, parameters = match.groups()
        signature = normalize(returns) + "(" + ",".join(parameter_type(p) for p in parameters.split(",")) + ")"
        if name in result:
            raise ValueError(f"Duplicate function declaration: {name}")
        result[name] = signature
    if not result:
        raise ValueError("No exported declarations found in the header")
    declared = len(re.findall(r"\bDUCKDB_C_API\b", source))
    if len(result) != declared:
        raise ValueError(f"Parsed {len(result)} of {declared} exported declarations")
    return result


def typedefs(header: Path | str) -> dict[str, str]:
    """Read typedefs and complete structure and union definitions."""
    source = clean_c(header.read_text() if isinstance(header, Path) else header)
    result: dict[str, str] = {}
    for start in re.finditer(r"\btypedef\b", source):
        depth = 0
        for end in range(start.end(), len(source)):
            char = source[end]
            if char in "{(":
                depth += 1
            elif char in "})":
                depth -= 1
            elif char == ";" and depth == 0:
                declaration = source[start.end():end]
                callback = re.search(r"\(\s*\*\s*(\w+)\s*\)", declaration)
                name = callback.group(1) if callback else re.findall(r"\w+", declaration)[-1]
                result[name] = normalize(declaration)
                break
    # Some typedefs precede their structure definitions. Include the later
    # fields, nested unions, and the Arrow structures without typedefs.
    for start in re.finditer(r"\b(struct|union)\s+(\w+)\s*{", source):
        depth = 1
        end = start.end()
        while depth and end < len(source):
            if source[end] == "{":
                depth += 1
            elif source[end] == "}":
                depth -= 1
            end += 1
        if depth:
            raise ValueError(f"Incomplete structure definition: {start.group(2)}")
        result[start.group(2)] = normalize(source[start.start():end])
    return result


def difference(before: dict[str, str], after: dict[str, str]) -> dict[str, object]:
    """Report added, removed, and changed API declarations."""
    return {
        "added": sorted(after.keys() - before.keys()),
        "removed": sorted(before.keys() - after.keys()),
        "changed": {name: {"before": before[name], "after": after[name]} for name in sorted(before.keys() & after.keys()) if before[name] != after[name]},
    }


SHARED_LAYOUTS = {
    "DuckDBV2HugeintT": ("DuckDBHugeInt", "duckdb_hugeint", "duckdb_v2_hugeint_t"),
    "DuckDBV2UhugeintT": ("DuckDBUHugeInt", "duckdb_uhugeint", "duckdb_v2_uhugeint_t"),
    "DuckDBV2IntervalT": ("DuckDBInterval", "duckdb_interval", "duckdb_v2_interval_t"),
    "DuckDBV2ListEntry": ("DuckDBListEntry", "duckdb_list_entry", "duckdb_v2_list_entry"),
    "ArrowSchema": ("ArrowSchema", "ArrowSchema", "ArrowSchema"),
    "ArrowArray": ("ArrowArray", "ArrowArray", "ArrowArray"),
    "ArrowArrayStream": ("ArrowArrayStream", "ArrowArrayStream", "ArrowArrayStream"),
    "DuckDBV2Bytes": ("DuckDBStringT", "duckdb_string_t", "duckdb_v2_bytes"),
}


def shared_layouts(v1: str, v2: str, bindings: dict[str, str]) -> dict[str, object]:
    """Verify equal C layouts with distinct generated Haskell types."""
    canonical = typedefs(v1) | typedefs(ROOT / "duckdb-ffi/cbits/duckdb_arrow.h")
    current = typedefs(v2)
    layouts: dict[str, object] = {}
    for v1_name, v2_name, target in (("DuckDBIdx", "DuckDBIdx", "Word64"), ("DuckDBSel", "DuckDBV2SelT", "Word32")):
        for version, name in (("v1", v1_name), ("v2", v2_name)):
            if not re.search(rf"^newtype\s+{name}\s*=\s*{name}\s*{{\s*\w+\s*::\s*HsBindgen\.Runtime\.LibC\.{target}\s*}}", bindings[version], re.M):
                raise ValueError(f"Shared integer type differs: {version}.{name}")
        layouts[v2_name] = {"v1": f"Database.DuckDB.FFI.{v1_name}", "v2": f"Database.DuckDB.FFI.V2.{v2_name}", "shared_haskell_type": False}
    for name, target in (("idx_t", "idx_t"), ("sel_t", "duckdb_v2_sel_t")):
        if canonical[name].removesuffix(name).strip() != current[target].removesuffix(target).strip():
            raise ValueError(f"Shared integer type differs: {target}")

    def fields(declaration: str) -> str:
        body = re.search(r"{(.*)}", declaration)
        if body is None:
            raise ValueError(f"Missing shared structure fields: {declaration}")
        value = re.sub(r"\bidx_t\b", "uint64_t", body.group(1))
        # Callback parameter names do not change the Arrow layout.
        value = re.sub(r"(\(\*\w+\)\()([^()]*)\)", lambda match: match.group(1) + ",".join(parameter_type(p) for p in match.group(2).split(",")) + ")", value)
        return normalize(value)

    for name, (target, before, after) in SHARED_LAYOUTS.items():
        for version, nominal in (("v1", target), ("v2", name)):
            if not re.search(rf"\binstance\s+BG\.Storable\s+{nominal}\b", bindings[version]):
                raise ValueError(f"Missing generated Storable instance: {version}.{nominal}")
        if fields(canonical[before]) != fields(current[after]):
            raise ValueError(f"Shared C structure differs: {name}")
        layouts[name] = {"v1": f"Database.DuckDB.FFI.{target}", "v2": f"Database.DuckDB.FFI.V2.{name}", "shared_haskell_type": False}
    return layouts


def audit() -> dict[str, object]:
    """Compare the legacy v1 API and the pinned preview APIs."""
    metadata, members = read_archive()
    legacy = ROOT / "duckdb-ffi/cbits/duckdb.h"
    v1 = members[PREFIX + "duckdb.h"].decode()
    v2 = members[PREFIX + "duckdb_v2.h"].decode()
    bindings = {
        "v1": (ROOT / "duckdb-ffi/generated/Database/DuckDB/FFI.hs").read_text(),
        "v2": (ROOT / "duckdb-ffi/generated/Database/DuckDB/FFI/V2.hs").read_text(),
    }
    arrow = (ROOT / "duckdb-ffi/cbits/duckdb_arrow.h").read_text()
    v1_coverage, v1_referenced = coverage(v1 + "\n" + arrow, bindings["v1"])
    v2_coverage, v2_referenced = coverage(v2, bindings["v2"])
    exports = set(functions(v1)) | set(functions(v2))
    return {
        "commit": metadata["commit"],
        "unknown_functions": sorted((v1_referenced | v2_referenced) - exports),
        "v1": v1_coverage,
        "v2": v2_coverage,
        "shared_types": shared_layouts(v1, v2, bindings),
        "v1_function_changes": difference(functions(legacy), functions(v1)),
        "v1_type_changes": difference(typedefs(legacy), typedefs(v1)),
    }


def coverage(header: str, bindings: str) -> tuple[dict[str, object], set[str]]:
    """Check generated exports, C shims, and both callback conversions."""
    declarations = functions(header)
    imports = {
        name: target
        for target, name in re.findall(r'foreign\s+import\s+ccall\s+(?:(?:safe|unsafe|interruptible)\s+)?"([^"\n]+)"\s+(\w+)\s*::', bindings)
    }
    exports = re.search(r"\bmodule\s+[\w.]+\s*\((.*?)\)\s*where\b", bindings, re.S)
    exported_names: set[str] = set(re.findall(r"\b\w+\b", exports[1])) if exports else set()
    signatures = set(re.findall(r"^([a-z_]\w*)\s*::", bindings, re.M))

    def exported(name: str) -> bool:
        return exports is None or name in exported_names

    bodies = dict(re.findall(r"^([a-z_]\w*)[ \t]*=[ \t]*(.*?)(?=^\S|\Z)", bindings, re.M | re.S))

    sources: list[str] = []
    for block in re.finditer(r"\.addCSource\s*\(\s*[\w.]+\.unlines\s*\[(.*?)\]\s*\)\s*\)", bindings, re.S):
        sources.append("\n".join(cast(str, json.loads(literal)) for literal in re.findall(r'"(?:\\.|[^"\\])*"', block[1])))
    if not sources:
        raise ValueError("No hs-bindgen C shims found")
    native = clean_c("\n".join(sources))
    shims = {
        shim: set(re.findall(r"\(\s*(duckdb_\w+)\s*\)\s*\(", implementation))
        for shim, implementation in re.findall(r"\b(hs_bindgen_\w+)\s*\([^{};]*\)\s*{([^{}]*)}", native)
    }
    bound: dict[str, str] = {}
    for name in declarations:
        public = "c_" + name
        helpers = set(re.findall(r"\b(hs_bindgen_[0-9a-f]+)\b", bodies.get(public, "")))
        if len(helpers) != 1 or not exported(public) or public not in signatures:
            continue
        helper = helpers.pop()
        base = helper + "_base"
        if name in shims.get(imports.get(base, ""), set()) and re.search(rf"\b{base}\b", bodies.get(helper, "")):
            bound[name] = "hs-bindgen C shim"

    # Read the native origin from generated documentation. This avoids a
    # second implementation of the generator's public naming policy.
    types: dict[str, list[tuple[str, str]]] = {}
    for declaration in re.finditer(r"\{-\|((?:(?!\n-\}).)*)\n-\}\s*\n(?:newtype|data)\s+(\w+)\b(.*?)(?=^\S|\Z)", bindings, re.M | re.S):
        origin = re.search(r"__C declaration:__ @(?:struct )?(\w+)@", declaration[1])
        if origin:
            types.setdefault(origin[1], []).append((declaration[2], declaration[3]))
    compact = re.sub(r"\s+", "", bindings)

    def conversions(target: str) -> bool:
        for clss, method, mode in (("ToFunPtr", "toFunPtr", "wrapper"), ("FromFunPtr", "fromFunPtr", "dynamic")):
            pattern = rf"instanceBG\.{clss}{re.escape(re.sub(r'\s+', '', target))}where{method}=(hs_bindgen_[0-9a-f]+)"
            instance = re.search(pattern, compact)
            if instance is None:
                return False
            helper = instance[1]
            base = helper + "_base"
            if imports.get(base) != mode or not re.search(rf"\b{base}\b", bodies.get(helper, "")):
                return False
        return True

    native_types = typedefs(header)
    callbacks = {name: declaration for name, declaration in native_types.items() if re.search(r"\(\*" + re.escape(name) + r"\)", declaration)}
    aliases: dict[str, str] = {}
    for name in callbacks:
        for alias, declaration in types.get(name, []):
            pointer = re.search(r"::\s*BG\.FunPtr\s+(\w+)", declaration)
            if pointer and any(auxiliary == pointer[1] for auxiliary, _ in types[name]) and exported(alias) and exported(pointer[1]) and conversions(pointer[1]):
                aliases[name] = alias
                break
    callback_typedef_count = len(callbacks)
    for structure in re.finditer(r"\bstruct\s+(Arrow\w+)\s*{([^{}]*)}", clean_c(header), re.S):
        for field in re.finditer(r"([^;{}]+?)\(\s*\*\s*(\w+)\s*\)\s*\([^;]*\)\s*;", structure[2]):
            name = f"{structure[1]}_{field[2]}_fn"
            callbacks[name] = normalize(field[0].removesuffix(";"))
            for alias, _ in types.get(structure[1], []):
                signature = re.search(rf'\btype\s+CFieldType\s+{alias}\s+"{field[2]}"\s*=\s*BG\.FunPtr\s*(.*?)\s*offset#', bindings, re.S)
                if signature and exported(alias) and conversions(signature[1]):
                    aliases[name] = f"{alias}.{field[2]}"
                    break
    referenced = {name for calls in shims.values() for name in calls}
    return (
        {
            "functions": len(declarations),
            "bound": bound,
            "missing_functions": sorted(declarations.keys() - bound.keys()),
            "callbacks": len(callbacks),
            "callback_typedefs": callback_typedef_count,
            "callback_aliases": aliases,
            "missing_callbacks": sorted(callbacks.keys() - aliases.keys()),
        },
        referenced,
    )


def compare_headers(directory: Path) -> int:
    """Reject a native header set that differs from the pinned API."""
    _, members = read_archive()
    report: dict[str, dict[str, dict[str, object]]] = {}
    failed = False
    for header in ("duckdb.h", "duckdb_v2.h"):
        report[header] = {
            "functions": difference(functions(members[PREFIX + header].decode()), functions(directory / header)),
            "types": difference(typedefs(members[PREFIX + header].decode()), typedefs(directory / header)),
        }
        for changes in report[header].values():
            if any(changes.values()):
                failed = True
    print(json.dumps(report, indent=2, sort_keys=True))
    return 1 if failed else 0


def main() -> int:
    """Run a deliberate refresh or the offline coverage check."""
    parser = argparse.ArgumentParser(description=__doc__)
    actions = parser.add_mutually_exclusive_group(required=True)
    actions.add_argument("--refresh", metavar="REF", help="fetch an explicit upstream commit, release branch (v2.0-cyanoptera), or tag")
    actions.add_argument("--check", action="store_true", help="verify SHA256 and require complete raw API coverage offline")
    actions.add_argument("--report", action="store_true", help="print the complete offline API coverage and v1 ABI comparison as JSON")
    actions.add_argument("--compare-headers", type=Path, metavar="DIR", help="require a native header set to match the pinned function and type declarations")
    args = parser.parse_args()
    if args.refresh:
        refresh(args.refresh)
        return 0
    if args.compare_headers:
        return compare_headers(args.compare_headers)
    report = audit()
    if args.report:
        print(json.dumps(report, indent=2, sort_keys=True))
        return 0
    failed = False
    print(f"Verified snapshot {report['commit']}")
    unknown = cast(list[str], report["unknown_functions"])
    if unknown:
        print(f"Native functions absent from the snapshot: {', '.join(unknown)}", file=sys.stderr)
        failed = True
    for version in ("v1", "v2"):
        surface = cast(dict[str, object], report[version])
        missing_functions = cast(list[str], surface["missing_functions"])
        missing_callbacks = cast(list[str], surface["missing_callbacks"])
        print(f"{version}: {surface['functions']} functions, {surface['callbacks']} callbacks ({surface['callback_typedefs']} typedefs)")
        for kind, missing in (("functions", missing_functions), ("callbacks", missing_callbacks)):
            if missing:
                print(f"Missing {version} {kind}: {', '.join(missing)}", file=sys.stderr)
                failed = True
    changes = cast(dict[str, object], report["v1_function_changes"])
    print(f"v1 changes: {len(cast(list[str], changes['added']))} added, {len(cast(list[str], changes['removed']))} removed, {len(cast(dict[str, object], changes['changed']))} changed signatures")
    return 1 if failed else 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, ValueError, KeyError, TypeError, tarfile.TarError) as error:
        print(f"duckdb-api: {error}", file=sys.stderr)
        sys.exit(1)
