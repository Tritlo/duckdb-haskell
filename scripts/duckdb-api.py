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


V1_CALLBACK_ALIASES = {
    "duckdb_delete_callback_t": "DuckDBDeleteCallback",
    "duckdb_copy_callback_t": "DuckDBCopyCallback",
    "duckdb_table_function_bind_t": "DuckDBTableFunctionBindFun",
    "duckdb_table_function_init_t": "DuckDBTableFunctionInitFun",
    "duckdb_table_function_t": "DuckDBTableFunctionFun",
    "duckdb_replacement_callback_t": "DuckDBReplacementCallback",
    "duckdb_logger_write_log_entry_t": "DuckDBLoggerWriteLogEntryFun",
    "duckdb_copy_function_bind_t": "DuckDBCopyFunctionBindFun",
    "duckdb_copy_function_global_init_t": "DuckDBCopyFunctionGlobalInitFun",
    "duckdb_copy_function_sink_t": "DuckDBCopyFunctionSinkFun",
    "duckdb_copy_function_finalize_t": "DuckDBCopyFunctionFinalizeFun",
    "duckdb_aggregate_state_size": "DuckDBAggregateStateSizeFun",
    "duckdb_aggregate_init_t": "DuckDBAggregateInitFun",
    "duckdb_aggregate_destroy_t": "DuckDBAggregateDestroyFun",
    "duckdb_aggregate_update_t": "DuckDBAggregateUpdateFun",
    "duckdb_aggregate_combine_t": "DuckDBAggregateCombineFun",
    "duckdb_aggregate_finalize_t": "DuckDBAggregateFinalizeFun",
    "duckdb_scalar_function_bind_t": "DuckDBScalarFunctionBindFun",
    "duckdb_scalar_function_init_t": "DuckDBScalarFunctionInitFun",
    "duckdb_scalar_function_t": "DuckDBScalarFunctionFun",
    "duckdb_cast_function_t": "DuckDBCastFunctionFun",
}


SHARED_LAYOUTS = {
    "DuckDBV2HugeintT": ("DuckDBHugeInt", "duckdb_hugeint", "duckdb_v2_hugeint_t"),
    "DuckDBV2UhugeintT": ("DuckDBUHugeInt", "duckdb_uhugeint", "duckdb_v2_uhugeint_t"),
    "DuckDBV2IntervalT": ("DuckDBInterval", "duckdb_interval", "duckdb_v2_interval_t"),
    "DuckDBV2ListEntry": ("DuckDBListEntry", "duckdb_list_entry", "duckdb_v2_list_entry"),
    "DuckDBV2ArrowSchema": ("ArrowSchema", "ArrowSchema", "ArrowSchema"),
    "DuckDBV2ArrowArray": ("ArrowArray", "ArrowArray", "ArrowArray"),
    "DuckDBV2ArrowArrayStream": ("ArrowArrayStream", "ArrowArrayStream", "ArrowArrayStream"),
}
SHARED_ARROW_CALLBACKS = {
    "ArrowSchema_release_fn": "ArrowSchemaRelease",
    "ArrowArray_release_fn": "ArrowArrayRelease",
    "ArrowArrayStream_get_schema_fn": "ArrowStreamGetSchema",
    "ArrowArrayStream_get_next_fn": "ArrowStreamGetNext",
    "ArrowArrayStream_get_last_error_fn": "ArrowStreamGetLastError",
    "ArrowArrayStream_release_fn": "ArrowStreamRelease",
}


def shared_layouts(v1: str, v2: str, bindings: str) -> dict[str, str]:
    """Verify canonical type aliases and their shared C declarations."""
    canonical = typedefs(v1) | typedefs(ROOT / "duckdb-ffi/cbits/duckdb_arrow.h")
    current = typedefs(v2)
    aliases = {name: types[0] for name, types in SHARED_LAYOUTS.items()}
    aliases.update({"DuckDBV2Idx": "DuckDBIdx", "DuckDBV2SelT": "DuckDBSel"})
    for name, target in aliases.items():
        if not re.search(rf"^type\s+{name}\s*=\s*{target}\s*$", bindings, re.M):
            raise ValueError(f"Missing shared type alias: {name} = {target}")
    for name, target in (("DuckDBIdx", "Word64"), ("DuckDBSel", "Word32")):
        if not re.search(rf"^type\s+{name}\s*=\s*{target}\s*$", bindings, re.M):
            raise ValueError(f"Shared integer type differs: {name}")
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

    for alias, (target, before, after) in SHARED_LAYOUTS.items():
        if not re.search(rf"\binstance\s+Storable\s+{target}\s+where\b", bindings):
            raise ValueError(f"Missing canonical Storable instance: {target}")
        if fields(canonical[before]) != fields(current[after]):
            raise ValueError(f"Shared C structure differs: {alias}")
    return aliases


def coverage(header: Path | str, bindings: str, wrappers: str) -> dict[str, object]:
    """Check that each C function and callback has a raw binding."""
    declarations = functions(header)
    imports = set(re.findall(r'foreign\s+import\s+(?:ccall|capi)\s+(?:(?:safe|unsafe|interruptible)\s+)?"([^"\n]+)"', bindings))
    bound: dict[str, str] = {}
    wrapper_calls: dict[str, set[str]] = {}
    for match in re.finditer(r"\b(wrapped_duckdb_\w+)\s*\([^{};]*\)\s*{", wrappers):
        depth = 1
        end = match.end()
        while depth and end < len(wrappers):
            if wrappers[end] == "{":
                depth += 1
            elif wrappers[end] == "}":
                depth -= 1
            end += 1
        wrapper_calls[match.group(1)] = set(re.findall(r"\b(duckdb_\w+)\s*\(", wrappers[match.end():end]))
    for name in declarations:
        if name in imports:
            bound[name] = "direct"
        elif f"wrapped_{name}" in imports and name in wrapper_calls.get(f"wrapped_{name}", set()):
            bound[name] = "C wrapper"
    callbacks = {name: declaration for name, declaration in typedefs(header).items() if re.search(r"\(\*" + re.escape(name) + r"\)", declaration)}
    callback_typedef_count = len(callbacks)
    # Arrow structures contain callback fields without separate typedefs.
    source = header.read_text() if isinstance(header, Path) else header
    for struct in re.finditer(r"\bstruct\s+(Arrow\w+)\s*{([^{}]*)}", clean_c(source), re.S):
        for field in re.finditer(r"\(\s*\*\s*(\w+)\s*\)\s*\([^;]*\)\s*;", struct.group(2)):
            callbacks[f"{struct.group(1)}_{field.group(1)}_fn"] = normalize(field.group(0))
    aliases: dict[str, str] = {}
    for name in callbacks:
        if name.startswith("Arrow"):
            alias = "DuckDBV2" + "".join(part[0].upper() + part[1:] for part in name.split("_"))
        else:
            alias = V1_CALLBACK_ALIASES.get(name, "DuckDB" + "".join(part[0].upper() + part[1:] for part in name.removeprefix("duckdb_").split("_")))
        if name.startswith(("duckdb_v2_", "Arrow")):
            patterns = [
                rf"\btype\s+{re.escape(alias)}\s*=",
                rf'foreign\s+import\s+ccall\s+(?:(?:safe|unsafe)\s+)?"wrapper"\s+mk{re.escape(alias)}\b',
                rf'foreign\s+import\s+ccall\s+(?:(?:safe|unsafe)\s+)?"dynamic"\s+call{re.escape(alias)}\b',
            ]
            present = all(re.search(pattern, bindings) for pattern in patterns)
            if not present and name in SHARED_ARROW_CALLBACKS:
                target = SHARED_ARROW_CALLBACKS[name]
                shared_patterns = [
                    patterns[0],
                    r"\bimport\s+Database\.DuckDB\.FFI\.Arrow\s+qualified\s+as\s+Arrow\b",
                    rf"^mk{alias}\s*=\s*Arrow\.wrap{target}\s*$",
                    rf"^call{alias}\s*=\s*Arrow\.mk{target}\s*$",
                    rf'foreign\s+import\s+ccall\s+(?:(?:safe|unsafe)\s+)?"wrapper"\s+wrap{target}\b',
                    rf'foreign\s+import\s+ccall\s+(?:(?:safe|unsafe)\s+)?"dynamic"\s+mk{target}\b',
                ]
                present = all(re.search(pattern, bindings, re.M) for pattern in shared_patterns)
        else:
            present = re.search(rf"\btype\s+{re.escape(alias)}\s*=\s*FunPtr\b", bindings) is not None
        if present:
            aliases[name] = alias
    return {"functions": len(declarations), "bound": bound, "missing_functions": sorted(declarations.keys() - bound.keys()), "callbacks": len(callbacks), "callback_typedefs": callback_typedef_count, "callback_aliases": aliases, "missing_callbacks": sorted(callbacks.keys() - aliases.keys())}


def audit() -> dict[str, object]:
    """Compare the legacy v1 API and the pinned preview APIs."""
    metadata, members = read_archive()
    legacy = ROOT / "duckdb-ffi/cbits/duckdb.h"
    v1 = members[PREFIX + "duckdb.h"].decode()
    v2 = members[PREFIX + "duckdb_v2.h"].decode()
    bindings = "\n".join(path.read_text() for path in sorted((ROOT / "duckdb-ffi/src").rglob("*")) if path.suffix in (".hs", ".hsc"))
    wrappers = "\n".join(clean_c(path.read_text()) for path in sorted((ROOT / "duckdb-ffi/cbits").glob("*.c")))
    exports = set(functions(v1)) | set(functions(v2))
    imports = {name.removeprefix("&") for name in re.findall(r'foreign\s+import\s+(?:ccall|capi)\s+(?:(?:safe|unsafe|interruptible)\s+)?"([^"\n]+)"', bindings)}
    referenced = {name for name in imports if name.startswith("duckdb_")}
    referenced.update(re.findall(r"\b(duckdb_\w+)\s*\(", wrappers))
    return {
        "commit": metadata["commit"],
        "unknown_functions": sorted(referenced - exports),
        "v1": coverage(v1, bindings, wrappers),
        "v2": coverage(v2, bindings, wrappers),
        "shared_types": shared_layouts(v1, v2, bindings),
        "v1_function_changes": difference(functions(legacy), functions(v1)),
        "v1_type_changes": difference(typedefs(legacy), typedefs(v1)),
    }


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
