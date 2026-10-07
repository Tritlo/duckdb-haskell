#!/usr/bin/env python3
# pyright: strict
"""Generate the complete V2 FFI from the pinned DuckDB header.

Use --check to compare the generated files without changing them. The parser
accepts the declaration forms in the snapshot. It fails on unknown C types or
unparsed public declarations. Haskell source files use the project formatter.
"""

from __future__ import annotations

import argparse
import hashlib
import re
import subprocess
import sys
import tarfile
from dataclasses import dataclass
from io import BytesIO
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
PACKAGE = ROOT / "duckdb-ffi"
ARCHIVE = PACKAGE / "vendor/duckdb-api.tar.gz"
CHECKSUM = PACKAGE / "vendor/duckdb-api.sha256"
HEADER_MEMBER = "duckdb-2.0/duckdb_v2.h"


def read_archive_header() -> str:
    """Read the pinned header from the verified archive without extraction."""
    payload = ARCHIVE.read_bytes()
    expected = CHECKSUM.read_text(encoding="ascii")
    if not re.fullmatch(r"[0-9a-f]{64}\n", expected):
        raise ValueError("vendor/duckdb-api.sha256 must contain one SHA256 digest and a newline")
    if hashlib.sha256(payload).hexdigest() != expected.rstrip():
        raise ValueError("vendor/duckdb-api.tar.gz does not match its SHA256 digest")
    with tarfile.open(fileobj=BytesIO(payload), mode="r:gz") as archive:
        members = [member for member in archive.getmembers() if member.name == HEADER_MEMBER]
        if len(members) != 1 or not members[0].isfile():
            raise ValueError(f"archive must contain exactly one regular file named {HEADER_MEMBER}")
        source = archive.extractfile(members[0])
        if source is None:
            raise ValueError(f"cannot read archive member {HEADER_MEMBER}")
        with source:
            return source.read().decode("utf-8")


def camel(name: str) -> str:
    return "".join(word[0].upper() + word[1:].lower() for word in name.split("_"))


def hs_name(name: str) -> str:
    if name.startswith("duckdb_v2_"):
        return "DuckDBV2" + camel(name.removeprefix("duckdb_v2_"))
    if name.startswith("DUCKDB_V2_"):
        return "DuckDBV2" + camel(name.removeprefix("DUCKDB_V2_"))
    if name.startswith("Arrow"):
        parts = name.split("_")
        return "DuckDBV2" + parts[0] + "".join(camel(part) for part in parts[1:])
    raise ValueError(f"unknown public name: {name}")


def strip_comments(source: str) -> str:
    # Preserve positions so documentation can be read from the original file.
    return re.sub(r"/\*.*?\*/|//[^\n]*", lambda m: "".join("\n" if c == "\n" else " " for c in m[0]), source, flags=re.S)


def source_doc(source: str, position: int, fallback: str) -> str:
    prefix = source[:position].rstrip()
    if prefix.endswith("*/"):
        start = prefix.rfind("/*!")
        if start >= 0 and "*/" not in prefix[start:-2]:
            raw = prefix[start + 3 : -2]
            lines = [re.sub(r"^\s*\* ?", "", line).rstrip() for line in raw.splitlines()]
            return "\n".join(lines).strip()
    last = prefix.splitlines()[-1] if prefix else ""
    if last.lstrip().startswith("//!"):
        return last.lstrip()[3:].strip()
    return fallback


def haddock(documentation: str) -> str:
    # Convert upstream Markdown and Doxygen markup. Keep the source wording.
    documentation = re.sub(r"(?m)^@param\s+(\w+)\s+", lambda m: "\n@" + m[1] + "@: ", documentation)
    documentation = re.sub(r"(?m)^@return\s+", "\nReturns ", documentation)

    def inline_code(match: re.Match[str]) -> str:
        if match[1] is not None:
            return "@" + match[1].replace("@", "\\@").replace("'", "\\'") + "@"
        if match[2] is not None:
            return match[0]
        name = match[3]
        if name.startswith(("DuckDBV2", "duckdbV2")):
            return match[0]
        return "@" + name + "@"

    # Existing Haddock code spans and Haskell links must stay unchanged.
    documentation = re.sub(r"`([^`]+)`|@([^@]+)@|(?<!\w)'([A-Za-z_][\w.]*)'(?!\w)", inline_code, documentation)
    documentation = documentation.replace("{-", "{ -").replace("-}", "- }")
    return "{- | " + documentation + "\n-}\n"


@dataclass(frozen=True)
class Parameter:
    ctype: str
    name: str


def parameters(text: str, unnamed: bool = False) -> list[Parameter]:
    if not text.strip() or text.strip() == "void":
        return []
    result: list[Parameter] = []
    for index, raw in enumerate(text.split(",")):
        raw = re.sub(r"\s+", " ", raw.strip())
        if unnamed and (raw.endswith("*") or raw in {"void", "int", "bool"}):
            result.append(Parameter(raw, f"arg{index}"))
            continue
        match = re.fullmatch(r"(.+?[\s*])(\w+)", raw)
        if not match:
            raise ValueError(f"cannot parse parameter: {raw}")
        result.append(Parameter(match[1].strip(), match[2]))
    return result


@dataclass(frozen=True)
class Function:
    result: str
    name: str
    params: list[Parameter]
    doc: str


# These layouts also occur in the original C API. Reuse their Haskell storage
# only while every field and callback signature matches the pinned V2 header.
SHARED_STRUCTURES: dict[str, tuple[str, tuple[tuple[str, str], ...]]] = {
    "ArrowSchema": ("ArrowSchema", (
        ("const char *", "format"), ("const char *", "name"), ("const char *", "metadata"),
        ("int64_t", "flags"), ("int64_t", "n_children"), ("struct ArrowSchema **", "children"),
        ("struct ArrowSchema *", "dictionary"), ("ArrowSchema_release_fn", "release"), ("void *", "private_data"),
    )),
    "ArrowArray": ("ArrowArray", (
        ("int64_t", "length"), ("int64_t", "null_count"), ("int64_t", "offset"),
        ("int64_t", "n_buffers"), ("int64_t", "n_children"), ("const void **", "buffers"),
        ("struct ArrowArray **", "children"), ("struct ArrowArray *", "dictionary"),
        ("ArrowArray_release_fn", "release"), ("void *", "private_data"),
    )),
    "ArrowArrayStream": ("ArrowArrayStream", (
        ("ArrowArrayStream_get_schema_fn", "get_schema"), ("ArrowArrayStream_get_next_fn", "get_next"),
        ("ArrowArrayStream_get_last_error_fn", "get_last_error"), ("ArrowArrayStream_release_fn", "release"),
        ("void *", "private_data"),
    )),
    "duckdb_v2_list_entry": ("DuckDBListEntry", (("idx_t", "offset"), ("idx_t", "length"))),
    "duckdb_v2_hugeint_t": ("DuckDBHugeInt", (("uint64_t", "lower"), ("int64_t", "upper"))),
    "duckdb_v2_uhugeint_t": ("DuckDBUHugeInt", (("uint64_t", "lower"), ("uint64_t", "upper"))),
    "duckdb_v2_interval_t": ("DuckDBInterval", (("int32_t", "months"), ("int32_t", "days"), ("int64_t", "micros"))),
}

SHARED_ARROW_CALLBACKS: dict[str, tuple[str, str, tuple[str, ...]]] = {
    "ArrowSchema_release_fn": ("ArrowSchemaRelease", "void", ("struct ArrowSchema *",)),
    "ArrowArray_release_fn": ("ArrowArrayRelease", "void", ("struct ArrowArray *",)),
    "ArrowArrayStream_get_schema_fn": ("ArrowStreamGetSchema", "int", ("struct ArrowArrayStream *", "struct ArrowSchema *")),
    "ArrowArrayStream_get_next_fn": ("ArrowStreamGetNext", "int", ("struct ArrowArrayStream *", "struct ArrowArray *")),
    "ArrowArrayStream_get_last_error_fn": ("ArrowStreamGetLastError", "const char *", ("struct ArrowArrayStream *",)),
    "ArrowArrayStream_release_fn": ("ArrowStreamRelease", "void", ("struct ArrowArrayStream *",)),
}


class Generator:
    def __init__(self, source: str) -> None:
        self.source = source
        self.clean = strip_comments(source)
        self.handles: dict[str, str] = {}
        self.aliases: dict[str, str] = {}
        self.enums: dict[str, list[str]] = {}
        self.callbacks: dict[str, Function] = {}
        self.structs: dict[str, list[Parameter]] = {}
        self.docs: dict[str, str] = {}
        self.functions: list[Function] = []
        self.parse()

    def parse(self) -> None:
        handle_pattern = r"typedef\s+struct\s+(\w+)\s*\{\s*void\s*\*\s*internal_ptr\s*;\s*\}\s*\*\s*(duckdb_v2_\w+)\s*;"
        for m in re.finditer(handle_pattern, self.clean):
            self.handles[m[2]] = m[1]
            self.docs[m[2]] = source_doc(self.source, m.start(), f"Opaque handle for @{m[2]}@.")
        self.handles["duckdb_v2_extension_handle"] = "_duckdb_extension_info"
        for m in re.finditer(r"typedef\s+enum\s+(DUCKDB_V2_\w+)\s*\{(.*?)\}\s*(\w+)\s*;", self.clean, re.S):
            if m[1] != m[3]:
                raise ValueError("enum tag and typedef differ")
            entries = [x.strip() for x in m[2].split(",") if x.strip()]
            names: list[str] = []
            for entry in entries:
                em = re.fullmatch(r"(DUCKDB_V2_\w+)\s*=\s*(0x[0-9A-Fa-f]+|[0-9]+)", entry)
                if not em:
                    raise ValueError(f"cannot parse enum member: {entry}")
                names.append(em[1])
                pos = self.clean.index(em[1], m.start())
                self.docs[em[1]] = source_doc(self.source, pos, f"The @{em[1]}@ constant.")
            self.enums[m[1]] = names
            self.docs[m[1]] = source_doc(self.source, m.start(), f"The @{m[1]}@ enumeration.")
        for m in re.finditer(r"typedef\s+([\w\s*]+?)\(\s*\*\s*(duckdb_v2_\w+)\s*\)\s*\((.*?)\)\s*;", self.clean, re.S):
            name = m[2]
            self.callbacks[name] = Function(m[1].strip(), name, parameters(m[3]), source_doc(self.source, m.start(), f"Callback for @{name}@."))
        for m in re.finditer(r"typedef\s+(\w+)\s+(duckdb_v2_\w+)\s*;", self.clean):
            self.aliases[m[2]] = m[1]
            self.docs[m[2]] = source_doc(self.source, m.start(), f"The @{m[2]}@ type alias.")
        for m in re.finditer(r"(?m)^struct\s+(duckdb_v2_\w+|Arrow\w+)\s*\{(.*?)^\};", self.clean, re.S):
            name = m[1]
            self.docs[name] = source_doc(self.source, m.start(), f"The @{name}@ structure.")
            if name == "duckdb_v2_bytes":
                # This is the only nested union in the snapshot. Verify its shape.
                normalized = re.sub(r"\s+", "", m[2])
                expected = "union{struct{uint32_tlength;charprefix[4];char*ptr;}pointer;struct{uint32_tlength;charinlined[12];}inlined;}value;"
                if normalized != expected:
                    raise ValueError("duckdb_v2_bytes layout changed; update its union representation")
                self.structs[name] = []
                continue
            fields: list[Parameter] = []
            for raw in m[2].split(";"):
                raw = raw.strip()
                if not raw:
                    continue
                fm = re.fullmatch(r"([\w\s*]+?)\(\s*\*\s*(\w+)\s*\)\s*\((.*?)\)", raw, re.S)
                if fm:
                    callback = f"{name}_{fm[2]}_fn"
                    self.callbacks[callback] = Function(fm[1].strip(), callback, parameters(fm[3], unnamed=True), f"The @{name}.{fm[2]}@ callback.")
                    fields.append(Parameter(callback, fm[2]))
                else:
                    fields.extend(parameters(raw))
            self.structs[name] = fields
        pattern = r"(?m)^DUCKDB_C_API\s+([\w\s*]+?)\s+(duckdb_v2_\w+)\s*\((.*?)\)\s*;"
        for m in re.finditer(pattern, self.clean, re.S):
            self.functions.append(Function(m[1].strip(), m[2], parameters(m[3]), source_doc(self.source, m.start(), f"Calls @{m[2]}@.")))
        declared = len(re.findall(r"(?m)^DUCKDB_C_API\b", self.clean))
        if len(self.functions) != declared:
            raise ValueError(f"parsed {len(self.functions)} of {declared} public declarations")
        if len({f.name for f in self.functions}) != len(self.functions):
            raise ValueError("duplicate public function")
        # Every public typedef must be recognized, including forward declarations.
        recognized = set(self.handles) | set(self.aliases) | set(self.enums) | set(self.callbacks) | set(self.structs) | {"idx_t"}
        for m in re.finditer(r"typedef\b.*?;", self.clean, re.S):
            text = m[0]
            if "internal_ptr" in text:
                continue  # The inner semicolon precedes the handle alias.
            names = re.findall(r"\b(?:duckdb_v2_\w+|DUCKDB_V2_\w+|idx_t)\b", text)
            if not names or names[-1] not in recognized:
                raise ValueError(f"unrecognized typedef: {text.strip()}")
        for function in [*self.functions, *self.callbacks.values()]:
            self.hs_type(function.result)
            if self.by_value(function.result):
                raise ValueError(f"structure-by-value result requires an explicit C bridge: {function.name}")
            for param in function.params:
                self.hs_type(param.ctype)
                if self.by_value(param.ctype):
                    raise ValueError(f"structure-by-value parameter requires an explicit C bridge: {function.name}.{param.name}")
        self.check_shared_types()

    def check_shared_types(self) -> None:
        if not re.search(r"typedef\s+uint64_t\s+idx_t\s*;", self.clean) or self.aliases.get("duckdb_v2_sel_t") != "uint32_t":
            raise ValueError("shared index or selection type changed")
        for name, (_, expected) in SHARED_STRUCTURES.items():
            actual = tuple((field.ctype, field.name) for field in self.structs.get(name, []))
            if actual != expected:
                raise ValueError(f"shared structure changed: {name}")
        callbacks = {name: (result, params) for name, (_, result, params) in SHARED_ARROW_CALLBACKS.items()}
        callbacks["duckdb_v2_opaque_destroy_fn"] = ("void", ("void *",))
        for name, expected in callbacks.items():
            callback = self.callbacks.get(name)
            if callback is None or (callback.result, tuple(param.ctype for param in callback.params)) != expected:
                raise ValueError(f"shared callback signature changed: {name}")

    def base(self, ctype: str) -> str:
        return re.sub(r"\b(?:const|struct)\b|\*", "", ctype).strip()

    def by_value(self, ctype: str) -> bool:
        if "*" in ctype:
            return False
        base = self.base(ctype)
        while base in self.aliases:
            base = self.aliases[base]
        return base in self.structs

    def hs_type(self, ctype: str) -> str:
        base = self.base(ctype)
        depth = ctype.count("*")
        primitives = {"void": "()", "bool": "CBool", "char": "CChar", "int": "CInt", "float": "CFloat", "double": "CDouble", "idx_t": "DuckDBV2Idx", "uint8_t": "Word8", "uint16_t": "Word16", "uint32_t": "Word32", "uint64_t": "Word64", "int8_t": "Int8", "int16_t": "Int16", "int32_t": "Int32", "int64_t": "Int64"}
        if base in primitives:
            hs = primitives[base]
        elif base in self.handles or base in self.aliases or base in self.enums or base in self.structs:
            hs = hs_name(base)
        elif base == "duckdb_v2_opaque_destroy_fn":
            hs = "DuckDBDeleteCallback"
        elif base in self.callbacks:
            hs = "FunPtr " + hs_name(base)
        else:
            raise ValueError(f"unknown C type: {ctype}")
        for _ in range(depth):
            hs = "Ptr " + (f"({hs})" if " " in hs else hs)
        return hs

    def signature(self, function: Function) -> str:
        parts = [self.hs_type(p.ctype) for p in function.params]
        result = self.hs_type(function.result)
        parts.append("IO " + (f"({result})" if " " in result else result))
        return " -> ".join(parts)

    def types(self) -> str:
        shared = ",\n    ".join(f"{target}(..)" for target, _ in SHARED_STRUCTURES.values())
        shared_names = "DuckDBIdx, DuckDBSel, DuckDBDeleteCallback"
        out = ["{-# LANGUAGE CPP #-}\n{-# LANGUAGE EmptyDataDecls #-}\n{-# LANGUAGE GeneralizedNewtypeDeriving #-}\n{-# LANGUAGE PatternSynonyms #-}\n{-# LANGUAGE RecordWildCards #-}\n\n", haddock("Raw types for the DuckDB V2 C API.\n\nGenerated from the pinned header by @scripts/gen_ffi_v2.py@.\nShared C layouts use the types from @Database.DuckDB.FFI.Types@.\nOther layouts and enum values come from the header through hsc2hs.\nKeep this module and the native library at the same upstream revision.\nSource documentation records the upstream API lifecycle status."), f"module Database.DuckDB.FFI.V2.Types (\n    module Database.DuckDB.FFI.V2.Types,\n    {shared},\n    {shared_names}\n    ) where\n\n", "#define DUCKDB_V2_API_ALLOW_UNSTABLE 1\n#include \"duckdb_v2.h\"\n\n", f"import Database.DuckDB.FFI.Types (\n    {shared},\n    {shared_names}\n    )\n", "import Data.Word (Word32, Word64)\nimport Foreign.C.Types (CBool(..), CChar(..), CInt(..))\nimport Foreign.Ptr (Ptr, FunPtr, castPtr, plusPtr)\nimport Foreign.Storable (Storable(..), peekByteOff, pokeByteOff)\n\n", haddock("DuckDB's unsigned index type."), "type DuckDBV2Idx = DuckDBIdx\n\n"]
        for name in self.handles:
            marker = hs_name(name.removesuffix("_handle"))
            out += [haddock(f"Opaque target of @{name}@.\n\nDo not read or write the target storage."), f"data {marker}\n\n", haddock(self.docs.get(name, f"The @{name}@ handle.")), f"type {hs_name(name)} = Ptr {marker}\n\n"]
        for name, target in self.aliases.items():
            hs_target = "DuckDBSel" if name == "duckdb_v2_sel_t" else self.hs_type(target)
            out += [haddock(self.docs[name]), f"type {hs_name(name)} = {hs_target}\n\n"]
        for name, members in self.enums.items():
            hs = hs_name(name)
            out += [haddock(self.docs[name]), f"newtype {hs} = {hs} (#{'{type ' + name + '}'})\n    deriving (Eq, Ord, Show, Read, Storable)\n\n"]
            for member in members:
                pat = hs_name(member)
                out += [haddock(self.docs[member]), f"pattern {pat} :: {hs}\npattern {pat} = {hs} (#{{const {member}}})\n\n"]
        for name, callback in self.callbacks.items():
            out += [haddock(callback.doc), f"type {hs_name(name)} = {self.signature(callback)}\n\n"]
        out += [haddock("Maximum inline byte count in @duckdb_v2_bytes@."), "duckdbV2BytesInlineLength :: Word32\nduckdbV2BytesInlineLength = #{const DUCKDB_V2_BYTES_INLINE_LENGTH}\n\n"]
        for constant in ("DUCKDB_V2_API_VERSION_MAJOR", "DUCKDB_V2_API_VERSION_MINOR", "DUCKDB_V2_API_VERSION_PATCH", "ARROW_FLAG_DICTIONARY_ORDERED", "ARROW_FLAG_NULLABLE", "ARROW_FLAG_MAP_KEYS_SORTED"):
            public = "duckdbV2" + camel(constant.removeprefix("DUCKDB_V2_").removeprefix("ARROW_"))
            if constant.startswith("ARROW_"):
                public = "duckdbV2Arrow" + camel(constant.removeprefix("ARROW_"))
            out += [haddock(f"The @{constant}@ constant from the pinned header."), f"{public} :: Word32\n{public} = #{{const {constant}}}\n\n"]
        for name in self.structs:
            out.append(self.structure(name))
        return "".join(out)

    def structure(self, name: str) -> str:
        hs = hs_name(name)
        if name in SHARED_STRUCTURES:
            target, _ = SHARED_STRUCTURES[name]
            return haddock(self.docs[name] + f"\n\nUses the shared @{target}@ storage and constructors.") + f"type {hs} = {target}\n\n"
        c_name = "struct " + name if name.startswith("Arrow") else name
        fields = self.structs[name]
        if name == "duckdb_v2_bytes":
            # Three fixed-width words preserve all 12 union bytes, including a
            # pointer on a 64-bit host, without interpreting or owning them.
            fields = [Parameter("uint32_t", "length"), Parameter("uint32_t", "storage0"), Parameter("uint32_t", "storage1"), Parameter("uint32_t", "storage2")]
            offsets = ["#{offset duckdb_v2_bytes, value.inlined.length}"] + [f"(#{{offset duckdb_v2_bytes, value.inlined.inlined}} + {i})" for i in (0, 4, 8)]
            doc = self.docs[name] + "\n\nThe three storage words preserve the union bytes. Read the pointer arm\nwith 'duckdbV2BytesPointer', or access the inline bytes with\n'duckdbV2BytesInlinePointer'. These functions borrow the supplied storage."
        else:
            offsets = [f"#{{offset {c_name}, {p.name}}}" for p in fields]
            doc = self.docs[name]
        record_names = ["duckdbV2" + hs.removeprefix("DuckDBV2") + camel(p.name) for p in fields]
        out = [haddock(doc), f"data {hs} = {hs}\n    {{ "]
        for index, p in enumerate(fields):
            if index:
                out.append("    , ")
            out.append(f"{record_names[index]} :: {self.hs_type(p.ctype)}\n")
        out += ["    } deriving (Eq, Show)\n\n", f"instance Storable {hs} where\n    sizeOf _ = #{{size {c_name}}}\n    alignment _ = #{{alignment {c_name}}}\n    peek ptr = {hs}\n"]
        for index, offset in enumerate(offsets):
            out.append(f"        {'<$>' if index == 0 else '<*>'} peekByteOff ptr {offset}\n")
        out.append(f"    poke ptr {hs}{{..}} = do\n")
        for record, offset in zip(record_names, offsets):
            out.append(f"        pokeByteOff ptr {offset} {record}\n")
        out.append("\n")
        if name == "duckdb_v2_bytes":
            out += [haddock("Return the borrowed payload pointer of the non-inline union arm.\n\nCall this only when the stored length exceeds 'duckdbV2BytesInlineLength'.\nThe pointer remains valid only while the owning vector or value is alive."), "duckdbV2BytesPointer :: Ptr DuckDBV2Bytes -> IO (Ptr CChar)\nduckdbV2BytesPointer ptr = peekByteOff ptr #{offset duckdb_v2_bytes, value.pointer.ptr}\n\n", haddock("Return a pointer to the inline bytes of the supplied storage.\n\nCall this only when the stored length is at most 'duckdbV2BytesInlineLength'.\nThe pointer remains valid only while the supplied storage is alive."), "duckdbV2BytesInlinePointer :: Ptr DuckDBV2Bytes -> Ptr CChar\nduckdbV2BytesInlinePointer ptr = castPtr ptr `plusPtr` #{offset duckdb_v2_bytes, value.inlined.inlined}\n\n"]
        return "".join(out)

    def imports(self) -> str:
        out = ["{-# LANGUAGE ForeignFunctionInterface #-}\n\n", haddock("Complete raw DuckDB V2 C API.\n\nGenerated from the pinned header by @scripts/gen_ffi_v2.py@.\nEvery import calls the C function directly with its native signature.\nEvery import is safe because calls can run registered Haskell callbacks.\nSource documentation records the upstream API lifecycle status.\nCallers must pin the matching native library."), "module Database.DuckDB.FFI.V2.Functions where\n\nimport qualified Database.DuckDB.FFI.Arrow as Arrow\nimport Database.DuckDB.FFI.V2.Types\nimport Data.Int (Int8, Int16, Int32, Int64)\nimport Data.Word (Word8, Word16, Word32, Word64)\nimport Foreign.C.Types (CBool(..), CChar(..), CInt(..), CFloat(..), CDouble(..))\nimport Foreign.Ptr (Ptr, FunPtr)\n\n"]
        for function in self.functions:
            out += [haddock(function.doc), f'foreign import ccall safe "{function.name}"\n    c_{function.name} :: {self.signature(function)}\n\n']
        for name, callback in self.callbacks.items():
            hs = hs_name(name)
            pointer = self.hs_type(name)
            io_pointer = f"({pointer})" if " " in pointer else pointer
            out.append(haddock(f"Create a function pointer for '{hs}'.\n\nKeep the pointer alive while DuckDB can invoke it. Free the pointer with\n@freeHaskellFunPtr@ after its final possible invocation.\nThe callback must not throw an exception across the C boundary."))
            if name in SHARED_ARROW_CALLBACKS:
                target, _, _ = SHARED_ARROW_CALLBACKS[name]
                out += [f"mk{hs} :: {hs} -> IO {io_pointer}\nmk{hs} = Arrow.wrap{target}\n\n", haddock(f"Call a function pointer with the '{hs}' signature."), f"call{hs} :: {pointer} -> {self.signature(callback)}\ncall{hs} = Arrow.mk{target}\n\n"]
            else:
                out += [f'foreign import ccall "wrapper"\n    mk{hs} :: {hs} -> IO {io_pointer}\n\n', haddock(f"Call a function pointer with the '{hs}' signature."), f'foreign import ccall safe "dynamic"\n    call{hs} :: {pointer} -> {self.signature(callback)}\n\n']
        return "".join(out)

def format_haskell(source: str, filename: Path) -> str:
    substitutions: dict[str, str] = {}
    if filename.suffix == ".hsc":
        # Fourmolu accepts CPP directives but does not parse hsc2hs expressions.
        # Replace each expression with a distinct Haskell token for formatting.
        # Restore the exact expression after the formatter has finished.
        def placeholder(match: re.Match[str]) -> str:
            macro = match[0]
            kind = "DuckDBV2HscType" if macro.startswith("#{type ") else "duckdbV2HscValue"
            token = kind + str(len(substitutions))
            substitutions[token] = macro
            return token

        source = re.sub(r"#\{[^}]+\}", placeholder, source)
    result = subprocess.run(["fourmolu", "--stdin-input-file", str(filename)], input=source, text=True, capture_output=True)
    if result.returncode:
        raise ValueError(result.stderr)
    formatted = result.stdout
    for token, macro in substitutions.items():
        formatted = re.sub(r"\b" + token + r"\b", lambda _: macro, formatted)
    return formatted


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--header", type=Path, help="Use a local header instead of the verified vendor archive")
    args = parser.parse_args()
    gen = Generator(args.header.read_text() if args.header is not None else read_archive_header())
    outputs = {
        PACKAGE / "src/Database/DuckDB/FFI/V2/Types.hsc": gen.types(),
        PACKAGE / "src/Database/DuckDB/FFI/V2/Functions.hs": gen.imports(),
        PACKAGE / "src/Database/DuckDB/FFI/V2.hs": "-- | Raw bindings for the pinned DuckDB V2 C API.\nmodule Database.DuckDB.FFI.V2 (\n    module Database.DuckDB.FFI.V2.Types,\n    module Database.DuckDB.FFI.V2.Functions,\n) where\n\nimport Database.DuckDB.FFI.V2.Functions\nimport Database.DuckDB.FFI.V2.Types\n",
    }
    failed: list[str] = []
    for path, source in outputs.items():
        if path.suffix in {".hs", ".hsc"}:
            source = format_haskell(source, path)
        if args.check:
            if not path.exists() or path.read_text() != source:
                failed.append(str(path.relative_to(ROOT)))
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(source)
    if failed:
        raise ValueError("generated files differ: " + ", ".join(failed))
    print(f"{'Checked' if args.check else 'Generated'} {len(gen.functions)} functions, {len(gen.callbacks)} callbacks, {len(gen.handles)} handles, {len(gen.enums)} enums, and {len(gen.structs)} layouts")


if __name__ == "__main__":
    try:
        main()
    except (ValueError, OSError, tarfile.TarError) as error:
        print(f"V2 generation failed: {error}", file=sys.stderr)
        sys.exit(1)
