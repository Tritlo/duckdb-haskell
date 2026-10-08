"""Generate opaque type specifications and C checks from binding metadata."""

import json
import pathlib
import re
import subprocess
import sys
from typing import Any


def opaque_spec(header: pathlib.Path, metadata: dict[str, Any]) -> dict[str, Any]:
    """Keep the pointee types of native handles opaque."""
    tags = {
        "struct " + tag
        for tag, fields in re.findall(
            r"typedef\s+struct\s+(_duckdb_\w+)\s*\{([^}]*)\}\s*\*\s*duckdb_\w+\s*;",
            header.read_text(),
        )
        if re.fullmatch(r"\s*void\s*\*\s*internal_ptr\s*;\s*", fields)
    }
    types = [
        {key: entry[key] for key in ("headers", "cname", "hsname")}
        for entry in metadata["ctypes"]
        if entry["cname"] in tags
    ]
    if {entry["cname"] for entry in types} != tags:
        raise ValueError("The binding metadata does not contain every native handle.")
    return {
        "version": metadata["version"],
        "ctypes": types,
        "hstypes": [
            {"hsname": entry["hsname"], "representation": "emptydata"}
            for entry in types
        ],
    }


def abi_checks(
    source: pathlib.Path, metadata: dict[str, Any], headers: pathlib.Path
) -> str:
    """Use Clang to evaluate the layouts represented by the generated bindings."""
    text = source.read_text()
    types = {
        entry["hsname"]
        for entry in metadata["hstypes"]
        if "StaticSize" in entry.get("instances", [])
    }
    records = {
        entry["hsname"]
        for entry in metadata["hstypes"]
        if {"IsStruct", "IsUnion"} & set(entry.get("instances", []))
    }
    c_types: dict[str, str] = {}
    for entry in metadata["ctypes"]:
        name, c_name = entry["hsname"], entry["cname"]
        if name in types and "@" not in c_name:
            # Prefer a typedef. Some anonymous records have a synthetic tag.
            if name not in c_types or " " not in c_name:
                c_types[name] = c_name
    field_pattern = re.compile(
        r'instance HasCField\.HasCField (\w+) "([^"]+)" where\s+'
        r'type CFieldType \w+ "[^"]+" =\s+(.*?)\s+'
        r"offset# = \\_ -> \\_ -> (\d+)",
        re.DOTALL,
    )
    fields: list[tuple[str, str, str, int]] = []
    for match in field_pattern.finditer(text):
        owner, _hs_field, child, offset = match.groups()
        if owner not in records:
            continue
        # Read the C name. A naming policy can change the Haskell field name.
        comment = text[text.rfind("{-|", 0, match.start()) : match.start()]
        declaration = re.search(r"__C declaration:__ @(\w+)@", comment)
        if declaration is None:
            raise ValueError(f"Cannot read the C field name for {owner}")
        fields.append((owner, declaration[1], child.strip(), int(offset)))
    while True:
        before = len(c_types)
        for owner, field, child, _offset in fields:
            if owner in c_types and child in types and child not in c_types:
                c_types[child] = f"__typeof__((({c_types[owner]} *)0)->{field})"
        if len(c_types) == before:
            break
    if types != c_types.keys():
        raise ValueError(
            f"Cannot map generated types to C: {sorted(types - c_types.keys())}"
        )
    layouts = {
        name: (int(size), int(alignment))
        for name, size, alignment in re.findall(
            r"instance Marshal\.StaticSize (\w+) where\s+"
            r"staticSizeOf = \\_ -> \((\d+) :: Int\)\s+"
            r"staticAlignment = \\_ -> \((\d+) :: Int\)",
            text,
        )
    }
    for size, alignment, name in re.findall(
        r"deriving via BG\.SizedByteArray (\d+) (\d+) instance Marshal\.StaticSize (\w+)",
        text,
    ):
        layouts[name] = (int(size), int(alignment))
    explicit_layouts = set(
        re.findall(r"^instance Marshal\.StaticSize (\w+) where", text, re.MULTILINE)
    )
    if not explicit_layouts <= layouts.keys() or not records <= layouts.keys():
        raise ValueError("Cannot read every generated record layout.")
    if {owner for owner, _field, _child, _offset in fields} != records:
        raise ValueError("Cannot read every generated record field.")
    field_count = sum(
        owner in records
        for owner in re.findall(
            r'^instance HasCField\.HasCField (\w+) "[^"]+" where',
            text,
            re.MULTILINE,
        )
    )
    if len(fields) != field_count:
        raise ValueError("Cannot read every generated field offset.")
    expressions: list[tuple[str, str]] = []
    generated_values: dict[str, int] = {}
    for name in sorted(types):
        c_type = c_types[name]
        expressions.extend(
            [
                (f"sizeof({c_type})", f"{name}: size"),
                (f"_Alignof({c_type})", f"{name}: alignment"),
            ]
        )
        if name in layouts:
            size, alignment = layouts[name]
            generated_values[f"sizeof({c_type})"] = size
            generated_values[f"_Alignof({c_type})"] = alignment
    for owner, field, _child, offset in fields:
        c_type = c_types[owner]
        expression = f"__builtin_offsetof({c_type}, {field})"
        expressions.extend(
            [
                (expression, f"{owner}.{field}: offset"),
                (f"sizeof((({c_type} *)0)->{field})", f"{owner}.{field}: size"),
            ]
        )
        generated_values[expression] = offset
    includes = '#include "duckdb.h"\n#include "duckdb_arrow.h"\n'
    probe = (
        includes
        + "enum {\n"
        + ",\n".join(
            f"hs_bindgen_abi_{index} = {expression}"
            for index, (expression, _label) in enumerate(expressions)
        )
        + "\n};\n"
    )
    result = subprocess.run(
        [
            "clang",
            "--target=x86_64-unknown-linux-gnu",
            "-ffreestanding",
            "-I",
            str(headers),
            "-Xclang",
            "-ast-dump=json",
            "-fsyntax-only",
            "-x",
            "c",
            "-",
        ],
        input=probe,
        capture_output=True,
        text=True,
        check=True,
    )
    values: dict[str, int] = {}

    def visit(node: dict[str, Any]) -> None:
        if node.get("kind") == "EnumConstantDecl" and node.get("name", "").startswith(
            "hs_bindgen_abi_"
        ):

            def constant(child: dict[str, Any]) -> int | None:
                if child.get("kind") == "ConstantExpr":
                    return int(child["value"])
                for inner in child.get("inner", []):
                    found = constant(inner)
                    if found is not None:
                        return found
                return None

            value = constant(node)
            if value is None:
                raise ValueError(f"Clang did not evaluate {node['name']}")
            values[node["name"]] = value
        for child in node.get("inner", []):
            visit(child)

    visit(json.loads(result.stdout))
    checks: list[str] = []
    for index, (expression, label) in enumerate(expressions):
        value = values[f"hs_bindgen_abi_{index}"]
        if expression in generated_values and value != generated_values[expression]:
            raise ValueError(f"Clang and the generated bindings disagree for {label}")
        checks.append(
            f'_Static_assert({expression} == {value}, "duckdb-ffi ABI: {label}");'
        )
    return (
        "/* Generated by scripts/generate-bindings.sh. Do not edit. */\n"
        + includes
        + "#if !(defined(__linux__) || (defined(__APPLE__) && "
        "defined(__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__))) || "
        "!(defined(__x86_64__) || defined(__aarch64__))\n"
        '#error "duckdb-ffi supports Linux and macOS on x86_64 and aarch64."\n'
        "#endif\n" + "\n".join(checks) + "\n"
    )


def compat_module(source: pathlib.Path, spellings: dict[str, dict[str, str]]) -> str:
    """Generate name aliases without changing the native types or calls."""
    text = source.read_text()
    type_names = set(re.findall(r"^(?:data|newtype|type) (\w+)", text, re.MULTILINE))
    legacy_types = spellings["types"]
    if not set(legacy_types.values()) <= type_names:
        raise ValueError("A legacy type name does not have a generated type.")

    def qualify(signature: str) -> str:
        def identifier(match: re.Match[str]) -> str:
            name = match[0]
            if "." in name or name == "IO" or not name[0].isupper():
                return name
            if name not in type_names:
                raise ValueError(f"Unknown generated signature type: {name}")
            return "Raw." + name

        return re.sub(r"[A-Za-z_][\w']*(?:\.[A-Za-z_][\w']*)*", identifier, signature)

    def clean_type(signature: str) -> str:
        without_blocks = re.sub(r"\{-.*?-\}", "", signature, flags=re.DOTALL)
        without_lines = re.sub(r"--[^\n]*", "", without_blocks)
        return qualify(" ".join(without_lines.split()))

    exports = ["module Raw"]
    declarations: list[str] = []
    for legacy, native in legacy_types.items():
        if legacy == native:
            continue
        exports.append(legacy)
        declarations.append(
            f"-- | Name for @Raw.{native}@.\ntype {legacy} = Raw.{native}"
        )

    raw_patterns = dict(re.findall(r"^pattern (\w+) :: (\w+)$", text, re.MULTILINE))
    for legacy, native in spellings["patterns"].items():
        if native not in raw_patterns:
            raise ValueError(f"Unknown generated enum pattern: {native}")
        if legacy == native:
            continue
        native_type = raw_patterns[native]
        pattern_type = qualify(native_type)
        constructor = "Raw." + native
        if native_type == "DUCKDB_TYPE":
            pattern_type = "DuckDBType"
            constructor = "Raw.Duckdb_type " + constructor
        exports.append("pattern " + legacy)
        declarations.append(
            f"-- | Name for @Raw.{native}@.\n"
            f"pattern {legacy} :: {pattern_type}\npattern {legacy} = {constructor}"
        )

    records = re.finditer(
        r"^(?:data|newtype) (\w+) = (\w+)\s*\{(.*?)^\s*\}",
        text,
        re.MULTILINE | re.DOTALL,
    )
    constructors: dict[str, tuple[str, list[str]]] = {}
    for record in records:
        owner, constructor, field_block = record.groups()
        fields = re.findall(
            r"(?:^|,\s*)\s*[\w']+ ::\s*(.*?)(?=,\s*[\w']+ ::|\Z)",
            field_block,
            re.DOTALL,
        )
        if not fields:
            raise ValueError(f"Cannot read the generated constructor for {owner}")
        constructors[owner] = (
            constructor,
            [clean_type(field) for field in fields],
        )
    for legacy, native in legacy_types.items():
        if legacy == native or native not in constructors:
            continue
        constructor, fields = constructors[native]
        arguments = " ".join(f"x{index}" for index in range(len(fields)))
        signature = " -> ".join([f"({field})" for field in fields] + [legacy])
        exports.append("pattern " + legacy)
        declarations.append(
            f"-- | Use the generated @Raw.{constructor}@ constructor.\n"
            f"pattern {legacy} :: {signature}\n"
            f"pattern {legacy} {arguments} = Raw.{constructor} {arguments}\n"
            f"{{-# COMPLETE {legacy} #-}}"
        )

    raw_exports = set(
        re.findall(
            r"^\s*, Database\.DuckDB\.FFI\.(duckdb_\w+)$",
            text,
            re.MULTILINE,
        )
    )
    functions: dict[str, str] = {}
    for match in re.finditer(r"^(duckdb_\w+) ::", text, re.MULTILINE):
        name = match[1]
        end = text.index("\n" + name + " =", match.end())
        signature = clean_type(text[match.end() : end])
        if not re.search(r"\bIO\b", signature):
            raise ValueError(f"The native function signature is not an IO call: {name}")
        functions[name] = signature
    if not functions or functions.keys() != raw_exports:
        raise ValueError("The generated exports and native function signatures differ.")
    for native, signature in functions.items():
        legacy = "c_" + native
        exports.append(legacy)
        declarations.append(
            f"-- | Name for @Raw.{native}@.\n"
            f"{legacy} :: {signature}\n{legacy} = Raw.{native}"
        )

    body = "\n\n".join(declarations) + "\n"
    imports: list[str] = []
    for line in re.findall(r"^import qualified .+$", text, re.MULTILINE):
        alias = line.split(" as ")[-1] if " as " in line else line.split()[-1]
        if re.search(r"(?<![\w.])" + re.escape(alias) + r"\.", body):
            imports.append(line)
    return (
        "-- Generated by scripts/generate-bindings.sh. Do not edit.\n"
        "{-# LANGUAGE DataKinds #-}\n{-# LANGUAGE PatternSynonyms #-}\n"
        "-- | Names from the earlier API. Types and calls use the generated API.\n"
        "-- Native handles, const pointers, callbacks, and records retain their types.\n"
        "-- This module does not restore old record selectors or NULL helpers.\n"
        "module Database.DuckDB.FFI.Compat\n  ( "
        + "\n  , ".join(exports)
        + "\n  ) where\n\n"
        "import Database.DuckDB.FFI as Raw\n" + "\n".join(imports) + "\n\n" + body
    )


def main() -> None:
    """Write one generated metadata artifact."""
    mode, input_path, metadata_path, output_path, *rest = sys.argv[1:]
    metadata = json.loads(pathlib.Path(metadata_path).read_text())
    if mode == "opaque":
        output = (
            json.dumps(opaque_spec(pathlib.Path(input_path), metadata), indent=2) + "\n"
        )
    elif mode == "abi":
        output = abi_checks(pathlib.Path(input_path), metadata, pathlib.Path(rest[0]))
    elif mode == "compat":
        output = compat_module(
            pathlib.Path(input_path), json.loads(pathlib.Path(rest[0]).read_text())
        )
    else:
        raise ValueError(f"Unknown metadata operation: {mode}")
    pathlib.Path(output_path).write_text(output)


if __name__ == "__main__":
    main()
