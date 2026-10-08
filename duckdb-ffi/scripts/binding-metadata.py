"""Generate naming specifications and C checks from binding metadata."""

import json
import pathlib
import re
import subprocess
import sys
from typing import Any


def generation_spec(
    header: pathlib.Path, metadata: dict[str, Any], names: dict[str, str]
) -> dict[str, Any]:
    """Preserve public type names and keep native handle pointees opaque."""
    renames = {native: public for public, native in names.items()}
    generated_names = {entry["hsname"] for entry in metadata["ctypes"]}
    if not renames.keys() <= generated_names:
        raise ValueError("The binding metadata does not contain every public type.")
    tags = {
        "struct " + tag
        for tag, fields in re.findall(
            r"typedef\s+struct\s+(_duckdb_\w+)\s*\{([^}]*)\}\s*\*\s*duckdb_\w+\s*;",
            header.read_text(),
        )
        if re.fullmatch(r"\s*void\s*\*\s*internal_ptr\s*;\s*", fields)
    }
    types = [
        {
            "headers": entry["headers"],
            "cname": entry["cname"],
            "hsname": renames.get(entry["hsname"], entry["hsname"]),
        }
        for entry in metadata["ctypes"]
        if entry["hsname"] in renames or entry["cname"] in tags
    ]
    if {entry["cname"] for entry in types if entry["cname"] in tags} != tags:
        raise ValueError("The binding metadata does not contain every native handle.")
    return {
        "version": metadata["version"],
        "ctypes": types,
        "hstypes": [
            {"hsname": entry["hsname"], "representation": "emptydata"}
            for entry in types
            if entry["cname"] in tags
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
    native_functions: set[str] = set()

    def visit(node: dict[str, Any]) -> None:
        if node.get("kind") == "FunctionDecl" and node.get("name", "").startswith(
            "duckdb_"
        ):
            native_functions.add("c_" + node["name"])
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
    generated_functions = re.findall(r"^(c_duckdb_\w+) ::", text, re.MULTILINE)
    if len(generated_functions) != len(set(generated_functions)):
        raise ValueError("The generated bindings contain duplicate function names.")
    if set(generated_functions) != native_functions:
        raise ValueError("The generated bindings do not contain every native function.")
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


def main() -> None:
    """Write one generated metadata artifact."""
    mode, input_path, metadata_path, output_path, *rest = sys.argv[1:]
    metadata = json.loads(pathlib.Path(metadata_path).read_text())
    if mode == "spec":
        output = json.dumps(
            generation_spec(
                pathlib.Path(input_path),
                metadata,
                json.loads(pathlib.Path(rest[0]).read_text()),
            ),
            indent=2,
        ) + "\n"
    elif mode == "abi":
        output = abi_checks(pathlib.Path(input_path), metadata, pathlib.Path(rest[0]))
    else:
        raise ValueError(f"Unknown metadata operation: {mode}")
    pathlib.Path(output_path).write_text(output)


if __name__ == "__main__":
    main()
