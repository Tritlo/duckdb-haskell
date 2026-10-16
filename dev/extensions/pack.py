#!/usr/bin/env python3
# pyright: strict
"""Copy the Haskell demo library and append unsigned DuckDB V2 metadata."""

from __future__ import annotations

import argparse
from pathlib import Path
import shutil


FIELD_SIZE = 32
SIGNATURE_SIZE = 256
SECTION_PREFIX = b"\x00\x93\x04\x10duckdb_signature\x80\x04"
OUTPUT_NAME = "haskell_demo.duckdb_extension"


def metadata_field(label: str, value: str) -> bytes:
    """Encode a nonempty ASCII field without truncation."""
    if not value or "\0" in value:
        raise ValueError(f"{label} must be nonempty and must not contain NUL")
    try:
        encoded = value.encode("ascii")
    except UnicodeEncodeError as error:
        raise ValueError(f"{label} must contain ASCII characters only") from error
    if len(encoded) > FIELD_SIZE:
        raise ValueError(f"{label} exceeds {FIELD_SIZE} bytes")
    return encoded.ljust(FIELD_SIZE, b"\0")


def footer(platform: str, duckdb_version: str, extension_version: str) -> bytes:
    """Match the pinned append_metadata.cmake field order and unsigned footer."""
    fields = [
        metadata_field("metadata magic", "4"),
        metadata_field("platform", platform),
        metadata_field("DuckDB version", duckdb_version),
        metadata_field("extension version", extension_version),
        metadata_field("ABI type", "C_STRUCT_UNSTABLE"),
        bytes(FIELD_SIZE),
        bytes(FIELD_SIZE),
        bytes(FIELD_SIZE),
    ]
    return SECTION_PREFIX + b"".join(reversed(fields)) + bytes(SIGNATURE_SIZE)


def check_library(header: bytes, platform: str) -> None:
    """Accept current 64-bit Linux ELF and thin macOS Mach-O shared libraries."""
    if len(header) < 32:
        raise ValueError("input is too short to be a supported shared library")
    magic = header[:4]
    if magic == b"\x7fELF":
        if len(header) < 64 or header[4] != 2 or header[5] not in (1, 2):
            raise ValueError("input must use the 64-bit ELF format")
        byte_order = "little" if header[5] == 1 else "big"
        if int.from_bytes(header[16:18], byte_order) != 3:
            raise ValueError("ELF input must be a shared object (ET_DYN)")
        if not platform.startswith("linux_"):
            raise ValueError("ELF input requires an explicit linux_* platform")
    elif magic in (b"\xcf\xfa\xed\xfe", b"\xfe\xed\xfa\xcf"):
        byte_order = "little" if magic == b"\xcf\xfa\xed\xfe" else "big"
        if int.from_bytes(header[12:16], byte_order) not in (6, 8):
            raise ValueError("Mach-O input must be a dynamic library or bundle")
        if not platform.startswith("osx_"):
            raise ValueError("Mach-O input requires an explicit osx_* platform")
    else:
        raise ValueError("input must be a 64-bit Linux ELF or thin macOS Mach-O library; Windows is unsupported")


def pack(library: Path, output: Path, platform: str, duckdb_version: str, extension_version: str) -> None:
    """Write a new extension file and keep the original library unchanged."""
    suffix = footer(platform, duckdb_version, extension_version)
    source = library.resolve(strict=True)
    destination = output.resolve()
    if source == destination:
        raise ValueError("input and output paths must differ")
    if output.name != OUTPUT_NAME:
        raise ValueError(f"output must be named {OUTPUT_NAME} to match the demo entrypoint")
    if not source.is_file():
        raise ValueError("input must be a regular shared-library file")
    with source.open("rb") as original:
        check_library(original.read(64), platform)
        size = original.seek(0, 2)
        if size >= 512:
            original.seek(-SIGNATURE_SIZE - FIELD_SIZE, 2)
            if original.read(FIELD_SIZE) == metadata_field("metadata magic", "4"):
                raise ValueError("input already contains DuckDB extension metadata")
        original.seek(0)
        # Exclusive creation also rejects existing files, hard links, and symlinks.
        target = output.open("xb")
        try:
            with target:
                shutil.copyfileobj(original, target)
                target.write(suffix)
        except BaseException:
            output.unlink(missing_ok=True)
            raise


def main() -> None:
    """Read explicit metadata and package one foreign-library build output."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("library", type=Path, help="explicit path to the built shared library")
    parser.add_argument("--output", type=Path, required=True, help=f"new path named {OUTPUT_NAME}; must not exist")
    parser.add_argument("--platform", required=True, help="exact DuckDB platform, for example linux_amd64 or osx_arm64")
    parser.add_argument("--duckdb-version", required=True, help="exact normalized loader version; the pinned dev build uses 13d1d8f56a")
    parser.add_argument("--extension-version", required=True, help="extension release version to record in the footer")
    options = parser.parse_args()
    try:
        pack(options.library, options.output, options.platform, options.duckdb_version, options.extension_version)
    except (OSError, ValueError) as error:
        parser.error(str(error))
    print(options.output)


if __name__ == "__main__":
    main()
