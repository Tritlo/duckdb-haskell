#!/usr/bin/env python3
# pyright: strict
"""Check a packaged Haskell extension with an explicitly selected DuckDB CLI."""

from __future__ import annotations

import argparse
import csv
import io
from pathlib import Path
import subprocess
import sys


TIMEOUT_SECONDS = 60
OUTPUT_LIMIT = 1200


def sql_literal(value: str) -> str:
    """Quote a path as a SQL string without changing its contents."""
    if "\0" in value:
        raise ValueError("extension path must not contain NUL")
    return "'" + value.replace("'", "''") + "'"


def short_output(value: str | bytes | None) -> str:
    """Limit captured diagnostics in a failure report."""
    text = (value.decode("utf-8", "replace") if isinstance(value, bytes) else (value or "")).strip()
    if len(text) > OUTPUT_LIMIT:
        return text[:OUTPUT_LIMIT] + "\n[truncated]"
    return text or "[empty]"


def check_process(
    duckdb: Path,
    extension: Path,
    label: str,
    statements: list[str],
    expected_rows: list[list[str]],
    expected_errors: tuple[str, ...] = (),
) -> None:
    """Start a fresh CLI, check its CSV results, and check expected SQL errors."""
    # The pinned shell supports these flags. STANDARD stdin continues on errors.
    command = [str(duckdb), "-unsigned", "-csv", "-noheader", "-no-init", "-batch", ":memory:"]
    sql = "\n".join([".bail off", f"LOAD {sql_literal(str(extension))};", *statements]) + "\n"
    try:
        result = subprocess.run(
            command,
            input=sql,
            text=True,
            encoding="utf-8",
            errors="replace",
            capture_output=True,
            check=False,
            timeout=TIMEOUT_SECONDS,
        )
    except subprocess.TimeoutExpired as error:
        raise RuntimeError(
            f"{label}: DuckDB CLI exceeded {TIMEOUT_SECONDS} seconds\n"
            f"stdout:\n{short_output(error.stdout)}\n"
            f"stderr:\n{short_output(error.stderr)}"
        ) from error
    expected_exit = 1 if expected_errors else 0
    problems: list[str] = []
    if result.returncode != expected_exit:
        problems.append(f"exit status {result.returncode}, expected {expected_exit}")
    try:
        actual_rows = list(csv.reader(io.StringIO(result.stdout), strict=True))
        if actual_rows != expected_rows:
            problems.append("CSV results differ from the expected rows")
    except csv.Error:
        problems.append("stdout is not valid CSV")
    for diagnostic in expected_errors:
        if diagnostic not in result.stderr:
            problems.append(f"missing diagnostic: {diagnostic}")
    if problems:
        raise RuntimeError(
            f"{label}: {'; '.join(problems)}\n"
            f"stdout:\n{short_output(result.stdout)}\n"
            f"stderr:\n{short_output(result.stderr)}"
        )
    print(f"PASS: {label}")


def smoke(duckdb: Path, extension: Path) -> None:
    """Check results and recovery in two independent extension-loading processes."""
    cli = duckdb.resolve(strict=True)
    library = extension.resolve(strict=True)
    if not cli.is_file() or not library.is_file():
        raise ValueError("DuckDB CLI and extension paths must identify files")
    check_process(
        cli,
        library,
        "load, repeated load, native batches, and NULL",
        [
            "SELECT 'loaded', haskell_answer();",
            f"LOAD {sql_literal(str(library))};",
            "SELECT 'reloaded', haskell_answer();",
            "SELECT 'native_batches', count(*), min(value), max(value), sum(value) "
            "FROM (SELECT haskell_add_one(i) AS value FROM range(10000) AS t(i));",
            "SELECT 'null', CASE WHEN haskell_add_one(NULL::BIGINT) IS NULL "
            "THEN 'preserved' ELSE 'changed' END;",
        ],
        [
            ["loaded", "42"],
            ["reloaded", "42"],
            ["native_batches", "10000", "1", "10000", "50005000"],
            ["null", "preserved"],
        ],
    )
    check_process(
        cli,
        library,
        "fresh runtime startup, callback errors, overflow, and recovery",
        [
            "SELECT 'fresh_process', haskell_answer();",
            "SELECT haskell_fail();",
            "SELECT 'after_callback_error', 42;",
            "SELECT haskell_add_one(9223372036854775807::BIGINT);",
            "SELECT 'after_overflow_error', 42;",
        ],
        [
            ["fresh_process", "42"],
            ["after_callback_error", "42"],
            ["after_overflow_error", "42"],
        ],
        ("Haskell callback failure", "Haskell BIGINT overflow"),
    )


def main() -> None:
    """Read explicit paths and report smoke-check failures without a traceback."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("extension", type=Path, help="path to the packaged haskell_demo.duckdb_extension")
    parser.add_argument("--duckdb", type=Path, required=True, help="explicit path to the matching DuckDB CLI")
    options = parser.parse_args()
    try:
        smoke(options.duckdb, options.extension)
    except (OSError, ValueError, RuntimeError) as error:
        print(f"smoke: {error}", file=sys.stderr)
        raise SystemExit(1) from error


if __name__ == "__main__":
    main()
