#!/usr/bin/env python3

"""Report foreign import wrappers that lack test coverage references."""

from __future__ import annotations

from pathlib import Path
import re
import sys


def project_root() -> Path:
  return Path(__file__).resolve().parent.parent


def find_foreign_functions(module_path: Path) -> list[str]:
  text = module_path.read_text(encoding="utf-8")
  pattern = re.compile(
      r'foreign import[^\n]*\n\s+([a-zA-Z_][\w]*)\s*::',
      re.MULTILINE,
  )
  names = {match.group(1) for match in pattern.finditer(text)}
  return sorted(names)


def collect_test_tokens(test_root: Path) -> set[str]:
  token_pattern = re.compile(r'c_duckdb_[A-Za-z0-9_]*')
  tokens: set[str] = set()
  for path in sorted(test_root.rglob("*.hs")):
    text = path.read_text(encoding="utf-8")
    tokens.update(token_pattern.findall(text))
  return tokens


def main() -> int:
  root = project_root()
  ffi_root = root / "src" / "Database" / "DuckDB" / "FFI"
  test_root = root / "test"

  if not ffi_root.is_dir():
    print(f"FFI directory not found: {ffi_root}", file=sys.stderr)
    return 1
  if not test_root.is_dir():
    print(f"Test directory not found: {test_root}", file=sys.stderr)
    return 1

  ffi_modules = sorted(ffi_root.glob("*.hs"))
  test_tokens = collect_test_tokens(test_root)

  untested: list[tuple[Path, list[str]]] = []
  for module_path in ffi_modules:
    foreign_functions = find_foreign_functions(module_path)
    missing = [name for name in foreign_functions if name not in test_tokens]
    if missing:
      untested.append((module_path.relative_to(root), missing))

  if not untested:
    print("All foreign import wrappers are referenced in tests.")
    return 0

  total_missing = 0
  for module_rel_path, names in untested:
    print(f"{module_rel_path}:")
    for name in names:
      print(f"  {name}")
    total_missing += len(names)

  print()
  print(
      f"{total_missing} foreign import wrapper(s) lack references "
      f"across {len(untested)} module(s).",
  )
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
