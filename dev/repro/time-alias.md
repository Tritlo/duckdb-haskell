# TIME alias cast and public vector construction

`time-alias.c` reproduces the TIME-to-alias cast failure in DuckDB 2.0.0.
It creates `clock_alias` through `duckdb_v2_factory_create_type_with_alias`.
SQL `CREATE TYPE name AS TIME` does not retain this alias metadata.
Both `vector_set_value` and `value_cast` return `ERROR_INPUT_INVALID` (2001).
The public mutable vector operation preserves the same microseconds and alias.

Run from the repository root with the pinned native library:

```sh
nix-shell dev/nix/shell.nix --run \
  'cc -std=c11 -Wall -Wextra -Iduckdb-ffi/cbits dev/repro/time-alias.c -lduckdb -o /tmp/time-alias && /tmp/time-alias'
```

The header and native library use the source recorded in
`duckdb-ffi/vendor/duckdb-api.json`: DuckDB 2.0.0, commit
`d54cbbe3df17e8e5ce68312f483b4155836112aa`, archive SHA256
`93e3d1f9c84b6ec3493237d21565c01c2c19b9862e54e5552d3d7999ccc3bbb4`.
No native source changes are required to run the reproducer.

`StandardVectorBuffer::SetValue` casts a non-NULL value when its type differs
from the vector type (`src/common/vector/flat_vector.cpp`).
`CastFunctionSet::GetCastFunction` applies the no-op cast only to equal types
(`src/function/cast/cast_function_set.cpp`). An alias differs from its base.
`DefaultCasts::TimeCastSwitch` has no TIME target case, so it selects
`TryVectorNullCast` (`src/function/cast/time_casts.cpp`).
`Vector::GetValue` restores the source alias after it reads the payload
(`src/common/types/vector.cpp`).

The binding fix applies to TIME leaves with explicit logical metadata in
composites. An unannotated scalar `FieldValue` does not carry alias metadata.
It cannot retain an alias through that representation.

## Binding checks

The new regression is `TIME aliases bind exact scalar and nested payloads
under session settings` in `duckdb-simple/test/ValueRegressionTests.hs`.
It checks 216 exact roundtrips: two alias names, three TimeZone settings,
six payloads, and six explicit-metadata/composite paths.
The names are `TIME` and `clock_alias`. The session zones are `UTC`,
`Europe/Stockholm`, and `America/New_York`. The payloads are NULL, midnight,
one microsecond after midnight, `12:34:56.123456`, `23:59:59.999999`, and
`24:00:00`. The paths are a scalar field, LIST, ARRAY, MAP value, nested
STRUCT, and UNION member. Separate assertions check microsecond truncation
and the existing invalid-time errors.

Enter the pinned shell and build all components before the focused Memcheck run.
Valgrind must be available on the host:

```sh
nix-shell dev/nix/shell.nix
cabal build all
alias_test_bin=$(cabal list-bin duckdb-simple-test)
valgrind --max-threads=1024 --vgdb=no --fair-sched=yes --leak-check=full \
  --show-leak-kinds=definite,indirect,possible \
  --errors-for-leak-kinds=definite,indirect --error-exitcode=97 \
  --undef-value-errors=no --num-callers=30 \
  "$alias_test_bin" -p '/TIME aliases bind/' +RTS -N1 -RTS
```

The flags match the repository CI. Invalid memory accesses and definite or
indirect leaks fail with exit 97. Possible leaks are reported. Undefined-value
diagnostics are disabled, as in CI. This check covers the selected test only.

## Local measurement

On 2026-11-17, GHC 9.14.1 and Valgrind 3.22.0 passed the focused test in
10.59 seconds. The local serial runner used a systemd scope with a 4G memory
cap, no swap, and a five-minute timeout. The job exited 0 and peaked at
1,876 MiB. Memcheck reported zero errors from zero contexts. Definitely lost,
indirectly lost,
and possibly lost each had zero bytes in zero blocks. Still reachable memory
was 4,621,932 bytes in 29 blocks. Suppressed memory and errors were zero.
The test used one Haskell capability and a maximum of 1,024 Valgrind threads.
These counts apply only to the selected test and the enabled diagnostics.

The retained native reproducer also passed in the same capped job. It reported
error 2001 for both casts and preserved 45,296,123,456 microseconds with the
alias through public vector storage.

The full ordinary check passed before Memcheck: FFI V1 (97 tests), FFI V2
(74 tests), duckdb-simple (836 tests), the nonthreaded suite, and the default
leak suite (10 batches). The additional focused selections passed 45 TIME
tests and six mixed-type tests. The extended 100-batch leak run was omitted.
