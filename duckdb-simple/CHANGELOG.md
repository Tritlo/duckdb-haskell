# Changelog

## Unreleased
- Added LIST/MAP coverage note: LIST columns decode into Haskell lists and MAP columns into strict `Map k v`, with matching parameter bindings via `ToField`.
- Taught `FromField` to interpret `FieldBigNum` as `Integer`/`Natural` and added matching `ToField` instances for `BigNum`, `Integer`, and `Natural` so BIGNUM parameters round-trip without truncation.
- Added `duckdbColumnType` helper and `DuckDBColumnType` class, exposing the DuckDB column type associated with each `ToField` instance.
- Fixed UUID decoding by undoing DuckDB’s upper-word bias and added a UUID round-trip regression test.

## 0.1.1.2
- Broadened `FieldValue` and `FromField` coverage to handle all DuckDB scalar types, including intervals, HUGEINT/UHUGEINT, decimals (with metadata), time/timestamp with additional precisions, timezone-aware values, bit strings, bignums, and enums.
- Fixed enum decoding for both query results and scalar-function inputs by honouring the logical type’s underlying storage width (uint8/uint16/uint32).
- Ensured decimal vectors read accurate width/scale metadata when materializing results or invoking UDFs.
- Added unsigned `ToField` bindings that route through DuckDB’s native uint creators and exposed `FromField Word`.
- Expanded the test suite with regressions covering unsigned round-trips, huge integers, intervals, decimals, and timezone-aware values.

## 0.1.0.0
- Initial release
