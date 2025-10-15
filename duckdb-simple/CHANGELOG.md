# Changelog

# Changelog

## 0.1.1.2
- Broadened `FieldValue` and `FromField` coverage to handle all DuckDB scalar types, including intervals, HUGEINT/UHUGEINT, decimals (with metadata), time/timestamp with additional precisions, timezone-aware values, bit strings, bignums, and enums.
- Fixed enum decoding for both query results and scalar-function inputs by honouring the logical type’s underlying storage width (uint8/uint16/uint32).
- Ensured decimal vectors read accurate width/scale metadata when materializing results or invoking UDFs.
- Added unsigned `ToField` bindings that route through DuckDB’s native uint creators and exposed `FromField Word`.
- Expanded the test suite with regressions covering unsigned round-trips, huge integers, intervals, decimals, and timezone-aware values.

## 0.1.0.0
- Initial release
