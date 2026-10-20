# Preview runtime only

`build-duckdb.sh` builds the pinned DuckDB development runtime for local and CI
validation. It verifies the source archive and compares its client headers with
the checked-in files. It does not run during ordinary package builds.

```sh
dev/preview/build-duckdb.sh /tmp/duckdb-2.0-native
```

Set `DUCKDB_BUILD_SHELL=ON` to also build and copy the matching CLI.
`DUCKDB_BUILD_JOBS` sets the compiler parallelism and defaults to two jobs.
The native output keeps the shared-library symlinks and their targets.

```sh
DUCKDB_BUILD_SHELL=ON dev/preview/build-duckdb.sh /tmp/duckdb-2.0-native
```

Remove this builder after the final DuckDB 2.0 native archives are available
and verified. Change CI, Docker, and Nix to use the final release inputs at that
time. The binding generator, header audit, and release tools remain in `dev/`.
