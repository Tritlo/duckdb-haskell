#!/usr/bin/env python3
"""
Probe DuckDB's deprecated Arrow C API with ctypes to observe pointer semantics.
"""
from __future__ import annotations

import ctypes
import os
import sys
from ctypes import c_char_p, c_int, c_int32, c_int64, c_uint64, c_void_p


HERE = os.path.abspath(os.path.dirname(__file__))
DEFAULT_LIB = os.path.abspath(os.path.join(HERE, "..", "cbits", "duckdb", "libduckdb.so"))

DuckDBSuccess = 0
DuckDBError = 1


class DuckDBDatabaseStruct(ctypes.Structure):
    _fields_ = [("internal_ptr", c_void_p)]


DuckDBDatabaseHandle = ctypes.POINTER(DuckDBDatabaseStruct)


class DuckDBConnectionStruct(ctypes.Structure):
    _fields_ = [("internal_ptr", c_void_p)]


DuckDBConnectionHandle = ctypes.POINTER(DuckDBConnectionStruct)


class DuckDBArrowStruct(ctypes.Structure):
    _fields_ = [("internal_ptr", c_void_p)]


DuckDBArrowHandle = ctypes.POINTER(DuckDBArrowStruct)


class DuckDBArrowSchemaStruct(ctypes.Structure):
    _fields_ = [("internal_ptr", c_void_p)]


DuckDBArrowSchemaHandle = ctypes.POINTER(DuckDBArrowSchemaStruct)


class DuckDBArrowArrayStruct(ctypes.Structure):
    _fields_ = [("internal_ptr", c_void_p)]


DuckDBArrowArrayHandle = ctypes.POINTER(DuckDBArrowArrayStruct)


class DuckDBArrowStreamStruct(ctypes.Structure):
    _fields_ = [("internal_ptr", c_void_p)]


DuckDBArrowStreamHandle = ctypes.POINTER(DuckDBArrowStreamStruct)


class DuckDBResult(ctypes.Structure):
    _fields_ = [
        ("deprecated_column_count", c_uint64),
        ("deprecated_row_count", c_uint64),
        ("deprecated_rows_changed", c_uint64),
        ("deprecated_columns", c_void_p),
        ("deprecated_error_message", c_char_p),
        ("internal_data", c_void_p),
    ]


class ArrowArray(ctypes.Structure):
    pass


ArrowArrayPtr = ctypes.POINTER(ArrowArray)
ArrowArrayReleaseFunc = ctypes.CFUNCTYPE(None, ArrowArrayPtr)

ArrowArray._fields_ = [
    ("length", c_int64),
    ("null_count", c_int64),
    ("offset", c_int64),
    ("n_buffers", c_int64),
    ("n_children", c_int64),
    ("buffers", ctypes.POINTER(c_void_p)),
    ("children", ctypes.POINTER(ArrowArrayPtr)),
    ("dictionary", ArrowArrayPtr),
    ("release", ArrowArrayReleaseFunc),
    ("private_data", c_void_p),
]


class ArrowSchema(ctypes.Structure):
    pass


ArrowSchemaPtr = ctypes.POINTER(ArrowSchema)
ArrowSchemaReleaseFunc = ctypes.CFUNCTYPE(None, ArrowSchemaPtr)

ArrowSchema._fields_ = [
    ("format", c_char_p),
    ("name", c_char_p),
    ("metadata", c_char_p),
    ("flags", c_int64),
    ("n_children", c_int64),
    ("children", ctypes.POINTER(ArrowSchemaPtr)),
    ("dictionary", ArrowSchemaPtr),
    ("release", ArrowSchemaReleaseFunc),
    ("private_data", c_void_p),
]


def load_library() -> ctypes.CDLL:
    lib_path = os.environ.get("DUCKDB_LIB_PATH", DEFAULT_LIB)
    try:
        return ctypes.CDLL(lib_path)
    except OSError as exc:
        print(f"Failed to load libduckdb from {lib_path}: {exc}", file=sys.stderr)
        print("Set DUCKDB_LIB_PATH to the shared library location.", file=sys.stderr)
        raise SystemExit(1) from exc


def configure_signatures(lib: ctypes.CDLL) -> None:
    lib.duckdb_open.argtypes = [c_char_p, ctypes.POINTER(DuckDBDatabaseHandle)]
    lib.duckdb_open.restype = c_int

    lib.duckdb_close.argtypes = [ctypes.POINTER(DuckDBDatabaseHandle)]
    lib.duckdb_close.restype = None

    lib.duckdb_connect.argtypes = [DuckDBDatabaseHandle, ctypes.POINTER(DuckDBConnectionHandle)]
    lib.duckdb_connect.restype = c_int

    lib.duckdb_disconnect.argtypes = [ctypes.POINTER(DuckDBConnectionHandle)]
    lib.duckdb_disconnect.restype = None

    lib.duckdb_query.argtypes = [DuckDBConnectionHandle, c_char_p, ctypes.POINTER(DuckDBResult)]
    lib.duckdb_query.restype = c_int

    lib.duckdb_destroy_result.argtypes = [ctypes.POINTER(DuckDBResult)]
    lib.duckdb_destroy_result.restype = None

    lib.duckdb_result_error.argtypes = [ctypes.POINTER(DuckDBResult)]
    lib.duckdb_result_error.restype = c_char_p

    lib.duckdb_query_arrow.argtypes = [DuckDBConnectionHandle, c_char_p, ctypes.POINTER(DuckDBArrowHandle)]
    lib.duckdb_query_arrow.restype = c_int

    lib.duckdb_query_arrow_schema.argtypes = [DuckDBArrowHandle, ctypes.POINTER(DuckDBArrowSchemaHandle)]
    lib.duckdb_query_arrow_schema.restype = c_int

    lib.duckdb_query_arrow_array.argtypes = [DuckDBArrowHandle, ctypes.POINTER(DuckDBArrowArrayHandle)]
    lib.duckdb_query_arrow_array.restype = c_int

    lib.duckdb_arrow_column_count.argtypes = [DuckDBArrowHandle]
    lib.duckdb_arrow_column_count.restype = c_uint64

    lib.duckdb_arrow_row_count.argtypes = [DuckDBArrowHandle]
    lib.duckdb_arrow_row_count.restype = c_uint64

    lib.duckdb_arrow_rows_changed.argtypes = [DuckDBArrowHandle]
    lib.duckdb_arrow_rows_changed.restype = c_uint64

    lib.duckdb_query_arrow_error.argtypes = [DuckDBArrowHandle]
    lib.duckdb_query_arrow_error.restype = c_char_p

    lib.duckdb_destroy_arrow.argtypes = [ctypes.POINTER(DuckDBArrowHandle)]
    lib.duckdb_destroy_arrow.restype = None

    lib.duckdb_arrow_array_scan.argtypes = [
        DuckDBConnectionHandle,
        c_char_p,
        DuckDBArrowSchemaHandle,
        DuckDBArrowArrayHandle,
        ctypes.POINTER(DuckDBArrowStreamHandle),
    ]
    lib.duckdb_arrow_array_scan.restype = c_int

    lib.duckdb_arrow_scan.argtypes = [DuckDBConnectionHandle, c_char_p, DuckDBArrowStreamHandle]
    lib.duckdb_arrow_scan.restype = c_int

    lib.duckdb_destroy_arrow_stream.argtypes = [ctypes.POINTER(DuckDBArrowStreamHandle)]
    lib.duckdb_destroy_arrow_stream.restype = None


def ensure_success(state: int, fn_name: str, error: str | None = None) -> None:
    if state == DuckDBSuccess:
        return
    detail = f" ({error})" if error else ""
    raise RuntimeError(f"{fn_name} failed with state={state}{detail}")


def run_ddl(lib: ctypes.CDLL, conn: DuckDBConnectionHandle, sql: str) -> None:
    result = DuckDBResult()
    state = lib.duckdb_query(conn, sql.encode("utf-8"), ctypes.byref(result))
    if state != DuckDBSuccess:
        message = lib.duckdb_result_error(ctypes.byref(result))
        text = message.decode("utf-8") if message else "unknown error"
        lib.duckdb_destroy_result(ctypes.byref(result))
        ensure_success(state, "duckdb_query", text)
    lib.duckdb_destroy_result(ctypes.byref(result))


def query_arrow(lib: ctypes.CDLL, conn: DuckDBConnectionHandle, sql: str) -> DuckDBArrowHandle:
    arrow = DuckDBArrowHandle()
    state = lib.duckdb_query_arrow(conn, sql.encode("utf-8"), ctypes.byref(arrow))
    if state == DuckDBSuccess:
        return arrow
    error_text = None
    if bool(arrow):
        message = lib.duckdb_query_arrow_error(arrow)
        if message:
            error_text = message.decode("utf-8", errors="replace")
    lib.duckdb_destroy_arrow(ctypes.byref(arrow))
    ensure_success(state, "duckdb_query_arrow", error_text)
    return arrow


def describe_schema(lib: ctypes.CDLL, arrow: DuckDBArrowHandle) -> list[tuple[str, str]]:
    schema = ArrowSchema()
    schema_ptr = ctypes.pointer(schema)
    schema_handle = ctypes.cast(schema_ptr, DuckDBArrowSchemaHandle)
    state = lib.duckdb_query_arrow_schema(arrow, ctypes.pointer(schema_handle))
    ensure_success(state, "duckdb_query_arrow_schema")
    columns: list[tuple[str, str]] = []
    root_format = schema.format.decode("utf-8") if schema.format else "<null>"
    print(f"Root ArrowSchema format='{root_format}', children={schema.n_children}")
    if schema.children and schema.n_children:
        child_array = ctypes.cast(schema.children, ctypes.POINTER(ArrowSchemaPtr))
        for idx in range(schema.n_children):
            child_ptr = child_array[idx]
            if not bool(child_ptr):
                print(f"  child[{idx}] is null")
                columns.append((f"col_{idx}", "<null>"))
                continue
            child = child_ptr.contents
            name = child.name.decode("utf-8") if child.name else f"col_{idx}"
            fmt = child.format.decode("utf-8") if child.format else "<null>"
            print(f"  child[{idx}] name='{name}', format='{fmt}', flags={child.flags}")
            columns.append((name, fmt))
    if schema.release:
        schema.release(schema_ptr)
    return columns


def describe_array(lib: ctypes.CDLL, arrow: DuckDBArrowHandle, columns: list[tuple[str, str]]) -> None:
    array = ArrowArray()
    array_ptr = ctypes.pointer(array)
    array_handle = ctypes.cast(array_ptr, DuckDBArrowArrayHandle)
    state = lib.duckdb_query_arrow_array(arrow, ctypes.pointer(array_handle))
    ensure_success(state, "duckdb_query_arrow_array")
    print(
        f"Root ArrowArray length={array.length}, null_count={array.null_count}, "
        f"offset={array.offset}, children={array.n_children}"
    )
    if array.children and array.n_children:
        child_array = ctypes.cast(array.children, ctypes.POINTER(ArrowArrayPtr))
        for idx in range(array.n_children):
            child_ptr = child_array[idx]
            if not bool(child_ptr):
                print(f"  child[{idx}] array is null")
                continue
            name, fmt = columns[idx] if idx < len(columns) else (f"col_{idx}", "<unknown>")
            child = child_ptr.contents
            print(
                f"  child[{idx}] name='{name}', format='{fmt}', length={child.length}, "
                f"nulls={child.null_count}, buffers={child.n_buffers}"
            )
            if child.buffers and child.n_buffers >= 2 and fmt in {"l", "q"}:
                buffers = child.buffers
                data_ptr = buffers[1]
                if data_ptr:
                    values = ctypes.cast(data_ptr, ctypes.POINTER(c_int64))
                    sample = values[0] if child.length > 0 else "<empty>"
                    print(f"    sample int64 value: {sample}")
            if child.buffers and child.n_buffers >= 3 and fmt in {"u", "U"}:
                buffers = child.buffers
                offsets_ptr = buffers[1]
                data_ptr = buffers[2]
                if offsets_ptr and data_ptr and child.length > 0:
                    offsets = ctypes.cast(offsets_ptr, ctypes.POINTER(c_int32))
                    start = offsets[0]
                    end = offsets[1]
                    raw = ctypes.string_at(data_ptr + start, end - start)
                    print(f"    sample utf8 value: {raw.decode('utf-8')}")
    if array.release:
        array.release(array_ptr)


def demonstrate_arrow_scans(lib: ctypes.CDLL, conn: DuckDBConnectionHandle) -> None:
    print("Probing duckdb_arrow_array_scan -> duckdb_arrow_scan interplay.")
    arrow = query_arrow(
        lib,
        conn,
        "SELECT i, label FROM arrow_stream_source ORDER BY i",
    )
    schema = ArrowSchema()
    schema_ptr = ctypes.pointer(schema)
    schema_handle = ctypes.cast(schema_ptr, DuckDBArrowSchemaHandle)
    ensure_success(
        lib.duckdb_query_arrow_schema(arrow, ctypes.pointer(schema_handle)),
        "duckdb_query_arrow_schema",
    )

    array = ArrowArray()
    array_ptr = ctypes.pointer(array)
    array_handle = ctypes.cast(array_ptr, DuckDBArrowArrayHandle)
    ensure_success(
        lib.duckdb_query_arrow_array(arrow, ctypes.pointer(array_handle)),
        "duckdb_query_arrow_array",
    )

    stream = DuckDBArrowStreamHandle()
    view_name = b"arrow_probe_array_view"
    stream_state = lib.duckdb_arrow_array_scan(
        conn,
        view_name,
        schema_handle,
        array_handle,
        ctypes.byref(stream),
    )
    stream_ptr = stream.contents.internal_ptr if bool(stream) else None
    print(
        f"duckdb_arrow_array_scan -> state={stream_state}, "
        f"stream_pointer={hex(stream_ptr) if stream_ptr else 'NULL'}"
    )

    if stream_state == DuckDBSuccess and bool(stream):
        view_state = lib.duckdb_arrow_scan(conn, b"arrow_probe_stream_view", stream)
        print(f"duckdb_arrow_scan -> state={view_state}")
        lib.duckdb_destroy_arrow_stream(ctypes.byref(stream))
        run_ddl(lib, conn, "DROP VIEW IF EXISTS arrow_probe_stream_view")
    run_ddl(lib, conn, "DROP VIEW IF EXISTS arrow_probe_array_view")

    if array.release:
        array.release(array_ptr)
    if schema.release:
        schema.release(schema_ptr)
    lib.duckdb_destroy_arrow(ctypes.byref(arrow))


def main() -> None:
    lib = load_library()
    configure_signatures(lib)
    print(f"Loaded DuckDB from {lib._name}")

    database = DuckDBDatabaseHandle()
    ensure_success(lib.duckdb_open(None, ctypes.byref(database)), "duckdb_open")
    print("Opened in-memory database.")

    connection = DuckDBConnectionHandle()
    ensure_success(lib.duckdb_connect(database, ctypes.byref(connection)), "duckdb_connect")
    print("Established connection.")

    run_ddl(lib, connection, "CREATE TABLE items(i BIGINT, label VARCHAR)")
    run_ddl(lib, connection, "INSERT INTO items VALUES (1, 'one'), (2, 'two'), (3, 'three')")
    print("Seeded table with three rows.")

    run_ddl(lib, connection, "CREATE TABLE arrow_stream_source(i BIGINT, label VARCHAR)")
    run_ddl(
        lib,
        connection,
        "INSERT INTO arrow_stream_source VALUES (7, 'seven'), (8, 'eight')",
    )
    print("Prepared arrow_stream_source for scan probing.")

    insert_arrow = query_arrow(lib, connection, "INSERT INTO items VALUES (42, 'arrow insert')")
    changed = lib.duckdb_arrow_rows_changed(insert_arrow)
    print(f"INSERT rows_changed reported via arrow handle: {changed}")
    lib.duckdb_destroy_arrow(ctypes.byref(insert_arrow))

    select_arrow = query_arrow(
        lib,
        connection,
        "SELECT i AS value, label FROM items ORDER BY i",
    )
    column_count = lib.duckdb_arrow_column_count(select_arrow)
    row_count = lib.duckdb_arrow_row_count(select_arrow)
    changed_rows = lib.duckdb_arrow_rows_changed(select_arrow)
    print(
        f"SELECT arrow handle: columns={column_count}, row_count={row_count}, "
        f"rows_changed={changed_rows}"
    )

    columns = describe_schema(lib, select_arrow)
    describe_array(lib, select_arrow, columns)

    demonstrate_arrow_scans(lib, connection)

    lib.duckdb_destroy_arrow(ctypes.byref(select_arrow))
    lib.duckdb_disconnect(ctypes.byref(connection))
    lib.duckdb_close(ctypes.byref(database))
    print("Cleaned up DuckDB handles.")


if __name__ == "__main__":
    main()
