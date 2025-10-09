#!/usr/bin/env python3
"""
Utility script to generate FFI modules for the DuckDB C API from api-list.md.
"""

from __future__ import annotations

import re
import textwrap
from collections import OrderedDict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Tuple


@dataclass
class FunctionDecl:
    name: str
    return_type: str
    params: List[Tuple[str, str]]


@dataclass
class ModuleSpec:
    section: str
    functions: List[FunctionDecl]


def strip_pointer_from_name(type_part: str, name_part: str) -> Tuple[str, str]:
    """Move any leading '*' characters from the identifier into the type."""
    pointer_count = 0
    while name_part.startswith("*"):
        name_part = name_part[1:]
        pointer_count += 1
    if pointer_count:
        type_tokens = type_part.strip().split()
        type_tokens.extend(["*"] * pointer_count)
        type_part = " ".join(type_tokens)
    return type_part.strip(), name_part.strip()


def parse_return_and_name(segment: str) -> Tuple[str, str]:
    tokens = segment.strip().split()
    if not tokens:
        raise ValueError(f"Invalid function declaration segment: {segment!r}")
    name_token = tokens[-1]
    type_tokens = tokens[:-1]
    type_part, name_part = strip_pointer_from_name(" ".join(type_tokens), name_token)
    return type_part, name_part


def parse_param(param: str) -> Tuple[str, str]:
    tokens = param.strip().split()
    if len(tokens) == 1:
        return tokens[0], ""
    type_part = " ".join(tokens[:-1])
    name_part = tokens[-1]
    return strip_pointer_from_name(type_part, name_part)


def parse_api_list(path: Path) -> OrderedDict[str, List[FunctionDecl]]:
    sections: OrderedDict[str, List[FunctionDecl]] = OrderedDict()
    section_name: str | None = None
    with path.open("r", encoding="utf8") as handle:
        for raw_line in handle:
            if raw_line.startswith("### "):
                section_name = raw_line.strip()[4:]
                sections[section_name] = []
                continue

            line = raw_line.strip()
            if not line.startswith("+ ["):
                continue
            decl = line[len("+ [x] ") :] if line.startswith("+ [x]") else line[len("+ [ ] ") :]
            if not decl.endswith(");"):
                raise ValueError(f"Unexpected declaration terminator: {line}")

            decl = decl[:-2]
            before_paren, params_raw = decl.split("(", 1)
            ret_type, func_name = parse_return_and_name(before_paren)
            params_list: List[Tuple[str, str]] = []
            params_content = params_raw.strip()
            if params_content not in {"", "void"}:
                for raw_param in params_content.split(","):
                    param_type, param_name = parse_param(raw_param)
                    params_list.append((param_type, param_name))
            sections[section_name].append(
                FunctionDecl(name=func_name, return_type=ret_type, params=params_list)
            )
    return sections


SIGNATURE_OVERRIDE: Dict[str, Tuple[str, List[str]]] = {
    "duckdb_query_progress": (
        "void",
        [
            "duckdb_connection connection",
            "duckdb_query_progress_type *out_progress",
        ],
    ),
    "duckdb_result_statement_type": ("duckdb_statement_type", ["duckdb_result *result"]),
    "duckdb_result_return_type": ("duckdb_result_type", ["duckdb_result *result"]),
    "duckdb_result_get_chunk": (
        "duckdb_data_chunk",
        ["duckdb_result *result", "idx_t chunk_index"],
    ),
    "duckdb_result_chunk_count": ("idx_t", ["duckdb_result *result"]),
    "duckdb_result_is_streaming": ("bool", ["duckdb_result *result"]),
    "duckdb_value_hugeint": (
        "void",
        ["duckdb_result *result", "idx_t col", "idx_t row", "duckdb_hugeint *out_value"],
    ),
    "duckdb_value_uhugeint": (
        "void",
        ["duckdb_result *result", "idx_t col", "idx_t row", "duckdb_uhugeint *out_value"],
    ),
    "duckdb_value_decimal": (
        "void",
        ["duckdb_result *result", "idx_t col", "idx_t row", "duckdb_decimal *out_value"],
    ),
    "duckdb_value_string": (
        "void",
        ["duckdb_result *result", "idx_t col", "idx_t row", "duckdb_string *out_value"],
    ),
    "duckdb_value_string_internal": (
        "void",
        ["duckdb_result *result", "idx_t col", "idx_t row", "duckdb_string *out_value"],
    ),
    "duckdb_value_blob": (
        "void",
        ["duckdb_result *result", "idx_t col", "idx_t row", "duckdb_blob *out_blob"],
    ),
    "duckdb_value_interval": (
        "void",
        ["duckdb_result *result", "idx_t col", "idx_t row", "duckdb_interval *out_interval"],
    ),
    "duckdb_bind_interval": (
        "duckdb_state",
        ["duckdb_prepared_statement statement", "idx_t param_idx", "duckdb_interval const *val"],
    ),
    "duckdb_bind_hugeint": (
        "duckdb_state",
        ["duckdb_prepared_statement statement", "idx_t param_idx", "duckdb_hugeint const *val"],
    ),
    "duckdb_bind_uhugeint": (
        "duckdb_state",
        ["duckdb_prepared_statement statement", "idx_t param_idx", "duckdb_uhugeint const *val"],
    ),
    "duckdb_bind_decimal": (
        "duckdb_state",
        ["duckdb_prepared_statement statement", "idx_t param_idx", "duckdb_decimal const *val"],
    ),
    "duckdb_string_is_inlined": ("bool", ["duckdb_string_t const *string"]),
    "duckdb_string_t_length": ("uint32_t", ["duckdb_string_t const *string"]),
    "duckdb_result_arrow_array": (
        "void",
        ["duckdb_result *result", "duckdb_data_chunk chunk", "duckdb_arrow_array *out_array"],
    ),
    "duckdb_stream_fetch_chunk": ("duckdb_data_chunk", ["duckdb_result *result"]),
    "duckdb_fetch_chunk": ("duckdb_data_chunk", ["duckdb_result *result"]),
    "duckdb_create_hugeint": ("duckdb_value", ["const duckdb_hugeint *input"]),
    "duckdb_create_uhugeint": ("duckdb_value", ["const duckdb_uhugeint *input"]),
    "duckdb_create_bignum": ("duckdb_value", ["const duckdb_bignum *input"]),
    "duckdb_create_decimal": ("duckdb_value", ["const duckdb_decimal *input"]),
    "duckdb_create_interval": ("duckdb_value", ["const duckdb_interval *input"]),
    "duckdb_create_bit": ("duckdb_value", ["const duckdb_bit *input"]),
    "duckdb_create_uuid": ("duckdb_value", ["const duckdb_uhugeint *input"]),
    "duckdb_append_hugeint": ("duckdb_state", ["duckdb_appender appender", "const duckdb_hugeint *value"]),
    "duckdb_append_uhugeint": ("duckdb_state", ["duckdb_appender appender", "const duckdb_uhugeint *value"]),
    "duckdb_append_interval": ("duckdb_state", ["duckdb_appender appender", "const duckdb_interval *value"]),
    "duckdb_get_hugeint": ("void", ["duckdb_value val", "duckdb_hugeint *out"]),
    "duckdb_get_uhugeint": ("void", ["duckdb_value val", "duckdb_uhugeint *out"]),
    "duckdb_get_bignum": ("void", ["duckdb_value val", "duckdb_bignum *out"]),
    "duckdb_get_decimal": ("void", ["duckdb_value val", "duckdb_decimal *out"]),
    "duckdb_get_interval": ("void", ["duckdb_value val", "duckdb_interval *out"]),
    "duckdb_get_blob": ("void", ["duckdb_value val", "duckdb_blob *out"]),
    "duckdb_get_bit": ("void", ["duckdb_value val", "duckdb_bit *out"]),
    "duckdb_get_uuid": ("void", ["duckdb_value val", "duckdb_uhugeint *out"]),
    "duckdb_hugeint_to_double": ("double", ["const duckdb_hugeint *value"]),
    "duckdb_double_to_hugeint": ("void", ["double input", "duckdb_hugeint *out"]),
    "duckdb_double_to_decimal": ("void", ["double input", "uint8_t width", "uint8_t scale", "duckdb_decimal *out"]),
    "duckdb_decimal_to_double": ("double", ["const duckdb_decimal *value"]),
    "duckdb_from_date": ("void", ["duckdb_date date", "duckdb_date_struct *out"]),
    "duckdb_to_date": ("duckdb_date", ["const duckdb_date_struct *input"]),
    "duckdb_from_time": ("void", ["duckdb_time time", "duckdb_time_struct *out"]),
    "duckdb_from_time_tz": ("void", ["duckdb_time_tz micros", "duckdb_time_tz_struct *out"]),
    "duckdb_to_time": ("duckdb_time", ["const duckdb_time_struct *input"]),
    "duckdb_from_timestamp": ("void", ["duckdb_timestamp ts", "duckdb_timestamp_struct *out"]),
    "duckdb_to_timestamp": ("duckdb_timestamp", ["const duckdb_timestamp_struct *input"]),
    "duckdb_uhugeint_to_double": ("double", ["const duckdb_uhugeint *value"]),
    "duckdb_double_to_uhugeint": ("void", ["double input", "duckdb_uhugeint *out"]),
}

SECTION_TO_MODULE = {
    "Open Connect": "OpenConnect",
    "Configuration": "Configuration",
    "Error Data": "ErrorData",
    "Query Execution": "QueryExecution",
    "Result Functions": "ResultFunctions",
    "Safe Fetch Functions": "SafeFetch",
    "Helpers": "Helpers",
    "Date Time Timestamp Helpers": "Helpers",
    "Hugeint Helpers": "Helpers",
    "Unsigned Hugeint Helpers": "Helpers",
    "Decimal Helpers": "Helpers",
    "Prepared Statements": "PreparedStatements",
    "Bind Values to Prepared Statements": "BindValues",
    "Execute Prepared Statements": "ExecutePrepared",
    "Extract Statements": "ExtractStatements",
    "Pending Result Interface": "PendingResult",
    "Value Interface": "ValueInterface",
    "Logical Type Interface": "LogicalTypes",
    "Data Chunk Interface": "DataChunk",
    "Vector Interface": "Vector",
    "Validity Mask Functions": "Validity",
    "Scalar Functions": "ScalarFunctions",
    "Selection Vector Interface": "SelectionVector",
    "Aggregate Functions": "AggregateFunctions",
    "Table Functions": "TableFunctions",
    "Table Function Bind": "TableFunctions",
    "Table Function Init": "TableFunctions",
    "Table Function": "TableFunctions",
    "Replacement Scans": "ReplacementScans",
    "Profiling Info": "ProfilingInfo",
    "Appender": "Appender",
    "Table Description": "TableDescription",
    "Arrow Interface": "Arrow",
    "Threading Information": "Threading",
    "Streaming Result Interface": "StreamingResult",
    "Cast Functions": "CastFunctions",
    "Expression Interface": "Expression",
}

WRAPPER_MAP = {
    "duckdb_query_progress": "wrapped_duckdb_query_progress",
    "duckdb_result_statement_type": "wrapped_duckdb_result_statement_type",
    "duckdb_result_return_type": "wrapped_duckdb_result_return_type",
    "duckdb_result_get_arrow_options": "wrapped_duckdb_result_get_arrow_options",
    "duckdb_result_get_chunk": "wrapped_duckdb_result_get_chunk",
    "duckdb_result_chunk_count": "wrapped_duckdb_result_chunk_count",
    "duckdb_result_is_streaming": "wrapped_duckdb_result_is_streaming",
    "duckdb_value_hugeint": "wrapped_duckdb_value_hugeint",
    "duckdb_value_uhugeint": "wrapped_duckdb_value_uhugeint",
    "duckdb_value_decimal": "wrapped_duckdb_value_decimal",
    "duckdb_value_string": "wrapped_duckdb_value_string",
    "duckdb_value_string_internal": "wrapped_duckdb_value_string_internal",
    "duckdb_value_blob": "wrapped_duckdb_value_blob",
    "duckdb_value_interval": "wrapped_duckdb_value_interval",
    "duckdb_bind_interval": "wrapped_duckdb_bind_interval",
    "duckdb_bind_hugeint": "wrapped_duckdb_bind_hugeint",
    "duckdb_bind_uhugeint": "wrapped_duckdb_bind_uhugeint",
    "duckdb_bind_decimal": "wrapped_duckdb_bind_decimal",
    "duckdb_string_is_inlined": "wrapped_duckdb_string_is_inlined",
    "duckdb_string_t_length": "wrapped_duckdb_string_t_length",
    "duckdb_result_arrow_array": "wrapped_duckdb_result_arrow_array",
    "duckdb_stream_fetch_chunk": "wrapped_duckdb_stream_fetch_chunk",
    "duckdb_fetch_chunk": "wrapped_duckdb_fetch_chunk",
    "duckdb_create_hugeint": "wrapped_duckdb_create_hugeint",
    "duckdb_create_uhugeint": "wrapped_duckdb_create_uhugeint",
    "duckdb_create_bignum": "wrapped_duckdb_create_bignum",
    "duckdb_create_decimal": "wrapped_duckdb_create_decimal",
    "duckdb_create_interval": "wrapped_duckdb_create_interval",
    "duckdb_create_bit": "wrapped_duckdb_create_bit",
    "duckdb_create_uuid": "wrapped_duckdb_create_uuid",
    "duckdb_append_hugeint": "wrapped_duckdb_append_hugeint",
    "duckdb_append_uhugeint": "wrapped_duckdb_append_uhugeint",
    "duckdb_append_interval": "wrapped_duckdb_append_interval",
    "duckdb_get_hugeint": "wrapped_duckdb_get_hugeint",
    "duckdb_get_uhugeint": "wrapped_duckdb_get_uhugeint",
    "duckdb_get_bignum": "wrapped_duckdb_get_bignum",
    "duckdb_get_decimal": "wrapped_duckdb_get_decimal",
    "duckdb_get_interval": "wrapped_duckdb_get_interval",
    "duckdb_get_blob": "wrapped_duckdb_get_blob",
    "duckdb_get_bit": "wrapped_duckdb_get_bit",
    "duckdb_get_uuid": "wrapped_duckdb_get_uuid",
    "duckdb_hugeint_to_double": "wrapped_duckdb_hugeint_to_double",
    "duckdb_double_to_hugeint": "wrapped_duckdb_double_to_hugeint",
    "duckdb_double_to_decimal": "wrapped_duckdb_double_to_decimal",
    "duckdb_decimal_to_double": "wrapped_duckdb_decimal_to_double",
    "duckdb_from_date": "wrapped_duckdb_from_date",
    "duckdb_to_date": "wrapped_duckdb_to_date",
    "duckdb_from_time": "wrapped_duckdb_from_time",
    "duckdb_from_time_tz": "wrapped_duckdb_from_time_tz",
    "duckdb_to_time": "wrapped_duckdb_to_time",
    "duckdb_from_timestamp": "wrapped_duckdb_from_timestamp",
    "duckdb_to_timestamp": "wrapped_duckdb_to_timestamp",
    "duckdb_uhugeint_to_double": "wrapped_duckdb_uhugeint_to_double",
    "duckdb_double_to_uhugeint": "wrapped_duckdb_double_to_uhugeint",
}

SAFE_FUNCTIONS = {
    "duckdb_get_or_create_from_cache",
    "duckdb_open",
    "duckdb_open_ext",
    "duckdb_connect",
    "duckdb_get_table_names",
    "duckdb_query",
    "duckdb_prepare",
    "duckdb_execute_prepared",
    "duckdb_execute_prepared_streaming",
    "duckdb_pending_prepared",
    "duckdb_pending_prepared_streaming",
    "duckdb_execute_pending",
    "duckdb_pending_execute_task",
    "duckdb_extract_statements",
    "duckdb_prepare_extracted_statement",
    "duckdb_query_arrow",
    "duckdb_query_arrow_array",
    "duckdb_execute_prepared_arrow",
    "duckdb_arrow_scan",
    "duckdb_arrow_array_scan",
    "duckdb_table_description_create",
    "duckdb_table_description_create_ext",
}

BASE_TYPE_MAP = {
    "duckdb_state": "DuckDBState",
    "duckdb_database": "DuckDBDatabase",
    "duckdb_connection": "DuckDBConnection",
    "duckdb_config": "DuckDBConfig",
    "duckdb_instance_cache": "DuckDBInstanceCache",
    "duckdb_result": "DuckDBResult",
    "duckdb_arrow_options": "DuckDBArrowOptions",
    "duckdb_arrow": "DuckDBArrow",
    "duckdb_arrow_schema": "DuckDBArrowSchema",
    "duckdb_arrow_array": "DuckDBArrowArray",
    "duckdb_arrow_stream": "DuckDBArrowStream",
    "duckdb_arrow_converted_schema": "DuckDBArrowConvertedSchema",
    "duckdb_prepared_statement": "DuckDBPreparedStatement",
    "duckdb_pending_result": "DuckDBPendingResult",
    "duckdb_logical_type": "DuckDBLogicalType",
    "duckdb_value": "DuckDBValue",
    "duckdb_error_data": "DuckDBErrorData",
    "duckdb_error_type": "DuckDBErrorType",
    "duckdb_client_context": "DuckDBClientContext",
    "duckdb_data_chunk": "DuckDBDataChunk",
    "duckdb_vector": "DuckDBVector",
    "duckdb_function_info": "DuckDBFunctionInfo",
    "duckdb_bind_info": "DuckDBBindInfo",
    "duckdb_scalar_function": "DuckDBScalarFunction",
    "duckdb_scalar_function_set": "DuckDBScalarFunctionSet",
    "duckdb_expression": "DuckDBExpression",
    "duckdb_table_function": "DuckDBTableFunction",
    "duckdb_table_description": "DuckDBTableDescription",
    "duckdb_appender": "DuckDBAppender",
    "duckdb_profiling_info": "DuckDBProfilingInfo",
    "duckdb_selection_vector": "DuckDBSelectionVector",
    "duckdb_create_type_info": "DuckDBCreateTypeInfo",
    "duckdb_init_info": "DuckDBInitInfo",
    "duckdb_cast_function": "DuckDBCastFunction",
    "duckdb_cast_mode": "DuckDBCastMode",
    "duckdb_aggregate_function": "DuckDBAggregateFunction",
    "duckdb_aggregate_function_set": "DuckDBAggregateFunctionSet",
    "duckdb_aggregate_state": "DuckDBAggregateState",
    "duckdb_replacement_scan_info": "DuckDBReplacementScanInfo",
    "duckdb_extracted_statements": "DuckDBExtractedStatements",
    "duckdb_task_state": "DuckDBTaskState",
    "duckdb_query_progress_type": "DuckDBQueryProgress",
    "duckdb_date": "DuckDBDate",
    "duckdb_date_struct": "DuckDBDateStruct",
    "duckdb_time": "DuckDBTime",
    "duckdb_time_struct": "DuckDBTimeStruct",
    "duckdb_time_ns": "DuckDBTimeNs",
    "duckdb_time_tz": "DuckDBTimeTz",
    "duckdb_time_tz_struct": "DuckDBTimeTzStruct",
    "duckdb_timestamp": "DuckDBTimestamp",
    "duckdb_timestamp_s": "DuckDBTimestampS",
    "duckdb_timestamp_ms": "DuckDBTimestampMs",
    "duckdb_timestamp_ns": "DuckDBTimestampNs",
    "duckdb_timestamp_struct": "DuckDBTimestampStruct",
    "duckdb_interval": "DuckDBInterval",
    "duckdb_hugeint": "DuckDBHugeInt",
    "duckdb_uhugeint": "DuckDBUHugeInt",
    "duckdb_decimal": "DuckDBDecimal",
    "duckdb_blob": "DuckDBBlob",
    "duckdb_bit": "DuckDBBit",
    "duckdb_bignum": "DuckDBBignum",
    "duckdb_string": "DuckDBString",
    "duckdb_string_t": "DuckDBStringT",
    "duckdb_result_type": "DuckDBResultType",
    "duckdb_statement_type": "DuckDBStatementType",
    "duckdb_pending_state": "DuckDBPendingState",
    "duckdb_scalar_function_t": "DuckDBScalarFunctionFun",
    "duckdb_scalar_function_bind_t": "DuckDBScalarFunctionBindFun",
    "duckdb_delete_callback_t": "DuckDBDeleteCallback",
    "duckdb_copy_callback_t": "DuckDBCopyCallback",
    "duckdb_table_function_bind_t": "DuckDBTableFunctionBindFun",
    "duckdb_table_function_init_t": "DuckDBTableFunctionInitFun",
    "duckdb_table_function_t": "DuckDBTableFunctionFun",
    "duckdb_cast_function_t": "DuckDBCastFunctionFun",
    "duckdb_aggregate_state_size": "DuckDBAggregateStateSizeFun",
    "duckdb_aggregate_init_t": "DuckDBAggregateInitFun",
    "duckdb_aggregate_update_t": "DuckDBAggregateUpdateFun",
    "duckdb_aggregate_combine_t": "DuckDBAggregateCombineFun",
    "duckdb_aggregate_finalize_t": "DuckDBAggregateFinalizeFun",
    "duckdb_aggregate_destroy_t": "DuckDBAggregateDestroyFun",
    "duckdb_replacement_callback_t": "DuckDBReplacementCallback",
    "duckdb_type": "DuckDBType",
    "ArrowArray": "ArrowArray",
    "ArrowSchema": "ArrowSchema",
}

NEWTYPE_CTYPES = {
    "DuckDBState": "CInt",
    "DuckDBResultType": "CInt",
    "DuckDBStatementType": "CInt",
    "DuckDBErrorType": "CInt",
    "DuckDBCastMode": "CInt",
    "DuckDBPendingState": "CInt",
}

PRIMITIVE_MAP = {
    "void": "()",
    "bool": "CBool",
    "int8_t": "Int8",
    "int16_t": "Int16",
    "int32_t": "Int32",
    "int64_t": "Int64",
    "uint8_t": "Word8",
    "uint16_t": "Word16",
    "uint32_t": "Word32",
    "uint64_t": "Word64",
    "size_t": "CSize",
    "idx_t": "DuckDBIdx",
    "float": "CFloat",
    "double": "CDouble",
    "char": "CChar",
    "sel_t": "DuckDBSel",
}


def normalize_type(c_type: str) -> str:
    result = c_type.strip()
    result = result.replace("const ", "").replace("const", "")
    result = result.replace("struct ", "")
    result = result.replace("volatile ", "")
    result = " ".join(result.split())
    return result


def map_c_type(c_type: str) -> Tuple[str, Dict[str, set | bool]]:
    info = {
        "need_ptr": False,
        "need_cstring": False,
        "ctypes": set(),
        "ints": set(),
        "words": set(),
    }
    pointer_level = c_type.count("*")
    base = normalize_type(c_type.replace("*", ""))
    if base == "unsigned char":
        base = "uint8_t"

    if base == "char":
        if pointer_level == 0:
            info["ctypes"].add("CChar")
            return "CChar", info
        result = "CString"
        info["need_cstring"] = True
        pointer_level -= 1
        while pointer_level > 0:
            info["need_ptr"] = True
            result = f"Ptr {result}"
            pointer_level -= 1
        return result, info

    if base in BASE_TYPE_MAP:
        result = BASE_TYPE_MAP[base]
    elif base in PRIMITIVE_MAP:
        result = PRIMITIVE_MAP[base]
    else:
        raise ValueError(f"Unknown base type: {base} (original: {c_type})")

    if result in {"CBool", "CDouble", "CFloat", "CSize", "CChar"}:
        info["ctypes"].add(result)
    elif result in {"Int8", "Int16", "Int32", "Int64"}:
        info["ints"].add(result)
    elif result in {"Word8", "Word16", "Word32", "Word64"}:
        info["words"].add(result)

    if result in NEWTYPE_CTYPES:
        info["ctypes"].add(NEWTYPE_CTYPES[result])

    while pointer_level > 0:
        info["need_ptr"] = True
        result = f"Ptr {result}"
        pointer_level -= 1

    return result, info


def collect_module_specs(sections: OrderedDict[str, List[FunctionDecl]]) -> Dict[str, ModuleSpec]:
    specs: Dict[str, ModuleSpec] = {}
    for section, functions in sections.items():
        module = SECTION_TO_MODULE.get(section)
        if not module:
            raise ValueError(f"No module mapping for section {section}")
        updated: List[FunctionDecl] = []
        for func in functions:
            override = SIGNATURE_OVERRIDE.get(func.name)
            if override:
                ret, params = override
                parsed_params = [parse_param(p) for p in params]
                updated.append(FunctionDecl(func.name, ret, parsed_params))
            else:
                updated.append(
                    FunctionDecl(func.name, func.return_type, list(func.params))
                )
        if module in specs:
            specs[module].functions.extend(updated)
        else:
            specs[module] = ModuleSpec(section=section, functions=updated)
    return specs


def format_imports(imports: Dict[str, set | bool]) -> List[str]:
    lines: List[str] = ["import Database.DuckDB.FFI.Types"]
    if imports["need_cstring"]:
        lines.append("import Foreign.C.String (CString)")
    ctypes = sorted(imports["ctypes"])
    if ctypes:
        lines.append(f"import Foreign.C.Types ({', '.join(f'{name} (..)' for name in ctypes)})")
    ints = sorted(imports["ints"])
    if ints:
        lines.append(f"import Data.Int ({', '.join(ints)})")
    words = sorted(imports["words"])
    if words:
        lines.append(f"import Data.Word ({', '.join(words)})")
    if imports["need_ptr"]:
        lines.append("import Foreign.Ptr (Ptr)")
    return lines


def wrap_result_type(ret_type: str) -> str:
    if ret_type.startswith("(") and ret_type.endswith(")"):
        return ret_type
    if " " in ret_type or "->" in ret_type:
        return f"({ret_type})"
    return ret_type


def generate_modules(specs: Dict[str, ModuleSpec], out_dir: Path) -> None:
    for module, spec in specs.items():
        imports = {
            "need_ptr": False,
            "need_cstring": False,
            "ctypes": set(),
            "ints": set(),
            "words": set(),
        }
        rendered_functions: List[str] = []
        export_names: List[str] = []

        for fdecl in spec.functions:
            ret_type, ret_info = map_c_type(fdecl.return_type)
            param_types: List[str] = []
            accum_info = [ret_info]
            for param_type, _ in fdecl.params:
                mapped, info = map_c_type(param_type)
                param_types.append(mapped)
                accum_info.append(info)
            for info in accum_info:
                for key in imports:
                    if isinstance(info[key], set):
                        imports[key].update(info[key])
                    else:
                        imports[key] = imports[key] or info[key]

            hs_name = f"c_{fdecl.name}"
            export_names.append(hs_name)
            symbol = WRAPPER_MAP.get(fdecl.name, fdecl.name)
            callconv = "safe" if fdecl.name in SAFE_FUNCTIONS else "unsafe"
            wrapped_ret = wrap_result_type(ret_type)
            if param_types:
                signature = " -> ".join(param_types + [f"IO {wrapped_ret}"])
            else:
                signature = f"IO {wrapped_ret}"
            rendered = textwrap.dedent(
                f'''\
                foreign import ccall {callconv!s} "{symbol}"
                  {hs_name} :: {signature}
                '''
            ).rstrip()
            rendered_functions.append(rendered)

        import_lines = format_imports(imports)
        exports_block = ",\n  ".join(export_names)
        module_header = (
            f"module Database.DuckDB.FFI.{module} (\n"
            f"  {exports_block}\n"
            f") where"
        )

        body = "\n\n".join(rendered_functions)
        content = module_header + "\n\n" + "\n".join(import_lines) + "\n\n" + body + "\n"
        out_path = out_dir / f"{module}.hs"
        out_path.write_text(content, encoding="utf8")


def main() -> None:
    repo_root = Path(__file__).resolve().parents[1]
    api_path = repo_root / "api-list.md"
    sections = parse_api_list(api_path)
    specs = collect_module_specs(sections)
    out_dir = repo_root / "src" / "Database" / "DuckDB" / "FFI"
    generate_modules(specs, out_dir)


if __name__ == "__main__":
    main()
