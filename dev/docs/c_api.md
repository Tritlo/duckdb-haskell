# docs/stable/clients/c/overview

_Source: https://duckdb.org/docs/stable/clients/c/overview.md_

---
layout: docu
redirect_from:
- /docs/api/c
- /docs/api/c/
- /docs/api/c/overview
- /docs/api/c/overview/
- /docs/clients/c/overview
title: Overview
---

> The latest stable version of the DuckDB C API is {{ site.current_duckdb_version }}.

DuckDB implements a custom C API modeled somewhat following the SQLite C API. The API is contained in the `duckdb.h` header. Continue to [Startup & Shutdown](https://duckdb.org/docs/stable/clients/c/connect.md) to get started, or check out the [Full API overview](https://duckdb.org/docs/stable/clients/c/api.md).

We also provide a SQLite API wrapper which means that if your applications is programmed against the SQLite C API, you can re-link to DuckDB and it should continue working. See the [`sqlite_api_wrapper`](https://github.com/duckdb/duckdb/tree/main/tools/sqlite3_api_wrapper) folder in our source repository for more information.

## Installation

The DuckDB C API can be installed as part of the `libduckdb` packages. Please see the [installation page](https://duckdb.org/install/index.html?environment=cplusplus) for details.


---

# docs/stable/clients/c/connect

_Source: https://duckdb.org/docs/stable/clients/c/connect.md_

---
layout: docu
redirect_from:
- /docs/api/c/connect
- /docs/api/c/connect/
- /docs/clients/c/connect
title: Startup & Shutdown
---

To use DuckDB, you must first initialize a `duckdb_database` handle using `duckdb_open()`. `duckdb_open()` takes as parameter the database file to read and write from. The special value `NULL` (`nullptr`) can be used to create an **in-memory database**. Note that for an in-memory database no data is persisted to disk (i.e., all data is lost when you exit the process).

With the `duckdb_database` handle, you can create one or many `duckdb_connection` using `duckdb_connect()`. While individual connections are thread-safe, they will be locked during querying. It is therefore recommended that each thread uses its own connection to allow for the best parallel performance.

All `duckdb_connection`s have to explicitly be disconnected with `duckdb_disconnect()` and the `duckdb_database` has to be explicitly closed with `duckdb_close()` to avoid memory and file handle leaking.

## Example

```c
duckdb_database db;
duckdb_connection con;

if (duckdb_open(NULL, &db) == DuckDBError) {
    // handle error
}
if (duckdb_connect(db, &con) == DuckDBError) {
    // handle error
}

// run queries...

// cleanup
duckdb_disconnect(&con);
duckdb_close(&db);
```

## API Reference Overview

duckdb_instance_cache duckdb_create_instance_cache();
duckdb_state duckdb_get_or_create_from_cache(duckdb_instance_cache instance_cache, const char *path, duckdb_database *out_database, duckdb_config config, char **out_error);
void duckdb_destroy_instance_cache(duckdb_instance_cache *instance_cache);
duckdb_state duckdb_open(const char *path, duckdb_database *out_database);
duckdb_state duckdb_open_ext(const char *path, duckdb_database *out_database, duckdb_config config, char **out_error);
void duckdb_close(duckdb_database *database);
duckdb_state duckdb_connect(duckdb_database database, duckdb_connection *out_connection);
void duckdb_interrupt(duckdb_connection connection);
duckdb_query_progress_type duckdb_query_progress(duckdb_connection connection);
void duckdb_disconnect(duckdb_connection *connection);
void duckdb_connection_get_client_context(duckdb_connection connection, duckdb_client_context *out_context);
void duckdb_connection_get_arrow_options(duckdb_connection connection, duckdb_arrow_options *out_arrow_options);
idx_t duckdb_client_context_get_connection_id(duckdb_client_context context);
void duckdb_destroy_client_context(duckdb_client_context *context);
void duckdb_destroy_arrow_options(duckdb_arrow_options *arrow_options);
const char *duckdb_library_version();
duckdb_value duckdb_get_table_names(duckdb_connection connection, const char *query, bool qualified);

#### `duckdb_create_instance_cache`

Creates a new database instance cache.
The instance cache is necessary if a client/program (re)opens multiple databases to the same file within the same
process. Must be destroyed with 'duckdb_destroy_instance_cache'.

##### Return Value

The database instance cache.

##### Syntax

duckdb_instance_cache duckdb_create_instance_cache(

);

#### `duckdb_get_or_create_from_cache`

Creates a new database instance in the instance cache, or retrieves an existing database instance.
Must be closed with 'duckdb_close'.

##### Syntax

duckdb_state duckdb_get_or_create_from_cache(
  duckdb_instance_cache instance_cache,
  const char *path,
  duckdb_database *out_database,
  duckdb_config config,
  char **out_error
);

##### Parameters

* `instance_cache`: The instance cache in which to create the database, or from which to take the database.
* `path`: Path to the database file on disk. Both `nullptr` and `:memory:` open or retrieve an in-memory database.
* `out_database`: The resulting cached database.
* `config`: (Optional) configuration used to create the database.
* `out_error`: If set and the function returns `DuckDBError`, this contains the error message.
Note that the error message must be freed using `duckdb_free`.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_destroy_instance_cache`

Destroys an existing database instance cache and de-allocates its memory.

##### Syntax

void duckdb_destroy_instance_cache(
  duckdb_instance_cache *instance_cache
);

##### Parameters

* `instance_cache`: The instance cache to destroy.

#### `duckdb_open`

Creates a new database or opens an existing database file stored at the given path.
If no path is given a new in-memory database is created instead.
The database must be closed with 'duckdb_close'.

##### Syntax

duckdb_state duckdb_open(
  const char *path,
  duckdb_database *out_database
);

##### Parameters

* `path`: Path to the database file on disk. Both `nullptr` and `:memory:` open an in-memory database.
* `out_database`: The result database object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_open_ext`

Extended version of duckdb_open. Creates a new database or opens an existing database file stored at the given path.
The database must be closed with 'duckdb_close'.

##### Syntax

duckdb_state duckdb_open_ext(
  const char *path,
  duckdb_database *out_database,
  duckdb_config config,
  char **out_error
);

##### Parameters

* `path`: Path to the database file on disk. Both `nullptr` and `:memory:` open an in-memory database.
* `out_database`: The result database object.
* `config`: (Optional) configuration used to start up the database.
* `out_error`: If set and the function returns `DuckDBError`, this contains the error message.
Note that the error message must be freed using `duckdb_free`.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_close`

Closes the specified database and de-allocates all memory allocated for that database.
This should be called after you are done with any database allocated through `duckdb_open` or `duckdb_open_ext`.
Note that failing to call `duckdb_close` (in case of e.g., a program crash) will not cause data corruption.
Still, it is recommended to always correctly close a database object after you are done with it.

##### Syntax

void duckdb_close(
  duckdb_database *database
);

##### Parameters

* `database`: The database object to shut down.

#### `duckdb_connect`

Opens a connection to a database. Connections are required to query the database, and store transactional state
associated with the connection.
The instantiated connection should be closed using 'duckdb_disconnect'.

##### Syntax

duckdb_state duckdb_connect(
  duckdb_database database,
  duckdb_connection *out_connection
);

##### Parameters

* `database`: The database file to connect to.
* `out_connection`: The result connection object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_interrupt`

Interrupt running query

##### Syntax

void duckdb_interrupt(
  duckdb_connection connection
);

##### Parameters

* `connection`: The connection to interrupt

#### `duckdb_query_progress`

Get progress of the running query

##### Syntax

duckdb_query_progress_type duckdb_query_progress(
  duckdb_connection connection
);

##### Parameters

* `connection`: The working connection

##### Return Value

-1 if no progress or a percentage of the progress

#### `duckdb_disconnect`

Closes the specified connection and de-allocates all memory allocated for that connection.

##### Syntax

void duckdb_disconnect(
  duckdb_connection *connection
);

##### Parameters

* `connection`: The connection to close.

#### `duckdb_connection_get_client_context`

Retrieves the client context of the connection.

##### Syntax

void duckdb_connection_get_client_context(
  duckdb_connection connection,
  duckdb_client_context *out_context
);

##### Parameters

* `connection`: The connection.
* `out_context`: The client context of the connection. Must be destroyed with `duckdb_destroy_client_context`.

#### `duckdb_connection_get_arrow_options`

Retrieves the arrow options of the connection.

##### Syntax

void duckdb_connection_get_arrow_options(
  duckdb_connection connection,
  duckdb_arrow_options *out_arrow_options
);

##### Parameters

* `connection`: The connection.

#### `duckdb_client_context_get_connection_id`

Returns the connection id of the client context.

##### Syntax

idx_t duckdb_client_context_get_connection_id(
  duckdb_client_context context
);

##### Parameters

* `context`: The client context.

##### Return Value

The connection id of the client context.

#### `duckdb_destroy_client_context`

Destroys the client context and deallocates its memory.

##### Syntax

void duckdb_destroy_client_context(
  duckdb_client_context *context
);

##### Parameters

* `context`: The client context to destroy.

#### `duckdb_destroy_arrow_options`

Destroys the arrow options and deallocates its memory.

##### Syntax

void duckdb_destroy_arrow_options(
  duckdb_arrow_options *arrow_options
);

##### Parameters

* `arrow_options`: The arrow options to destroy.

#### `duckdb_library_version`

Returns the version of the linked DuckDB, with a version postfix for dev versions

Usually used for developing C extensions that must return this for a compatibility check.

##### Syntax

const char *duckdb_library_version(

);

#### `duckdb_get_table_names`

Get the list of (fully qualified) table names of the query.

##### Syntax

duckdb_value duckdb_get_table_names(
  duckdb_connection connection,
  const char *query,
  bool qualified
);

##### Parameters

* `connection`: The connection for which to get the table names.
* `query`: The query for which to get the table names.
* `qualified`: Returns fully qualified table names (catalog.schema.table), if set to true, else only the (not
escaped) table names.

##### Return Value

A duckdb_value of type VARCHAR[] containing the (fully qualified) table names of the query. Must be destroyed
with duckdb_destroy_value.


---

# docs/stable/clients/c/config

_Source: https://duckdb.org/docs/stable/clients/c/config.md_

---
layout: docu
redirect_from:
- /docs/api/c/config
- /docs/api/c/config/
- /docs/clients/c/config
title: Configuration
---

Configuration options can be provided to change different settings of the database system. Note that many of these
settings can be changed later on using [`PRAGMA` statements](../../configuration/pragmas) as well. The configuration object
should be created, filled with values and passed to `duckdb_open_ext`.

## Example

```c
duckdb_database db;
duckdb_config config;

// create the configuration object
if (duckdb_create_config(&config) == DuckDBError) {
    // handle error
}
// set some configuration options
duckdb_set_config(config, "access_mode", "READ_WRITE"); // or READ_ONLY
duckdb_set_config(config, "threads", "8");
duckdb_set_config(config, "max_memory", "8GB");
duckdb_set_config(config, "default_order", "DESC");

// open the database using the configuration
if (duckdb_open_ext(NULL, &db, config, NULL) == DuckDBError) {
    // handle error
}
// cleanup the configuration object
duckdb_destroy_config(&config);

// run queries...

// cleanup
duckdb_close(&db);
```

## API Reference Overview

duckdb_state duckdb_create_config(duckdb_config *out_config);
size_t duckdb_config_count();
duckdb_state duckdb_get_config_flag(size_t index, const char **out_name, const char **out_description);
duckdb_state duckdb_set_config(duckdb_config config, const char *name, const char *option);
void duckdb_destroy_config(duckdb_config *config);

#### `duckdb_create_config`

Initializes an empty configuration object that can be used to provide start-up options for the DuckDB instance
through `duckdb_open_ext`.
The duckdb_config must be destroyed using 'duckdb_destroy_config'

This will always succeed unless there is a malloc failure.

Note that `duckdb_destroy_config` should always be called on the resulting config, even if the function returns
`DuckDBError`.

##### Syntax

duckdb_state duckdb_create_config(
  duckdb_config *out_config
);

##### Parameters

* `out_config`: The result configuration object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_config_count`

This returns the total amount of configuration options available for usage with `duckdb_get_config_flag`.

This should not be called in a loop as it internally loops over all the options.

##### Return Value

The amount of config options available.

##### Syntax

size_t duckdb_config_count(

);

#### `duckdb_get_config_flag`

Obtains a human-readable name and description of a specific configuration option. This can be used to e.g.
display configuration options. This will succeed unless `index` is out of range (i.e., `>= duckdb_config_count`).

The result name or description MUST NOT be freed.

##### Syntax

duckdb_state duckdb_get_config_flag(
  size_t index,
  const char **out_name,
  const char **out_description
);

##### Parameters

* `index`: The index of the configuration option (between 0 and `duckdb_config_count`)
* `out_name`: A name of the configuration flag.
* `out_description`: A description of the configuration flag.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_set_config`

Sets the specified option for the specified configuration. The configuration option is indicated by name.
To obtain a list of config options, see `duckdb_get_config_flag`.

In the source code, configuration options are defined in `config.cpp`.

This can fail if either the name is invalid, or if the value provided for the option is invalid.

##### Syntax

duckdb_state duckdb_set_config(
  duckdb_config config,
  const char *name,
  const char *option
);

##### Parameters

* `config`: The configuration object to set the option on.
* `name`: The name of the configuration flag to set.
* `option`: The value to set the configuration flag to.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_destroy_config`

Destroys the specified configuration object and de-allocates all memory allocated for the object.

##### Syntax

void duckdb_destroy_config(
  duckdb_config *config
);

##### Parameters

* `config`: The configuration object to destroy.


---

# docs/stable/clients/c/query

_Source: https://duckdb.org/docs/stable/clients/c/query.md_

---
layout: docu
redirect_from:
- /docs/api/c/query
- /docs/api/c/query/
- /docs/clients/c/query
title: Query
---

The `duckdb_query` method allows SQL queries to be run in DuckDB from C. This method takes two parameters, a (null-terminated) SQL query string and a `duckdb_result` result pointer. The result pointer may be `NULL` if the application is not interested in the result set or if the query produces no result. After the result is consumed, the `duckdb_destroy_result` method should be used to clean up the result.

Elements can be extracted from the `duckdb_result` object using a variety of methods. The `duckdb_column_count` can be used to extract the number of columns. `duckdb_column_name` and `duckdb_column_type` can be used to extract the names and types of individual columns.

## Example

```c
duckdb_state state;
duckdb_result result;

// create a table
state = duckdb_query(con, "CREATE TABLE integers (i INTEGER, j INTEGER);", NULL);
if (state == DuckDBError) {
    // handle error
}
// insert three rows into the table
state = duckdb_query(con, "INSERT INTO integers VALUES (3, 4), (5, 6), (7, NULL);", NULL);
if (state == DuckDBError) {
    // handle error
}
// query rows again
state = duckdb_query(con, "SELECT * FROM integers", &result);
if (state == DuckDBError) {
    // handle error
}
// handle the result
// ...

// destroy the result after we are done with it
duckdb_destroy_result(&result);
```

## Value Extraction

Values can be extracted using either the `duckdb_fetch_chunk` function, or using the `duckdb_value` convenience functions. The `duckdb_fetch_chunk` function directly hands you data chunks in DuckDB's native array format and can therefore be very fast. The `duckdb_value` functions perform bounds- and type-checking, and will automatically cast values to the desired type. This makes them more convenient and easier to use, at the expense of being slower.

See the [Types](https://duckdb.org/docs/stable/clients/c/types.md) page for more information.

> For optimal performance, use `duckdb_fetch_chunk` to extract data from the query result.
> The `duckdb_value` functions perform internal type-checking, bounds-checking and casting which makes them slower.

### `duckdb_fetch_chunk`

Below is an end-to-end example that prints the above result to CSV format using the `duckdb_fetch_chunk` function.
Note that the function is NOT generic: we do need to know exactly what the types of the result columns are.

```c
duckdb_database db;
duckdb_connection con;
duckdb_open(nullptr, &db);
duckdb_connect(db, &con);

duckdb_result res;
duckdb_query(con, "CREATE TABLE integers (i INTEGER, j INTEGER);", NULL);
duckdb_query(con, "INSERT INTO integers VALUES (3, 4), (5, 6), (7, NULL);", NULL);
duckdb_query(con, "SELECT * FROM integers;", &res);

// iterate until result is exhausted
while (true) {
    duckdb_data_chunk result = duckdb_fetch_chunk(res);
    if (!result) {
        // result is exhausted
        break;
    }
    // get the number of rows from the data chunk
    idx_t row_count = duckdb_data_chunk_get_size(result);
    // get the first column
    duckdb_vector col1 = duckdb_data_chunk_get_vector(result, 0);
    int32_t *col1_data = (int32_t *) duckdb_vector_get_data(col1);
    uint64_t *col1_validity = duckdb_vector_get_validity(col1);

    // get the second column
    duckdb_vector col2 = duckdb_data_chunk_get_vector(result, 1);
    int32_t *col2_data = (int32_t *) duckdb_vector_get_data(col2);
    uint64_t *col2_validity = duckdb_vector_get_validity(col2);

    // iterate over the rows
    for (idx_t row = 0; row < row_count; row++) {
        if (duckdb_validity_row_is_valid(col1_validity, row)) {
            printf("%d", col1_data[row]);
        } else {
            printf("NULL");
        }
        printf(",");
        if (duckdb_validity_row_is_valid(col2_validity, row)) {
            printf("%d", col2_data[row]);
        } else {
            printf("NULL");
        }
        printf("\n");
    }
    duckdb_destroy_data_chunk(&result);
}
// clean-up
duckdb_destroy_result(&res);
duckdb_disconnect(&con);
duckdb_close(&db);
```

This prints the following result:

```csv
3,4
5,6
7,NULL
```

### `duckdb_value`

> Deprecated The `duckdb_value` functions are deprecated and are scheduled for removal in a future release.

Below is an example that prints the above result to CSV format using the `duckdb_value_varchar` function.
Note that the function is generic: we do not need to know about the types of the individual result columns.

```c
// print the above result to CSV format using `duckdb_value_varchar`
idx_t row_count = duckdb_row_count(&result);
idx_t column_count = duckdb_column_count(&result);
for (idx_t row = 0; row < row_count; row++) {
    for (idx_t col = 0; col < column_count; col++) {
        if (col > 0) printf(",");
        auto str_val = duckdb_value_varchar(&result, col, row);
        printf("%s", str_val);
        duckdb_free(str_val);
   }
   printf("\n");
}
```

## API Reference Overview

duckdb_state duckdb_query(duckdb_connection connection, const char *query, duckdb_result *out_result);
void duckdb_destroy_result(duckdb_result *result);
const char *duckdb_column_name(duckdb_result *result, idx_t col);
duckdb_type duckdb_column_type(duckdb_result *result, idx_t col);
duckdb_statement_type duckdb_result_statement_type(duckdb_result result);
duckdb_logical_type duckdb_column_logical_type(duckdb_result *result, idx_t col);
duckdb_arrow_options duckdb_result_get_arrow_options(duckdb_result *result);
idx_t duckdb_column_count(duckdb_result *result);
idx_t duckdb_row_count(duckdb_result *result);
idx_t duckdb_rows_changed(duckdb_result *result);
void *duckdb_column_data(duckdb_result *result, idx_t col);
bool *duckdb_nullmask_data(duckdb_result *result, idx_t col);
const char *duckdb_result_error(duckdb_result *result);
duckdb_error_type duckdb_result_error_type(duckdb_result *result);

#### `duckdb_query`

Executes a SQL query within a connection and stores the full (materialized) result in the out_result pointer.
If the query fails to execute, DuckDBError is returned and the error message can be retrieved by calling
`duckdb_result_error`.

Note that after running `duckdb_query`, `duckdb_destroy_result` must be called on the result object even if the
query fails, otherwise the error stored within the result will not be freed correctly.

##### Syntax

duckdb_state duckdb_query(
  duckdb_connection connection,
  const char *query,
  duckdb_result *out_result
);

##### Parameters

* `connection`: The connection to perform the query in.
* `query`: The SQL query to run.
* `out_result`: The query result.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_destroy_result`

Closes the result and de-allocates all memory allocated for that result.

##### Syntax

void duckdb_destroy_result(
  duckdb_result *result
);

##### Parameters

* `result`: The result to destroy.

#### `duckdb_column_name`

Returns the column name of the specified column. The result should not need to be freed; the column names will
automatically be destroyed when the result is destroyed.

Returns `NULL` if the column is out of range.

##### Syntax

const char *duckdb_column_name(
  duckdb_result *result,
  idx_t col
);

##### Parameters

* `result`: The result object to fetch the column name from.
* `col`: The column index.

##### Return Value

The column name of the specified column.

#### `duckdb_column_type`

Returns the column type of the specified column.

Returns `DUCKDB_TYPE_INVALID` if the column is out of range.

##### Syntax

duckdb_type duckdb_column_type(
  duckdb_result *result,
  idx_t col
);

##### Parameters

* `result`: The result object to fetch the column type from.
* `col`: The column index.

##### Return Value

The column type of the specified column.

#### `duckdb_result_statement_type`

Returns the statement type of the statement that was executed

##### Syntax

duckdb_statement_type duckdb_result_statement_type(
  duckdb_result result
);

##### Parameters

* `result`: The result object to fetch the statement type from.

##### Return Value

duckdb_statement_type value or DUCKDB_STATEMENT_TYPE_INVALID

#### `duckdb_column_logical_type`

Returns the logical column type of the specified column.

The return type of this call should be destroyed with `duckdb_destroy_logical_type`.

Returns `NULL` if the column is out of range.

##### Syntax

duckdb_logical_type duckdb_column_logical_type(
  duckdb_result *result,
  idx_t col
);

##### Parameters

* `result`: The result object to fetch the column type from.
* `col`: The column index.

##### Return Value

The logical column type of the specified column.

#### `duckdb_result_get_arrow_options`

Returns the arrow options associated with the given result. These options are definitions of how the arrow arrays/schema
should be produced.

##### Syntax

duckdb_arrow_options duckdb_result_get_arrow_options(
  duckdb_result *result
);

##### Parameters

* `result`: The result object to fetch arrow options from.

##### Return Value

The arrow options associated with the given result. This must be destroyed with
`duckdb_destroy_arrow_options`.

#### `duckdb_column_count`

Returns the number of columns present in a the result object.

##### Syntax

idx_t duckdb_column_count(
  duckdb_result *result
);

##### Parameters

* `result`: The result object.

##### Return Value

The number of columns present in the result object.

#### `duckdb_row_count`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Returns the number of rows present in the result object.

##### Syntax

idx_t duckdb_row_count(
  duckdb_result *result
);

##### Parameters

* `result`: The result object.

##### Return Value

The number of rows present in the result object.

#### `duckdb_rows_changed`

Returns the number of rows changed by the query stored in the result. This is relevant only for INSERT/UPDATE/DELETE
queries. For other queries the rows_changed will be 0.

##### Syntax

idx_t duckdb_rows_changed(
  duckdb_result *result
);

##### Parameters

* `result`: The result object.

##### Return Value

The number of rows changed.

#### `duckdb_column_data`

> Deprecated This method has been deprecated. Prefer using `duckdb_result_get_chunk` instead.

Returns the data of a specific column of a result in columnar format.

The function returns a dense array which contains the result data. The exact type stored in the array depends on the
corresponding duckdb_type (as provided by `duckdb_column_type`). For the exact type by which the data should be
accessed, see the comments in [the types section](types) or the `DUCKDB_TYPE` enum.

For example, for a column of type `DUCKDB_TYPE_INTEGER`, rows can be accessed in the following manner:
```c
int32_t *data = (int32_t *) duckdb_column_data(&result, 0);
printf("Data for row %d: %d\n", row, data[row]);
```

##### Syntax

void *duckdb_column_data(
  duckdb_result *result,
  idx_t col
);

##### Parameters

* `result`: The result object to fetch the column data from.
* `col`: The column index.

##### Return Value

The column data of the specified column.

#### `duckdb_nullmask_data`

> Deprecated This method has been deprecated. Prefer using `duckdb_result_get_chunk` instead.

Returns the nullmask of a specific column of a result in columnar format. The nullmask indicates for every row
whether or not the corresponding row is `NULL`. If a row is `NULL`, the values present in the array provided
by `duckdb_column_data` are undefined.

```c
int32_t *data = (int32_t *) duckdb_column_data(&result, 0);
bool *nullmask = duckdb_nullmask_data(&result, 0);
if (nullmask[row]) {
    printf("Data for row %d: NULL\n", row);
} else {
    printf("Data for row %d: %d\n", row, data[row]);
}
```

##### Syntax

bool *duckdb_nullmask_data(
  duckdb_result *result,
  idx_t col
);

##### Parameters

* `result`: The result object to fetch the nullmask from.
* `col`: The column index.

##### Return Value

The nullmask of the specified column.

#### `duckdb_result_error`

Returns the error message contained within the result. The error is only set if `duckdb_query` returns `DuckDBError`.

The result of this function must not be freed. It will be cleaned up when `duckdb_destroy_result` is called.

##### Syntax

const char *duckdb_result_error(
  duckdb_result *result
);

##### Parameters

* `result`: The result object to fetch the error from.

##### Return Value

The error of the result.

#### `duckdb_result_error_type`

Returns the result error type contained within the result. The error is only set if `duckdb_query` returns
`DuckDBError`.

##### Syntax

duckdb_error_type duckdb_result_error_type(
  duckdb_result *result
);

##### Parameters

* `result`: The result object to fetch the error from.

##### Return Value

The error type of the result.


---

# docs/stable/clients/c/data_chunk

_Source: https://duckdb.org/docs/stable/clients/c/data_chunk.md_

---
layout: docu
redirect_from:
- /docs/api/c/data_chunk
- /docs/api/c/data_chunk/
- /docs/clients/c/data_chunk
title: Data Chunks
---

Data chunks represent a horizontal slice of a table. They hold a number of [vectors](https://duckdb.org/docs/stable/clients/c/vector.md), that can each hold up to the `VECTOR_SIZE` rows. The vector size can be obtained through the `duckdb_vector_size` function and is configurable, but is usually set to `2048`.

Data chunks and vectors are what DuckDB uses natively to store and represent data. For this reason, the data chunk interface is the most efficient way of interfacing with DuckDB. Be aware, however, that correctly interfacing with DuckDB using the data chunk API does require knowledge of DuckDB's internal vector format.

Data chunks can be used in two manners:

* **Reading Data**: Data chunks can be obtained from query results using the `duckdb_fetch_chunk` method, or as input to a user-defined function. In this case, the [vector methods](https://duckdb.org/docs/stable/clients/c/vector.md) can be used to read individual values.
* **Writing Data**: Data chunks can be created using `duckdb_create_data_chunk`. The data chunk can then be filled with values and used in `duckdb_append_data_chunk` to write data to the database.

The primary manner of interfacing with data chunks is by obtaining the internal vectors of the data chunk using the `duckdb_data_chunk_get_vector` method. Afterwards, the [vector methods](https://duckdb.org/docs/stable/clients/c/vector.md) can be used to read from or write to the individual vectors.

## API Reference Overview

duckdb_data_chunk duckdb_create_data_chunk(duckdb_logical_type *types, idx_t column_count);
void duckdb_destroy_data_chunk(duckdb_data_chunk *chunk);
void duckdb_data_chunk_reset(duckdb_data_chunk chunk);
idx_t duckdb_data_chunk_get_column_count(duckdb_data_chunk chunk);
duckdb_vector duckdb_data_chunk_get_vector(duckdb_data_chunk chunk, idx_t col_idx);
idx_t duckdb_data_chunk_get_size(duckdb_data_chunk chunk);
void duckdb_data_chunk_set_size(duckdb_data_chunk chunk, idx_t size);

#### `duckdb_create_data_chunk`

Creates an empty data chunk with the specified column types.
The result must be destroyed with `duckdb_destroy_data_chunk`.

##### Syntax

duckdb_data_chunk duckdb_create_data_chunk(
  duckdb_logical_type *types,
  idx_t column_count
);

##### Parameters

* `types`: An array of column types. Column types can not contain ANY and INVALID types.
* `column_count`: The number of columns.

##### Return Value

The data chunk.

#### `duckdb_destroy_data_chunk`

Destroys the data chunk and de-allocates all memory allocated for that chunk.

##### Syntax

void duckdb_destroy_data_chunk(
  duckdb_data_chunk *chunk
);

##### Parameters

* `chunk`: The data chunk to destroy.

#### `duckdb_data_chunk_reset`

Resets a data chunk, clearing the validity masks and setting the cardinality of the data chunk to 0.
After calling this method, you must call `duckdb_vector_get_validity` and `duckdb_vector_get_data` to obtain current
data and validity pointers

##### Syntax

void duckdb_data_chunk_reset(
  duckdb_data_chunk chunk
);

##### Parameters

* `chunk`: The data chunk to reset.

#### `duckdb_data_chunk_get_column_count`

Retrieves the number of columns in a data chunk.

##### Syntax

idx_t duckdb_data_chunk_get_column_count(
  duckdb_data_chunk chunk
);

##### Parameters

* `chunk`: The data chunk to get the data from

##### Return Value

The number of columns in the data chunk

#### `duckdb_data_chunk_get_vector`

Retrieves the vector at the specified column index in the data chunk.

The pointer to the vector is valid for as long as the chunk is alive.
It does NOT need to be destroyed.

##### Syntax

duckdb_vector duckdb_data_chunk_get_vector(
  duckdb_data_chunk chunk,
  idx_t col_idx
);

##### Parameters

* `chunk`: The data chunk to get the data from

##### Return Value

The vector

#### `duckdb_data_chunk_get_size`

Retrieves the current number of tuples in a data chunk.

##### Syntax

idx_t duckdb_data_chunk_get_size(
  duckdb_data_chunk chunk
);

##### Parameters

* `chunk`: The data chunk to get the data from

##### Return Value

The number of tuples in the data chunk

#### `duckdb_data_chunk_set_size`

Sets the current number of tuples in a data chunk.

##### Syntax

void duckdb_data_chunk_set_size(
  duckdb_data_chunk chunk,
  idx_t size
);

##### Parameters

* `chunk`: The data chunk to set the size in
* `size`: The number of tuples in the data chunk


---

# docs/stable/clients/c/vector

_Source: https://duckdb.org/docs/stable/clients/c/vector.md_

---
layout: docu
redirect_from:
- /docs/api/c/vector
- /docs/api/c/vector/
- /docs/clients/c/vector
title: Vectors
---

Vectors represent a horizontal slice of a column. They hold a number of values of a specific type, similar to an array. Vectors are the core data representation used in DuckDB. Vectors are typically stored within [data chunks](https://duckdb.org/docs/stable/clients/c/data_chunk.md).

The vector and data chunk interfaces are the most efficient way of interacting with DuckDB, allowing for the highest performance. However, the interfaces are also difficult to use and care must be taken when using them.

## Vector Format

Vectors are arrays of a specific data type. The logical type of a vector can be obtained using `duckdb_vector_get_column_type`. The type id of the logical type can then be obtained using `duckdb_get_type_id`.

Vectors themselves do not have sizes. Instead, the parent data chunk has a size (that can be obtained through `duckdb_data_chunk_get_size`). All vectors that belong to a data chunk have the same size.

### Primitive Types

For primitive types, the underlying array can be obtained using the `duckdb_vector_get_data` method. The array can then be accessed using the correct native type. Below is a table that contains a mapping of the `duckdb_type` to the native type of the array.

|       duckdb_type        |    NativeType    |
|--------------------------|------------------|
| DUCKDB_TYPE_BOOLEAN      | bool             |
| DUCKDB_TYPE_TINYINT      | int8_t           |
| DUCKDB_TYPE_SMALLINT     | int16_t          |
| DUCKDB_TYPE_INTEGER      | int32_t          |
| DUCKDB_TYPE_BIGINT       | int64_t          |
| DUCKDB_TYPE_UTINYINT     | uint8_t          |
| DUCKDB_TYPE_USMALLINT    | uint16_t         |
| DUCKDB_TYPE_UINTEGER     | uint32_t         |
| DUCKDB_TYPE_UBIGINT      | uint64_t         |
| DUCKDB_TYPE_FLOAT        | float            |
| DUCKDB_TYPE_DOUBLE       | double           |
| DUCKDB_TYPE_TIMESTAMP    | duckdb_timestamp |
| DUCKDB_TYPE_DATE         | duckdb_date      |
| DUCKDB_TYPE_TIME         | duckdb_time      |
| DUCKDB_TYPE_INTERVAL     | duckdb_interval  |
| DUCKDB_TYPE_HUGEINT      | duckdb_hugeint   |
| DUCKDB_TYPE_UHUGEINT     | duckdb_uhugeint  |
| DUCKDB_TYPE_VARCHAR      | duckdb_string_t  |
| DUCKDB_TYPE_BLOB         | duckdb_string_t  |
| DUCKDB_TYPE_TIMESTAMP_S  | duckdb_timestamp |
| DUCKDB_TYPE_TIMESTAMP_MS | duckdb_timestamp |
| DUCKDB_TYPE_TIMESTAMP_NS | duckdb_timestamp |
| DUCKDB_TYPE_UUID         | duckdb_hugeint   |
| DUCKDB_TYPE_TIME_TZ      | duckdb_time_tz   |
| DUCKDB_TYPE_TIMESTAMP_TZ | duckdb_timestamp |

### `NULL` Values

Any value in a vector can be `NULL`. When a value is `NULL`, the values contained within the primary array at that index is undefined (and can be uninitialized). The validity mask is a bitmask consisting of `uint64_t` elements. For every `64` values in the vector, one `uint64_t` element exists (rounded up). The validity mask has its bit set to 1 if the value is valid, or set to 0 if the value is invalid (i.e .`NULL`).

The bits of the bitmask can be read directly, or the slower helper method `duckdb_validity_row_is_valid` can be used to check whether or not a value is `NULL`.

The `duckdb_vector_get_validity` returns a pointer to the validity mask. Note that if all values in a vector are valid, this function **might** return `nullptr` in which case the validity mask does not need to be checked.

### Strings

String values are stored as a `duckdb_string_t`. This is a special struct that stores the string inline (if it is short, i.e., `<= 12 bytes`) or a pointer to the string data if it is longer than `12` bytes.

```c
typedef struct {
	union {
		struct {
			uint32_t length;
			char prefix[4];
			char *ptr;
		} pointer;
		struct {
			uint32_t length;
			char inlined[12];
		} inlined;
	} value;
} duckdb_string_t;
```

The length can either be accessed directly, or the `duckdb_string_is_inlined` can be used to check if a string is inlined.

### Decimals

Decimals are stored as integer values internally. The exact native type depends on the `width` of the decimal type, as shown in the following table:

| Width |   NativeType   |
|-------|----------------|
| <= 4  | int16_t        |
| <= 9  | int32_t        |
| <= 18 | int64_t        |
| <= 38 | duckdb_hugeint |

The `duckdb_decimal_internal_type` can be used to obtain the internal type of the decimal.

Decimals are stored as integer values multiplied by `10^scale`. The scale of a decimal can be obtained using `duckdb_decimal_scale`. For example, a decimal value of `10.5` with type `DECIMAL(8, 3)` is stored internally as an `int32_t` value of `10500`. In order to obtain the correct decimal value, the value should be divided by the appropriate power-of-ten.

### Enums

Enums are stored as unsigned integer values internally. The exact native type depends on the size of the enum dictionary, as shown in the following table:

| Dictionary size | NativeType |
|-----------------|------------|
| <= 255          | uint8_t    |
| <= 65535        | uint16_t   |
| <= 4294967295   | uint32_t   |

The `duckdb_enum_internal_type` can be used to obtain the internal type of the enum.

In order to obtain the actual string value of the enum, the `duckdb_enum_dictionary_value` function must be used to obtain the enum value that corresponds to the given dictionary entry. Note that the enum dictionary is the same for the entire column – and so only needs to be constructed once.

### Structs

Structs are nested types that contain any number of child types. Think of them like a `struct` in C. The way to access struct data using vectors is to access the child vectors recursively using the `duckdb_struct_vector_get_child` method.

The struct vector itself does not have any data (i.e., you should not use `duckdb_vector_get_data` method on the struct). **However**, the struct vector itself **does** have a validity mask. The reason for this is that the child elements of a struct can be `NULL`, but the struct **itself** can also be `NULL`.

### Lists

Lists are nested types that contain a single child type, repeated `x` times per row. Think of them like a variable-length array in C. The way to access list data using vectors is to access the child vector using the `duckdb_list_vector_get_child` method.

The `duckdb_vector_get_data` must be used to get the offsets and lengths of the lists stored as `duckdb_list_entry`, that can then be applied to the child vector.

```c
typedef struct {
	uint64_t offset;
	uint64_t length;
} duckdb_list_entry;
```

Note that both list entries itself **and** any children stored in the lists can also be `NULL`. This must be checked using the validity mask again.

### Arrays

Arrays are nested types that contain a single child type, repeated exactly `array_size` times per row. Think of them like a fixed-size array in C. Arrays work exactly the same as lists, **except** the length and offset of each entry is fixed. The fixed array size can be obtained by using `duckdb_array_type_array_size`. The data for entry `n` then resides at `offset = n * array_size`, and always has `length = array_size`.

Note that much like lists, arrays can still be `NULL`, which must be checked using the validity mask.

## Examples

Below are several full end-to-end examples of how to interact with vectors.

### Example: Reading an int64 Vector with `NULL` Values

```c
duckdb_database db;
duckdb_connection con;
duckdb_open(nullptr, &db);
duckdb_connect(db, &con);

duckdb_result res;
duckdb_query(con, "SELECT CASE WHEN i%2=0 THEN NULL ELSE i END res_col FROM range(10) t(i)", &res);

// iterate until result is exhausted
while (true) {
	duckdb_data_chunk result = duckdb_fetch_chunk(res);
	if (!result) {
		// result is exhausted
		break;
	}
	// get the number of rows from the data chunk
	idx_t row_count = duckdb_data_chunk_get_size(result);
	// get the first column
	duckdb_vector res_col = duckdb_data_chunk_get_vector(result, 0);
	// get the native array and the validity mask of the vector
	int64_t *vector_data = (int64_t *) duckdb_vector_get_data(res_col);
	uint64_t *vector_validity = duckdb_vector_get_validity(res_col);
	// iterate over the rows
	for (idx_t row = 0; row < row_count; row++) {
		if (duckdb_validity_row_is_valid(vector_validity, row)) {
			printf("%lld\n", vector_data[row]);
		} else {
			printf("NULL\n");
		}
	}
	duckdb_destroy_data_chunk(&result);
}
// clean-up
duckdb_destroy_result(&res);
duckdb_disconnect(&con);
duckdb_close(&db);
```

### Example: Reading a String Vector

```c
duckdb_database db;
duckdb_connection con;
duckdb_open(nullptr, &db);
duckdb_connect(db, &con);

duckdb_result res;
duckdb_query(con, "SELECT CASE WHEN i%2=0 THEN CONCAT('short_', i) ELSE CONCAT('longstringprefix', i) END FROM range(10) t(i)", &res);

// iterate until result is exhausted
while (true) {
	duckdb_data_chunk result = duckdb_fetch_chunk(res);
	if (!result) {
		// result is exhausted
		break;
	}
	// get the number of rows from the data chunk
	idx_t row_count = duckdb_data_chunk_get_size(result);
	// get the first column
	duckdb_vector res_col = duckdb_data_chunk_get_vector(result, 0);
	// get the native array and the validity mask of the vector
	duckdb_string_t *vector_data = (duckdb_string_t *) duckdb_vector_get_data(res_col);
	uint64_t *vector_validity = duckdb_vector_get_validity(res_col);
	// iterate over the rows
	for (idx_t row = 0; row < row_count; row++) {
		if (duckdb_validity_row_is_valid(vector_validity, row)) {
			duckdb_string_t str = vector_data[row];
			if (duckdb_string_is_inlined(str)) {
				// use inlined string
				printf("%.*s\n", str.value.inlined.length, str.value.inlined.inlined);
			} else {
				// follow string pointer
				printf("%.*s\n", str.value.pointer.length, str.value.pointer.ptr);
			}
		} else {
			printf("NULL\n");
		}
	}
	duckdb_destroy_data_chunk(&result);
}
// clean-up
duckdb_destroy_result(&res);
duckdb_disconnect(&con);
duckdb_close(&db);
```

### Example: Reading a Struct Vector

```c
duckdb_database db;
duckdb_connection con;
duckdb_open(nullptr, &db);
duckdb_connect(db, &con);

duckdb_result res;
duckdb_query(con, "SELECT CASE WHEN i%5=0 THEN NULL ELSE {'col1': i, 'col2': CASE WHEN i%2=0 THEN NULL ELSE 100 + i * 42 END} END FROM range(10) t(i)", &res);

// iterate until result is exhausted
while (true) {
	duckdb_data_chunk result = duckdb_fetch_chunk(res);
	if (!result) {
		// result is exhausted
		break;
	}
	// get the number of rows from the data chunk
	idx_t row_count = duckdb_data_chunk_get_size(result);
	// get the struct column
	duckdb_vector struct_col = duckdb_data_chunk_get_vector(result, 0);
	uint64_t *struct_validity = duckdb_vector_get_validity(struct_col);
	// get the child columns of the struct
	duckdb_vector col1_vector = duckdb_struct_vector_get_child(struct_col, 0);
	int64_t *col1_data = (int64_t *) duckdb_vector_get_data(col1_vector);
	uint64_t *col1_validity = duckdb_vector_get_validity(col1_vector);

	duckdb_vector col2_vector = duckdb_struct_vector_get_child(struct_col, 1);
	int64_t *col2_data = (int64_t *) duckdb_vector_get_data(col2_vector);
	uint64_t *col2_validity = duckdb_vector_get_validity(col2_vector);

	// iterate over the rows
	for (idx_t row = 0; row < row_count; row++) {
		if (!duckdb_validity_row_is_valid(struct_validity, row)) {
			// entire struct is NULL
			printf("NULL\n");
			continue;
		}
		// read col1
		printf("{'col1': ");
		if (!duckdb_validity_row_is_valid(col1_validity, row)) {
			// col1 is NULL
			printf("NULL");
		} else {
			printf("%lld", col1_data[row]);
		}
		printf(", 'col2': ");
		if (!duckdb_validity_row_is_valid(col2_validity, row)) {
			// col2 is NULL
			printf("NULL");
		} else {
			printf("%lld", col2_data[row]);
		}
		printf("}\n");
	}
	duckdb_destroy_data_chunk(&result);
}
// clean-up
duckdb_destroy_result(&res);
duckdb_disconnect(&con);
duckdb_close(&db);
```

### Example: Reading a List Vector

```c
duckdb_database db;
duckdb_connection con;
duckdb_open(nullptr, &db);
duckdb_connect(db, &con);

duckdb_result res;
duckdb_query(con, "SELECT CASE WHEN i % 5 = 0 THEN NULL WHEN i % 2 = 0 THEN [i, i + 1] ELSE [i * 42, NULL, i * 84] END FROM range(10) t(i)", &res);

// iterate until result is exhausted
while (true) {
	duckdb_data_chunk result = duckdb_fetch_chunk(res);
	if (!result) {
		// result is exhausted
		break;
	}
	// get the number of rows from the data chunk
	idx_t row_count = duckdb_data_chunk_get_size(result);
	// get the list column
	duckdb_vector list_col = duckdb_data_chunk_get_vector(result, 0);
	duckdb_list_entry *list_data = (duckdb_list_entry *) duckdb_vector_get_data(list_col);
	uint64_t *list_validity = duckdb_vector_get_validity(list_col);
	// get the child column of the list
	duckdb_vector list_child = duckdb_list_vector_get_child(list_col);
	int64_t *child_data = (int64_t *) duckdb_vector_get_data(list_child);
	uint64_t *child_validity = duckdb_vector_get_validity(list_child);

	// iterate over the rows
	for (idx_t row = 0; row < row_count; row++) {
		if (!duckdb_validity_row_is_valid(list_validity, row)) {
			// entire list is NULL
			printf("NULL\n");
			continue;
		}
		// read the list offsets for this row
		duckdb_list_entry list = list_data[row];
		printf("[");
		for (idx_t child_idx = list.offset; child_idx < list.offset + list.length; child_idx++) {
			if (child_idx > list.offset) {
				printf(", ");
			}
			if (!duckdb_validity_row_is_valid(child_validity, child_idx)) {
				// col1 is NULL
				printf("NULL");
			} else {
				printf("%lld", child_data[child_idx]);
			}
		}
		printf("]\n");
	}
	duckdb_destroy_data_chunk(&result);
}
// clean-up
duckdb_destroy_result(&res);
duckdb_disconnect(&con);
duckdb_close(&db);
```

## API Reference Overview

duckdb_vector duckdb_create_vector(duckdb_logical_type type, idx_t capacity);
void duckdb_destroy_vector(duckdb_vector *vector);
duckdb_logical_type duckdb_vector_get_column_type(duckdb_vector vector);
void *duckdb_vector_get_data(duckdb_vector vector);
uint64_t *duckdb_vector_get_validity(duckdb_vector vector);
void duckdb_vector_ensure_validity_writable(duckdb_vector vector);
void duckdb_vector_assign_string_element(duckdb_vector vector, idx_t index, const char *str);
void duckdb_vector_assign_string_element_len(duckdb_vector vector, idx_t index, const char *str, idx_t str_len);
duckdb_vector duckdb_list_vector_get_child(duckdb_vector vector);
idx_t duckdb_list_vector_get_size(duckdb_vector vector);
duckdb_state duckdb_list_vector_set_size(duckdb_vector vector, idx_t size);
duckdb_state duckdb_list_vector_reserve(duckdb_vector vector, idx_t required_capacity);
duckdb_vector duckdb_struct_vector_get_child(duckdb_vector vector, idx_t index);
duckdb_vector duckdb_array_vector_get_child(duckdb_vector vector);
void duckdb_slice_vector(duckdb_vector vector, duckdb_selection_vector sel, idx_t len);
void duckdb_vector_copy_sel(duckdb_vector src, duckdb_vector dst, duckdb_selection_vector sel, idx_t src_count, idx_t src_offset, idx_t dst_offset);
void duckdb_vector_reference_value(duckdb_vector vector, duckdb_value value);
void duckdb_vector_reference_vector(duckdb_vector to_vector, duckdb_vector from_vector);

### Validity Mask Functions

bool duckdb_validity_row_is_valid(uint64_t *validity, idx_t row);
void duckdb_validity_set_row_validity(uint64_t *validity, idx_t row, bool valid);
void duckdb_validity_set_row_invalid(uint64_t *validity, idx_t row);
void duckdb_validity_set_row_valid(uint64_t *validity, idx_t row);

#### `duckdb_create_vector`

Creates a flat vector. Must be destroyed with `duckdb_destroy_vector`.

##### Syntax

duckdb_vector duckdb_create_vector(
  duckdb_logical_type type,
  idx_t capacity
);

##### Parameters

* `type`: The logical type of the vector.
* `capacity`: The capacity of the vector.

##### Return Value

The vector.

#### `duckdb_destroy_vector`

Destroys the vector and de-allocates its memory.

##### Syntax

void duckdb_destroy_vector(
  duckdb_vector *vector
);

##### Parameters

* `vector`: A pointer to the vector.

#### `duckdb_vector_get_column_type`

Retrieves the column type of the specified vector.

The result must be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_vector_get_column_type(
  duckdb_vector vector
);

##### Parameters

* `vector`: The vector get the data from

##### Return Value

The type of the vector

#### `duckdb_vector_get_data`

Retrieves the data pointer of the vector.

The data pointer can be used to read or write values from the vector.
How to read or write values depends on the type of the vector.

##### Syntax

void *duckdb_vector_get_data(
  duckdb_vector vector
);

##### Parameters

* `vector`: The vector to get the data from

##### Return Value

The data pointer

#### `duckdb_vector_get_validity`

Retrieves the validity mask pointer of the specified vector.

If all values are valid, this function MIGHT return NULL!

The validity mask is a bitset that signifies null-ness within the data chunk.
It is a series of uint64_t values, where each uint64_t value contains validity for 64 tuples.
The bit is set to 1 if the value is valid (i.e., not NULL) or 0 if the value is invalid (i.e., NULL).

Validity of a specific value can be obtained like this:

idx_t entry_idx = row_idx / 64;
idx_t idx_in_entry = row_idx % 64;
bool is_valid = validity_mask[entry_idx] & (1 << idx_in_entry);

Alternatively, the (slower) duckdb_validity_row_is_valid function can be used.

##### Syntax

uint64_t *duckdb_vector_get_validity(
  duckdb_vector vector
);

##### Parameters

* `vector`: The vector to get the data from

##### Return Value

The pointer to the validity mask, or NULL if no validity mask is present

#### `duckdb_vector_ensure_validity_writable`

Ensures the validity mask is writable by allocating it.

After this function is called, `duckdb_vector_get_validity` will ALWAYS return non-NULL.
This allows NULL values to be written to the vector, regardless of whether a validity mask was present before.

##### Syntax

void duckdb_vector_ensure_validity_writable(
  duckdb_vector vector
);

##### Parameters

* `vector`: The vector to alter

#### `duckdb_vector_assign_string_element`

Assigns a string element in the vector at the specified location.

##### Syntax

void duckdb_vector_assign_string_element(
  duckdb_vector vector,
  idx_t index,
  const char *str
);

##### Parameters

* `vector`: The vector to alter
* `index`: The row position in the vector to assign the string to
* `str`: The null-terminated string

#### `duckdb_vector_assign_string_element_len`

Assigns a string element in the vector at the specified location. You may also use this function to assign BLOBs.

##### Syntax

void duckdb_vector_assign_string_element_len(
  duckdb_vector vector,
  idx_t index,
  const char *str,
  idx_t str_len
);

##### Parameters

* `vector`: The vector to alter
* `index`: The row position in the vector to assign the string to
* `str`: The string
* `str_len`: The length of the string (in bytes)

#### `duckdb_list_vector_get_child`

Retrieves the child vector of a list vector.

The resulting vector is valid as long as the parent vector is valid.

##### Syntax

duckdb_vector duckdb_list_vector_get_child(
  duckdb_vector vector
);

##### Parameters

* `vector`: The vector

##### Return Value

The child vector

#### `duckdb_list_vector_get_size`

Returns the size of the child vector of the list.

##### Syntax

idx_t duckdb_list_vector_get_size(
  duckdb_vector vector
);

##### Parameters

* `vector`: The vector

##### Return Value

The size of the child list

#### `duckdb_list_vector_set_size`

Sets the total size of the underlying child-vector of a list vector.

##### Syntax

duckdb_state duckdb_list_vector_set_size(
  duckdb_vector vector,
  idx_t size
);

##### Parameters

* `vector`: The list vector.
* `size`: The size of the child list.

##### Return Value

The duckdb state. Returns DuckDBError if the vector is nullptr.

#### `duckdb_list_vector_reserve`

Sets the total capacity of the underlying child-vector of a list.

After calling this method, you must call `duckdb_vector_get_validity` and `duckdb_vector_get_data` to obtain current
data and validity pointers

##### Syntax

duckdb_state duckdb_list_vector_reserve(
  duckdb_vector vector,
  idx_t required_capacity
);

##### Parameters

* `vector`: The list vector.
* `required_capacity`: the total capacity to reserve.

##### Return Value

The duckdb state. Returns DuckDBError if the vector is nullptr.

#### `duckdb_struct_vector_get_child`

Retrieves the child vector of a struct vector.
The resulting vector is valid as long as the parent vector is valid.

##### Syntax

duckdb_vector duckdb_struct_vector_get_child(
  duckdb_vector vector,
  idx_t index
);

##### Parameters

* `vector`: The vector
* `index`: The child index

##### Return Value

The child vector

#### `duckdb_array_vector_get_child`

Retrieves the child vector of an array vector.
The resulting vector is valid as long as the parent vector is valid.
The resulting vector has the size of the parent vector multiplied by the array size.

##### Syntax

duckdb_vector duckdb_array_vector_get_child(
  duckdb_vector vector
);

##### Parameters

* `vector`: The vector

##### Return Value

The child vector

#### `duckdb_slice_vector`

Slice a vector with a selection vector.
The length of the selection vector must be less than or equal to the length of the vector.
Turns the vector into a dictionary vector.

##### Syntax

void duckdb_slice_vector(
  duckdb_vector vector,
  duckdb_selection_vector sel,
  idx_t len
);

##### Parameters

* `vector`: The vector to slice.
* `sel`: The selection vector.
* `len`: The length of the selection vector.

#### `duckdb_vector_copy_sel`

Copy the src vector to the dst with a selection vector that identifies which indices to copy.

##### Syntax

void duckdb_vector_copy_sel(
  duckdb_vector src,
  duckdb_vector dst,
  duckdb_selection_vector sel,
  idx_t src_count,
  idx_t src_offset,
  idx_t dst_offset
);

##### Parameters

* `src`: The vector to copy from.
* `dst`: The vector to copy to.
* `sel`: The selection vector. The length of the selection vector should not be more than the length of the src
vector
* `src_count`: The number of entries from selection vector to copy. Think of this as the effective length of the
selection vector starting from index 0
* `src_offset`: The offset in the selection vector to copy from (important: actual number of items copied =
src_count - src_offset).
* `dst_offset`: The offset in the dst vector to start copying to.

#### `duckdb_vector_reference_value`

Copies the value from `value` to `vector`.

##### Syntax

void duckdb_vector_reference_value(
  duckdb_vector vector,
  duckdb_value value
);

##### Parameters

* `vector`: The receiving vector.
* `value`: The value to copy into the vector.

#### `duckdb_vector_reference_vector`

Changes `to_vector` to reference `from_vector. After, the vectors share ownership of the data.

##### Syntax

void duckdb_vector_reference_vector(
  duckdb_vector to_vector,
  duckdb_vector from_vector
);

##### Parameters

* `to_vector`: The receiving vector.
* `from_vector`: The vector to reference.

#### `duckdb_validity_row_is_valid`

Returns whether or not a row is valid (i.e., not NULL) in the given validity mask.

##### Syntax

bool duckdb_validity_row_is_valid(
  uint64_t *validity,
  idx_t row
);

##### Parameters

* `validity`: The validity mask, as obtained through `duckdb_vector_get_validity`
* `row`: The row index

##### Return Value

true if the row is valid, false otherwise

#### `duckdb_validity_set_row_validity`

In a validity mask, sets a specific row to either valid or invalid.

Note that `duckdb_vector_ensure_validity_writable` should be called before calling `duckdb_vector_get_validity`,
to ensure that there is a validity mask to write to.

##### Syntax

void duckdb_validity_set_row_validity(
  uint64_t *validity,
  idx_t row,
  bool valid
);

##### Parameters

* `validity`: The validity mask, as obtained through `duckdb_vector_get_validity`.
* `row`: The row index
* `valid`: Whether or not to set the row to valid, or invalid

#### `duckdb_validity_set_row_invalid`

In a validity mask, sets a specific row to invalid.

Equivalent to `duckdb_validity_set_row_validity` with valid set to false.

##### Syntax

void duckdb_validity_set_row_invalid(
  uint64_t *validity,
  idx_t row
);

##### Parameters

* `validity`: The validity mask
* `row`: The row index

#### `duckdb_validity_set_row_valid`

In a validity mask, sets a specific row to valid.

Equivalent to `duckdb_validity_set_row_validity` with valid set to true.

##### Syntax

void duckdb_validity_set_row_valid(
  uint64_t *validity,
  idx_t row
);

##### Parameters

* `validity`: The validity mask
* `row`: The row index


---

# docs/stable/clients/c/value

_Source: https://duckdb.org/docs/stable/clients/c/value.md_

---
layout: docu
redirect_from:
- /docs/api/c/value
- /docs/api/c/value/
- /docs/clients/c/value
title: Values
---

The value class represents a single value of any type.

## API Reference Overview

void duckdb_destroy_value(duckdb_value *value);
duckdb_value duckdb_create_varchar(const char *text);
duckdb_value duckdb_create_varchar_length(const char *text, idx_t length);
duckdb_value duckdb_create_bool(bool input);
duckdb_value duckdb_create_int8(int8_t input);
duckdb_value duckdb_create_uint8(uint8_t input);
duckdb_value duckdb_create_int16(int16_t input);
duckdb_value duckdb_create_uint16(uint16_t input);
duckdb_value duckdb_create_int32(int32_t input);
duckdb_value duckdb_create_uint32(uint32_t input);
duckdb_value duckdb_create_uint64(uint64_t input);
duckdb_value duckdb_create_int64(int64_t val);
duckdb_value duckdb_create_hugeint(duckdb_hugeint input);
duckdb_value duckdb_create_uhugeint(duckdb_uhugeint input);
duckdb_value duckdb_create_bignum(duckdb_bignum input);
duckdb_value duckdb_create_decimal(duckdb_decimal input);
duckdb_value duckdb_create_float(float input);
duckdb_value duckdb_create_double(double input);
duckdb_value duckdb_create_date(duckdb_date input);
duckdb_value duckdb_create_time(duckdb_time input);
duckdb_value duckdb_create_time_ns(duckdb_time_ns input);
duckdb_value duckdb_create_time_tz_value(duckdb_time_tz value);
duckdb_value duckdb_create_timestamp(duckdb_timestamp input);
duckdb_value duckdb_create_timestamp_tz(duckdb_timestamp input);
duckdb_value duckdb_create_timestamp_s(duckdb_timestamp_s input);
duckdb_value duckdb_create_timestamp_ms(duckdb_timestamp_ms input);
duckdb_value duckdb_create_timestamp_ns(duckdb_timestamp_ns input);
duckdb_value duckdb_create_interval(duckdb_interval input);
duckdb_value duckdb_create_blob(const uint8_t *data, idx_t length);
duckdb_value duckdb_create_bit(duckdb_bit input);
duckdb_value duckdb_create_uuid(duckdb_uhugeint input);
bool duckdb_get_bool(duckdb_value val);
int8_t duckdb_get_int8(duckdb_value val);
uint8_t duckdb_get_uint8(duckdb_value val);
int16_t duckdb_get_int16(duckdb_value val);
uint16_t duckdb_get_uint16(duckdb_value val);
int32_t duckdb_get_int32(duckdb_value val);
uint32_t duckdb_get_uint32(duckdb_value val);
int64_t duckdb_get_int64(duckdb_value val);
uint64_t duckdb_get_uint64(duckdb_value val);
duckdb_hugeint duckdb_get_hugeint(duckdb_value val);
duckdb_uhugeint duckdb_get_uhugeint(duckdb_value val);
duckdb_bignum duckdb_get_bignum(duckdb_value val);
duckdb_decimal duckdb_get_decimal(duckdb_value val);
float duckdb_get_float(duckdb_value val);
double duckdb_get_double(duckdb_value val);
duckdb_date duckdb_get_date(duckdb_value val);
duckdb_time duckdb_get_time(duckdb_value val);
duckdb_time_ns duckdb_get_time_ns(duckdb_value val);
duckdb_time_tz duckdb_get_time_tz(duckdb_value val);
duckdb_timestamp duckdb_get_timestamp(duckdb_value val);
duckdb_timestamp duckdb_get_timestamp_tz(duckdb_value val);
duckdb_timestamp_s duckdb_get_timestamp_s(duckdb_value val);
duckdb_timestamp_ms duckdb_get_timestamp_ms(duckdb_value val);
duckdb_timestamp_ns duckdb_get_timestamp_ns(duckdb_value val);
duckdb_interval duckdb_get_interval(duckdb_value val);
duckdb_logical_type duckdb_get_value_type(duckdb_value val);
duckdb_blob duckdb_get_blob(duckdb_value val);
duckdb_bit duckdb_get_bit(duckdb_value val);
duckdb_uhugeint duckdb_get_uuid(duckdb_value val);
char *duckdb_get_varchar(duckdb_value value);
duckdb_value duckdb_create_struct_value(duckdb_logical_type type, duckdb_value *values);
duckdb_value duckdb_create_list_value(duckdb_logical_type type, duckdb_value *values, idx_t value_count);
duckdb_value duckdb_create_array_value(duckdb_logical_type type, duckdb_value *values, idx_t value_count);
duckdb_value duckdb_create_map_value(duckdb_logical_type map_type, duckdb_value *keys, duckdb_value *values, idx_t entry_count);
duckdb_value duckdb_create_union_value(duckdb_logical_type union_type, idx_t tag_index, duckdb_value value);
idx_t duckdb_get_map_size(duckdb_value value);
duckdb_value duckdb_get_map_key(duckdb_value value, idx_t index);
duckdb_value duckdb_get_map_value(duckdb_value value, idx_t index);
bool duckdb_is_null_value(duckdb_value value);
duckdb_value duckdb_create_null_value();
idx_t duckdb_get_list_size(duckdb_value value);
duckdb_value duckdb_get_list_child(duckdb_value value, idx_t index);
duckdb_value duckdb_create_enum_value(duckdb_logical_type type, uint64_t value);
uint64_t duckdb_get_enum_value(duckdb_value value);
duckdb_value duckdb_get_struct_child(duckdb_value value, idx_t index);
char *duckdb_value_to_string(duckdb_value value);

#### `duckdb_destroy_value`

Destroys the value and de-allocates all memory allocated for that type.

##### Syntax

void duckdb_destroy_value(
  duckdb_value *value
);

##### Parameters

* `value`: The value to destroy.

#### `duckdb_create_varchar`

Creates a value from a null-terminated string

##### Syntax

duckdb_value duckdb_create_varchar(
  const char *text
);

##### Parameters

* `text`: The null-terminated string

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_varchar_length`

Creates a value from a string

##### Syntax

duckdb_value duckdb_create_varchar_length(
  const char *text,
  idx_t length
);

##### Parameters

* `text`: The text
* `length`: The length of the text

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_bool`

Creates a value from a boolean

##### Syntax

duckdb_value duckdb_create_bool(
  bool input
);

##### Parameters

* `input`: The boolean value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_int8`

Creates a value from an int8_t (a tinyint)

##### Syntax

duckdb_value duckdb_create_int8(
  int8_t input
);

##### Parameters

* `input`: The tinyint value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_uint8`

Creates a value from a uint8_t (a utinyint)

##### Syntax

duckdb_value duckdb_create_uint8(
  uint8_t input
);

##### Parameters

* `input`: The utinyint value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_int16`

Creates a value from an int16_t (a smallint)

##### Syntax

duckdb_value duckdb_create_int16(
  int16_t input
);

##### Parameters

* `input`: The smallint value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_uint16`

Creates a value from a uint16_t (a usmallint)

##### Syntax

duckdb_value duckdb_create_uint16(
  uint16_t input
);

##### Parameters

* `input`: The usmallint value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_int32`

Creates a value from an int32_t (an integer)

##### Syntax

duckdb_value duckdb_create_int32(
  int32_t input
);

##### Parameters

* `input`: The integer value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_uint32`

Creates a value from a uint32_t (a uinteger)

##### Syntax

duckdb_value duckdb_create_uint32(
  uint32_t input
);

##### Parameters

* `input`: The uinteger value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_uint64`

Creates a value from a uint64_t (a ubigint)

##### Syntax

duckdb_value duckdb_create_uint64(
  uint64_t input
);

##### Parameters

* `input`: The ubigint value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_int64`

Creates a value from an int64

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_int64(
  int64_t val
);

#### `duckdb_create_hugeint`

Creates a value from a hugeint

##### Syntax

duckdb_value duckdb_create_hugeint(
  duckdb_hugeint input
);

##### Parameters

* `input`: The hugeint value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_uhugeint`

Creates a value from a uhugeint

##### Syntax

duckdb_value duckdb_create_uhugeint(
  duckdb_uhugeint input
);

##### Parameters

* `input`: The uhugeint value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_bignum`

Creates a BIGNUM value from a duckdb_bignum

##### Syntax

duckdb_value duckdb_create_bignum(
  duckdb_bignum input
);

##### Parameters

* `input`: The duckdb_bignum value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_decimal`

Creates a DECIMAL value from a duckdb_decimal

##### Syntax

duckdb_value duckdb_create_decimal(
  duckdb_decimal input
);

##### Parameters

* `input`: The duckdb_decimal value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_float`

Creates a value from a float

##### Syntax

duckdb_value duckdb_create_float(
  float input
);

##### Parameters

* `input`: The float value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_double`

Creates a value from a double

##### Syntax

duckdb_value duckdb_create_double(
  double input
);

##### Parameters

* `input`: The double value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_date`

Creates a value from a date

##### Syntax

duckdb_value duckdb_create_date(
  duckdb_date input
);

##### Parameters

* `input`: The date value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_time`

Creates a value from a time

##### Syntax

duckdb_value duckdb_create_time(
  duckdb_time input
);

##### Parameters

* `input`: The time value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_time_ns`

Creates a value from a time_ns

##### Syntax

duckdb_value duckdb_create_time_ns(
  duckdb_time_ns input
);

##### Parameters

* `input`: The time value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_time_tz_value`

Creates a value from a time_tz.
Not to be confused with `duckdb_create_time_tz`, which creates a duckdb_time_tz_t.

##### Syntax

duckdb_value duckdb_create_time_tz_value(
  duckdb_time_tz value
);

##### Parameters

* `value`: The time_tz value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_timestamp`

Creates a TIMESTAMP value from a duckdb_timestamp

##### Syntax

duckdb_value duckdb_create_timestamp(
  duckdb_timestamp input
);

##### Parameters

* `input`: The duckdb_timestamp value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_timestamp_tz`

Creates a TIMESTAMP_TZ value from a duckdb_timestamp

##### Syntax

duckdb_value duckdb_create_timestamp_tz(
  duckdb_timestamp input
);

##### Parameters

* `input`: The duckdb_timestamp value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_timestamp_s`

Creates a TIMESTAMP_S value from a duckdb_timestamp_s

##### Syntax

duckdb_value duckdb_create_timestamp_s(
  duckdb_timestamp_s input
);

##### Parameters

* `input`: The duckdb_timestamp_s value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_timestamp_ms`

Creates a TIMESTAMP_MS value from a duckdb_timestamp_ms

##### Syntax

duckdb_value duckdb_create_timestamp_ms(
  duckdb_timestamp_ms input
);

##### Parameters

* `input`: The duckdb_timestamp_ms value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_timestamp_ns`

Creates a TIMESTAMP_NS value from a duckdb_timestamp_ns

##### Syntax

duckdb_value duckdb_create_timestamp_ns(
  duckdb_timestamp_ns input
);

##### Parameters

* `input`: The duckdb_timestamp_ns value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_interval`

Creates a value from an interval

##### Syntax

duckdb_value duckdb_create_interval(
  duckdb_interval input
);

##### Parameters

* `input`: The interval value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_blob`

Creates a value from a blob

##### Syntax

duckdb_value duckdb_create_blob(
  const uint8_t *data,
  idx_t length
);

##### Parameters

* `data`: The blob data
* `length`: The length of the blob data

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_bit`

Creates a BIT value from a duckdb_bit

##### Syntax

duckdb_value duckdb_create_bit(
  duckdb_bit input
);

##### Parameters

* `input`: The duckdb_bit value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_uuid`

Creates a UUID value from a uhugeint

##### Syntax

duckdb_value duckdb_create_uuid(
  duckdb_uhugeint input
);

##### Parameters

* `input`: The duckdb_uhugeint containing the UUID

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_get_bool`

Returns the boolean value of the given value.

##### Syntax

bool duckdb_get_bool(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a boolean

##### Return Value

A boolean, or false if the value cannot be converted

#### `duckdb_get_int8`

Returns the int8_t value of the given value.

##### Syntax

int8_t duckdb_get_int8(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a tinyint

##### Return Value

A int8_t, or MinValue if the value cannot be converted

#### `duckdb_get_uint8`

Returns the uint8_t value of the given value.

##### Syntax

uint8_t duckdb_get_uint8(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a utinyint

##### Return Value

A uint8_t, or MinValue if the value cannot be converted

#### `duckdb_get_int16`

Returns the int16_t value of the given value.

##### Syntax

int16_t duckdb_get_int16(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a smallint

##### Return Value

A int16_t, or MinValue if the value cannot be converted

#### `duckdb_get_uint16`

Returns the uint16_t value of the given value.

##### Syntax

uint16_t duckdb_get_uint16(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a usmallint

##### Return Value

A uint16_t, or MinValue if the value cannot be converted

#### `duckdb_get_int32`

Returns the int32_t value of the given value.

##### Syntax

int32_t duckdb_get_int32(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing an integer

##### Return Value

A int32_t, or MinValue if the value cannot be converted

#### `duckdb_get_uint32`

Returns the uint32_t value of the given value.

##### Syntax

uint32_t duckdb_get_uint32(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a uinteger

##### Return Value

A uint32_t, or MinValue if the value cannot be converted

#### `duckdb_get_int64`

Returns the int64_t value of the given value.

##### Syntax

int64_t duckdb_get_int64(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a bigint

##### Return Value

A int64_t, or MinValue if the value cannot be converted

#### `duckdb_get_uint64`

Returns the uint64_t value of the given value.

##### Syntax

uint64_t duckdb_get_uint64(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a ubigint

##### Return Value

A uint64_t, or MinValue if the value cannot be converted

#### `duckdb_get_hugeint`

Returns the hugeint value of the given value.

##### Syntax

duckdb_hugeint duckdb_get_hugeint(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a hugeint

##### Return Value

A duckdb_hugeint, or MinValue if the value cannot be converted

#### `duckdb_get_uhugeint`

Returns the uhugeint value of the given value.

##### Syntax

duckdb_uhugeint duckdb_get_uhugeint(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a uhugeint

##### Return Value

A duckdb_uhugeint, or MinValue if the value cannot be converted

#### `duckdb_get_bignum`

Returns the duckdb_bignum value of the given value.
The `data` field must be destroyed with `duckdb_free`.

##### Syntax

duckdb_bignum duckdb_get_bignum(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a BIGNUM

##### Return Value

A duckdb_bignum. The `data` field must be destroyed with `duckdb_free`.

#### `duckdb_get_decimal`

Returns the duckdb_decimal value of the given value.

##### Syntax

duckdb_decimal duckdb_get_decimal(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a DECIMAL

##### Return Value

A duckdb_decimal, or MinValue if the value cannot be converted

#### `duckdb_get_float`

Returns the float value of the given value.

##### Syntax

float duckdb_get_float(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a float

##### Return Value

A float, or NAN if the value cannot be converted

#### `duckdb_get_double`

Returns the double value of the given value.

##### Syntax

double duckdb_get_double(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a double

##### Return Value

A double, or NAN if the value cannot be converted

#### `duckdb_get_date`

Returns the date value of the given value.

##### Syntax

duckdb_date duckdb_get_date(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a date

##### Return Value

A duckdb_date, or MinValue if the value cannot be converted

#### `duckdb_get_time`

Returns the time value of the given value.

##### Syntax

duckdb_time duckdb_get_time(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a time

##### Return Value

A duckdb_time, or MinValue if the value cannot be converted

#### `duckdb_get_time_ns`

Returns the time_ns value of the given value.

##### Syntax

duckdb_time_ns duckdb_get_time_ns(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a time_ns

##### Return Value

A duckdb_time_ns, or MinValue if the value cannot be converted

#### `duckdb_get_time_tz`

Returns the time_tz value of the given value.

##### Syntax

duckdb_time_tz duckdb_get_time_tz(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a time_tz

##### Return Value

A duckdb_time_tz, or MinValue if the value cannot be converted

#### `duckdb_get_timestamp`

Returns the TIMESTAMP value of the given value.

##### Syntax

duckdb_timestamp duckdb_get_timestamp(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a TIMESTAMP

##### Return Value

A duckdb_timestamp, or MinValue if the value cannot be converted

#### `duckdb_get_timestamp_tz`

Returns the TIMESTAMP_TZ value of the given value.

##### Syntax

duckdb_timestamp duckdb_get_timestamp_tz(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a TIMESTAMP_TZ

##### Return Value

A duckdb_timestamp, or MinValue if the value cannot be converted

#### `duckdb_get_timestamp_s`

Returns the duckdb_timestamp_s value of the given value.

##### Syntax

duckdb_timestamp_s duckdb_get_timestamp_s(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a TIMESTAMP_S

##### Return Value

A duckdb_timestamp_s, or MinValue if the value cannot be converted

#### `duckdb_get_timestamp_ms`

Returns the duckdb_timestamp_ms value of the given value.

##### Syntax

duckdb_timestamp_ms duckdb_get_timestamp_ms(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a TIMESTAMP_MS

##### Return Value

A duckdb_timestamp_ms, or MinValue if the value cannot be converted

#### `duckdb_get_timestamp_ns`

Returns the duckdb_timestamp_ns value of the given value.

##### Syntax

duckdb_timestamp_ns duckdb_get_timestamp_ns(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a TIMESTAMP_NS

##### Return Value

A duckdb_timestamp_ns, or MinValue if the value cannot be converted

#### `duckdb_get_interval`

Returns the interval value of the given value.

##### Syntax

duckdb_interval duckdb_get_interval(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a interval

##### Return Value

A duckdb_interval, or MinValue if the value cannot be converted

#### `duckdb_get_value_type`

Returns the type of the given value. The type is valid as long as the value is not destroyed.
The type itself must not be destroyed.

##### Syntax

duckdb_logical_type duckdb_get_value_type(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value

##### Return Value

A duckdb_logical_type.

#### `duckdb_get_blob`

Returns the blob value of the given value.

##### Syntax

duckdb_blob duckdb_get_blob(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a blob

##### Return Value

A duckdb_blob

#### `duckdb_get_bit`

Returns the duckdb_bit value of the given value.
The `data` field must be destroyed with `duckdb_free`.

##### Syntax

duckdb_bit duckdb_get_bit(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a BIT

##### Return Value

A duckdb_bit

#### `duckdb_get_uuid`

Returns a duckdb_uhugeint representing the UUID value of the given value.

##### Syntax

duckdb_uhugeint duckdb_get_uuid(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a UUID

##### Return Value

A duckdb_uhugeint representing the UUID value

#### `duckdb_get_varchar`

Obtains a string representation of the given value.
The result must be destroyed with `duckdb_free`.

##### Syntax

char *duckdb_get_varchar(
  duckdb_value value
);

##### Parameters

* `value`: The value

##### Return Value

The string value. This must be destroyed with `duckdb_free`.

#### `duckdb_create_struct_value`

Creates a struct value from a type and an array of values. Must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_struct_value(
  duckdb_logical_type type,
  duckdb_value *values
);

##### Parameters

* `type`: The type of the struct
* `values`: The values for the struct fields

##### Return Value

The struct value, or nullptr, if any child type is `DUCKDB_TYPE_ANY` or `DUCKDB_TYPE_INVALID`.

#### `duckdb_create_list_value`

Creates a list value from a child (element) type and an array of values of length `value_count`.
Must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_list_value(
  duckdb_logical_type type,
  duckdb_value *values,
  idx_t value_count
);

##### Parameters

* `type`: The type of the list
* `values`: The values for the list
* `value_count`: The number of values in the list

##### Return Value

The list value, or nullptr, if the child type is `DUCKDB_TYPE_ANY` or `DUCKDB_TYPE_INVALID`.

#### `duckdb_create_array_value`

Creates an array value from a child (element) type and an array of values of length `value_count`.
Must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_array_value(
  duckdb_logical_type type,
  duckdb_value *values,
  idx_t value_count
);

##### Parameters

* `type`: The type of the array
* `values`: The values for the array
* `value_count`: The number of values in the array

##### Return Value

The array value, or nullptr, if the child type is `DUCKDB_TYPE_ANY` or `DUCKDB_TYPE_INVALID`.

#### `duckdb_create_map_value`

Creates a map value from a map type and two arrays, one for the keys and one for the values, each of length
`entry_count`. Must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_map_value(
  duckdb_logical_type map_type,
  duckdb_value *keys,
  duckdb_value *values,
  idx_t entry_count
);

##### Parameters

* `map_type`: The map type
* `keys`: The keys of the map
* `values`: The values of the map
* `entry_count`: The number of entrys (key-value pairs) in the map

##### Return Value

The map value, or nullptr, if the parameters are invalid.

#### `duckdb_create_union_value`

Creates a union value from a union type, a tag index, and a value.
Must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_union_value(
  duckdb_logical_type union_type,
  idx_t tag_index,
  duckdb_value value
);

##### Parameters

* `union_type`: The union type
* `tag_index`: The index of the tag of the union
* `value`: The value of the union for that tag

##### Return Value

The union value, or nullptr, if the parameters are invalid.

#### `duckdb_get_map_size`

Returns the number of elements in a MAP value.

##### Syntax

idx_t duckdb_get_map_size(
  duckdb_value value
);

##### Parameters

* `value`: The MAP value.

##### Return Value

The number of elements in the map.

#### `duckdb_get_map_key`

Returns the MAP key at index as a duckdb_value.

##### Syntax

duckdb_value duckdb_get_map_key(
  duckdb_value value,
  idx_t index
);

##### Parameters

* `value`: The MAP value.
* `index`: The index of the key.

##### Return Value

The key as a duckdb_value.

#### `duckdb_get_map_value`

Returns the MAP value at index as a duckdb_value.

##### Syntax

duckdb_value duckdb_get_map_value(
  duckdb_value value,
  idx_t index
);

##### Parameters

* `value`: The MAP value.
* `index`: The index of the value.

##### Return Value

The value as a duckdb_value.

#### `duckdb_is_null_value`

Returns whether the value's type is SQLNULL or not.

##### Syntax

bool duckdb_is_null_value(
  duckdb_value value
);

##### Parameters

* `value`: The value to check.

##### Return Value

True, if the value's type is SQLNULL, otherwise false.

#### `duckdb_create_null_value`

Creates a value of type SQLNULL.

##### Return Value

The duckdb_value representing SQLNULL. This must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_null_value(

);

#### `duckdb_get_list_size`

Returns the number of elements in a LIST value.

##### Syntax

idx_t duckdb_get_list_size(
  duckdb_value value
);

##### Parameters

* `value`: The LIST value.

##### Return Value

The number of elements in the list.

#### `duckdb_get_list_child`

Returns the LIST child at index as a duckdb_value.

##### Syntax

duckdb_value duckdb_get_list_child(
  duckdb_value value,
  idx_t index
);

##### Parameters

* `value`: The LIST value.
* `index`: The index of the child.

##### Return Value

The child as a duckdb_value.

#### `duckdb_create_enum_value`

Creates an enum value from a type and a value. Must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_enum_value(
  duckdb_logical_type type,
  uint64_t value
);

##### Parameters

* `type`: The type of the enum
* `value`: The value for the enum

##### Return Value

The enum value, or nullptr.

#### `duckdb_get_enum_value`

Returns the enum value of the given value.

##### Syntax

uint64_t duckdb_get_enum_value(
  duckdb_value value
);

##### Parameters

* `value`: A duckdb_value containing an enum

##### Return Value

A uint64_t, or MinValue if the value cannot be converted

#### `duckdb_get_struct_child`

Returns the STRUCT child at index as a duckdb_value.

##### Syntax

duckdb_value duckdb_get_struct_child(
  duckdb_value value,
  idx_t index
);

##### Parameters

* `value`: The STRUCT value.
* `index`: The index of the child.

##### Return Value

The child as a duckdb_value.

#### `duckdb_value_to_string`

Returns the SQL string representation of the given value.

##### Syntax

char *duckdb_value_to_string(
  duckdb_value value
);

##### Parameters

* `value`: A duckdb_value.

##### Return Value

The SQL string representation as a null-terminated string. The result must be freed with `duckdb_free`.


---

# docs/stable/clients/c/types

_Source: https://duckdb.org/docs/stable/clients/c/types.md_

---
layout: docu
redirect_from:
- /docs/api/c/types
- /docs/api/c/types/
- /docs/clients/c/types
title: Types
---

DuckDB is a strongly typed database system. As such, every column has a single type specified. This type is constant
over the entire column. That is to say, a column that is labeled as an `INTEGER` column will only contain `INTEGER`
values.

DuckDB also supports columns of composite types. For example, it is possible to define an array of integers (`INTEGER[]`). It is also possible to define types as arbitrary structs (`ROW(i INTEGER, j VARCHAR)`). For that reason, native DuckDB type objects are not mere enums, but a class that can potentially be nested.

Types in the C API are modeled using an enum (`duckdb_type`) and a complex class (`duckdb_logical_type`). For most primitive types, e.g., integers or varchars, the enum is sufficient. For more complex types, such as lists, structs or decimals, the logical type must be used.

```c
typedef enum DUCKDB_TYPE {
  DUCKDB_TYPE_INVALID = 0,
  DUCKDB_TYPE_BOOLEAN = 1,
  DUCKDB_TYPE_TINYINT = 2,
  DUCKDB_TYPE_SMALLINT = 3,
  DUCKDB_TYPE_INTEGER = 4,
  DUCKDB_TYPE_BIGINT = 5,
  DUCKDB_TYPE_UTINYINT = 6,
  DUCKDB_TYPE_USMALLINT = 7,
  DUCKDB_TYPE_UINTEGER = 8,
  DUCKDB_TYPE_UBIGINT = 9,
  DUCKDB_TYPE_FLOAT = 10,
  DUCKDB_TYPE_DOUBLE = 11,
  DUCKDB_TYPE_TIMESTAMP = 12,
  DUCKDB_TYPE_DATE = 13,
  DUCKDB_TYPE_TIME = 14,
  DUCKDB_TYPE_INTERVAL = 15,
  DUCKDB_TYPE_HUGEINT = 16,
  DUCKDB_TYPE_UHUGEINT = 32,
  DUCKDB_TYPE_VARCHAR = 17,
  DUCKDB_TYPE_BLOB = 18,
  DUCKDB_TYPE_DECIMAL = 19,
  DUCKDB_TYPE_TIMESTAMP_S = 20,
  DUCKDB_TYPE_TIMESTAMP_MS = 21,
  DUCKDB_TYPE_TIMESTAMP_NS = 22,
  DUCKDB_TYPE_ENUM = 23,
  DUCKDB_TYPE_LIST = 24,
  DUCKDB_TYPE_STRUCT = 25,
  DUCKDB_TYPE_MAP = 26,
  DUCKDB_TYPE_ARRAY = 33,
  DUCKDB_TYPE_UUID = 27,
  DUCKDB_TYPE_UNION = 28,
  DUCKDB_TYPE_BIT = 29,
  DUCKDB_TYPE_TIME_TZ = 30,
  DUCKDB_TYPE_TIMESTAMP_TZ = 31,
} duckdb_type;
```

## Functions

The enum type of a column in the result can be obtained using the `duckdb_column_type` function. The logical type of a column can be obtained using the `duckdb_column_logical_type` function.

### `duckdb_value`

The `duckdb_value` functions will auto-cast values as required. For example, it is no problem to use
`duckdb_value_double` on a column of type `duckdb_value_int32`. The value will be auto-cast and returned as a double.
Note that in certain cases the cast may fail. For example, this can happen if we request a `duckdb_value_int8` and the value does not fit within an `int8` value. In this case, a default value will be returned (usually `0` or `nullptr`). The same default value will also be returned if the corresponding value is `NULL`.

The `duckdb_value_is_null` function can be used to check if a specific value is `NULL` or not.

The exception to the auto-cast rule is the `duckdb_value_varchar_internal` function. This function does not auto-cast and only works for `VARCHAR` columns. The reason this function exists is that the result does not need to be freed.

> `duckdb_value_varchar` and `duckdb_value_blob` require the result to be de-allocated using `duckdb_free`.

### `duckdb_fetch_chunk`

The `duckdb_fetch_chunk` function can be used to read data chunks from a DuckDB result set, and is the most efficient way of reading data from a DuckDB result using the C API. It is also the only way of reading data of certain types from a DuckDB result. For example, the `duckdb_value` functions do not support structural reading of composite types (lists or structs) or more complex types like enums and decimals.

For more information about data chunks, see the [documentation on data chunks](https://duckdb.org/docs/stable/clients/c/data_chunk.md).

## API Reference Overview

duckdb_data_chunk duckdb_result_get_chunk(duckdb_result result, idx_t chunk_index);
bool duckdb_result_is_streaming(duckdb_result result);
idx_t duckdb_result_chunk_count(duckdb_result result);
duckdb_result_type duckdb_result_return_type(duckdb_result result);

### Date Time Timestamp Helpers

duckdb_date_struct duckdb_from_date(duckdb_date date);
duckdb_date duckdb_to_date(duckdb_date_struct date);
bool duckdb_is_finite_date(duckdb_date date);
duckdb_time_struct duckdb_from_time(duckdb_time time);
duckdb_time_tz duckdb_create_time_tz(int64_t micros, int32_t offset);
duckdb_time_tz_struct duckdb_from_time_tz(duckdb_time_tz micros);
duckdb_time duckdb_to_time(duckdb_time_struct time);
duckdb_timestamp_struct duckdb_from_timestamp(duckdb_timestamp ts);
duckdb_timestamp duckdb_to_timestamp(duckdb_timestamp_struct ts);
bool duckdb_is_finite_timestamp(duckdb_timestamp ts);
bool duckdb_is_finite_timestamp_s(duckdb_timestamp_s ts);
bool duckdb_is_finite_timestamp_ms(duckdb_timestamp_ms ts);
bool duckdb_is_finite_timestamp_ns(duckdb_timestamp_ns ts);

### Hugeint Helpers

double duckdb_hugeint_to_double(duckdb_hugeint val);
duckdb_hugeint duckdb_double_to_hugeint(double val);

### Decimal Helpers

duckdb_decimal duckdb_double_to_decimal(double val, uint8_t width, uint8_t scale);
double duckdb_decimal_to_double(duckdb_decimal val);

### Logical Type Interface

duckdb_logical_type duckdb_create_logical_type(duckdb_type type);
char *duckdb_logical_type_get_alias(duckdb_logical_type type);
void duckdb_logical_type_set_alias(duckdb_logical_type type, const char *alias);
duckdb_logical_type duckdb_create_list_type(duckdb_logical_type type);
duckdb_logical_type duckdb_create_array_type(duckdb_logical_type type, idx_t array_size);
duckdb_logical_type duckdb_create_map_type(duckdb_logical_type key_type, duckdb_logical_type value_type);
duckdb_logical_type duckdb_create_union_type(duckdb_logical_type *member_types, const char **member_names, idx_t member_count);
duckdb_logical_type duckdb_create_struct_type(duckdb_logical_type *member_types, const char **member_names, idx_t member_count);
duckdb_logical_type duckdb_create_enum_type(const char **member_names, idx_t member_count);
duckdb_logical_type duckdb_create_decimal_type(uint8_t width, uint8_t scale);
duckdb_type duckdb_get_type_id(duckdb_logical_type type);
uint8_t duckdb_decimal_width(duckdb_logical_type type);
uint8_t duckdb_decimal_scale(duckdb_logical_type type);
duckdb_type duckdb_decimal_internal_type(duckdb_logical_type type);
duckdb_type duckdb_enum_internal_type(duckdb_logical_type type);
uint32_t duckdb_enum_dictionary_size(duckdb_logical_type type);
char *duckdb_enum_dictionary_value(duckdb_logical_type type, idx_t index);
duckdb_logical_type duckdb_list_type_child_type(duckdb_logical_type type);
duckdb_logical_type duckdb_array_type_child_type(duckdb_logical_type type);
idx_t duckdb_array_type_array_size(duckdb_logical_type type);
duckdb_logical_type duckdb_map_type_key_type(duckdb_logical_type type);
duckdb_logical_type duckdb_map_type_value_type(duckdb_logical_type type);
idx_t duckdb_struct_type_child_count(duckdb_logical_type type);
char *duckdb_struct_type_child_name(duckdb_logical_type type, idx_t index);
duckdb_logical_type duckdb_struct_type_child_type(duckdb_logical_type type, idx_t index);
idx_t duckdb_union_type_member_count(duckdb_logical_type type);
char *duckdb_union_type_member_name(duckdb_logical_type type, idx_t index);
duckdb_logical_type duckdb_union_type_member_type(duckdb_logical_type type, idx_t index);
void duckdb_destroy_logical_type(duckdb_logical_type *type);
duckdb_state duckdb_register_logical_type(duckdb_connection con, duckdb_logical_type type, duckdb_create_type_info info);

#### `duckdb_result_get_chunk`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Fetches a data chunk from the duckdb_result. This function should be called repeatedly until the result is exhausted.

The result must be destroyed with `duckdb_destroy_data_chunk`.

This function supersedes all `duckdb_value` functions, as well as the `duckdb_column_data` and `duckdb_nullmask_data`
functions. It results in significantly better performance, and should be preferred in newer code-bases.

If this function is used, none of the other result functions can be used and vice versa (i.e., this function cannot be
mixed with the legacy result functions).

Use `duckdb_result_chunk_count` to figure out how many chunks there are in the result.

##### Syntax

duckdb_data_chunk duckdb_result_get_chunk(
  duckdb_result result,
  idx_t chunk_index
);

##### Parameters

* `result`: The result object to fetch the data chunk from.
* `chunk_index`: The chunk index to fetch from.

##### Return Value

The resulting data chunk. Returns `NULL` if the chunk index is out of bounds.

#### `duckdb_result_is_streaming`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Checks if the type of the internal result is StreamQueryResult.

##### Syntax

bool duckdb_result_is_streaming(
  duckdb_result result
);

##### Parameters

* `result`: The result object to check.

##### Return Value

Whether or not the result object is of the type StreamQueryResult

#### `duckdb_result_chunk_count`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Returns the number of data chunks present in the result.

##### Syntax

idx_t duckdb_result_chunk_count(
  duckdb_result result
);

##### Parameters

* `result`: The result object

##### Return Value

Number of data chunks present in the result.

#### `duckdb_result_return_type`

Returns the return_type of the given result, or DUCKDB_RETURN_TYPE_INVALID on error

##### Syntax

duckdb_result_type duckdb_result_return_type(
  duckdb_result result
);

##### Parameters

* `result`: The result object

##### Return Value

The return_type

#### `duckdb_from_date`

Decompose a `duckdb_date` object into year, month and date (stored as `duckdb_date_struct`).

##### Syntax

duckdb_date_struct duckdb_from_date(
  duckdb_date date
);

##### Parameters

* `date`: The date object, as obtained from a `DUCKDB_TYPE_DATE` column.

##### Return Value

The `duckdb_date_struct` with the decomposed elements.

#### `duckdb_to_date`

Re-compose a `duckdb_date` from year, month and date (`duckdb_date_struct`).

##### Syntax

duckdb_date duckdb_to_date(
  duckdb_date_struct date
);

##### Parameters

* `date`: The year, month and date stored in a `duckdb_date_struct`.

##### Return Value

The `duckdb_date` element.

#### `duckdb_is_finite_date`

Test a `duckdb_date` to see if it is a finite value.

##### Syntax

bool duckdb_is_finite_date(
  duckdb_date date
);

##### Parameters

* `date`: The date object, as obtained from a `DUCKDB_TYPE_DATE` column.

##### Return Value

True if the date is finite, false if it is ±infinity.

#### `duckdb_from_time`

Decompose a `duckdb_time` object into hour, minute, second and microsecond (stored as `duckdb_time_struct`).

##### Syntax

duckdb_time_struct duckdb_from_time(
  duckdb_time time
);

##### Parameters

* `time`: The time object, as obtained from a `DUCKDB_TYPE_TIME` column.

##### Return Value

The `duckdb_time_struct` with the decomposed elements.

#### `duckdb_create_time_tz`

Create a `duckdb_time_tz` object from micros and a timezone offset.

##### Syntax

duckdb_time_tz duckdb_create_time_tz(
  int64_t micros,
  int32_t offset
);

##### Parameters

* `micros`: The microsecond component of the time.
* `offset`: The timezone offset component of the time.

##### Return Value

The `duckdb_time_tz` element.

#### `duckdb_from_time_tz`

Decompose a TIME_TZ objects into micros and a timezone offset.

Use `duckdb_from_time` to further decompose the micros into hour, minute, second and microsecond.

##### Syntax

duckdb_time_tz_struct duckdb_from_time_tz(
  duckdb_time_tz micros
);

##### Parameters

* `micros`: The time object, as obtained from a `DUCKDB_TYPE_TIME_TZ` column.

#### `duckdb_to_time`

Re-compose a `duckdb_time` from hour, minute, second and microsecond (`duckdb_time_struct`).

##### Syntax

duckdb_time duckdb_to_time(
  duckdb_time_struct time
);

##### Parameters

* `time`: The hour, minute, second and microsecond in a `duckdb_time_struct`.

##### Return Value

The `duckdb_time` element.

#### `duckdb_from_timestamp`

Decompose a `duckdb_timestamp` object into a `duckdb_timestamp_struct`.

##### Syntax

duckdb_timestamp_struct duckdb_from_timestamp(
  duckdb_timestamp ts
);

##### Parameters

* `ts`: The ts object, as obtained from a `DUCKDB_TYPE_TIMESTAMP` column.

##### Return Value

The `duckdb_timestamp_struct` with the decomposed elements.

#### `duckdb_to_timestamp`

Re-compose a `duckdb_timestamp` from a duckdb_timestamp_struct.

##### Syntax

duckdb_timestamp duckdb_to_timestamp(
  duckdb_timestamp_struct ts
);

##### Parameters

* `ts`: The de-composed elements in a `duckdb_timestamp_struct`.

##### Return Value

The `duckdb_timestamp` element.

#### `duckdb_is_finite_timestamp`

Test a `duckdb_timestamp` to see if it is a finite value.

##### Syntax

bool duckdb_is_finite_timestamp(
  duckdb_timestamp ts
);

##### Parameters

* `ts`: The duckdb_timestamp object, as obtained from a `DUCKDB_TYPE_TIMESTAMP` column.

##### Return Value

True if the timestamp is finite, false if it is ±infinity.

#### `duckdb_is_finite_timestamp_s`

Test a `duckdb_timestamp_s` to see if it is a finite value.

##### Syntax

bool duckdb_is_finite_timestamp_s(
  duckdb_timestamp_s ts
);

##### Parameters

* `ts`: The duckdb_timestamp_s object, as obtained from a `DUCKDB_TYPE_TIMESTAMP_S` column.

##### Return Value

True if the timestamp is finite, false if it is ±infinity.

#### `duckdb_is_finite_timestamp_ms`

Test a `duckdb_timestamp_ms` to see if it is a finite value.

##### Syntax

bool duckdb_is_finite_timestamp_ms(
  duckdb_timestamp_ms ts
);

##### Parameters

* `ts`: The duckdb_timestamp_ms object, as obtained from a `DUCKDB_TYPE_TIMESTAMP_MS` column.

##### Return Value

True if the timestamp is finite, false if it is ±infinity.

#### `duckdb_is_finite_timestamp_ns`

Test a `duckdb_timestamp_ns` to see if it is a finite value.

##### Syntax

bool duckdb_is_finite_timestamp_ns(
  duckdb_timestamp_ns ts
);

##### Parameters

* `ts`: The duckdb_timestamp_ns object, as obtained from a `DUCKDB_TYPE_TIMESTAMP_NS` column.

##### Return Value

True if the timestamp is finite, false if it is ±infinity.

#### `duckdb_hugeint_to_double`

Converts a duckdb_hugeint object (as obtained from a `DUCKDB_TYPE_HUGEINT` column) into a double.

##### Syntax

double duckdb_hugeint_to_double(
  duckdb_hugeint val
);

##### Parameters

* `val`: The hugeint value.

##### Return Value

The converted `double` element.

#### `duckdb_double_to_hugeint`

Converts a double value to a duckdb_hugeint object.

If the conversion fails because the double value is too big the result will be 0.

##### Syntax

duckdb_hugeint duckdb_double_to_hugeint(
  double val
);

##### Parameters

* `val`: The double value.

##### Return Value

The converted `duckdb_hugeint` element.

#### `duckdb_double_to_decimal`

Converts a double value to a duckdb_decimal object.

If the conversion fails because the double value is too big, or the width/scale are invalid the result will be 0.

##### Syntax

duckdb_decimal duckdb_double_to_decimal(
  double val,
  uint8_t width,
  uint8_t scale
);

##### Parameters

* `val`: The double value.

##### Return Value

The converted `duckdb_decimal` element.

#### `duckdb_decimal_to_double`

Converts a duckdb_decimal object (as obtained from a `DUCKDB_TYPE_DECIMAL` column) into a double.

##### Syntax

double duckdb_decimal_to_double(
  duckdb_decimal val
);

##### Parameters

* `val`: The decimal value.

##### Return Value

The converted `double` element.

#### `duckdb_create_logical_type`

Creates a `duckdb_logical_type` from a primitive type.
The resulting logical type must be destroyed with `duckdb_destroy_logical_type`.

Returns an invalid logical type, if type is: `DUCKDB_TYPE_INVALID`, `DUCKDB_TYPE_DECIMAL`, `DUCKDB_TYPE_ENUM`,
`DUCKDB_TYPE_LIST`, `DUCKDB_TYPE_STRUCT`, `DUCKDB_TYPE_MAP`, `DUCKDB_TYPE_ARRAY`, or `DUCKDB_TYPE_UNION`.

##### Syntax

duckdb_logical_type duckdb_create_logical_type(
  duckdb_type type
);

##### Parameters

* `type`: The primitive type to create.

##### Return Value

The logical type.

#### `duckdb_logical_type_get_alias`

Returns the alias of a duckdb_logical_type, if set, else `nullptr`.
The result must be destroyed with `duckdb_free`.

##### Syntax

char *duckdb_logical_type_get_alias(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type

##### Return Value

The alias or `nullptr`

#### `duckdb_logical_type_set_alias`

Sets the alias of a duckdb_logical_type.

##### Syntax

void duckdb_logical_type_set_alias(
  duckdb_logical_type type,
  const char *alias
);

##### Parameters

* `type`: The logical type
* `alias`: The alias to set

#### `duckdb_create_list_type`

Creates a LIST type from its child type.
The return type must be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_create_list_type(
  duckdb_logical_type type
);

##### Parameters

* `type`: The child type of the list

##### Return Value

The logical type.

#### `duckdb_create_array_type`

Creates an ARRAY type from its child type.
The return type must be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_create_array_type(
  duckdb_logical_type type,
  idx_t array_size
);

##### Parameters

* `type`: The child type of the array.
* `array_size`: The number of elements in the array.

##### Return Value

The logical type.

#### `duckdb_create_map_type`

Creates a MAP type from its key type and value type.
The return type must be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_create_map_type(
  duckdb_logical_type key_type,
  duckdb_logical_type value_type
);

##### Parameters

* `key_type`: The map's key type.
* `value_type`: The map's value type.

##### Return Value

The logical type.

#### `duckdb_create_union_type`

Creates a UNION type from the passed arrays.
The return type must be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_create_union_type(
  duckdb_logical_type *member_types,
  const char **member_names,
  idx_t member_count
);

##### Parameters

* `member_types`: The array of union member types.
* `member_names`: The union member names.
* `member_count`: The number of union members.

##### Return Value

The logical type.

#### `duckdb_create_struct_type`

Creates a STRUCT type based on the member types and names.
The resulting type must be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_create_struct_type(
  duckdb_logical_type *member_types,
  const char **member_names,
  idx_t member_count
);

##### Parameters

* `member_types`: The array of types of the struct members.
* `member_names`: The array of names of the struct members.
* `member_count`: The number of members of the struct.

##### Return Value

The logical type.

#### `duckdb_create_enum_type`

Creates an ENUM type from the passed member name array.
The resulting type should be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_create_enum_type(
  const char **member_names,
  idx_t member_count
);

##### Parameters

* `member_names`: The array of names that the enum should consist of.
* `member_count`: The number of elements that were specified in the array.

##### Return Value

The logical type.

#### `duckdb_create_decimal_type`

Creates a DECIMAL type with the specified width and scale.
The resulting type should be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_create_decimal_type(
  uint8_t width,
  uint8_t scale
);

##### Parameters

* `width`: The width of the decimal type
* `scale`: The scale of the decimal type

##### Return Value

The logical type.

#### `duckdb_get_type_id`

Retrieves the enum `duckdb_type` of a `duckdb_logical_type`.

##### Syntax

duckdb_type duckdb_get_type_id(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type.

##### Return Value

The `duckdb_type` id.

#### `duckdb_decimal_width`

Retrieves the width of a decimal type.

##### Syntax

uint8_t duckdb_decimal_width(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The width of the decimal type

#### `duckdb_decimal_scale`

Retrieves the scale of a decimal type.

##### Syntax

uint8_t duckdb_decimal_scale(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The scale of the decimal type

#### `duckdb_decimal_internal_type`

Retrieves the internal storage type of a decimal type.

##### Syntax

duckdb_type duckdb_decimal_internal_type(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The internal type of the decimal type

#### `duckdb_enum_internal_type`

Retrieves the internal storage type of an enum type.

##### Syntax

duckdb_type duckdb_enum_internal_type(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The internal type of the enum type

#### `duckdb_enum_dictionary_size`

Retrieves the dictionary size of the enum type.

##### Syntax

uint32_t duckdb_enum_dictionary_size(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The dictionary size of the enum type

#### `duckdb_enum_dictionary_value`

Retrieves the dictionary value at the specified position from the enum.

The result must be freed with `duckdb_free`.

##### Syntax

char *duckdb_enum_dictionary_value(
  duckdb_logical_type type,
  idx_t index
);

##### Parameters

* `type`: The logical type object
* `index`: The index in the dictionary

##### Return Value

The string value of the enum type. Must be freed with `duckdb_free`.

#### `duckdb_list_type_child_type`

Retrieves the child type of the given LIST type. Also accepts MAP types.
The result must be freed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_list_type_child_type(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type, either LIST or MAP.

##### Return Value

The child type of the LIST or MAP type.

#### `duckdb_array_type_child_type`

Retrieves the child type of the given ARRAY type.

The result must be freed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_array_type_child_type(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type. Must be ARRAY.

##### Return Value

The child type of the ARRAY type.

#### `duckdb_array_type_array_size`

Retrieves the array size of the given array type.

##### Syntax

idx_t duckdb_array_type_array_size(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The fixed number of elements the values of this array type can store.

#### `duckdb_map_type_key_type`

Retrieves the key type of the given map type.

The result must be freed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_map_type_key_type(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The key type of the map type. Must be destroyed with `duckdb_destroy_logical_type`.

#### `duckdb_map_type_value_type`

Retrieves the value type of the given map type.

The result must be freed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_map_type_value_type(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The value type of the map type. Must be destroyed with `duckdb_destroy_logical_type`.

#### `duckdb_struct_type_child_count`

Returns the number of children of a struct type.

##### Syntax

idx_t duckdb_struct_type_child_count(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The number of children of a struct type.

#### `duckdb_struct_type_child_name`

Retrieves the name of the struct child.

The result must be freed with `duckdb_free`.

##### Syntax

char *duckdb_struct_type_child_name(
  duckdb_logical_type type,
  idx_t index
);

##### Parameters

* `type`: The logical type object
* `index`: The child index

##### Return Value

The name of the struct type. Must be freed with `duckdb_free`.

#### `duckdb_struct_type_child_type`

Retrieves the child type of the given struct type at the specified index.

The result must be freed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_struct_type_child_type(
  duckdb_logical_type type,
  idx_t index
);

##### Parameters

* `type`: The logical type object
* `index`: The child index

##### Return Value

The child type of the struct type. Must be destroyed with `duckdb_destroy_logical_type`.

#### `duckdb_union_type_member_count`

Returns the number of members that the union type has.

##### Syntax

idx_t duckdb_union_type_member_count(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type (union) object

##### Return Value

The number of members of a union type.

#### `duckdb_union_type_member_name`

Retrieves the name of the union member.

The result must be freed with `duckdb_free`.

##### Syntax

char *duckdb_union_type_member_name(
  duckdb_logical_type type,
  idx_t index
);

##### Parameters

* `type`: The logical type object
* `index`: The child index

##### Return Value

The name of the union member. Must be freed with `duckdb_free`.

#### `duckdb_union_type_member_type`

Retrieves the child type of the given union member at the specified index.

The result must be freed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_union_type_member_type(
  duckdb_logical_type type,
  idx_t index
);

##### Parameters

* `type`: The logical type object
* `index`: The child index

##### Return Value

The child type of the union member. Must be destroyed with `duckdb_destroy_logical_type`.

#### `duckdb_destroy_logical_type`

Destroys the logical type and de-allocates all memory allocated for that type.

##### Syntax

void duckdb_destroy_logical_type(
  duckdb_logical_type *type
);

##### Parameters

* `type`: The logical type to destroy.

#### `duckdb_register_logical_type`

Registers a custom type within the given connection.
The type must have an alias

##### Syntax

duckdb_state duckdb_register_logical_type(
  duckdb_connection con,
  duckdb_logical_type type,
  duckdb_create_type_info info
);

##### Parameters

* `con`: The connection to use
* `type`: The custom type to register

##### Return Value

Whether or not the registration was successful.


---

# docs/stable/clients/c/prepared

_Source: https://duckdb.org/docs/stable/clients/c/prepared.md_

---
layout: docu
redirect_from:
- /docs/api/c/prepared
- /docs/api/c/prepared/
- /docs/clients/c/prepared
title: Prepared Statements
---

A prepared statement is a parameterized query. The query is prepared with question marks (`?`) or dollar symbols (`$1`) indicating the parameters of the query. Values can then be bound to these parameters, after which the prepared statement can be executed using those parameters. A single query can be prepared once and executed many times.

Prepared statements are useful to:

* Easily supply parameters to functions while avoiding string concatenation/SQL injection attacks.
* Speeding up queries that will be executed many times with different parameters.

DuckDB supports prepared statements in the C API with the `duckdb_prepare` method. The `duckdb_bind` family of functions is used to supply values for subsequent execution of the prepared statement using `duckdb_execute_prepared`. After we are done with the prepared statement it can be cleaned up using the `duckdb_destroy_prepare` method.

## Example

```c
duckdb_prepared_statement stmt;
duckdb_result result;
if (duckdb_prepare(con, "INSERT INTO integers VALUES ($1, $2)", &stmt) == DuckDBError) {
    // handle error
}

duckdb_bind_int32(stmt, 1, 42); // the parameter index starts counting at 1!
duckdb_bind_int32(stmt, 2, 43);
// NULL as second parameter means no result set is requested
duckdb_execute_prepared(stmt, NULL);
duckdb_destroy_prepare(&stmt);

// we can also query result sets using prepared statements
if (duckdb_prepare(con, "SELECT * FROM integers WHERE i = ?", &stmt) == DuckDBError) {
    // handle error
}
duckdb_bind_int32(stmt, 1, 42);
duckdb_execute_prepared(stmt, &result);

// do something with result

// clean up
duckdb_destroy_result(&result);
duckdb_destroy_prepare(&stmt);
```

After calling `duckdb_prepare`, the prepared statement parameters can be inspected using `duckdb_nparams` and `duckdb_param_type`. In case the prepare fails, the error can be obtained through `duckdb_prepare_error`.

It is not required that the `duckdb_bind` family of functions matches the prepared statement parameter type exactly. The values will be auto-cast to the required value as required. For example, calling `duckdb_bind_int8` on a parameter type of `DUCKDB_TYPE_INTEGER` will work as expected.

> Warning Do **not** use prepared statements to insert large amounts of data into DuckDB. Instead it is recommended to use the [Appender](https://duckdb.org/docs/stable/clients/c/appender.md).

## API Reference Overview

duckdb_state duckdb_prepare(duckdb_connection connection, const char *query, duckdb_prepared_statement *out_prepared_statement);
void duckdb_destroy_prepare(duckdb_prepared_statement *prepared_statement);
const char *duckdb_prepare_error(duckdb_prepared_statement prepared_statement);
idx_t duckdb_nparams(duckdb_prepared_statement prepared_statement);
const char *duckdb_parameter_name(duckdb_prepared_statement prepared_statement, idx_t index);
duckdb_type duckdb_param_type(duckdb_prepared_statement prepared_statement, idx_t param_idx);
duckdb_logical_type duckdb_param_logical_type(duckdb_prepared_statement prepared_statement, idx_t param_idx);
duckdb_state duckdb_clear_bindings(duckdb_prepared_statement prepared_statement);
duckdb_statement_type duckdb_prepared_statement_type(duckdb_prepared_statement statement);
idx_t duckdb_prepared_statement_column_count(duckdb_prepared_statement prepared_statement);
const char *duckdb_prepared_statement_column_name(duckdb_prepared_statement prepared_statement, idx_t col_idx);
duckdb_logical_type duckdb_prepared_statement_column_logical_type(duckdb_prepared_statement prepared_statement, idx_t col_idx);
duckdb_type duckdb_prepared_statement_column_type(duckdb_prepared_statement prepared_statement, idx_t col_idx);

#### `duckdb_prepare`

Create a prepared statement object from a query.

Note that after calling `duckdb_prepare`, the prepared statement should always be destroyed using
`duckdb_destroy_prepare`, even if the prepare fails.

If the prepare fails, `duckdb_prepare_error` can be called to obtain the reason why the prepare failed.

##### Syntax

duckdb_state duckdb_prepare(
  duckdb_connection connection,
  const char *query,
  duckdb_prepared_statement *out_prepared_statement
);

##### Parameters

* `connection`: The connection object
* `query`: The SQL query to prepare
* `out_prepared_statement`: The resulting prepared statement object

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_destroy_prepare`

Closes the prepared statement and de-allocates all memory allocated for the statement.

##### Syntax

void duckdb_destroy_prepare(
  duckdb_prepared_statement *prepared_statement
);

##### Parameters

* `prepared_statement`: The prepared statement to destroy.

#### `duckdb_prepare_error`

Returns the error message associated with the given prepared statement.
If the prepared statement has no error message, this returns `nullptr` instead.

The error message should not be freed. It will be de-allocated when `duckdb_destroy_prepare` is called.

##### Syntax

const char *duckdb_prepare_error(
  duckdb_prepared_statement prepared_statement
);

##### Parameters

* `prepared_statement`: The prepared statement to obtain the error from.

##### Return Value

The error message, or `nullptr` if there is none.

#### `duckdb_nparams`

Returns the number of parameters that can be provided to the given prepared statement.

Returns 0 if the query was not successfully prepared.

##### Syntax

idx_t duckdb_nparams(
  duckdb_prepared_statement prepared_statement
);

##### Parameters

* `prepared_statement`: The prepared statement to obtain the number of parameters for.

#### `duckdb_parameter_name`

Returns the name used to identify the parameter
The returned string should be freed using `duckdb_free`.

Returns NULL if the index is out of range for the provided prepared statement.

##### Syntax

const char *duckdb_parameter_name(
  duckdb_prepared_statement prepared_statement,
  idx_t index
);

##### Parameters

* `prepared_statement`: The prepared statement for which to get the parameter name from.

#### `duckdb_param_type`

Returns the parameter type for the parameter at the given index.

Returns `DUCKDB_TYPE_INVALID` if the parameter index is out of range or the statement was not successfully prepared.

##### Syntax

duckdb_type duckdb_param_type(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx
);

##### Parameters

* `prepared_statement`: The prepared statement.
* `param_idx`: The parameter index.

##### Return Value

The parameter type

#### `duckdb_param_logical_type`

Returns the logical type for the parameter at the given index.

Returns `nullptr` if the parameter index is out of range or the statement was not successfully prepared.

The return type of this call should be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_param_logical_type(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx
);

##### Parameters

* `prepared_statement`: The prepared statement.
* `param_idx`: The parameter index.

##### Return Value

The logical type of the parameter

#### `duckdb_clear_bindings`

Clear the params bind to the prepared statement.

##### Syntax

duckdb_state duckdb_clear_bindings(
  duckdb_prepared_statement prepared_statement
);

#### `duckdb_prepared_statement_type`

Returns the statement type of the statement to be executed

##### Syntax

duckdb_statement_type duckdb_prepared_statement_type(
  duckdb_prepared_statement statement
);

##### Parameters

* `statement`: The prepared statement.

##### Return Value

duckdb_statement_type value or DUCKDB_STATEMENT_TYPE_INVALID

#### `duckdb_prepared_statement_column_count`

Returns the number of columns present in a the result of the prepared statement. If any of the column types are invalid,
the result will be 1.

##### Syntax

idx_t duckdb_prepared_statement_column_count(
  duckdb_prepared_statement prepared_statement
);

##### Parameters

* `prepared_statement`: The prepared statement.

##### Return Value

The number of columns present in the result of the prepared statement.

#### `duckdb_prepared_statement_column_name`

Returns the name of the specified column of the result of the prepared_statement.
The returned string should be freed using `duckdb_free`.

Returns `nullptr` if the column is out of range.

##### Syntax

const char *duckdb_prepared_statement_column_name(
  duckdb_prepared_statement prepared_statement,
  idx_t col_idx
);

##### Parameters

* `prepared_statement`: The prepared statement.
* `col_idx`: The column index.

##### Return Value

The column name of the specified column.

#### `duckdb_prepared_statement_column_logical_type`

Returns the column type of the specified column of the result of the prepared_statement.

Returns `DUCKDB_TYPE_INVALID` if the column is out of range.
The return type of this call should be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_prepared_statement_column_logical_type(
  duckdb_prepared_statement prepared_statement,
  idx_t col_idx
);

##### Parameters

* `prepared_statement`: The prepared statement to fetch the column type from.
* `col_idx`: The column index.

##### Return Value

The logical type of the specified column.

#### `duckdb_prepared_statement_column_type`

Returns the column type of the specified column of the result of the prepared_statement.

Returns `DUCKDB_TYPE_INVALID` if the column is out of range.

##### Syntax

duckdb_type duckdb_prepared_statement_column_type(
  duckdb_prepared_statement prepared_statement,
  idx_t col_idx
);

##### Parameters

* `prepared_statement`: The prepared statement to fetch the column type from.
* `col_idx`: The column index.

##### Return Value

The type of the specified column.


---

# docs/stable/clients/c/appender

_Source: https://duckdb.org/docs/stable/clients/c/appender.md_

---
layout: docu
redirect_from:
- /docs/api/c/appender
- /docs/api/c/appender/
- /docs/clients/c/appender
title: Appender
---

Appenders are the most efficient way of loading data into DuckDB from within the C interface, and are recommended for
fast data loading. The appender is much faster than using prepared statements or individual `INSERT INTO` statements.

Appends are made in row-wise format. For every column, a `duckdb_append_[type]` call should be made, after which
the row should be finished by calling `duckdb_appender_end_row`. After all rows have been appended,
`duckdb_appender_destroy` should be used to finalize the appender and clean up the resulting memory.

Note that `duckdb_appender_destroy` should always be called on the resulting appender, even if the function returns
`DuckDBError`.

## Example

```c
duckdb_query(con, "CREATE TABLE people (id INTEGER, name VARCHAR)", NULL);

duckdb_appender appender;
if (duckdb_appender_create(con, NULL, "people", &appender) == DuckDBError) {
  // handle error
}
// append the first row (1, Mark)
duckdb_append_int32(appender, 1);
duckdb_append_varchar(appender, "Mark");
duckdb_appender_end_row(appender);

// append the second row (2, Hannes)
duckdb_append_int32(appender, 2);
duckdb_append_varchar(appender, "Hannes");
duckdb_appender_end_row(appender);

// finish appending and flush all the rows to the table
duckdb_appender_destroy(&appender);
```

## API Reference Overview

duckdb_state duckdb_appender_create(duckdb_connection connection, const char *schema, const char *table, duckdb_appender *out_appender);
duckdb_state duckdb_appender_create_ext(duckdb_connection connection, const char *catalog, const char *schema, const char *table, duckdb_appender *out_appender);
duckdb_state duckdb_appender_create_query(duckdb_connection connection, const char *query, idx_t column_count, duckdb_logical_type *types, const char *table_name, const char **column_names, duckdb_appender *out_appender);
idx_t duckdb_appender_column_count(duckdb_appender appender);
duckdb_logical_type duckdb_appender_column_type(duckdb_appender appender, idx_t col_idx);
const char *duckdb_appender_error(duckdb_appender appender);
duckdb_error_data duckdb_appender_error_data(duckdb_appender appender);
duckdb_state duckdb_appender_flush(duckdb_appender appender);
duckdb_state duckdb_appender_close(duckdb_appender appender);
duckdb_state duckdb_appender_destroy(duckdb_appender *appender);
duckdb_state duckdb_appender_add_column(duckdb_appender appender, const char *name);
duckdb_state duckdb_appender_clear_columns(duckdb_appender appender);
duckdb_state duckdb_appender_begin_row(duckdb_appender appender);
duckdb_state duckdb_appender_end_row(duckdb_appender appender);
duckdb_state duckdb_append_default(duckdb_appender appender);
duckdb_state duckdb_append_default_to_chunk(duckdb_appender appender, duckdb_data_chunk chunk, idx_t col, idx_t row);
duckdb_state duckdb_append_bool(duckdb_appender appender, bool value);
duckdb_state duckdb_append_int8(duckdb_appender appender, int8_t value);
duckdb_state duckdb_append_int16(duckdb_appender appender, int16_t value);
duckdb_state duckdb_append_int32(duckdb_appender appender, int32_t value);
duckdb_state duckdb_append_int64(duckdb_appender appender, int64_t value);
duckdb_state duckdb_append_hugeint(duckdb_appender appender, duckdb_hugeint value);
duckdb_state duckdb_append_uint8(duckdb_appender appender, uint8_t value);
duckdb_state duckdb_append_uint16(duckdb_appender appender, uint16_t value);
duckdb_state duckdb_append_uint32(duckdb_appender appender, uint32_t value);
duckdb_state duckdb_append_uint64(duckdb_appender appender, uint64_t value);
duckdb_state duckdb_append_uhugeint(duckdb_appender appender, duckdb_uhugeint value);
duckdb_state duckdb_append_float(duckdb_appender appender, float value);
duckdb_state duckdb_append_double(duckdb_appender appender, double value);
duckdb_state duckdb_append_date(duckdb_appender appender, duckdb_date value);
duckdb_state duckdb_append_time(duckdb_appender appender, duckdb_time value);
duckdb_state duckdb_append_timestamp(duckdb_appender appender, duckdb_timestamp value);
duckdb_state duckdb_append_interval(duckdb_appender appender, duckdb_interval value);
duckdb_state duckdb_append_varchar(duckdb_appender appender, const char *val);
duckdb_state duckdb_append_varchar_length(duckdb_appender appender, const char *val, idx_t length);
duckdb_state duckdb_append_blob(duckdb_appender appender, const void *data, idx_t length);
duckdb_state duckdb_append_null(duckdb_appender appender);
duckdb_state duckdb_append_value(duckdb_appender appender, duckdb_value value);
duckdb_state duckdb_append_data_chunk(duckdb_appender appender, duckdb_data_chunk chunk);

#### `duckdb_appender_create`

Creates an appender object.

Note that the object must be destroyed with `duckdb_appender_destroy`.

##### Syntax

duckdb_state duckdb_appender_create(
  duckdb_connection connection,
  const char *schema,
  const char *table,
  duckdb_appender *out_appender
);

##### Parameters

* `connection`: The connection context to create the appender in.
* `schema`: The schema of the table to append to, or `nullptr` for the default schema.
* `table`: The table name to append to.
* `out_appender`: The resulting appender object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_create_ext`

Creates an appender object.

Note that the object must be destroyed with `duckdb_appender_destroy`.

##### Syntax

duckdb_state duckdb_appender_create_ext(
  duckdb_connection connection,
  const char *catalog,
  const char *schema,
  const char *table,
  duckdb_appender *out_appender
);

##### Parameters

* `connection`: The connection context to create the appender in.
* `catalog`: The catalog of the table to append to, or `nullptr` for the default catalog.
* `schema`: The schema of the table to append to, or `nullptr` for the default schema.
* `table`: The table name to append to.
* `out_appender`: The resulting appender object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_create_query`

Creates an appender object that executes the given query with any data appended to it.

Note that the object must be destroyed with `duckdb_appender_destroy`.

##### Syntax

duckdb_state duckdb_appender_create_query(
  duckdb_connection connection,
  const char *query,
  idx_t column_count,
  duckdb_logical_type *types,
  const char *table_name,
  const char **column_names,
  duckdb_appender *out_appender
);

##### Parameters

* `connection`: The connection context to create the appender in.
* `query`: The query to execute, can be an INSERT, DELETE, UPDATE or MERGE INTO statement.
* `column_count`: The number of columns to append.
* `types`: The types of the columns to append.
* `table_name`: (optionally) the table name used to refer to the appended data, defaults to "appended_data".
* `column_names`: (optionally) the list of column names, defaults to "col1", "col2", ...
* `out_appender`: The resulting appender object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_column_count`

Returns the number of columns that belong to the appender.
If there is no active column list, then this equals the table's physical columns.

##### Syntax

idx_t duckdb_appender_column_count(
  duckdb_appender appender
);

##### Parameters

* `appender`: The appender to get the column count from.

##### Return Value

The number of columns in the data chunks.

#### `duckdb_appender_column_type`

Returns the type of the column at the specified index. This is either a type in the active column list, or the same type
as a column in the receiving table.

Note: The resulting type must be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_appender_column_type(
  duckdb_appender appender,
  idx_t col_idx
);

##### Parameters

* `appender`: The appender to get the column type from.
* `col_idx`: The index of the column to get the type of.

##### Return Value

The `duckdb_logical_type` of the column.

#### `duckdb_appender_error`

> Warning Deprecation notice. This method is scheduled for removal in a future release.
Use duckdb_appender_error_data instead.

Returns the error message associated with the appender.
If the appender has no error message, this returns `nullptr` instead.

The error message should not be freed. It will be de-allocated when `duckdb_appender_destroy` is called.

##### Syntax

const char *duckdb_appender_error(
  duckdb_appender appender
);

##### Parameters

* `appender`: The appender to get the error from.

##### Return Value

The error message, or `nullptr` if there is none.

#### `duckdb_appender_error_data`

Returns the error data associated with the appender.
Must be destroyed with duckdb_destroy_error_data.

##### Syntax

duckdb_error_data duckdb_appender_error_data(
  duckdb_appender appender
);

##### Parameters

* `appender`: The appender to get the error data from.

##### Return Value

The error data.

#### `duckdb_appender_flush`

Flush the appender to the table, forcing the cache of the appender to be cleared. If flushing the data triggers a
constraint violation or any other error, then all data is invalidated, and this function returns DuckDBError.
It is not possible to append more values. Call duckdb_appender_error_data to obtain the error data followed by
duckdb_appender_destroy to destroy the invalidated appender.

##### Syntax

duckdb_state duckdb_appender_flush(
  duckdb_appender appender
);

##### Parameters

* `appender`: The appender to flush.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_close`

Closes the appender by flushing all intermediate states and closing it for further appends. If flushing the data
triggers a constraint violation or any other error, then all data is invalidated, and this function returns DuckDBError.
Call duckdb_appender_error_data to obtain the error data followed by duckdb_appender_destroy to destroy the invalidated
appender.

##### Syntax

duckdb_state duckdb_appender_close(
  duckdb_appender appender
);

##### Parameters

* `appender`: The appender to flush and close.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_destroy`

Closes the appender by flushing all intermediate states to the table and destroying it. By destroying it, this function
de-allocates all memory associated with the appender. If flushing the data triggers a constraint violation,
then all data is invalidated, and this function returns DuckDBError. Due to the destruction of the appender, it is no
longer possible to obtain the specific error message with duckdb_appender_error. Therefore, call duckdb_appender_close
before destroying the appender, if you need insights into the specific error.

##### Syntax

duckdb_state duckdb_appender_destroy(
  duckdb_appender *appender
);

##### Parameters

* `appender`: The appender to flush, close and destroy.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_add_column`

Appends a column to the active column list of the appender. Immediately flushes all previous data.

The active column list specifies all columns that are expected when flushing the data. Any non-active columns are filled
with their default values, or NULL.

##### Syntax

duckdb_state duckdb_appender_add_column(
  duckdb_appender appender,
  const char *name
);

##### Parameters

* `appender`: The appender to add the column to.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_clear_columns`

Removes all columns from the active column list of the appender, resetting the appender to treat all columns as active.
Immediately flushes all previous data.

##### Syntax

duckdb_state duckdb_appender_clear_columns(
  duckdb_appender appender
);

##### Parameters

* `appender`: The appender to clear the columns from.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_begin_row`

A nop function, provided for backwards compatibility reasons. Does nothing. Only `duckdb_appender_end_row` is required.

##### Syntax

duckdb_state duckdb_appender_begin_row(
  duckdb_appender appender
);

#### `duckdb_appender_end_row`

Finish the current row of appends. After end_row is called, the next row can be appended.

##### Syntax

duckdb_state duckdb_appender_end_row(
  duckdb_appender appender
);

##### Parameters

* `appender`: The appender.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_append_default`

Append a DEFAULT value (NULL if DEFAULT not available for column) to the appender.

##### Syntax

duckdb_state duckdb_append_default(
  duckdb_appender appender
);

#### `duckdb_append_default_to_chunk`

Append a DEFAULT value, at the specified row and column, (NULL if DEFAULT not available for column) to the chunk created
from the specified appender. The default value of the column must be a constant value. Non-deterministic expressions
like nextval('seq') or random() are not supported.

##### Syntax

duckdb_state duckdb_append_default_to_chunk(
  duckdb_appender appender,
  duckdb_data_chunk chunk,
  idx_t col,
  idx_t row
);

##### Parameters

* `appender`: The appender to get the default value from.
* `chunk`: The data chunk to append the default value to.
* `col`: The chunk column index to append the default value to.
* `row`: The chunk row index to append the default value to.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_append_bool`

Append a bool value to the appender.

##### Syntax

duckdb_state duckdb_append_bool(
  duckdb_appender appender,
  bool value
);

#### `duckdb_append_int8`

Append an int8_t value to the appender.

##### Syntax

duckdb_state duckdb_append_int8(
  duckdb_appender appender,
  int8_t value
);

#### `duckdb_append_int16`

Append an int16_t value to the appender.

##### Syntax

duckdb_state duckdb_append_int16(
  duckdb_appender appender,
  int16_t value
);

#### `duckdb_append_int32`

Append an int32_t value to the appender.

##### Syntax

duckdb_state duckdb_append_int32(
  duckdb_appender appender,
  int32_t value
);

#### `duckdb_append_int64`

Append an int64_t value to the appender.

##### Syntax

duckdb_state duckdb_append_int64(
  duckdb_appender appender,
  int64_t value
);

#### `duckdb_append_hugeint`

Append a duckdb_hugeint value to the appender.

##### Syntax

duckdb_state duckdb_append_hugeint(
  duckdb_appender appender,
  duckdb_hugeint value
);

#### `duckdb_append_uint8`

Append a uint8_t value to the appender.

##### Syntax

duckdb_state duckdb_append_uint8(
  duckdb_appender appender,
  uint8_t value
);

#### `duckdb_append_uint16`

Append a uint16_t value to the appender.

##### Syntax

duckdb_state duckdb_append_uint16(
  duckdb_appender appender,
  uint16_t value
);

#### `duckdb_append_uint32`

Append a uint32_t value to the appender.

##### Syntax

duckdb_state duckdb_append_uint32(
  duckdb_appender appender,
  uint32_t value
);

#### `duckdb_append_uint64`

Append a uint64_t value to the appender.

##### Syntax

duckdb_state duckdb_append_uint64(
  duckdb_appender appender,
  uint64_t value
);

#### `duckdb_append_uhugeint`

Append a duckdb_uhugeint value to the appender.

##### Syntax

duckdb_state duckdb_append_uhugeint(
  duckdb_appender appender,
  duckdb_uhugeint value
);

#### `duckdb_append_float`

Append a float value to the appender.

##### Syntax

duckdb_state duckdb_append_float(
  duckdb_appender appender,
  float value
);

#### `duckdb_append_double`

Append a double value to the appender.

##### Syntax

duckdb_state duckdb_append_double(
  duckdb_appender appender,
  double value
);

#### `duckdb_append_date`

Append a duckdb_date value to the appender.

##### Syntax

duckdb_state duckdb_append_date(
  duckdb_appender appender,
  duckdb_date value
);

#### `duckdb_append_time`

Append a duckdb_time value to the appender.

##### Syntax

duckdb_state duckdb_append_time(
  duckdb_appender appender,
  duckdb_time value
);

#### `duckdb_append_timestamp`

Append a duckdb_timestamp value to the appender.

##### Syntax

duckdb_state duckdb_append_timestamp(
  duckdb_appender appender,
  duckdb_timestamp value
);

#### `duckdb_append_interval`

Append a duckdb_interval value to the appender.

##### Syntax

duckdb_state duckdb_append_interval(
  duckdb_appender appender,
  duckdb_interval value
);

#### `duckdb_append_varchar`

Append a varchar value to the appender.

##### Syntax

duckdb_state duckdb_append_varchar(
  duckdb_appender appender,
  const char *val
);

#### `duckdb_append_varchar_length`

Append a varchar value to the appender.

##### Syntax

duckdb_state duckdb_append_varchar_length(
  duckdb_appender appender,
  const char *val,
  idx_t length
);

#### `duckdb_append_blob`

Append a blob value to the appender.

##### Syntax

duckdb_state duckdb_append_blob(
  duckdb_appender appender,
  const void *data,
  idx_t length
);

#### `duckdb_append_null`

Append a NULL value to the appender (of any type).

##### Syntax

duckdb_state duckdb_append_null(
  duckdb_appender appender
);

#### `duckdb_append_value`

Append a duckdb_value to the appender.

##### Syntax

duckdb_state duckdb_append_value(
  duckdb_appender appender,
  duckdb_value value
);

#### `duckdb_append_data_chunk`

Appends a pre-filled data chunk to the specified appender.
 Attempts casting, if the data chunk types do not match the active appender types.

##### Syntax

duckdb_state duckdb_append_data_chunk(
  duckdb_appender appender,
  duckdb_data_chunk chunk
);

##### Parameters

* `appender`: The appender to append to.
* `chunk`: The data chunk to append.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.


---

# docs/stable/clients/c/table_functions

_Source: https://duckdb.org/docs/stable/clients/c/table_functions.md_

---
layout: docu
redirect_from:
- /docs/api/c/table_functions
- /docs/api/c/table_functions/
- /docs/clients/c/table_functions
title: Table Functions
---

The table function API can be used to define a table function that can then be called from within DuckDB in the `FROM` clause of a query.

## API Reference Overview

duckdb_table_function duckdb_create_table_function();
void duckdb_destroy_table_function(duckdb_table_function *table_function);
void duckdb_table_function_set_name(duckdb_table_function table_function, const char *name);
void duckdb_table_function_add_parameter(duckdb_table_function table_function, duckdb_logical_type type);
void duckdb_table_function_add_named_parameter(duckdb_table_function table_function, const char *name, duckdb_logical_type type);
void duckdb_table_function_set_extra_info(duckdb_table_function table_function, void *extra_info, duckdb_delete_callback_t destroy);
void duckdb_table_function_set_bind(duckdb_table_function table_function, duckdb_table_function_bind_t bind);
void duckdb_table_function_set_init(duckdb_table_function table_function, duckdb_table_function_init_t init);
void duckdb_table_function_set_local_init(duckdb_table_function table_function, duckdb_table_function_init_t init);
void duckdb_table_function_set_function(duckdb_table_function table_function, duckdb_table_function_t function);
void duckdb_table_function_supports_projection_pushdown(duckdb_table_function table_function, bool pushdown);
duckdb_state duckdb_register_table_function(duckdb_connection con, duckdb_table_function function);

### Table Function Bind

void *duckdb_bind_get_extra_info(duckdb_bind_info info);
void duckdb_table_function_get_client_context(duckdb_bind_info info, duckdb_client_context *out_context);
void duckdb_bind_add_result_column(duckdb_bind_info info, const char *name, duckdb_logical_type type);
idx_t duckdb_bind_get_parameter_count(duckdb_bind_info info);
duckdb_value duckdb_bind_get_parameter(duckdb_bind_info info, idx_t index);
duckdb_value duckdb_bind_get_named_parameter(duckdb_bind_info info, const char *name);
void duckdb_bind_set_bind_data(duckdb_bind_info info, void *bind_data, duckdb_delete_callback_t destroy);
void duckdb_bind_set_cardinality(duckdb_bind_info info, idx_t cardinality, bool is_exact);
void duckdb_bind_set_error(duckdb_bind_info info, const char *error);

### Table Function Init

void *duckdb_init_get_extra_info(duckdb_init_info info);
void *duckdb_init_get_bind_data(duckdb_init_info info);
void duckdb_init_set_init_data(duckdb_init_info info, void *init_data, duckdb_delete_callback_t destroy);
idx_t duckdb_init_get_column_count(duckdb_init_info info);
idx_t duckdb_init_get_column_index(duckdb_init_info info, idx_t column_index);
void duckdb_init_set_max_threads(duckdb_init_info info, idx_t max_threads);
void duckdb_init_set_error(duckdb_init_info info, const char *error);

### Table Function

void *duckdb_function_get_extra_info(duckdb_function_info info);
void *duckdb_function_get_bind_data(duckdb_function_info info);
void *duckdb_function_get_init_data(duckdb_function_info info);
void *duckdb_function_get_local_init_data(duckdb_function_info info);
void duckdb_function_set_error(duckdb_function_info info, const char *error);

#### `duckdb_create_table_function`

Creates a new empty table function.

The return value should be destroyed with `duckdb_destroy_table_function`.

##### Return Value

The table function object.

##### Syntax

duckdb_table_function duckdb_create_table_function(

);

#### `duckdb_destroy_table_function`

Destroys the given table function object.

##### Syntax

void duckdb_destroy_table_function(
  duckdb_table_function *table_function
);

##### Parameters

* `table_function`: The table function to destroy

#### `duckdb_table_function_set_name`

Sets the name of the given table function.

##### Syntax

void duckdb_table_function_set_name(
  duckdb_table_function table_function,
  const char *name
);

##### Parameters

* `table_function`: The table function
* `name`: The name of the table function

#### `duckdb_table_function_add_parameter`

Adds a parameter to the table function.

##### Syntax

void duckdb_table_function_add_parameter(
  duckdb_table_function table_function,
  duckdb_logical_type type
);

##### Parameters

* `table_function`: The table function.
* `type`: The parameter type. Cannot contain INVALID.

#### `duckdb_table_function_add_named_parameter`

Adds a named parameter to the table function.

##### Syntax

void duckdb_table_function_add_named_parameter(
  duckdb_table_function table_function,
  const char *name,
  duckdb_logical_type type
);

##### Parameters

* `table_function`: The table function.
* `name`: The parameter name.
* `type`: The parameter type. Cannot contain INVALID.

#### `duckdb_table_function_set_extra_info`

Assigns extra information to the table function that can be fetched during binding, etc.

##### Syntax

void duckdb_table_function_set_extra_info(
  duckdb_table_function table_function,
  void *extra_info,
  duckdb_delete_callback_t destroy
);

##### Parameters

* `table_function`: The table function
* `extra_info`: The extra information
* `destroy`: The callback that will be called to destroy the extra information (if any)

#### `duckdb_table_function_set_bind`

Sets the bind function of the table function.

##### Syntax

void duckdb_table_function_set_bind(
  duckdb_table_function table_function,
  duckdb_table_function_bind_t bind
);

##### Parameters

* `table_function`: The table function
* `bind`: The bind function

#### `duckdb_table_function_set_init`

Sets the init function of the table function.

##### Syntax

void duckdb_table_function_set_init(
  duckdb_table_function table_function,
  duckdb_table_function_init_t init
);

##### Parameters

* `table_function`: The table function
* `init`: The init function

#### `duckdb_table_function_set_local_init`

Sets the thread-local init function of the table function.

##### Syntax

void duckdb_table_function_set_local_init(
  duckdb_table_function table_function,
  duckdb_table_function_init_t init
);

##### Parameters

* `table_function`: The table function
* `init`: The init function

#### `duckdb_table_function_set_function`

Sets the main function of the table function.

##### Syntax

void duckdb_table_function_set_function(
  duckdb_table_function table_function,
  duckdb_table_function_t function
);

##### Parameters

* `table_function`: The table function
* `function`: The function

#### `duckdb_table_function_supports_projection_pushdown`

Sets whether or not the given table function supports projection pushdown.

If this is set to true, the system will provide a list of all required columns in the `init` stage through
the `duckdb_init_get_column_count` and `duckdb_init_get_column_index` functions.
If this is set to false (the default), the system will expect all columns to be projected.

##### Syntax

void duckdb_table_function_supports_projection_pushdown(
  duckdb_table_function table_function,
  bool pushdown
);

##### Parameters

* `table_function`: The table function
* `pushdown`: True if the table function supports projection pushdown, false otherwise.

#### `duckdb_register_table_function`

Register the table function object within the given connection.

The function requires at least a name, a bind function, an init function and a main function.

If the function is incomplete or a function with this name already exists DuckDBError is returned.

##### Syntax

duckdb_state duckdb_register_table_function(
  duckdb_connection con,
  duckdb_table_function function
);

##### Parameters

* `con`: The connection to register it in.
* `function`: The function pointer

##### Return Value

Whether or not the registration was successful.

#### `duckdb_bind_get_extra_info`

Retrieves the extra info of the function as set in `duckdb_table_function_set_extra_info`.

##### Syntax

void *duckdb_bind_get_extra_info(
  duckdb_bind_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The extra info

#### `duckdb_table_function_get_client_context`

Retrieves the client context of the bind info of a table function.

##### Syntax

void duckdb_table_function_get_client_context(
  duckdb_bind_info info,
  duckdb_client_context *out_context
);

##### Parameters

* `info`: The bind info object of the table function.
* `out_context`: The client context of the bind info. Must be destroyed with `duckdb_destroy_client_context`.

#### `duckdb_bind_add_result_column`

Adds a result column to the output of the table function.

##### Syntax

void duckdb_bind_add_result_column(
  duckdb_bind_info info,
  const char *name,
  duckdb_logical_type type
);

##### Parameters

* `info`: The table function's bind info.
* `name`: The column name.
* `type`: The logical column type.

#### `duckdb_bind_get_parameter_count`

Retrieves the number of regular (non-named) parameters to the function.

##### Syntax

idx_t duckdb_bind_get_parameter_count(
  duckdb_bind_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The number of parameters

#### `duckdb_bind_get_parameter`

Retrieves the parameter at the given index.

The result must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_bind_get_parameter(
  duckdb_bind_info info,
  idx_t index
);

##### Parameters

* `info`: The info object
* `index`: The index of the parameter to get

##### Return Value

The value of the parameter. Must be destroyed with `duckdb_destroy_value`.

#### `duckdb_bind_get_named_parameter`

Retrieves a named parameter with the given name.

The result must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_bind_get_named_parameter(
  duckdb_bind_info info,
  const char *name
);

##### Parameters

* `info`: The info object
* `name`: The name of the parameter

##### Return Value

The value of the parameter. Must be destroyed with `duckdb_destroy_value`.

#### `duckdb_bind_set_bind_data`

Sets the user-provided bind data in the bind object of the table function.
This object can be retrieved again during execution.

##### Syntax

void duckdb_bind_set_bind_data(
  duckdb_bind_info info,
  void *bind_data,
  duckdb_delete_callback_t destroy
);

##### Parameters

* `info`: The bind info of the table function.
* `bind_data`: The bind data object.
* `destroy`: The callback to destroy the bind data (if any).

#### `duckdb_bind_set_cardinality`

Sets the cardinality estimate for the table function, used for optimization.

##### Syntax

void duckdb_bind_set_cardinality(
  duckdb_bind_info info,
  idx_t cardinality,
  bool is_exact
);

##### Parameters

* `info`: The bind data object.
* `is_exact`: Whether or not the cardinality estimate is exact, or an approximation

#### `duckdb_bind_set_error`

Report that an error has occurred while calling bind on a table function.

##### Syntax

void duckdb_bind_set_error(
  duckdb_bind_info info,
  const char *error
);

##### Parameters

* `info`: The info object
* `error`: The error message

#### `duckdb_init_get_extra_info`

Retrieves the extra info of the function as set in `duckdb_table_function_set_extra_info`.

##### Syntax

void *duckdb_init_get_extra_info(
  duckdb_init_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The extra info

#### `duckdb_init_get_bind_data`

Gets the bind data set by `duckdb_bind_set_bind_data` during the bind.

Note that the bind data should be considered as read-only.
For tracking state, use the init data instead.

##### Syntax

void *duckdb_init_get_bind_data(
  duckdb_init_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The bind data object

#### `duckdb_init_set_init_data`

Sets the user-provided init data in the init object. This object can be retrieved again during execution.

##### Syntax

void duckdb_init_set_init_data(
  duckdb_init_info info,
  void *init_data,
  duckdb_delete_callback_t destroy
);

##### Parameters

* `info`: The info object
* `init_data`: The init data object.
* `destroy`: The callback that will be called to destroy the init data (if any)

#### `duckdb_init_get_column_count`

Returns the number of projected columns.

This function must be used if projection pushdown is enabled to figure out which columns to emit.

##### Syntax

idx_t duckdb_init_get_column_count(
  duckdb_init_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The number of projected columns.

#### `duckdb_init_get_column_index`

Returns the column index of the projected column at the specified position.

This function must be used if projection pushdown is enabled to figure out which columns to emit.

##### Syntax

idx_t duckdb_init_get_column_index(
  duckdb_init_info info,
  idx_t column_index
);

##### Parameters

* `info`: The info object
* `column_index`: The index at which to get the projected column index, from 0..duckdb_init_get_column_count(info)

##### Return Value

The column index of the projected column.

#### `duckdb_init_set_max_threads`

Sets how many threads can process this table function in parallel (default: 1)

##### Syntax

void duckdb_init_set_max_threads(
  duckdb_init_info info,
  idx_t max_threads
);

##### Parameters

* `info`: The info object
* `max_threads`: The maximum amount of threads that can process this table function

#### `duckdb_init_set_error`

Report that an error has occurred while calling init.

##### Syntax

void duckdb_init_set_error(
  duckdb_init_info info,
  const char *error
);

##### Parameters

* `info`: The info object
* `error`: The error message

#### `duckdb_function_get_extra_info`

Retrieves the extra info of the function as set in `duckdb_table_function_set_extra_info`.

##### Syntax

void *duckdb_function_get_extra_info(
  duckdb_function_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The extra info

#### `duckdb_function_get_bind_data`

Gets the table function's bind data set by `duckdb_bind_set_bind_data`.

Note that the bind data is read-only.
For tracking state, use the init data instead.

##### Syntax

void *duckdb_function_get_bind_data(
  duckdb_function_info info
);

##### Parameters

* `info`: The function info object.

##### Return Value

The bind data object.

#### `duckdb_function_get_init_data`

Gets the init data set by `duckdb_init_set_init_data` during the init.

##### Syntax

void *duckdb_function_get_init_data(
  duckdb_function_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The init data object

#### `duckdb_function_get_local_init_data`

Gets the thread-local init data set by `duckdb_init_set_init_data` during the local_init.

##### Syntax

void *duckdb_function_get_local_init_data(
  duckdb_function_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The init data object

#### `duckdb_function_set_error`

Report that an error has occurred while executing the function.

##### Syntax

void duckdb_function_set_error(
  duckdb_function_info info,
  const char *error
);

##### Parameters

* `info`: The info object
* `error`: The error message


---

# docs/stable/clients/c/replacement_scans

_Source: https://duckdb.org/docs/stable/clients/c/replacement_scans.md_

---
layout: docu
redirect_from:
- /docs/api/c/replacement_scans
- /docs/api/c/replacement_scans/
- /docs/clients/c/replacement_scans
title: Replacement Scans
---

The replacement scan API can be used to register a callback that is called when a table is read that does not exist in the catalog. For example, when a query such as `SELECT * FROM my_table` is executed and `my_table` does not exist, the replacement scan callback will be called with `my_table` as parameter. The replacement scan can then insert a table function with a specific parameter to replace the read of the table.

## API Reference Overview

void duckdb_add_replacement_scan(duckdb_database db, duckdb_replacement_callback_t replacement, void *extra_data, duckdb_delete_callback_t delete_callback);
void duckdb_replacement_scan_set_function_name(duckdb_replacement_scan_info info, const char *function_name);
void duckdb_replacement_scan_add_parameter(duckdb_replacement_scan_info info, duckdb_value parameter);
void duckdb_replacement_scan_set_error(duckdb_replacement_scan_info info, const char *error);

#### `duckdb_add_replacement_scan`

Add a replacement scan definition to the specified database.

##### Syntax

void duckdb_add_replacement_scan(
  duckdb_database db,
  duckdb_replacement_callback_t replacement,
  void *extra_data,
  duckdb_delete_callback_t delete_callback
);

##### Parameters

* `db`: The database object to add the replacement scan to
* `replacement`: The replacement scan callback
* `extra_data`: Extra data that is passed back into the specified callback
* `delete_callback`: The delete callback to call on the extra data, if any

#### `duckdb_replacement_scan_set_function_name`

Sets the replacement function name. If this function is called in the replacement callback,
the replacement scan is performed. If it is not called, the replacement callback is not performed.

##### Syntax

void duckdb_replacement_scan_set_function_name(
  duckdb_replacement_scan_info info,
  const char *function_name
);

##### Parameters

* `info`: The info object
* `function_name`: The function name to substitute.

#### `duckdb_replacement_scan_add_parameter`

Adds a parameter to the replacement scan function.

##### Syntax

void duckdb_replacement_scan_add_parameter(
  duckdb_replacement_scan_info info,
  duckdb_value parameter
);

##### Parameters

* `info`: The info object
* `parameter`: The parameter to add.

#### `duckdb_replacement_scan_set_error`

Report that an error has occurred while executing the replacement scan.

##### Syntax

void duckdb_replacement_scan_set_error(
  duckdb_replacement_scan_info info,
  const char *error
);

##### Parameters

* `info`: The info object
* `error`: The error message


---

# docs/stable/clients/c/api

_Source: https://duckdb.org/docs/stable/clients/c/api.md_

---
layout: docu
redirect_from:
- /docs/api/c/api
- /docs/api/c/api/
- /docs/clients/c/api
title: Complete API
---

This page contains the reference for DuckDB's C API.

> The reference contains several deprecation notices. These concern methods whose long-term availability is not guaranteed as they may be removed in the future. That said, DuckDB's developers plan to carry out deprecations slowly as several of the deprecated methods do not yet have a fully functional alternative. Therefore, they will not removed before the alternative is available, and even then, there will be a grace period of a few minor versions before removing them. The reason that the methods are already deprecated in v1.0 is to denote that they are not part of the v1.0 stable API, which contains methods that are available long-term.

## API Reference Overview

### Open Connect

duckdb_instance_cache duckdb_create_instance_cache();
duckdb_state duckdb_get_or_create_from_cache(duckdb_instance_cache instance_cache, const char *path, duckdb_database *out_database, duckdb_config config, char **out_error);
void duckdb_destroy_instance_cache(duckdb_instance_cache *instance_cache);
duckdb_state duckdb_open(const char *path, duckdb_database *out_database);
duckdb_state duckdb_open_ext(const char *path, duckdb_database *out_database, duckdb_config config, char **out_error);
void duckdb_close(duckdb_database *database);
duckdb_state duckdb_connect(duckdb_database database, duckdb_connection *out_connection);
void duckdb_interrupt(duckdb_connection connection);
duckdb_query_progress_type duckdb_query_progress(duckdb_connection connection);
void duckdb_disconnect(duckdb_connection *connection);
void duckdb_connection_get_client_context(duckdb_connection connection, duckdb_client_context *out_context);
void duckdb_connection_get_arrow_options(duckdb_connection connection, duckdb_arrow_options *out_arrow_options);
idx_t duckdb_client_context_get_connection_id(duckdb_client_context context);
void duckdb_destroy_client_context(duckdb_client_context *context);
void duckdb_destroy_arrow_options(duckdb_arrow_options *arrow_options);
const char *duckdb_library_version();
duckdb_value duckdb_get_table_names(duckdb_connection connection, const char *query, bool qualified);

### Configuration

duckdb_state duckdb_create_config(duckdb_config *out_config);
size_t duckdb_config_count();
duckdb_state duckdb_get_config_flag(size_t index, const char **out_name, const char **out_description);
duckdb_state duckdb_set_config(duckdb_config config, const char *name, const char *option);
void duckdb_destroy_config(duckdb_config *config);

### Error Data

duckdb_error_data duckdb_create_error_data(duckdb_error_type type, const char *message);
void duckdb_destroy_error_data(duckdb_error_data *error_data);
duckdb_error_type duckdb_error_data_error_type(duckdb_error_data error_data);
const char *duckdb_error_data_message(duckdb_error_data error_data);
bool duckdb_error_data_has_error(duckdb_error_data error_data);

### Query Execution

duckdb_state duckdb_query(duckdb_connection connection, const char *query, duckdb_result *out_result);
void duckdb_destroy_result(duckdb_result *result);
const char *duckdb_column_name(duckdb_result *result, idx_t col);
duckdb_type duckdb_column_type(duckdb_result *result, idx_t col);
duckdb_statement_type duckdb_result_statement_type(duckdb_result result);
duckdb_logical_type duckdb_column_logical_type(duckdb_result *result, idx_t col);
duckdb_arrow_options duckdb_result_get_arrow_options(duckdb_result *result);
idx_t duckdb_column_count(duckdb_result *result);
idx_t duckdb_row_count(duckdb_result *result);
idx_t duckdb_rows_changed(duckdb_result *result);
void *duckdb_column_data(duckdb_result *result, idx_t col);
bool *duckdb_nullmask_data(duckdb_result *result, idx_t col);
const char *duckdb_result_error(duckdb_result *result);
duckdb_error_type duckdb_result_error_type(duckdb_result *result);

### Result Functions

duckdb_data_chunk duckdb_result_get_chunk(duckdb_result result, idx_t chunk_index);
bool duckdb_result_is_streaming(duckdb_result result);
idx_t duckdb_result_chunk_count(duckdb_result result);
duckdb_result_type duckdb_result_return_type(duckdb_result result);

### Safe Fetch Functions

bool duckdb_value_boolean(duckdb_result *result, idx_t col, idx_t row);
int8_t duckdb_value_int8(duckdb_result *result, idx_t col, idx_t row);
int16_t duckdb_value_int16(duckdb_result *result, idx_t col, idx_t row);
int32_t duckdb_value_int32(duckdb_result *result, idx_t col, idx_t row);
int64_t duckdb_value_int64(duckdb_result *result, idx_t col, idx_t row);
duckdb_hugeint duckdb_value_hugeint(duckdb_result *result, idx_t col, idx_t row);
duckdb_uhugeint duckdb_value_uhugeint(duckdb_result *result, idx_t col, idx_t row);
duckdb_decimal duckdb_value_decimal(duckdb_result *result, idx_t col, idx_t row);
uint8_t duckdb_value_uint8(duckdb_result *result, idx_t col, idx_t row);
uint16_t duckdb_value_uint16(duckdb_result *result, idx_t col, idx_t row);
uint32_t duckdb_value_uint32(duckdb_result *result, idx_t col, idx_t row);
uint64_t duckdb_value_uint64(duckdb_result *result, idx_t col, idx_t row);
float duckdb_value_float(duckdb_result *result, idx_t col, idx_t row);
double duckdb_value_double(duckdb_result *result, idx_t col, idx_t row);
duckdb_date duckdb_value_date(duckdb_result *result, idx_t col, idx_t row);
duckdb_time duckdb_value_time(duckdb_result *result, idx_t col, idx_t row);
duckdb_timestamp duckdb_value_timestamp(duckdb_result *result, idx_t col, idx_t row);
duckdb_interval duckdb_value_interval(duckdb_result *result, idx_t col, idx_t row);
char *duckdb_value_varchar(duckdb_result *result, idx_t col, idx_t row);
duckdb_string duckdb_value_string(duckdb_result *result, idx_t col, idx_t row);
char *duckdb_value_varchar_internal(duckdb_result *result, idx_t col, idx_t row);
duckdb_string duckdb_value_string_internal(duckdb_result *result, idx_t col, idx_t row);
duckdb_blob duckdb_value_blob(duckdb_result *result, idx_t col, idx_t row);
bool duckdb_value_is_null(duckdb_result *result, idx_t col, idx_t row);

### Helpers

void *duckdb_malloc(size_t size);
void duckdb_free(void *ptr);
idx_t duckdb_vector_size();
bool duckdb_string_is_inlined(duckdb_string_t string);
uint32_t duckdb_string_t_length(duckdb_string_t string);
const char *duckdb_string_t_data(duckdb_string_t *string);

### Date Time Timestamp Helpers

duckdb_date_struct duckdb_from_date(duckdb_date date);
duckdb_date duckdb_to_date(duckdb_date_struct date);
bool duckdb_is_finite_date(duckdb_date date);
duckdb_time_struct duckdb_from_time(duckdb_time time);
duckdb_time_tz duckdb_create_time_tz(int64_t micros, int32_t offset);
duckdb_time_tz_struct duckdb_from_time_tz(duckdb_time_tz micros);
duckdb_time duckdb_to_time(duckdb_time_struct time);
duckdb_timestamp_struct duckdb_from_timestamp(duckdb_timestamp ts);
duckdb_timestamp duckdb_to_timestamp(duckdb_timestamp_struct ts);
bool duckdb_is_finite_timestamp(duckdb_timestamp ts);
bool duckdb_is_finite_timestamp_s(duckdb_timestamp_s ts);
bool duckdb_is_finite_timestamp_ms(duckdb_timestamp_ms ts);
bool duckdb_is_finite_timestamp_ns(duckdb_timestamp_ns ts);

### Hugeint Helpers

double duckdb_hugeint_to_double(duckdb_hugeint val);
duckdb_hugeint duckdb_double_to_hugeint(double val);

### Unsigned Hugeint Helpers

double duckdb_uhugeint_to_double(duckdb_uhugeint val);
duckdb_uhugeint duckdb_double_to_uhugeint(double val);

### Decimal Helpers

duckdb_decimal duckdb_double_to_decimal(double val, uint8_t width, uint8_t scale);
double duckdb_decimal_to_double(duckdb_decimal val);

### Prepared Statements

duckdb_state duckdb_prepare(duckdb_connection connection, const char *query, duckdb_prepared_statement *out_prepared_statement);
void duckdb_destroy_prepare(duckdb_prepared_statement *prepared_statement);
const char *duckdb_prepare_error(duckdb_prepared_statement prepared_statement);
idx_t duckdb_nparams(duckdb_prepared_statement prepared_statement);
const char *duckdb_parameter_name(duckdb_prepared_statement prepared_statement, idx_t index);
duckdb_type duckdb_param_type(duckdb_prepared_statement prepared_statement, idx_t param_idx);
duckdb_logical_type duckdb_param_logical_type(duckdb_prepared_statement prepared_statement, idx_t param_idx);
duckdb_state duckdb_clear_bindings(duckdb_prepared_statement prepared_statement);
duckdb_statement_type duckdb_prepared_statement_type(duckdb_prepared_statement statement);
idx_t duckdb_prepared_statement_column_count(duckdb_prepared_statement prepared_statement);
const char *duckdb_prepared_statement_column_name(duckdb_prepared_statement prepared_statement, idx_t col_idx);
duckdb_logical_type duckdb_prepared_statement_column_logical_type(duckdb_prepared_statement prepared_statement, idx_t col_idx);
duckdb_type duckdb_prepared_statement_column_type(duckdb_prepared_statement prepared_statement, idx_t col_idx);

### Bind Values to Prepared Statements

duckdb_state duckdb_bind_value(duckdb_prepared_statement prepared_statement, idx_t param_idx, duckdb_value val);
duckdb_state duckdb_bind_parameter_index(duckdb_prepared_statement prepared_statement, idx_t *param_idx_out, const char *name);
duckdb_state duckdb_bind_boolean(duckdb_prepared_statement prepared_statement, idx_t param_idx, bool val);
duckdb_state duckdb_bind_int8(duckdb_prepared_statement prepared_statement, idx_t param_idx, int8_t val);
duckdb_state duckdb_bind_int16(duckdb_prepared_statement prepared_statement, idx_t param_idx, int16_t val);
duckdb_state duckdb_bind_int32(duckdb_prepared_statement prepared_statement, idx_t param_idx, int32_t val);
duckdb_state duckdb_bind_int64(duckdb_prepared_statement prepared_statement, idx_t param_idx, int64_t val);
duckdb_state duckdb_bind_hugeint(duckdb_prepared_statement prepared_statement, idx_t param_idx, duckdb_hugeint val);
duckdb_state duckdb_bind_uhugeint(duckdb_prepared_statement prepared_statement, idx_t param_idx, duckdb_uhugeint val);
duckdb_state duckdb_bind_decimal(duckdb_prepared_statement prepared_statement, idx_t param_idx, duckdb_decimal val);
duckdb_state duckdb_bind_uint8(duckdb_prepared_statement prepared_statement, idx_t param_idx, uint8_t val);
duckdb_state duckdb_bind_uint16(duckdb_prepared_statement prepared_statement, idx_t param_idx, uint16_t val);
duckdb_state duckdb_bind_uint32(duckdb_prepared_statement prepared_statement, idx_t param_idx, uint32_t val);
duckdb_state duckdb_bind_uint64(duckdb_prepared_statement prepared_statement, idx_t param_idx, uint64_t val);
duckdb_state duckdb_bind_float(duckdb_prepared_statement prepared_statement, idx_t param_idx, float val);
duckdb_state duckdb_bind_double(duckdb_prepared_statement prepared_statement, idx_t param_idx, double val);
duckdb_state duckdb_bind_date(duckdb_prepared_statement prepared_statement, idx_t param_idx, duckdb_date val);
duckdb_state duckdb_bind_time(duckdb_prepared_statement prepared_statement, idx_t param_idx, duckdb_time val);
duckdb_state duckdb_bind_timestamp(duckdb_prepared_statement prepared_statement, idx_t param_idx, duckdb_timestamp val);
duckdb_state duckdb_bind_timestamp_tz(duckdb_prepared_statement prepared_statement, idx_t param_idx, duckdb_timestamp val);
duckdb_state duckdb_bind_interval(duckdb_prepared_statement prepared_statement, idx_t param_idx, duckdb_interval val);
duckdb_state duckdb_bind_varchar(duckdb_prepared_statement prepared_statement, idx_t param_idx, const char *val);
duckdb_state duckdb_bind_varchar_length(duckdb_prepared_statement prepared_statement, idx_t param_idx, const char *val, idx_t length);
duckdb_state duckdb_bind_blob(duckdb_prepared_statement prepared_statement, idx_t param_idx, const void *data, idx_t length);
duckdb_state duckdb_bind_null(duckdb_prepared_statement prepared_statement, idx_t param_idx);

### Execute Prepared Statements

duckdb_state duckdb_execute_prepared(duckdb_prepared_statement prepared_statement, duckdb_result *out_result);
duckdb_state duckdb_execute_prepared_streaming(duckdb_prepared_statement prepared_statement, duckdb_result *out_result);

### Extract Statements

idx_t duckdb_extract_statements(duckdb_connection connection, const char *query, duckdb_extracted_statements *out_extracted_statements);
duckdb_state duckdb_prepare_extracted_statement(duckdb_connection connection, duckdb_extracted_statements extracted_statements, idx_t index, duckdb_prepared_statement *out_prepared_statement);
const char *duckdb_extract_statements_error(duckdb_extracted_statements extracted_statements);
void duckdb_destroy_extracted(duckdb_extracted_statements *extracted_statements);

### Pending Result Interface

duckdb_state duckdb_pending_prepared(duckdb_prepared_statement prepared_statement, duckdb_pending_result *out_result);
duckdb_state duckdb_pending_prepared_streaming(duckdb_prepared_statement prepared_statement, duckdb_pending_result *out_result);
void duckdb_destroy_pending(duckdb_pending_result *pending_result);
const char *duckdb_pending_error(duckdb_pending_result pending_result);
duckdb_pending_state duckdb_pending_execute_task(duckdb_pending_result pending_result);
duckdb_pending_state duckdb_pending_execute_check_state(duckdb_pending_result pending_result);
duckdb_state duckdb_execute_pending(duckdb_pending_result pending_result, duckdb_result *out_result);
bool duckdb_pending_execution_is_finished(duckdb_pending_state pending_state);

### Value Interface

void duckdb_destroy_value(duckdb_value *value);
duckdb_value duckdb_create_varchar(const char *text);
duckdb_value duckdb_create_varchar_length(const char *text, idx_t length);
duckdb_value duckdb_create_bool(bool input);
duckdb_value duckdb_create_int8(int8_t input);
duckdb_value duckdb_create_uint8(uint8_t input);
duckdb_value duckdb_create_int16(int16_t input);
duckdb_value duckdb_create_uint16(uint16_t input);
duckdb_value duckdb_create_int32(int32_t input);
duckdb_value duckdb_create_uint32(uint32_t input);
duckdb_value duckdb_create_uint64(uint64_t input);
duckdb_value duckdb_create_int64(int64_t val);
duckdb_value duckdb_create_hugeint(duckdb_hugeint input);
duckdb_value duckdb_create_uhugeint(duckdb_uhugeint input);
duckdb_value duckdb_create_bignum(duckdb_bignum input);
duckdb_value duckdb_create_decimal(duckdb_decimal input);
duckdb_value duckdb_create_float(float input);
duckdb_value duckdb_create_double(double input);
duckdb_value duckdb_create_date(duckdb_date input);
duckdb_value duckdb_create_time(duckdb_time input);
duckdb_value duckdb_create_time_ns(duckdb_time_ns input);
duckdb_value duckdb_create_time_tz_value(duckdb_time_tz value);
duckdb_value duckdb_create_timestamp(duckdb_timestamp input);
duckdb_value duckdb_create_timestamp_tz(duckdb_timestamp input);
duckdb_value duckdb_create_timestamp_s(duckdb_timestamp_s input);
duckdb_value duckdb_create_timestamp_ms(duckdb_timestamp_ms input);
duckdb_value duckdb_create_timestamp_ns(duckdb_timestamp_ns input);
duckdb_value duckdb_create_interval(duckdb_interval input);
duckdb_value duckdb_create_blob(const uint8_t *data, idx_t length);
duckdb_value duckdb_create_bit(duckdb_bit input);
duckdb_value duckdb_create_uuid(duckdb_uhugeint input);
bool duckdb_get_bool(duckdb_value val);
int8_t duckdb_get_int8(duckdb_value val);
uint8_t duckdb_get_uint8(duckdb_value val);
int16_t duckdb_get_int16(duckdb_value val);
uint16_t duckdb_get_uint16(duckdb_value val);
int32_t duckdb_get_int32(duckdb_value val);
uint32_t duckdb_get_uint32(duckdb_value val);
int64_t duckdb_get_int64(duckdb_value val);
uint64_t duckdb_get_uint64(duckdb_value val);
duckdb_hugeint duckdb_get_hugeint(duckdb_value val);
duckdb_uhugeint duckdb_get_uhugeint(duckdb_value val);
duckdb_bignum duckdb_get_bignum(duckdb_value val);
duckdb_decimal duckdb_get_decimal(duckdb_value val);
float duckdb_get_float(duckdb_value val);
double duckdb_get_double(duckdb_value val);
duckdb_date duckdb_get_date(duckdb_value val);
duckdb_time duckdb_get_time(duckdb_value val);
duckdb_time_ns duckdb_get_time_ns(duckdb_value val);
duckdb_time_tz duckdb_get_time_tz(duckdb_value val);
duckdb_timestamp duckdb_get_timestamp(duckdb_value val);
duckdb_timestamp duckdb_get_timestamp_tz(duckdb_value val);
duckdb_timestamp_s duckdb_get_timestamp_s(duckdb_value val);
duckdb_timestamp_ms duckdb_get_timestamp_ms(duckdb_value val);
duckdb_timestamp_ns duckdb_get_timestamp_ns(duckdb_value val);
duckdb_interval duckdb_get_interval(duckdb_value val);
duckdb_logical_type duckdb_get_value_type(duckdb_value val);
duckdb_blob duckdb_get_blob(duckdb_value val);
duckdb_bit duckdb_get_bit(duckdb_value val);
duckdb_uhugeint duckdb_get_uuid(duckdb_value val);
char *duckdb_get_varchar(duckdb_value value);
duckdb_value duckdb_create_struct_value(duckdb_logical_type type, duckdb_value *values);
duckdb_value duckdb_create_list_value(duckdb_logical_type type, duckdb_value *values, idx_t value_count);
duckdb_value duckdb_create_array_value(duckdb_logical_type type, duckdb_value *values, idx_t value_count);
duckdb_value duckdb_create_map_value(duckdb_logical_type map_type, duckdb_value *keys, duckdb_value *values, idx_t entry_count);
duckdb_value duckdb_create_union_value(duckdb_logical_type union_type, idx_t tag_index, duckdb_value value);
idx_t duckdb_get_map_size(duckdb_value value);
duckdb_value duckdb_get_map_key(duckdb_value value, idx_t index);
duckdb_value duckdb_get_map_value(duckdb_value value, idx_t index);
bool duckdb_is_null_value(duckdb_value value);
duckdb_value duckdb_create_null_value();
idx_t duckdb_get_list_size(duckdb_value value);
duckdb_value duckdb_get_list_child(duckdb_value value, idx_t index);
duckdb_value duckdb_create_enum_value(duckdb_logical_type type, uint64_t value);
uint64_t duckdb_get_enum_value(duckdb_value value);
duckdb_value duckdb_get_struct_child(duckdb_value value, idx_t index);
char *duckdb_value_to_string(duckdb_value value);

### Logical Type Interface

duckdb_logical_type duckdb_create_logical_type(duckdb_type type);
char *duckdb_logical_type_get_alias(duckdb_logical_type type);
void duckdb_logical_type_set_alias(duckdb_logical_type type, const char *alias);
duckdb_logical_type duckdb_create_list_type(duckdb_logical_type type);
duckdb_logical_type duckdb_create_array_type(duckdb_logical_type type, idx_t array_size);
duckdb_logical_type duckdb_create_map_type(duckdb_logical_type key_type, duckdb_logical_type value_type);
duckdb_logical_type duckdb_create_union_type(duckdb_logical_type *member_types, const char **member_names, idx_t member_count);
duckdb_logical_type duckdb_create_struct_type(duckdb_logical_type *member_types, const char **member_names, idx_t member_count);
duckdb_logical_type duckdb_create_enum_type(const char **member_names, idx_t member_count);
duckdb_logical_type duckdb_create_decimal_type(uint8_t width, uint8_t scale);
duckdb_type duckdb_get_type_id(duckdb_logical_type type);
uint8_t duckdb_decimal_width(duckdb_logical_type type);
uint8_t duckdb_decimal_scale(duckdb_logical_type type);
duckdb_type duckdb_decimal_internal_type(duckdb_logical_type type);
duckdb_type duckdb_enum_internal_type(duckdb_logical_type type);
uint32_t duckdb_enum_dictionary_size(duckdb_logical_type type);
char *duckdb_enum_dictionary_value(duckdb_logical_type type, idx_t index);
duckdb_logical_type duckdb_list_type_child_type(duckdb_logical_type type);
duckdb_logical_type duckdb_array_type_child_type(duckdb_logical_type type);
idx_t duckdb_array_type_array_size(duckdb_logical_type type);
duckdb_logical_type duckdb_map_type_key_type(duckdb_logical_type type);
duckdb_logical_type duckdb_map_type_value_type(duckdb_logical_type type);
idx_t duckdb_struct_type_child_count(duckdb_logical_type type);
char *duckdb_struct_type_child_name(duckdb_logical_type type, idx_t index);
duckdb_logical_type duckdb_struct_type_child_type(duckdb_logical_type type, idx_t index);
idx_t duckdb_union_type_member_count(duckdb_logical_type type);
char *duckdb_union_type_member_name(duckdb_logical_type type, idx_t index);
duckdb_logical_type duckdb_union_type_member_type(duckdb_logical_type type, idx_t index);
void duckdb_destroy_logical_type(duckdb_logical_type *type);
duckdb_state duckdb_register_logical_type(duckdb_connection con, duckdb_logical_type type, duckdb_create_type_info info);

### Data Chunk Interface

duckdb_data_chunk duckdb_create_data_chunk(duckdb_logical_type *types, idx_t column_count);
void duckdb_destroy_data_chunk(duckdb_data_chunk *chunk);
void duckdb_data_chunk_reset(duckdb_data_chunk chunk);
idx_t duckdb_data_chunk_get_column_count(duckdb_data_chunk chunk);
duckdb_vector duckdb_data_chunk_get_vector(duckdb_data_chunk chunk, idx_t col_idx);
idx_t duckdb_data_chunk_get_size(duckdb_data_chunk chunk);
void duckdb_data_chunk_set_size(duckdb_data_chunk chunk, idx_t size);

### Vector Interface

duckdb_vector duckdb_create_vector(duckdb_logical_type type, idx_t capacity);
void duckdb_destroy_vector(duckdb_vector *vector);
duckdb_logical_type duckdb_vector_get_column_type(duckdb_vector vector);
void *duckdb_vector_get_data(duckdb_vector vector);
uint64_t *duckdb_vector_get_validity(duckdb_vector vector);
void duckdb_vector_ensure_validity_writable(duckdb_vector vector);
void duckdb_vector_assign_string_element(duckdb_vector vector, idx_t index, const char *str);
void duckdb_vector_assign_string_element_len(duckdb_vector vector, idx_t index, const char *str, idx_t str_len);
duckdb_vector duckdb_list_vector_get_child(duckdb_vector vector);
idx_t duckdb_list_vector_get_size(duckdb_vector vector);
duckdb_state duckdb_list_vector_set_size(duckdb_vector vector, idx_t size);
duckdb_state duckdb_list_vector_reserve(duckdb_vector vector, idx_t required_capacity);
duckdb_vector duckdb_struct_vector_get_child(duckdb_vector vector, idx_t index);
duckdb_vector duckdb_array_vector_get_child(duckdb_vector vector);
void duckdb_slice_vector(duckdb_vector vector, duckdb_selection_vector sel, idx_t len);
void duckdb_vector_copy_sel(duckdb_vector src, duckdb_vector dst, duckdb_selection_vector sel, idx_t src_count, idx_t src_offset, idx_t dst_offset);
void duckdb_vector_reference_value(duckdb_vector vector, duckdb_value value);
void duckdb_vector_reference_vector(duckdb_vector to_vector, duckdb_vector from_vector);

### Validity Mask Functions

bool duckdb_validity_row_is_valid(uint64_t *validity, idx_t row);
void duckdb_validity_set_row_validity(uint64_t *validity, idx_t row, bool valid);
void duckdb_validity_set_row_invalid(uint64_t *validity, idx_t row);
void duckdb_validity_set_row_valid(uint64_t *validity, idx_t row);

### Scalar Functions

duckdb_scalar_function duckdb_create_scalar_function();
void duckdb_destroy_scalar_function(duckdb_scalar_function *scalar_function);
void duckdb_scalar_function_set_name(duckdb_scalar_function scalar_function, const char *name);
void duckdb_scalar_function_set_varargs(duckdb_scalar_function scalar_function, duckdb_logical_type type);
void duckdb_scalar_function_set_special_handling(duckdb_scalar_function scalar_function);
void duckdb_scalar_function_set_volatile(duckdb_scalar_function scalar_function);
void duckdb_scalar_function_add_parameter(duckdb_scalar_function scalar_function, duckdb_logical_type type);
void duckdb_scalar_function_set_return_type(duckdb_scalar_function scalar_function, duckdb_logical_type type);
void duckdb_scalar_function_set_extra_info(duckdb_scalar_function scalar_function, void *extra_info, duckdb_delete_callback_t destroy);
void duckdb_scalar_function_set_bind(duckdb_scalar_function scalar_function, duckdb_scalar_function_bind_t bind);
void duckdb_scalar_function_set_bind_data(duckdb_bind_info info, void *bind_data, duckdb_delete_callback_t destroy);
void duckdb_scalar_function_set_bind_data_copy(duckdb_bind_info info, duckdb_copy_callback_t copy);
void duckdb_scalar_function_bind_set_error(duckdb_bind_info info, const char *error);
void duckdb_scalar_function_set_function(duckdb_scalar_function scalar_function, duckdb_scalar_function_t function);
duckdb_state duckdb_register_scalar_function(duckdb_connection con, duckdb_scalar_function scalar_function);
void *duckdb_scalar_function_get_extra_info(duckdb_function_info info);
void *duckdb_scalar_function_bind_get_extra_info(duckdb_bind_info info);
void *duckdb_scalar_function_get_bind_data(duckdb_function_info info);
void duckdb_scalar_function_get_client_context(duckdb_bind_info info, duckdb_client_context *out_context);
void duckdb_scalar_function_set_error(duckdb_function_info info, const char *error);
duckdb_scalar_function_set duckdb_create_scalar_function_set(const char *name);
void duckdb_destroy_scalar_function_set(duckdb_scalar_function_set *scalar_function_set);
duckdb_state duckdb_add_scalar_function_to_set(duckdb_scalar_function_set set, duckdb_scalar_function function);
duckdb_state duckdb_register_scalar_function_set(duckdb_connection con, duckdb_scalar_function_set set);
idx_t duckdb_scalar_function_bind_get_argument_count(duckdb_bind_info info);
duckdb_expression duckdb_scalar_function_bind_get_argument(duckdb_bind_info info, idx_t index);

### Selection Vector Interface

duckdb_selection_vector duckdb_create_selection_vector(idx_t size);
void duckdb_destroy_selection_vector(duckdb_selection_vector sel);
sel_t *duckdb_selection_vector_get_data_ptr(duckdb_selection_vector sel);

### Aggregate Functions

duckdb_aggregate_function duckdb_create_aggregate_function();
void duckdb_destroy_aggregate_function(duckdb_aggregate_function *aggregate_function);
void duckdb_aggregate_function_set_name(duckdb_aggregate_function aggregate_function, const char *name);
void duckdb_aggregate_function_add_parameter(duckdb_aggregate_function aggregate_function, duckdb_logical_type type);
void duckdb_aggregate_function_set_return_type(duckdb_aggregate_function aggregate_function, duckdb_logical_type type);
void duckdb_aggregate_function_set_functions(duckdb_aggregate_function aggregate_function, duckdb_aggregate_state_size state_size, duckdb_aggregate_init_t state_init, duckdb_aggregate_update_t update, duckdb_aggregate_combine_t combine, duckdb_aggregate_finalize_t finalize);
void duckdb_aggregate_function_set_destructor(duckdb_aggregate_function aggregate_function, duckdb_aggregate_destroy_t destroy);
duckdb_state duckdb_register_aggregate_function(duckdb_connection con, duckdb_aggregate_function aggregate_function);
void duckdb_aggregate_function_set_special_handling(duckdb_aggregate_function aggregate_function);
void duckdb_aggregate_function_set_extra_info(duckdb_aggregate_function aggregate_function, void *extra_info, duckdb_delete_callback_t destroy);
void *duckdb_aggregate_function_get_extra_info(duckdb_function_info info);
void duckdb_aggregate_function_set_error(duckdb_function_info info, const char *error);
duckdb_aggregate_function_set duckdb_create_aggregate_function_set(const char *name);
void duckdb_destroy_aggregate_function_set(duckdb_aggregate_function_set *aggregate_function_set);
duckdb_state duckdb_add_aggregate_function_to_set(duckdb_aggregate_function_set set, duckdb_aggregate_function function);
duckdb_state duckdb_register_aggregate_function_set(duckdb_connection con, duckdb_aggregate_function_set set);

### Table Functions

duckdb_table_function duckdb_create_table_function();
void duckdb_destroy_table_function(duckdb_table_function *table_function);
void duckdb_table_function_set_name(duckdb_table_function table_function, const char *name);
void duckdb_table_function_add_parameter(duckdb_table_function table_function, duckdb_logical_type type);
void duckdb_table_function_add_named_parameter(duckdb_table_function table_function, const char *name, duckdb_logical_type type);
void duckdb_table_function_set_extra_info(duckdb_table_function table_function, void *extra_info, duckdb_delete_callback_t destroy);
void duckdb_table_function_set_bind(duckdb_table_function table_function, duckdb_table_function_bind_t bind);
void duckdb_table_function_set_init(duckdb_table_function table_function, duckdb_table_function_init_t init);
void duckdb_table_function_set_local_init(duckdb_table_function table_function, duckdb_table_function_init_t init);
void duckdb_table_function_set_function(duckdb_table_function table_function, duckdb_table_function_t function);
void duckdb_table_function_supports_projection_pushdown(duckdb_table_function table_function, bool pushdown);
duckdb_state duckdb_register_table_function(duckdb_connection con, duckdb_table_function function);

### Table Function Bind

void *duckdb_bind_get_extra_info(duckdb_bind_info info);
void duckdb_table_function_get_client_context(duckdb_bind_info info, duckdb_client_context *out_context);
void duckdb_bind_add_result_column(duckdb_bind_info info, const char *name, duckdb_logical_type type);
idx_t duckdb_bind_get_parameter_count(duckdb_bind_info info);
duckdb_value duckdb_bind_get_parameter(duckdb_bind_info info, idx_t index);
duckdb_value duckdb_bind_get_named_parameter(duckdb_bind_info info, const char *name);
void duckdb_bind_set_bind_data(duckdb_bind_info info, void *bind_data, duckdb_delete_callback_t destroy);
void duckdb_bind_set_cardinality(duckdb_bind_info info, idx_t cardinality, bool is_exact);
void duckdb_bind_set_error(duckdb_bind_info info, const char *error);

### Table Function Init

void *duckdb_init_get_extra_info(duckdb_init_info info);
void *duckdb_init_get_bind_data(duckdb_init_info info);
void duckdb_init_set_init_data(duckdb_init_info info, void *init_data, duckdb_delete_callback_t destroy);
idx_t duckdb_init_get_column_count(duckdb_init_info info);
idx_t duckdb_init_get_column_index(duckdb_init_info info, idx_t column_index);
void duckdb_init_set_max_threads(duckdb_init_info info, idx_t max_threads);
void duckdb_init_set_error(duckdb_init_info info, const char *error);

### Table Function

void *duckdb_function_get_extra_info(duckdb_function_info info);
void *duckdb_function_get_bind_data(duckdb_function_info info);
void *duckdb_function_get_init_data(duckdb_function_info info);
void *duckdb_function_get_local_init_data(duckdb_function_info info);
void duckdb_function_set_error(duckdb_function_info info, const char *error);

### Replacement Scans

void duckdb_add_replacement_scan(duckdb_database db, duckdb_replacement_callback_t replacement, void *extra_data, duckdb_delete_callback_t delete_callback);
void duckdb_replacement_scan_set_function_name(duckdb_replacement_scan_info info, const char *function_name);
void duckdb_replacement_scan_add_parameter(duckdb_replacement_scan_info info, duckdb_value parameter);
void duckdb_replacement_scan_set_error(duckdb_replacement_scan_info info, const char *error);

### Profiling Info

duckdb_profiling_info duckdb_get_profiling_info(duckdb_connection connection);
duckdb_value duckdb_profiling_info_get_value(duckdb_profiling_info info, const char *key);
duckdb_value duckdb_profiling_info_get_metrics(duckdb_profiling_info info);
idx_t duckdb_profiling_info_get_child_count(duckdb_profiling_info info);
duckdb_profiling_info duckdb_profiling_info_get_child(duckdb_profiling_info info, idx_t index);

### Appender

duckdb_state duckdb_appender_create(duckdb_connection connection, const char *schema, const char *table, duckdb_appender *out_appender);
duckdb_state duckdb_appender_create_ext(duckdb_connection connection, const char *catalog, const char *schema, const char *table, duckdb_appender *out_appender);
duckdb_state duckdb_appender_create_query(duckdb_connection connection, const char *query, idx_t column_count, duckdb_logical_type *types, const char *table_name, const char **column_names, duckdb_appender *out_appender);
idx_t duckdb_appender_column_count(duckdb_appender appender);
duckdb_logical_type duckdb_appender_column_type(duckdb_appender appender, idx_t col_idx);
const char *duckdb_appender_error(duckdb_appender appender);
duckdb_error_data duckdb_appender_error_data(duckdb_appender appender);
duckdb_state duckdb_appender_flush(duckdb_appender appender);
duckdb_state duckdb_appender_close(duckdb_appender appender);
duckdb_state duckdb_appender_destroy(duckdb_appender *appender);
duckdb_state duckdb_appender_add_column(duckdb_appender appender, const char *name);
duckdb_state duckdb_appender_clear_columns(duckdb_appender appender);
duckdb_state duckdb_appender_begin_row(duckdb_appender appender);
duckdb_state duckdb_appender_end_row(duckdb_appender appender);
duckdb_state duckdb_append_default(duckdb_appender appender);
duckdb_state duckdb_append_default_to_chunk(duckdb_appender appender, duckdb_data_chunk chunk, idx_t col, idx_t row);
duckdb_state duckdb_append_bool(duckdb_appender appender, bool value);
duckdb_state duckdb_append_int8(duckdb_appender appender, int8_t value);
duckdb_state duckdb_append_int16(duckdb_appender appender, int16_t value);
duckdb_state duckdb_append_int32(duckdb_appender appender, int32_t value);
duckdb_state duckdb_append_int64(duckdb_appender appender, int64_t value);
duckdb_state duckdb_append_hugeint(duckdb_appender appender, duckdb_hugeint value);
duckdb_state duckdb_append_uint8(duckdb_appender appender, uint8_t value);
duckdb_state duckdb_append_uint16(duckdb_appender appender, uint16_t value);
duckdb_state duckdb_append_uint32(duckdb_appender appender, uint32_t value);
duckdb_state duckdb_append_uint64(duckdb_appender appender, uint64_t value);
duckdb_state duckdb_append_uhugeint(duckdb_appender appender, duckdb_uhugeint value);
duckdb_state duckdb_append_float(duckdb_appender appender, float value);
duckdb_state duckdb_append_double(duckdb_appender appender, double value);
duckdb_state duckdb_append_date(duckdb_appender appender, duckdb_date value);
duckdb_state duckdb_append_time(duckdb_appender appender, duckdb_time value);
duckdb_state duckdb_append_timestamp(duckdb_appender appender, duckdb_timestamp value);
duckdb_state duckdb_append_interval(duckdb_appender appender, duckdb_interval value);
duckdb_state duckdb_append_varchar(duckdb_appender appender, const char *val);
duckdb_state duckdb_append_varchar_length(duckdb_appender appender, const char *val, idx_t length);
duckdb_state duckdb_append_blob(duckdb_appender appender, const void *data, idx_t length);
duckdb_state duckdb_append_null(duckdb_appender appender);
duckdb_state duckdb_append_value(duckdb_appender appender, duckdb_value value);
duckdb_state duckdb_append_data_chunk(duckdb_appender appender, duckdb_data_chunk chunk);

### Table Description

duckdb_state duckdb_table_description_create(duckdb_connection connection, const char *schema, const char *table, duckdb_table_description *out);
duckdb_state duckdb_table_description_create_ext(duckdb_connection connection, const char *catalog, const char *schema, const char *table, duckdb_table_description *out);
void duckdb_table_description_destroy(duckdb_table_description *table_description);
const char *duckdb_table_description_error(duckdb_table_description table_description);
duckdb_state duckdb_column_has_default(duckdb_table_description table_description, idx_t index, bool *out);
char *duckdb_table_description_get_column_name(duckdb_table_description table_description, idx_t index);

### Arrow Interface

duckdb_error_data duckdb_to_arrow_schema(duckdb_arrow_options arrow_options, duckdb_logical_type *types, const char **names, idx_t column_count, struct ArrowSchema *out_schema);
duckdb_error_data duckdb_data_chunk_to_arrow(duckdb_arrow_options arrow_options, duckdb_data_chunk chunk, struct ArrowArray *out_arrow_array);
duckdb_error_data duckdb_schema_from_arrow(duckdb_connection connection, struct ArrowSchema *schema, duckdb_arrow_converted_schema *out_types);
duckdb_error_data duckdb_data_chunk_from_arrow(duckdb_connection connection, struct ArrowArray *arrow_array, duckdb_arrow_converted_schema converted_schema, duckdb_data_chunk *out_chunk);
void duckdb_destroy_arrow_converted_schema(duckdb_arrow_converted_schema *arrow_converted_schema);
duckdb_state duckdb_query_arrow(duckdb_connection connection, const char *query, duckdb_arrow *out_result);
duckdb_state duckdb_query_arrow_schema(duckdb_arrow result, duckdb_arrow_schema *out_schema);
duckdb_state duckdb_prepared_arrow_schema(duckdb_prepared_statement prepared, duckdb_arrow_schema *out_schema);
void duckdb_result_arrow_array(duckdb_result result, duckdb_data_chunk chunk, duckdb_arrow_array *out_array);
duckdb_state duckdb_query_arrow_array(duckdb_arrow result, duckdb_arrow_array *out_array);
idx_t duckdb_arrow_column_count(duckdb_arrow result);
idx_t duckdb_arrow_row_count(duckdb_arrow result);
idx_t duckdb_arrow_rows_changed(duckdb_arrow result);
const char *duckdb_query_arrow_error(duckdb_arrow result);
void duckdb_destroy_arrow(duckdb_arrow *result);
void duckdb_destroy_arrow_stream(duckdb_arrow_stream *stream_p);
duckdb_state duckdb_execute_prepared_arrow(duckdb_prepared_statement prepared_statement, duckdb_arrow *out_result);
duckdb_state duckdb_arrow_scan(duckdb_connection connection, const char *table_name, duckdb_arrow_stream arrow);
duckdb_state duckdb_arrow_array_scan(duckdb_connection connection, const char *table_name, duckdb_arrow_schema arrow_schema, duckdb_arrow_array arrow_array, duckdb_arrow_stream *out_stream);

### Threading Information

void duckdb_execute_tasks(duckdb_database database, idx_t max_tasks);
duckdb_task_state duckdb_create_task_state(duckdb_database database);
void duckdb_execute_tasks_state(duckdb_task_state state);
idx_t duckdb_execute_n_tasks_state(duckdb_task_state state, idx_t max_tasks);
void duckdb_finish_execution(duckdb_task_state state);
bool duckdb_task_state_is_finished(duckdb_task_state state);
void duckdb_destroy_task_state(duckdb_task_state state);
bool duckdb_execution_is_finished(duckdb_connection con);

### Streaming Result Interface

duckdb_data_chunk duckdb_stream_fetch_chunk(duckdb_result result);
duckdb_data_chunk duckdb_fetch_chunk(duckdb_result result);

### Cast Functions

duckdb_cast_function duckdb_create_cast_function();
void duckdb_cast_function_set_source_type(duckdb_cast_function cast_function, duckdb_logical_type source_type);
void duckdb_cast_function_set_target_type(duckdb_cast_function cast_function, duckdb_logical_type target_type);
void duckdb_cast_function_set_implicit_cast_cost(duckdb_cast_function cast_function, int64_t cost);
void duckdb_cast_function_set_function(duckdb_cast_function cast_function, duckdb_cast_function_t function);
void duckdb_cast_function_set_extra_info(duckdb_cast_function cast_function, void *extra_info, duckdb_delete_callback_t destroy);
void *duckdb_cast_function_get_extra_info(duckdb_function_info info);
duckdb_cast_mode duckdb_cast_function_get_cast_mode(duckdb_function_info info);
void duckdb_cast_function_set_error(duckdb_function_info info, const char *error);
void duckdb_cast_function_set_row_error(duckdb_function_info info, const char *error, idx_t row, duckdb_vector output);
duckdb_state duckdb_register_cast_function(duckdb_connection con, duckdb_cast_function cast_function);
void duckdb_destroy_cast_function(duckdb_cast_function *cast_function);

### Expression Interface

void duckdb_destroy_expression(duckdb_expression *expr);
duckdb_logical_type duckdb_expression_return_type(duckdb_expression expr);
bool duckdb_expression_is_foldable(duckdb_expression expr);
duckdb_error_data duckdb_expression_fold(duckdb_client_context context, duckdb_expression expr, duckdb_value *out_value);

#### `duckdb_create_instance_cache`

Creates a new database instance cache.
The instance cache is necessary if a client/program (re)opens multiple databases to the same file within the same
process. Must be destroyed with 'duckdb_destroy_instance_cache'.

##### Return Value

The database instance cache.

##### Syntax

duckdb_instance_cache duckdb_create_instance_cache(

);

#### `duckdb_get_or_create_from_cache`

Creates a new database instance in the instance cache, or retrieves an existing database instance.
Must be closed with 'duckdb_close'.

##### Syntax

duckdb_state duckdb_get_or_create_from_cache(
  duckdb_instance_cache instance_cache,
  const char *path,
  duckdb_database *out_database,
  duckdb_config config,
  char **out_error
);

##### Parameters

* `instance_cache`: The instance cache in which to create the database, or from which to take the database.
* `path`: Path to the database file on disk. Both `nullptr` and `:memory:` open or retrieve an in-memory database.
* `out_database`: The resulting cached database.
* `config`: (Optional) configuration used to create the database.
* `out_error`: If set and the function returns `DuckDBError`, this contains the error message.
Note that the error message must be freed using `duckdb_free`.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_destroy_instance_cache`

Destroys an existing database instance cache and de-allocates its memory.

##### Syntax

void duckdb_destroy_instance_cache(
  duckdb_instance_cache *instance_cache
);

##### Parameters

* `instance_cache`: The instance cache to destroy.

#### `duckdb_open`

Creates a new database or opens an existing database file stored at the given path.
If no path is given a new in-memory database is created instead.
The database must be closed with 'duckdb_close'.

##### Syntax

duckdb_state duckdb_open(
  const char *path,
  duckdb_database *out_database
);

##### Parameters

* `path`: Path to the database file on disk. Both `nullptr` and `:memory:` open an in-memory database.
* `out_database`: The result database object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_open_ext`

Extended version of duckdb_open. Creates a new database or opens an existing database file stored at the given path.
The database must be closed with 'duckdb_close'.

##### Syntax

duckdb_state duckdb_open_ext(
  const char *path,
  duckdb_database *out_database,
  duckdb_config config,
  char **out_error
);

##### Parameters

* `path`: Path to the database file on disk. Both `nullptr` and `:memory:` open an in-memory database.
* `out_database`: The result database object.
* `config`: (Optional) configuration used to start up the database.
* `out_error`: If set and the function returns `DuckDBError`, this contains the error message.
Note that the error message must be freed using `duckdb_free`.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_close`

Closes the specified database and de-allocates all memory allocated for that database.
This should be called after you are done with any database allocated through `duckdb_open` or `duckdb_open_ext`.
Note that failing to call `duckdb_close` (in case of e.g., a program crash) will not cause data corruption.
Still, it is recommended to always correctly close a database object after you are done with it.

##### Syntax

void duckdb_close(
  duckdb_database *database
);

##### Parameters

* `database`: The database object to shut down.

#### `duckdb_connect`

Opens a connection to a database. Connections are required to query the database, and store transactional state
associated with the connection.
The instantiated connection should be closed using 'duckdb_disconnect'.

##### Syntax

duckdb_state duckdb_connect(
  duckdb_database database,
  duckdb_connection *out_connection
);

##### Parameters

* `database`: The database file to connect to.
* `out_connection`: The result connection object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_interrupt`

Interrupt running query

##### Syntax

void duckdb_interrupt(
  duckdb_connection connection
);

##### Parameters

* `connection`: The connection to interrupt

#### `duckdb_query_progress`

Get progress of the running query

##### Syntax

duckdb_query_progress_type duckdb_query_progress(
  duckdb_connection connection
);

##### Parameters

* `connection`: The working connection

##### Return Value

-1 if no progress or a percentage of the progress

#### `duckdb_disconnect`

Closes the specified connection and de-allocates all memory allocated for that connection.

##### Syntax

void duckdb_disconnect(
  duckdb_connection *connection
);

##### Parameters

* `connection`: The connection to close.

#### `duckdb_connection_get_client_context`

Retrieves the client context of the connection.

##### Syntax

void duckdb_connection_get_client_context(
  duckdb_connection connection,
  duckdb_client_context *out_context
);

##### Parameters

* `connection`: The connection.
* `out_context`: The client context of the connection. Must be destroyed with `duckdb_destroy_client_context`.

#### `duckdb_connection_get_arrow_options`

Retrieves the arrow options of the connection.

##### Syntax

void duckdb_connection_get_arrow_options(
  duckdb_connection connection,
  duckdb_arrow_options *out_arrow_options
);

##### Parameters

* `connection`: The connection.

#### `duckdb_client_context_get_connection_id`

Returns the connection id of the client context.

##### Syntax

idx_t duckdb_client_context_get_connection_id(
  duckdb_client_context context
);

##### Parameters

* `context`: The client context.

##### Return Value

The connection id of the client context.

#### `duckdb_destroy_client_context`

Destroys the client context and deallocates its memory.

##### Syntax

void duckdb_destroy_client_context(
  duckdb_client_context *context
);

##### Parameters

* `context`: The client context to destroy.

#### `duckdb_destroy_arrow_options`

Destroys the arrow options and deallocates its memory.

##### Syntax

void duckdb_destroy_arrow_options(
  duckdb_arrow_options *arrow_options
);

##### Parameters

* `arrow_options`: The arrow options to destroy.

#### `duckdb_library_version`

Returns the version of the linked DuckDB, with a version postfix for dev versions

Usually used for developing C extensions that must return this for a compatibility check.

##### Syntax

const char *duckdb_library_version(

);

#### `duckdb_get_table_names`

Get the list of (fully qualified) table names of the query.

##### Syntax

duckdb_value duckdb_get_table_names(
  duckdb_connection connection,
  const char *query,
  bool qualified
);

##### Parameters

* `connection`: The connection for which to get the table names.
* `query`: The query for which to get the table names.
* `qualified`: Returns fully qualified table names (catalog.schema.table), if set to true, else only the (not
escaped) table names.

##### Return Value

A duckdb_value of type VARCHAR[] containing the (fully qualified) table names of the query. Must be destroyed
with duckdb_destroy_value.

#### `duckdb_create_config`

Initializes an empty configuration object that can be used to provide start-up options for the DuckDB instance
through `duckdb_open_ext`.
The duckdb_config must be destroyed using 'duckdb_destroy_config'

This will always succeed unless there is a malloc failure.

Note that `duckdb_destroy_config` should always be called on the resulting config, even if the function returns
`DuckDBError`.

##### Syntax

duckdb_state duckdb_create_config(
  duckdb_config *out_config
);

##### Parameters

* `out_config`: The result configuration object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_config_count`

This returns the total amount of configuration options available for usage with `duckdb_get_config_flag`.

This should not be called in a loop as it internally loops over all the options.

##### Return Value

The amount of config options available.

##### Syntax

size_t duckdb_config_count(

);

#### `duckdb_get_config_flag`

Obtains a human-readable name and description of a specific configuration option. This can be used to e.g.
display configuration options. This will succeed unless `index` is out of range (i.e., `>= duckdb_config_count`).

The result name or description MUST NOT be freed.

##### Syntax

duckdb_state duckdb_get_config_flag(
  size_t index,
  const char **out_name,
  const char **out_description
);

##### Parameters

* `index`: The index of the configuration option (between 0 and `duckdb_config_count`)
* `out_name`: A name of the configuration flag.
* `out_description`: A description of the configuration flag.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_set_config`

Sets the specified option for the specified configuration. The configuration option is indicated by name.
To obtain a list of config options, see `duckdb_get_config_flag`.

In the source code, configuration options are defined in `config.cpp`.

This can fail if either the name is invalid, or if the value provided for the option is invalid.

##### Syntax

duckdb_state duckdb_set_config(
  duckdb_config config,
  const char *name,
  const char *option
);

##### Parameters

* `config`: The configuration object to set the option on.
* `name`: The name of the configuration flag to set.
* `option`: The value to set the configuration flag to.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_destroy_config`

Destroys the specified configuration object and de-allocates all memory allocated for the object.

##### Syntax

void duckdb_destroy_config(
  duckdb_config *config
);

##### Parameters

* `config`: The configuration object to destroy.

#### `duckdb_create_error_data`

Creates duckdb_error_data.
Must be destroyed with `duckdb_destroy_error_data`.

##### Syntax

duckdb_error_data duckdb_create_error_data(
  duckdb_error_type type,
  const char *message
);

##### Parameters

* `type`: The error type.
* `message`: The error message.

##### Return Value

The error data.

#### `duckdb_destroy_error_data`

Destroys the error data and deallocates its memory.

##### Syntax

void duckdb_destroy_error_data(
  duckdb_error_data *error_data
);

##### Parameters

* `error_data`: The error data to destroy.

#### `duckdb_error_data_error_type`

Returns the duckdb_error_type of the error data.

##### Syntax

duckdb_error_type duckdb_error_data_error_type(
  duckdb_error_data error_data
);

##### Parameters

* `error_data`: The error data.

##### Return Value

The error type.

#### `duckdb_error_data_message`

Returns the error message of the error data. Must not be freed.

##### Syntax

const char *duckdb_error_data_message(
  duckdb_error_data error_data
);

##### Parameters

* `error_data`: The error data.

##### Return Value

The error message.

#### `duckdb_error_data_has_error`

Returns whether the error data contains an error or not.

##### Syntax

bool duckdb_error_data_has_error(
  duckdb_error_data error_data
);

##### Parameters

* `error_data`: The error data.

##### Return Value

True, if the error data contains an exception, else false.

#### `duckdb_query`

Executes a SQL query within a connection and stores the full (materialized) result in the out_result pointer.
If the query fails to execute, DuckDBError is returned and the error message can be retrieved by calling
`duckdb_result_error`.

Note that after running `duckdb_query`, `duckdb_destroy_result` must be called on the result object even if the
query fails, otherwise the error stored within the result will not be freed correctly.

##### Syntax

duckdb_state duckdb_query(
  duckdb_connection connection,
  const char *query,
  duckdb_result *out_result
);

##### Parameters

* `connection`: The connection to perform the query in.
* `query`: The SQL query to run.
* `out_result`: The query result.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_destroy_result`

Closes the result and de-allocates all memory allocated for that result.

##### Syntax

void duckdb_destroy_result(
  duckdb_result *result
);

##### Parameters

* `result`: The result to destroy.

#### `duckdb_column_name`

Returns the column name of the specified column. The result should not need to be freed; the column names will
automatically be destroyed when the result is destroyed.

Returns `NULL` if the column is out of range.

##### Syntax

const char *duckdb_column_name(
  duckdb_result *result,
  idx_t col
);

##### Parameters

* `result`: The result object to fetch the column name from.
* `col`: The column index.

##### Return Value

The column name of the specified column.

#### `duckdb_column_type`

Returns the column type of the specified column.

Returns `DUCKDB_TYPE_INVALID` if the column is out of range.

##### Syntax

duckdb_type duckdb_column_type(
  duckdb_result *result,
  idx_t col
);

##### Parameters

* `result`: The result object to fetch the column type from.
* `col`: The column index.

##### Return Value

The column type of the specified column.

#### `duckdb_result_statement_type`

Returns the statement type of the statement that was executed

##### Syntax

duckdb_statement_type duckdb_result_statement_type(
  duckdb_result result
);

##### Parameters

* `result`: The result object to fetch the statement type from.

##### Return Value

duckdb_statement_type value or DUCKDB_STATEMENT_TYPE_INVALID

#### `duckdb_column_logical_type`

Returns the logical column type of the specified column.

The return type of this call should be destroyed with `duckdb_destroy_logical_type`.

Returns `NULL` if the column is out of range.

##### Syntax

duckdb_logical_type duckdb_column_logical_type(
  duckdb_result *result,
  idx_t col
);

##### Parameters

* `result`: The result object to fetch the column type from.
* `col`: The column index.

##### Return Value

The logical column type of the specified column.

#### `duckdb_result_get_arrow_options`

Returns the arrow options associated with the given result. These options are definitions of how the arrow arrays/schema
should be produced.

##### Syntax

duckdb_arrow_options duckdb_result_get_arrow_options(
  duckdb_result *result
);

##### Parameters

* `result`: The result object to fetch arrow options from.

##### Return Value

The arrow options associated with the given result. This must be destroyed with
`duckdb_destroy_arrow_options`.

#### `duckdb_column_count`

Returns the number of columns present in a the result object.

##### Syntax

idx_t duckdb_column_count(
  duckdb_result *result
);

##### Parameters

* `result`: The result object.

##### Return Value

The number of columns present in the result object.

#### `duckdb_row_count`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Returns the number of rows present in the result object.

##### Syntax

idx_t duckdb_row_count(
  duckdb_result *result
);

##### Parameters

* `result`: The result object.

##### Return Value

The number of rows present in the result object.

#### `duckdb_rows_changed`

Returns the number of rows changed by the query stored in the result. This is relevant only for INSERT/UPDATE/DELETE
queries. For other queries the rows_changed will be 0.

##### Syntax

idx_t duckdb_rows_changed(
  duckdb_result *result
);

##### Parameters

* `result`: The result object.

##### Return Value

The number of rows changed.

#### `duckdb_column_data`

> Deprecated This method has been deprecated. Prefer using `duckdb_result_get_chunk` instead.

Returns the data of a specific column of a result in columnar format.

The function returns a dense array which contains the result data. The exact type stored in the array depends on the
corresponding duckdb_type (as provided by `duckdb_column_type`). For the exact type by which the data should be
accessed, see the comments in [the types section](types) or the `DUCKDB_TYPE` enum.

For example, for a column of type `DUCKDB_TYPE_INTEGER`, rows can be accessed in the following manner:
```c
int32_t *data = (int32_t *) duckdb_column_data(&result, 0);
printf("Data for row %d: %d\n", row, data[row]);
```

##### Syntax

void *duckdb_column_data(
  duckdb_result *result,
  idx_t col
);

##### Parameters

* `result`: The result object to fetch the column data from.
* `col`: The column index.

##### Return Value

The column data of the specified column.

#### `duckdb_nullmask_data`

> Deprecated This method has been deprecated. Prefer using `duckdb_result_get_chunk` instead.

Returns the nullmask of a specific column of a result in columnar format. The nullmask indicates for every row
whether or not the corresponding row is `NULL`. If a row is `NULL`, the values present in the array provided
by `duckdb_column_data` are undefined.

```c
int32_t *data = (int32_t *) duckdb_column_data(&result, 0);
bool *nullmask = duckdb_nullmask_data(&result, 0);
if (nullmask[row]) {
    printf("Data for row %d: NULL\n", row);
} else {
    printf("Data for row %d: %d\n", row, data[row]);
}
```

##### Syntax

bool *duckdb_nullmask_data(
  duckdb_result *result,
  idx_t col
);

##### Parameters

* `result`: The result object to fetch the nullmask from.
* `col`: The column index.

##### Return Value

The nullmask of the specified column.

#### `duckdb_result_error`

Returns the error message contained within the result. The error is only set if `duckdb_query` returns `DuckDBError`.

The result of this function must not be freed. It will be cleaned up when `duckdb_destroy_result` is called.

##### Syntax

const char *duckdb_result_error(
  duckdb_result *result
);

##### Parameters

* `result`: The result object to fetch the error from.

##### Return Value

The error of the result.

#### `duckdb_result_error_type`

Returns the result error type contained within the result. The error is only set if `duckdb_query` returns
`DuckDBError`.

##### Syntax

duckdb_error_type duckdb_result_error_type(
  duckdb_result *result
);

##### Parameters

* `result`: The result object to fetch the error from.

##### Return Value

The error type of the result.

#### `duckdb_result_get_chunk`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Fetches a data chunk from the duckdb_result. This function should be called repeatedly until the result is exhausted.

The result must be destroyed with `duckdb_destroy_data_chunk`.

This function supersedes all `duckdb_value` functions, as well as the `duckdb_column_data` and `duckdb_nullmask_data`
functions. It results in significantly better performance, and should be preferred in newer code-bases.

If this function is used, none of the other result functions can be used and vice versa (i.e., this function cannot be
mixed with the legacy result functions).

Use `duckdb_result_chunk_count` to figure out how many chunks there are in the result.

##### Syntax

duckdb_data_chunk duckdb_result_get_chunk(
  duckdb_result result,
  idx_t chunk_index
);

##### Parameters

* `result`: The result object to fetch the data chunk from.
* `chunk_index`: The chunk index to fetch from.

##### Return Value

The resulting data chunk. Returns `NULL` if the chunk index is out of bounds.

#### `duckdb_result_is_streaming`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Checks if the type of the internal result is StreamQueryResult.

##### Syntax

bool duckdb_result_is_streaming(
  duckdb_result result
);

##### Parameters

* `result`: The result object to check.

##### Return Value

Whether or not the result object is of the type StreamQueryResult

#### `duckdb_result_chunk_count`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Returns the number of data chunks present in the result.

##### Syntax

idx_t duckdb_result_chunk_count(
  duckdb_result result
);

##### Parameters

* `result`: The result object

##### Return Value

Number of data chunks present in the result.

#### `duckdb_result_return_type`

Returns the return_type of the given result, or DUCKDB_RETURN_TYPE_INVALID on error

##### Syntax

duckdb_result_type duckdb_result_return_type(
  duckdb_result result
);

##### Parameters

* `result`: The result object

##### Return Value

The return_type

#### `duckdb_value_boolean`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The boolean value at the specified location, or false if the value cannot be converted.

##### Syntax

bool duckdb_value_boolean(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_int8`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The int8_t value at the specified location, or 0 if the value cannot be converted.

##### Syntax

int8_t duckdb_value_int8(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_int16`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The int16_t value at the specified location, or 0 if the value cannot be converted.

##### Syntax

int16_t duckdb_value_int16(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_int32`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The int32_t value at the specified location, or 0 if the value cannot be converted.

##### Syntax

int32_t duckdb_value_int32(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_int64`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The int64_t value at the specified location, or 0 if the value cannot be converted.

##### Syntax

int64_t duckdb_value_int64(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_hugeint`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The duckdb_hugeint value at the specified location, or 0 if the value cannot be converted.

##### Syntax

duckdb_hugeint duckdb_value_hugeint(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_uhugeint`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The duckdb_uhugeint value at the specified location, or 0 if the value cannot be converted.

##### Syntax

duckdb_uhugeint duckdb_value_uhugeint(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_decimal`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The duckdb_decimal value at the specified location, or 0 if the value cannot be converted.

##### Syntax

duckdb_decimal duckdb_value_decimal(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_uint8`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The uint8_t value at the specified location, or 0 if the value cannot be converted.

##### Syntax

uint8_t duckdb_value_uint8(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_uint16`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The uint16_t value at the specified location, or 0 if the value cannot be converted.

##### Syntax

uint16_t duckdb_value_uint16(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_uint32`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The uint32_t value at the specified location, or 0 if the value cannot be converted.

##### Syntax

uint32_t duckdb_value_uint32(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_uint64`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The uint64_t value at the specified location, or 0 if the value cannot be converted.

##### Syntax

uint64_t duckdb_value_uint64(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_float`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The float value at the specified location, or 0 if the value cannot be converted.

##### Syntax

float duckdb_value_float(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_double`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The double value at the specified location, or 0 if the value cannot be converted.

##### Syntax

double duckdb_value_double(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_date`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The duckdb_date value at the specified location, or 0 if the value cannot be converted.

##### Syntax

duckdb_date duckdb_value_date(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_time`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The duckdb_time value at the specified location, or 0 if the value cannot be converted.

##### Syntax

duckdb_time duckdb_value_time(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_timestamp`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The duckdb_timestamp value at the specified location, or 0 if the value cannot be converted.

##### Syntax

duckdb_timestamp duckdb_value_timestamp(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_interval`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The duckdb_interval value at the specified location, or 0 if the value cannot be converted.

##### Syntax

duckdb_interval duckdb_value_interval(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_varchar`

> Deprecated This method has been deprecated. Use duckdb_value_string instead. This function does not work correctly if the string contains null
bytes.

##### Return Value

The text value at the specified location as a null-terminated string, or nullptr if the value cannot be
converted. The result must be freed with `duckdb_free`.

##### Syntax

char *duckdb_value_varchar(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_string`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

No support for nested types, and for other complex types.
The resulting field "string.data" must be freed with `duckdb_free.`

##### Return Value

The string value at the specified location. Attempts to cast the result value to string.

##### Syntax

duckdb_string duckdb_value_string(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_varchar_internal`

> Deprecated This method has been deprecated. Use duckdb_value_string_internal instead. This function does not work correctly if the string contains
null bytes.

##### Return Value

The char* value at the specified location. ONLY works on VARCHAR columns and does not auto-cast.
If the column is NOT a VARCHAR column this function will return NULL.

The result must NOT be freed.

##### Syntax

char *duckdb_value_varchar_internal(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_string_internal`

> Deprecated This method has been deprecated. Use duckdb_value_string_internal instead. This function does not work correctly if the string contains
null bytes.

##### Return Value

The char* value at the specified location. ONLY works on VARCHAR columns and does not auto-cast.
If the column is NOT a VARCHAR column this function will return NULL.

The result must NOT be freed.

##### Syntax

duckdb_string duckdb_value_string_internal(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_blob`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

The duckdb_blob value at the specified location. Returns a blob with blob.data set to nullptr if the
value cannot be converted. The resulting field "blob.data" must be freed with `duckdb_free.`

##### Syntax

duckdb_blob duckdb_value_blob(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_value_is_null`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

##### Return Value

Returns true if the value at the specified index is NULL, and false otherwise.

##### Syntax

bool duckdb_value_is_null(
  duckdb_result *result,
  idx_t col,
  idx_t row
);

#### `duckdb_malloc`

Allocate `size` bytes of memory using the duckdb internal malloc function. Any memory allocated in this manner
should be freed using `duckdb_free`.

##### Syntax

void *duckdb_malloc(
  size_t size
);

##### Parameters

* `size`: The number of bytes to allocate.

##### Return Value

A pointer to the allocated memory region.

#### `duckdb_free`

Free a value returned from `duckdb_malloc`, `duckdb_value_varchar`, `duckdb_value_blob`, or
`duckdb_value_string`.

##### Syntax

void duckdb_free(
  void *ptr
);

##### Parameters

* `ptr`: The memory region to de-allocate.

#### `duckdb_vector_size`

The internal vector size used by DuckDB.
This is the amount of tuples that will fit into a data chunk created by `duckdb_create_data_chunk`.

##### Return Value

The vector size.

##### Syntax

idx_t duckdb_vector_size(

);

#### `duckdb_string_is_inlined`

Whether or not the duckdb_string_t value is inlined.
This means that the data of the string does not have a separate allocation.

##### Syntax

bool duckdb_string_is_inlined(
  duckdb_string_t string
);

#### `duckdb_string_t_length`

Get the string length of a string_t

##### Syntax

uint32_t duckdb_string_t_length(
  duckdb_string_t string
);

##### Parameters

* `string`: The string to get the length of.

##### Return Value

The length.

#### `duckdb_string_t_data`

Get a pointer to the string data of a string_t

##### Syntax

const char *duckdb_string_t_data(
  duckdb_string_t *string
);

##### Parameters

* `string`: The string to get the pointer to.

##### Return Value

The pointer.

#### `duckdb_from_date`

Decompose a `duckdb_date` object into year, month and date (stored as `duckdb_date_struct`).

##### Syntax

duckdb_date_struct duckdb_from_date(
  duckdb_date date
);

##### Parameters

* `date`: The date object, as obtained from a `DUCKDB_TYPE_DATE` column.

##### Return Value

The `duckdb_date_struct` with the decomposed elements.

#### `duckdb_to_date`

Re-compose a `duckdb_date` from year, month and date (`duckdb_date_struct`).

##### Syntax

duckdb_date duckdb_to_date(
  duckdb_date_struct date
);

##### Parameters

* `date`: The year, month and date stored in a `duckdb_date_struct`.

##### Return Value

The `duckdb_date` element.

#### `duckdb_is_finite_date`

Test a `duckdb_date` to see if it is a finite value.

##### Syntax

bool duckdb_is_finite_date(
  duckdb_date date
);

##### Parameters

* `date`: The date object, as obtained from a `DUCKDB_TYPE_DATE` column.

##### Return Value

True if the date is finite, false if it is ±infinity.

#### `duckdb_from_time`

Decompose a `duckdb_time` object into hour, minute, second and microsecond (stored as `duckdb_time_struct`).

##### Syntax

duckdb_time_struct duckdb_from_time(
  duckdb_time time
);

##### Parameters

* `time`: The time object, as obtained from a `DUCKDB_TYPE_TIME` column.

##### Return Value

The `duckdb_time_struct` with the decomposed elements.

#### `duckdb_create_time_tz`

Create a `duckdb_time_tz` object from micros and a timezone offset.

##### Syntax

duckdb_time_tz duckdb_create_time_tz(
  int64_t micros,
  int32_t offset
);

##### Parameters

* `micros`: The microsecond component of the time.
* `offset`: The timezone offset component of the time.

##### Return Value

The `duckdb_time_tz` element.

#### `duckdb_from_time_tz`

Decompose a TIME_TZ objects into micros and a timezone offset.

Use `duckdb_from_time` to further decompose the micros into hour, minute, second and microsecond.

##### Syntax

duckdb_time_tz_struct duckdb_from_time_tz(
  duckdb_time_tz micros
);

##### Parameters

* `micros`: The time object, as obtained from a `DUCKDB_TYPE_TIME_TZ` column.

#### `duckdb_to_time`

Re-compose a `duckdb_time` from hour, minute, second and microsecond (`duckdb_time_struct`).

##### Syntax

duckdb_time duckdb_to_time(
  duckdb_time_struct time
);

##### Parameters

* `time`: The hour, minute, second and microsecond in a `duckdb_time_struct`.

##### Return Value

The `duckdb_time` element.

#### `duckdb_from_timestamp`

Decompose a `duckdb_timestamp` object into a `duckdb_timestamp_struct`.

##### Syntax

duckdb_timestamp_struct duckdb_from_timestamp(
  duckdb_timestamp ts
);

##### Parameters

* `ts`: The ts object, as obtained from a `DUCKDB_TYPE_TIMESTAMP` column.

##### Return Value

The `duckdb_timestamp_struct` with the decomposed elements.

#### `duckdb_to_timestamp`

Re-compose a `duckdb_timestamp` from a duckdb_timestamp_struct.

##### Syntax

duckdb_timestamp duckdb_to_timestamp(
  duckdb_timestamp_struct ts
);

##### Parameters

* `ts`: The de-composed elements in a `duckdb_timestamp_struct`.

##### Return Value

The `duckdb_timestamp` element.

#### `duckdb_is_finite_timestamp`

Test a `duckdb_timestamp` to see if it is a finite value.

##### Syntax

bool duckdb_is_finite_timestamp(
  duckdb_timestamp ts
);

##### Parameters

* `ts`: The duckdb_timestamp object, as obtained from a `DUCKDB_TYPE_TIMESTAMP` column.

##### Return Value

True if the timestamp is finite, false if it is ±infinity.

#### `duckdb_is_finite_timestamp_s`

Test a `duckdb_timestamp_s` to see if it is a finite value.

##### Syntax

bool duckdb_is_finite_timestamp_s(
  duckdb_timestamp_s ts
);

##### Parameters

* `ts`: The duckdb_timestamp_s object, as obtained from a `DUCKDB_TYPE_TIMESTAMP_S` column.

##### Return Value

True if the timestamp is finite, false if it is ±infinity.

#### `duckdb_is_finite_timestamp_ms`

Test a `duckdb_timestamp_ms` to see if it is a finite value.

##### Syntax

bool duckdb_is_finite_timestamp_ms(
  duckdb_timestamp_ms ts
);

##### Parameters

* `ts`: The duckdb_timestamp_ms object, as obtained from a `DUCKDB_TYPE_TIMESTAMP_MS` column.

##### Return Value

True if the timestamp is finite, false if it is ±infinity.

#### `duckdb_is_finite_timestamp_ns`

Test a `duckdb_timestamp_ns` to see if it is a finite value.

##### Syntax

bool duckdb_is_finite_timestamp_ns(
  duckdb_timestamp_ns ts
);

##### Parameters

* `ts`: The duckdb_timestamp_ns object, as obtained from a `DUCKDB_TYPE_TIMESTAMP_NS` column.

##### Return Value

True if the timestamp is finite, false if it is ±infinity.

#### `duckdb_hugeint_to_double`

Converts a duckdb_hugeint object (as obtained from a `DUCKDB_TYPE_HUGEINT` column) into a double.

##### Syntax

double duckdb_hugeint_to_double(
  duckdb_hugeint val
);

##### Parameters

* `val`: The hugeint value.

##### Return Value

The converted `double` element.

#### `duckdb_double_to_hugeint`

Converts a double value to a duckdb_hugeint object.

If the conversion fails because the double value is too big the result will be 0.

##### Syntax

duckdb_hugeint duckdb_double_to_hugeint(
  double val
);

##### Parameters

* `val`: The double value.

##### Return Value

The converted `duckdb_hugeint` element.

#### `duckdb_uhugeint_to_double`

Converts a duckdb_uhugeint object (as obtained from a `DUCKDB_TYPE_UHUGEINT` column) into a double.

##### Syntax

double duckdb_uhugeint_to_double(
  duckdb_uhugeint val
);

##### Parameters

* `val`: The uhugeint value.

##### Return Value

The converted `double` element.

#### `duckdb_double_to_uhugeint`

Converts a double value to a duckdb_uhugeint object.

If the conversion fails because the double value is too big the result will be 0.

##### Syntax

duckdb_uhugeint duckdb_double_to_uhugeint(
  double val
);

##### Parameters

* `val`: The double value.

##### Return Value

The converted `duckdb_uhugeint` element.

#### `duckdb_double_to_decimal`

Converts a double value to a duckdb_decimal object.

If the conversion fails because the double value is too big, or the width/scale are invalid the result will be 0.

##### Syntax

duckdb_decimal duckdb_double_to_decimal(
  double val,
  uint8_t width,
  uint8_t scale
);

##### Parameters

* `val`: The double value.

##### Return Value

The converted `duckdb_decimal` element.

#### `duckdb_decimal_to_double`

Converts a duckdb_decimal object (as obtained from a `DUCKDB_TYPE_DECIMAL` column) into a double.

##### Syntax

double duckdb_decimal_to_double(
  duckdb_decimal val
);

##### Parameters

* `val`: The decimal value.

##### Return Value

The converted `double` element.

#### `duckdb_prepare`

Create a prepared statement object from a query.

Note that after calling `duckdb_prepare`, the prepared statement should always be destroyed using
`duckdb_destroy_prepare`, even if the prepare fails.

If the prepare fails, `duckdb_prepare_error` can be called to obtain the reason why the prepare failed.

##### Syntax

duckdb_state duckdb_prepare(
  duckdb_connection connection,
  const char *query,
  duckdb_prepared_statement *out_prepared_statement
);

##### Parameters

* `connection`: The connection object
* `query`: The SQL query to prepare
* `out_prepared_statement`: The resulting prepared statement object

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_destroy_prepare`

Closes the prepared statement and de-allocates all memory allocated for the statement.

##### Syntax

void duckdb_destroy_prepare(
  duckdb_prepared_statement *prepared_statement
);

##### Parameters

* `prepared_statement`: The prepared statement to destroy.

#### `duckdb_prepare_error`

Returns the error message associated with the given prepared statement.
If the prepared statement has no error message, this returns `nullptr` instead.

The error message should not be freed. It will be de-allocated when `duckdb_destroy_prepare` is called.

##### Syntax

const char *duckdb_prepare_error(
  duckdb_prepared_statement prepared_statement
);

##### Parameters

* `prepared_statement`: The prepared statement to obtain the error from.

##### Return Value

The error message, or `nullptr` if there is none.

#### `duckdb_nparams`

Returns the number of parameters that can be provided to the given prepared statement.

Returns 0 if the query was not successfully prepared.

##### Syntax

idx_t duckdb_nparams(
  duckdb_prepared_statement prepared_statement
);

##### Parameters

* `prepared_statement`: The prepared statement to obtain the number of parameters for.

#### `duckdb_parameter_name`

Returns the name used to identify the parameter
The returned string should be freed using `duckdb_free`.

Returns NULL if the index is out of range for the provided prepared statement.

##### Syntax

const char *duckdb_parameter_name(
  duckdb_prepared_statement prepared_statement,
  idx_t index
);

##### Parameters

* `prepared_statement`: The prepared statement for which to get the parameter name from.

#### `duckdb_param_type`

Returns the parameter type for the parameter at the given index.

Returns `DUCKDB_TYPE_INVALID` if the parameter index is out of range or the statement was not successfully prepared.

##### Syntax

duckdb_type duckdb_param_type(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx
);

##### Parameters

* `prepared_statement`: The prepared statement.
* `param_idx`: The parameter index.

##### Return Value

The parameter type

#### `duckdb_param_logical_type`

Returns the logical type for the parameter at the given index.

Returns `nullptr` if the parameter index is out of range or the statement was not successfully prepared.

The return type of this call should be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_param_logical_type(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx
);

##### Parameters

* `prepared_statement`: The prepared statement.
* `param_idx`: The parameter index.

##### Return Value

The logical type of the parameter

#### `duckdb_clear_bindings`

Clear the params bind to the prepared statement.

##### Syntax

duckdb_state duckdb_clear_bindings(
  duckdb_prepared_statement prepared_statement
);

#### `duckdb_prepared_statement_type`

Returns the statement type of the statement to be executed

##### Syntax

duckdb_statement_type duckdb_prepared_statement_type(
  duckdb_prepared_statement statement
);

##### Parameters

* `statement`: The prepared statement.

##### Return Value

duckdb_statement_type value or DUCKDB_STATEMENT_TYPE_INVALID

#### `duckdb_prepared_statement_column_count`

Returns the number of columns present in a the result of the prepared statement. If any of the column types are invalid,
the result will be 1.

##### Syntax

idx_t duckdb_prepared_statement_column_count(
  duckdb_prepared_statement prepared_statement
);

##### Parameters

* `prepared_statement`: The prepared statement.

##### Return Value

The number of columns present in the result of the prepared statement.

#### `duckdb_prepared_statement_column_name`

Returns the name of the specified column of the result of the prepared_statement.
The returned string should be freed using `duckdb_free`.

Returns `nullptr` if the column is out of range.

##### Syntax

const char *duckdb_prepared_statement_column_name(
  duckdb_prepared_statement prepared_statement,
  idx_t col_idx
);

##### Parameters

* `prepared_statement`: The prepared statement.
* `col_idx`: The column index.

##### Return Value

The column name of the specified column.

#### `duckdb_prepared_statement_column_logical_type`

Returns the column type of the specified column of the result of the prepared_statement.

Returns `DUCKDB_TYPE_INVALID` if the column is out of range.
The return type of this call should be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_prepared_statement_column_logical_type(
  duckdb_prepared_statement prepared_statement,
  idx_t col_idx
);

##### Parameters

* `prepared_statement`: The prepared statement to fetch the column type from.
* `col_idx`: The column index.

##### Return Value

The logical type of the specified column.

#### `duckdb_prepared_statement_column_type`

Returns the column type of the specified column of the result of the prepared_statement.

Returns `DUCKDB_TYPE_INVALID` if the column is out of range.

##### Syntax

duckdb_type duckdb_prepared_statement_column_type(
  duckdb_prepared_statement prepared_statement,
  idx_t col_idx
);

##### Parameters

* `prepared_statement`: The prepared statement to fetch the column type from.
* `col_idx`: The column index.

##### Return Value

The type of the specified column.

#### `duckdb_bind_value`

Binds a value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_value(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  duckdb_value val
);

#### `duckdb_bind_parameter_index`

Retrieve the index of the parameter for the prepared statement, identified by name

##### Syntax

duckdb_state duckdb_bind_parameter_index(
  duckdb_prepared_statement prepared_statement,
  idx_t *param_idx_out,
  const char *name
);

#### `duckdb_bind_boolean`

Binds a bool value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_boolean(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  bool val
);

#### `duckdb_bind_int8`

Binds an int8_t value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_int8(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  int8_t val
);

#### `duckdb_bind_int16`

Binds an int16_t value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_int16(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  int16_t val
);

#### `duckdb_bind_int32`

Binds an int32_t value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_int32(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  int32_t val
);

#### `duckdb_bind_int64`

Binds an int64_t value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_int64(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  int64_t val
);

#### `duckdb_bind_hugeint`

Binds a duckdb_hugeint value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_hugeint(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  duckdb_hugeint val
);

#### `duckdb_bind_uhugeint`

Binds a duckdb_uhugeint value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_uhugeint(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  duckdb_uhugeint val
);

#### `duckdb_bind_decimal`

Binds a duckdb_decimal value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_decimal(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  duckdb_decimal val
);

#### `duckdb_bind_uint8`

Binds a uint8_t value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_uint8(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  uint8_t val
);

#### `duckdb_bind_uint16`

Binds a uint16_t value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_uint16(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  uint16_t val
);

#### `duckdb_bind_uint32`

Binds a uint32_t value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_uint32(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  uint32_t val
);

#### `duckdb_bind_uint64`

Binds a uint64_t value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_uint64(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  uint64_t val
);

#### `duckdb_bind_float`

Binds a float value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_float(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  float val
);

#### `duckdb_bind_double`

Binds a double value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_double(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  double val
);

#### `duckdb_bind_date`

Binds a duckdb_date value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_date(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  duckdb_date val
);

#### `duckdb_bind_time`

Binds a duckdb_time value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_time(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  duckdb_time val
);

#### `duckdb_bind_timestamp`

Binds a duckdb_timestamp value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_timestamp(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  duckdb_timestamp val
);

#### `duckdb_bind_timestamp_tz`

Binds a duckdb_timestamp value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_timestamp_tz(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  duckdb_timestamp val
);

#### `duckdb_bind_interval`

Binds a duckdb_interval value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_interval(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  duckdb_interval val
);

#### `duckdb_bind_varchar`

Binds a null-terminated varchar value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_varchar(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  const char *val
);

#### `duckdb_bind_varchar_length`

Binds a varchar value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_varchar_length(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  const char *val,
  idx_t length
);

#### `duckdb_bind_blob`

Binds a blob value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_blob(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx,
  const void *data,
  idx_t length
);

#### `duckdb_bind_null`

Binds a NULL value to the prepared statement at the specified index.

##### Syntax

duckdb_state duckdb_bind_null(
  duckdb_prepared_statement prepared_statement,
  idx_t param_idx
);

#### `duckdb_execute_prepared`

Executes the prepared statement with the given bound parameters, and returns a materialized query result.

This method can be called multiple times for each prepared statement, and the parameters can be modified
between calls to this function.

Note that the result must be freed with `duckdb_destroy_result`.

##### Syntax

duckdb_state duckdb_execute_prepared(
  duckdb_prepared_statement prepared_statement,
  duckdb_result *out_result
);

##### Parameters

* `prepared_statement`: The prepared statement to execute.
* `out_result`: The query result.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_execute_prepared_streaming`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Executes the prepared statement with the given bound parameters, and returns an optionally-streaming query result.
To determine if the resulting query was in fact streamed, use `duckdb_result_is_streaming`

This method can be called multiple times for each prepared statement, and the parameters can be modified
between calls to this function.

Note that the result must be freed with `duckdb_destroy_result`.

##### Syntax

duckdb_state duckdb_execute_prepared_streaming(
  duckdb_prepared_statement prepared_statement,
  duckdb_result *out_result
);

##### Parameters

* `prepared_statement`: The prepared statement to execute.
* `out_result`: The query result.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_extract_statements`

Extract all statements from a query.
Note that after calling `duckdb_extract_statements`, the extracted statements should always be destroyed using
`duckdb_destroy_extracted`, even if no statements were extracted.

If the extract fails, `duckdb_extract_statements_error` can be called to obtain the reason why the extract failed.

##### Syntax

idx_t duckdb_extract_statements(
  duckdb_connection connection,
  const char *query,
  duckdb_extracted_statements *out_extracted_statements
);

##### Parameters

* `connection`: The connection object
* `query`: The SQL query to extract
* `out_extracted_statements`: The resulting extracted statements object

##### Return Value

The number of extracted statements or 0 on failure.

#### `duckdb_prepare_extracted_statement`

Prepare an extracted statement.
Note that after calling `duckdb_prepare_extracted_statement`, the prepared statement should always be destroyed using
`duckdb_destroy_prepare`, even if the prepare fails.

If the prepare fails, `duckdb_prepare_error` can be called to obtain the reason why the prepare failed.

##### Syntax

duckdb_state duckdb_prepare_extracted_statement(
  duckdb_connection connection,
  duckdb_extracted_statements extracted_statements,
  idx_t index,
  duckdb_prepared_statement *out_prepared_statement
);

##### Parameters

* `connection`: The connection object
* `extracted_statements`: The extracted statements object
* `index`: The index of the extracted statement to prepare
* `out_prepared_statement`: The resulting prepared statement object

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_extract_statements_error`

Returns the error message contained within the extracted statements.
The result of this function must not be freed. It will be cleaned up when `duckdb_destroy_extracted` is called.

##### Syntax

const char *duckdb_extract_statements_error(
  duckdb_extracted_statements extracted_statements
);

##### Parameters

* `extracted_statements`: The extracted statements to fetch the error from.

##### Return Value

The error of the extracted statements.

#### `duckdb_destroy_extracted`

De-allocates all memory allocated for the extracted statements.

##### Syntax

void duckdb_destroy_extracted(
  duckdb_extracted_statements *extracted_statements
);

##### Parameters

* `extracted_statements`: The extracted statements to destroy.

#### `duckdb_pending_prepared`

Executes the prepared statement with the given bound parameters, and returns a pending result.
The pending result represents an intermediate structure for a query that is not yet fully executed.
The pending result can be used to incrementally execute a query, returning control to the client between tasks.

Note that after calling `duckdb_pending_prepared`, the pending result should always be destroyed using
`duckdb_destroy_pending`, even if this function returns DuckDBError.

##### Syntax

duckdb_state duckdb_pending_prepared(
  duckdb_prepared_statement prepared_statement,
  duckdb_pending_result *out_result
);

##### Parameters

* `prepared_statement`: The prepared statement to execute.
* `out_result`: The pending query result.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_pending_prepared_streaming`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Executes the prepared statement with the given bound parameters, and returns a pending result.
This pending result will create a streaming duckdb_result when executed.
The pending result represents an intermediate structure for a query that is not yet fully executed.

Note that after calling `duckdb_pending_prepared_streaming`, the pending result should always be destroyed using
`duckdb_destroy_pending`, even if this function returns DuckDBError.

##### Syntax

duckdb_state duckdb_pending_prepared_streaming(
  duckdb_prepared_statement prepared_statement,
  duckdb_pending_result *out_result
);

##### Parameters

* `prepared_statement`: The prepared statement to execute.
* `out_result`: The pending query result.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_destroy_pending`

Closes the pending result and de-allocates all memory allocated for the result.

##### Syntax

void duckdb_destroy_pending(
  duckdb_pending_result *pending_result
);

##### Parameters

* `pending_result`: The pending result to destroy.

#### `duckdb_pending_error`

Returns the error message contained within the pending result.

The result of this function must not be freed. It will be cleaned up when `duckdb_destroy_pending` is called.

##### Syntax

const char *duckdb_pending_error(
  duckdb_pending_result pending_result
);

##### Parameters

* `pending_result`: The pending result to fetch the error from.

##### Return Value

The error of the pending result.

#### `duckdb_pending_execute_task`

Executes a single task within the query, returning whether or not the query is ready.

If this returns DUCKDB_PENDING_RESULT_READY, the duckdb_execute_pending function can be called to obtain the result.
If this returns DUCKDB_PENDING_RESULT_NOT_READY, the duckdb_pending_execute_task function should be called again.
If this returns DUCKDB_PENDING_ERROR, an error occurred during execution.

The error message can be obtained by calling duckdb_pending_error on the pending_result.

##### Syntax

duckdb_pending_state duckdb_pending_execute_task(
  duckdb_pending_result pending_result
);

##### Parameters

* `pending_result`: The pending result to execute a task within.

##### Return Value

The state of the pending result after the execution.

#### `duckdb_pending_execute_check_state`

If this returns DUCKDB_PENDING_RESULT_READY, the duckdb_execute_pending function can be called to obtain the result.
If this returns DUCKDB_PENDING_RESULT_NOT_READY, the duckdb_pending_execute_check_state function should be called again.
If this returns DUCKDB_PENDING_ERROR, an error occurred during execution.

The error message can be obtained by calling duckdb_pending_error on the pending_result.

##### Syntax

duckdb_pending_state duckdb_pending_execute_check_state(
  duckdb_pending_result pending_result
);

##### Parameters

* `pending_result`: The pending result.

##### Return Value

The state of the pending result.

#### `duckdb_execute_pending`

Fully execute a pending query result, returning the final query result.

If duckdb_pending_execute_task has been called until DUCKDB_PENDING_RESULT_READY was returned, this will return fast.
Otherwise, all remaining tasks must be executed first.

Note that the result must be freed with `duckdb_destroy_result`.

##### Syntax

duckdb_state duckdb_execute_pending(
  duckdb_pending_result pending_result,
  duckdb_result *out_result
);

##### Parameters

* `pending_result`: The pending result to execute.
* `out_result`: The result object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_pending_execution_is_finished`

Returns whether a duckdb_pending_state is finished executing. For example if `pending_state` is
DUCKDB_PENDING_RESULT_READY, this function will return true.

##### Syntax

bool duckdb_pending_execution_is_finished(
  duckdb_pending_state pending_state
);

##### Parameters

* `pending_state`: The pending state on which to decide whether to finish execution.

##### Return Value

Boolean indicating pending execution should be considered finished.

#### `duckdb_destroy_value`

Destroys the value and de-allocates all memory allocated for that type.

##### Syntax

void duckdb_destroy_value(
  duckdb_value *value
);

##### Parameters

* `value`: The value to destroy.

#### `duckdb_create_varchar`

Creates a value from a null-terminated string

##### Syntax

duckdb_value duckdb_create_varchar(
  const char *text
);

##### Parameters

* `text`: The null-terminated string

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_varchar_length`

Creates a value from a string

##### Syntax

duckdb_value duckdb_create_varchar_length(
  const char *text,
  idx_t length
);

##### Parameters

* `text`: The text
* `length`: The length of the text

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_bool`

Creates a value from a boolean

##### Syntax

duckdb_value duckdb_create_bool(
  bool input
);

##### Parameters

* `input`: The boolean value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_int8`

Creates a value from an int8_t (a tinyint)

##### Syntax

duckdb_value duckdb_create_int8(
  int8_t input
);

##### Parameters

* `input`: The tinyint value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_uint8`

Creates a value from a uint8_t (a utinyint)

##### Syntax

duckdb_value duckdb_create_uint8(
  uint8_t input
);

##### Parameters

* `input`: The utinyint value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_int16`

Creates a value from an int16_t (a smallint)

##### Syntax

duckdb_value duckdb_create_int16(
  int16_t input
);

##### Parameters

* `input`: The smallint value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_uint16`

Creates a value from a uint16_t (a usmallint)

##### Syntax

duckdb_value duckdb_create_uint16(
  uint16_t input
);

##### Parameters

* `input`: The usmallint value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_int32`

Creates a value from an int32_t (an integer)

##### Syntax

duckdb_value duckdb_create_int32(
  int32_t input
);

##### Parameters

* `input`: The integer value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_uint32`

Creates a value from a uint32_t (a uinteger)

##### Syntax

duckdb_value duckdb_create_uint32(
  uint32_t input
);

##### Parameters

* `input`: The uinteger value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_uint64`

Creates a value from a uint64_t (a ubigint)

##### Syntax

duckdb_value duckdb_create_uint64(
  uint64_t input
);

##### Parameters

* `input`: The ubigint value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_int64`

Creates a value from an int64

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_int64(
  int64_t val
);

#### `duckdb_create_hugeint`

Creates a value from a hugeint

##### Syntax

duckdb_value duckdb_create_hugeint(
  duckdb_hugeint input
);

##### Parameters

* `input`: The hugeint value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_uhugeint`

Creates a value from a uhugeint

##### Syntax

duckdb_value duckdb_create_uhugeint(
  duckdb_uhugeint input
);

##### Parameters

* `input`: The uhugeint value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_bignum`

Creates a BIGNUM value from a duckdb_bignum

##### Syntax

duckdb_value duckdb_create_bignum(
  duckdb_bignum input
);

##### Parameters

* `input`: The duckdb_bignum value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_decimal`

Creates a DECIMAL value from a duckdb_decimal

##### Syntax

duckdb_value duckdb_create_decimal(
  duckdb_decimal input
);

##### Parameters

* `input`: The duckdb_decimal value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_float`

Creates a value from a float

##### Syntax

duckdb_value duckdb_create_float(
  float input
);

##### Parameters

* `input`: The float value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_double`

Creates a value from a double

##### Syntax

duckdb_value duckdb_create_double(
  double input
);

##### Parameters

* `input`: The double value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_date`

Creates a value from a date

##### Syntax

duckdb_value duckdb_create_date(
  duckdb_date input
);

##### Parameters

* `input`: The date value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_time`

Creates a value from a time

##### Syntax

duckdb_value duckdb_create_time(
  duckdb_time input
);

##### Parameters

* `input`: The time value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_time_ns`

Creates a value from a time_ns

##### Syntax

duckdb_value duckdb_create_time_ns(
  duckdb_time_ns input
);

##### Parameters

* `input`: The time value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_time_tz_value`

Creates a value from a time_tz.
Not to be confused with `duckdb_create_time_tz`, which creates a duckdb_time_tz_t.

##### Syntax

duckdb_value duckdb_create_time_tz_value(
  duckdb_time_tz value
);

##### Parameters

* `value`: The time_tz value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_timestamp`

Creates a TIMESTAMP value from a duckdb_timestamp

##### Syntax

duckdb_value duckdb_create_timestamp(
  duckdb_timestamp input
);

##### Parameters

* `input`: The duckdb_timestamp value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_timestamp_tz`

Creates a TIMESTAMP_TZ value from a duckdb_timestamp

##### Syntax

duckdb_value duckdb_create_timestamp_tz(
  duckdb_timestamp input
);

##### Parameters

* `input`: The duckdb_timestamp value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_timestamp_s`

Creates a TIMESTAMP_S value from a duckdb_timestamp_s

##### Syntax

duckdb_value duckdb_create_timestamp_s(
  duckdb_timestamp_s input
);

##### Parameters

* `input`: The duckdb_timestamp_s value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_timestamp_ms`

Creates a TIMESTAMP_MS value from a duckdb_timestamp_ms

##### Syntax

duckdb_value duckdb_create_timestamp_ms(
  duckdb_timestamp_ms input
);

##### Parameters

* `input`: The duckdb_timestamp_ms value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_timestamp_ns`

Creates a TIMESTAMP_NS value from a duckdb_timestamp_ns

##### Syntax

duckdb_value duckdb_create_timestamp_ns(
  duckdb_timestamp_ns input
);

##### Parameters

* `input`: The duckdb_timestamp_ns value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_interval`

Creates a value from an interval

##### Syntax

duckdb_value duckdb_create_interval(
  duckdb_interval input
);

##### Parameters

* `input`: The interval value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_blob`

Creates a value from a blob

##### Syntax

duckdb_value duckdb_create_blob(
  const uint8_t *data,
  idx_t length
);

##### Parameters

* `data`: The blob data
* `length`: The length of the blob data

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_bit`

Creates a BIT value from a duckdb_bit

##### Syntax

duckdb_value duckdb_create_bit(
  duckdb_bit input
);

##### Parameters

* `input`: The duckdb_bit value

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_create_uuid`

Creates a UUID value from a uhugeint

##### Syntax

duckdb_value duckdb_create_uuid(
  duckdb_uhugeint input
);

##### Parameters

* `input`: The duckdb_uhugeint containing the UUID

##### Return Value

The value. This must be destroyed with `duckdb_destroy_value`.

#### `duckdb_get_bool`

Returns the boolean value of the given value.

##### Syntax

bool duckdb_get_bool(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a boolean

##### Return Value

A boolean, or false if the value cannot be converted

#### `duckdb_get_int8`

Returns the int8_t value of the given value.

##### Syntax

int8_t duckdb_get_int8(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a tinyint

##### Return Value

A int8_t, or MinValue if the value cannot be converted

#### `duckdb_get_uint8`

Returns the uint8_t value of the given value.

##### Syntax

uint8_t duckdb_get_uint8(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a utinyint

##### Return Value

A uint8_t, or MinValue if the value cannot be converted

#### `duckdb_get_int16`

Returns the int16_t value of the given value.

##### Syntax

int16_t duckdb_get_int16(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a smallint

##### Return Value

A int16_t, or MinValue if the value cannot be converted

#### `duckdb_get_uint16`

Returns the uint16_t value of the given value.

##### Syntax

uint16_t duckdb_get_uint16(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a usmallint

##### Return Value

A uint16_t, or MinValue if the value cannot be converted

#### `duckdb_get_int32`

Returns the int32_t value of the given value.

##### Syntax

int32_t duckdb_get_int32(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing an integer

##### Return Value

A int32_t, or MinValue if the value cannot be converted

#### `duckdb_get_uint32`

Returns the uint32_t value of the given value.

##### Syntax

uint32_t duckdb_get_uint32(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a uinteger

##### Return Value

A uint32_t, or MinValue if the value cannot be converted

#### `duckdb_get_int64`

Returns the int64_t value of the given value.

##### Syntax

int64_t duckdb_get_int64(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a bigint

##### Return Value

A int64_t, or MinValue if the value cannot be converted

#### `duckdb_get_uint64`

Returns the uint64_t value of the given value.

##### Syntax

uint64_t duckdb_get_uint64(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a ubigint

##### Return Value

A uint64_t, or MinValue if the value cannot be converted

#### `duckdb_get_hugeint`

Returns the hugeint value of the given value.

##### Syntax

duckdb_hugeint duckdb_get_hugeint(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a hugeint

##### Return Value

A duckdb_hugeint, or MinValue if the value cannot be converted

#### `duckdb_get_uhugeint`

Returns the uhugeint value of the given value.

##### Syntax

duckdb_uhugeint duckdb_get_uhugeint(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a uhugeint

##### Return Value

A duckdb_uhugeint, or MinValue if the value cannot be converted

#### `duckdb_get_bignum`

Returns the duckdb_bignum value of the given value.
The `data` field must be destroyed with `duckdb_free`.

##### Syntax

duckdb_bignum duckdb_get_bignum(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a BIGNUM

##### Return Value

A duckdb_bignum. The `data` field must be destroyed with `duckdb_free`.

#### `duckdb_get_decimal`

Returns the duckdb_decimal value of the given value.

##### Syntax

duckdb_decimal duckdb_get_decimal(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a DECIMAL

##### Return Value

A duckdb_decimal, or MinValue if the value cannot be converted

#### `duckdb_get_float`

Returns the float value of the given value.

##### Syntax

float duckdb_get_float(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a float

##### Return Value

A float, or NAN if the value cannot be converted

#### `duckdb_get_double`

Returns the double value of the given value.

##### Syntax

double duckdb_get_double(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a double

##### Return Value

A double, or NAN if the value cannot be converted

#### `duckdb_get_date`

Returns the date value of the given value.

##### Syntax

duckdb_date duckdb_get_date(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a date

##### Return Value

A duckdb_date, or MinValue if the value cannot be converted

#### `duckdb_get_time`

Returns the time value of the given value.

##### Syntax

duckdb_time duckdb_get_time(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a time

##### Return Value

A duckdb_time, or MinValue if the value cannot be converted

#### `duckdb_get_time_ns`

Returns the time_ns value of the given value.

##### Syntax

duckdb_time_ns duckdb_get_time_ns(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a time_ns

##### Return Value

A duckdb_time_ns, or MinValue if the value cannot be converted

#### `duckdb_get_time_tz`

Returns the time_tz value of the given value.

##### Syntax

duckdb_time_tz duckdb_get_time_tz(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a time_tz

##### Return Value

A duckdb_time_tz, or MinValue if the value cannot be converted

#### `duckdb_get_timestamp`

Returns the TIMESTAMP value of the given value.

##### Syntax

duckdb_timestamp duckdb_get_timestamp(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a TIMESTAMP

##### Return Value

A duckdb_timestamp, or MinValue if the value cannot be converted

#### `duckdb_get_timestamp_tz`

Returns the TIMESTAMP_TZ value of the given value.

##### Syntax

duckdb_timestamp duckdb_get_timestamp_tz(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a TIMESTAMP_TZ

##### Return Value

A duckdb_timestamp, or MinValue if the value cannot be converted

#### `duckdb_get_timestamp_s`

Returns the duckdb_timestamp_s value of the given value.

##### Syntax

duckdb_timestamp_s duckdb_get_timestamp_s(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a TIMESTAMP_S

##### Return Value

A duckdb_timestamp_s, or MinValue if the value cannot be converted

#### `duckdb_get_timestamp_ms`

Returns the duckdb_timestamp_ms value of the given value.

##### Syntax

duckdb_timestamp_ms duckdb_get_timestamp_ms(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a TIMESTAMP_MS

##### Return Value

A duckdb_timestamp_ms, or MinValue if the value cannot be converted

#### `duckdb_get_timestamp_ns`

Returns the duckdb_timestamp_ns value of the given value.

##### Syntax

duckdb_timestamp_ns duckdb_get_timestamp_ns(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a TIMESTAMP_NS

##### Return Value

A duckdb_timestamp_ns, or MinValue if the value cannot be converted

#### `duckdb_get_interval`

Returns the interval value of the given value.

##### Syntax

duckdb_interval duckdb_get_interval(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a interval

##### Return Value

A duckdb_interval, or MinValue if the value cannot be converted

#### `duckdb_get_value_type`

Returns the type of the given value. The type is valid as long as the value is not destroyed.
The type itself must not be destroyed.

##### Syntax

duckdb_logical_type duckdb_get_value_type(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value

##### Return Value

A duckdb_logical_type.

#### `duckdb_get_blob`

Returns the blob value of the given value.

##### Syntax

duckdb_blob duckdb_get_blob(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a blob

##### Return Value

A duckdb_blob

#### `duckdb_get_bit`

Returns the duckdb_bit value of the given value.
The `data` field must be destroyed with `duckdb_free`.

##### Syntax

duckdb_bit duckdb_get_bit(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a BIT

##### Return Value

A duckdb_bit

#### `duckdb_get_uuid`

Returns a duckdb_uhugeint representing the UUID value of the given value.

##### Syntax

duckdb_uhugeint duckdb_get_uuid(
  duckdb_value val
);

##### Parameters

* `val`: A duckdb_value containing a UUID

##### Return Value

A duckdb_uhugeint representing the UUID value

#### `duckdb_get_varchar`

Obtains a string representation of the given value.
The result must be destroyed with `duckdb_free`.

##### Syntax

char *duckdb_get_varchar(
  duckdb_value value
);

##### Parameters

* `value`: The value

##### Return Value

The string value. This must be destroyed with `duckdb_free`.

#### `duckdb_create_struct_value`

Creates a struct value from a type and an array of values. Must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_struct_value(
  duckdb_logical_type type,
  duckdb_value *values
);

##### Parameters

* `type`: The type of the struct
* `values`: The values for the struct fields

##### Return Value

The struct value, or nullptr, if any child type is `DUCKDB_TYPE_ANY` or `DUCKDB_TYPE_INVALID`.

#### `duckdb_create_list_value`

Creates a list value from a child (element) type and an array of values of length `value_count`.
Must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_list_value(
  duckdb_logical_type type,
  duckdb_value *values,
  idx_t value_count
);

##### Parameters

* `type`: The type of the list
* `values`: The values for the list
* `value_count`: The number of values in the list

##### Return Value

The list value, or nullptr, if the child type is `DUCKDB_TYPE_ANY` or `DUCKDB_TYPE_INVALID`.

#### `duckdb_create_array_value`

Creates an array value from a child (element) type and an array of values of length `value_count`.
Must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_array_value(
  duckdb_logical_type type,
  duckdb_value *values,
  idx_t value_count
);

##### Parameters

* `type`: The type of the array
* `values`: The values for the array
* `value_count`: The number of values in the array

##### Return Value

The array value, or nullptr, if the child type is `DUCKDB_TYPE_ANY` or `DUCKDB_TYPE_INVALID`.

#### `duckdb_create_map_value`

Creates a map value from a map type and two arrays, one for the keys and one for the values, each of length
`entry_count`. Must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_map_value(
  duckdb_logical_type map_type,
  duckdb_value *keys,
  duckdb_value *values,
  idx_t entry_count
);

##### Parameters

* `map_type`: The map type
* `keys`: The keys of the map
* `values`: The values of the map
* `entry_count`: The number of entrys (key-value pairs) in the map

##### Return Value

The map value, or nullptr, if the parameters are invalid.

#### `duckdb_create_union_value`

Creates a union value from a union type, a tag index, and a value.
Must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_union_value(
  duckdb_logical_type union_type,
  idx_t tag_index,
  duckdb_value value
);

##### Parameters

* `union_type`: The union type
* `tag_index`: The index of the tag of the union
* `value`: The value of the union for that tag

##### Return Value

The union value, or nullptr, if the parameters are invalid.

#### `duckdb_get_map_size`

Returns the number of elements in a MAP value.

##### Syntax

idx_t duckdb_get_map_size(
  duckdb_value value
);

##### Parameters

* `value`: The MAP value.

##### Return Value

The number of elements in the map.

#### `duckdb_get_map_key`

Returns the MAP key at index as a duckdb_value.

##### Syntax

duckdb_value duckdb_get_map_key(
  duckdb_value value,
  idx_t index
);

##### Parameters

* `value`: The MAP value.
* `index`: The index of the key.

##### Return Value

The key as a duckdb_value.

#### `duckdb_get_map_value`

Returns the MAP value at index as a duckdb_value.

##### Syntax

duckdb_value duckdb_get_map_value(
  duckdb_value value,
  idx_t index
);

##### Parameters

* `value`: The MAP value.
* `index`: The index of the value.

##### Return Value

The value as a duckdb_value.

#### `duckdb_is_null_value`

Returns whether the value's type is SQLNULL or not.

##### Syntax

bool duckdb_is_null_value(
  duckdb_value value
);

##### Parameters

* `value`: The value to check.

##### Return Value

True, if the value's type is SQLNULL, otherwise false.

#### `duckdb_create_null_value`

Creates a value of type SQLNULL.

##### Return Value

The duckdb_value representing SQLNULL. This must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_null_value(

);

#### `duckdb_get_list_size`

Returns the number of elements in a LIST value.

##### Syntax

idx_t duckdb_get_list_size(
  duckdb_value value
);

##### Parameters

* `value`: The LIST value.

##### Return Value

The number of elements in the list.

#### `duckdb_get_list_child`

Returns the LIST child at index as a duckdb_value.

##### Syntax

duckdb_value duckdb_get_list_child(
  duckdb_value value,
  idx_t index
);

##### Parameters

* `value`: The LIST value.
* `index`: The index of the child.

##### Return Value

The child as a duckdb_value.

#### `duckdb_create_enum_value`

Creates an enum value from a type and a value. Must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_create_enum_value(
  duckdb_logical_type type,
  uint64_t value
);

##### Parameters

* `type`: The type of the enum
* `value`: The value for the enum

##### Return Value

The enum value, or nullptr.

#### `duckdb_get_enum_value`

Returns the enum value of the given value.

##### Syntax

uint64_t duckdb_get_enum_value(
  duckdb_value value
);

##### Parameters

* `value`: A duckdb_value containing an enum

##### Return Value

A uint64_t, or MinValue if the value cannot be converted

#### `duckdb_get_struct_child`

Returns the STRUCT child at index as a duckdb_value.

##### Syntax

duckdb_value duckdb_get_struct_child(
  duckdb_value value,
  idx_t index
);

##### Parameters

* `value`: The STRUCT value.
* `index`: The index of the child.

##### Return Value

The child as a duckdb_value.

#### `duckdb_value_to_string`

Returns the SQL string representation of the given value.

##### Syntax

char *duckdb_value_to_string(
  duckdb_value value
);

##### Parameters

* `value`: A duckdb_value.

##### Return Value

The SQL string representation as a null-terminated string. The result must be freed with `duckdb_free`.

#### `duckdb_create_logical_type`

Creates a `duckdb_logical_type` from a primitive type.
The resulting logical type must be destroyed with `duckdb_destroy_logical_type`.

Returns an invalid logical type, if type is: `DUCKDB_TYPE_INVALID`, `DUCKDB_TYPE_DECIMAL`, `DUCKDB_TYPE_ENUM`,
`DUCKDB_TYPE_LIST`, `DUCKDB_TYPE_STRUCT`, `DUCKDB_TYPE_MAP`, `DUCKDB_TYPE_ARRAY`, or `DUCKDB_TYPE_UNION`.

##### Syntax

duckdb_logical_type duckdb_create_logical_type(
  duckdb_type type
);

##### Parameters

* `type`: The primitive type to create.

##### Return Value

The logical type.

#### `duckdb_logical_type_get_alias`

Returns the alias of a duckdb_logical_type, if set, else `nullptr`.
The result must be destroyed with `duckdb_free`.

##### Syntax

char *duckdb_logical_type_get_alias(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type

##### Return Value

The alias or `nullptr`

#### `duckdb_logical_type_set_alias`

Sets the alias of a duckdb_logical_type.

##### Syntax

void duckdb_logical_type_set_alias(
  duckdb_logical_type type,
  const char *alias
);

##### Parameters

* `type`: The logical type
* `alias`: The alias to set

#### `duckdb_create_list_type`

Creates a LIST type from its child type.
The return type must be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_create_list_type(
  duckdb_logical_type type
);

##### Parameters

* `type`: The child type of the list

##### Return Value

The logical type.

#### `duckdb_create_array_type`

Creates an ARRAY type from its child type.
The return type must be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_create_array_type(
  duckdb_logical_type type,
  idx_t array_size
);

##### Parameters

* `type`: The child type of the array.
* `array_size`: The number of elements in the array.

##### Return Value

The logical type.

#### `duckdb_create_map_type`

Creates a MAP type from its key type and value type.
The return type must be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_create_map_type(
  duckdb_logical_type key_type,
  duckdb_logical_type value_type
);

##### Parameters

* `key_type`: The map's key type.
* `value_type`: The map's value type.

##### Return Value

The logical type.

#### `duckdb_create_union_type`

Creates a UNION type from the passed arrays.
The return type must be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_create_union_type(
  duckdb_logical_type *member_types,
  const char **member_names,
  idx_t member_count
);

##### Parameters

* `member_types`: The array of union member types.
* `member_names`: The union member names.
* `member_count`: The number of union members.

##### Return Value

The logical type.

#### `duckdb_create_struct_type`

Creates a STRUCT type based on the member types and names.
The resulting type must be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_create_struct_type(
  duckdb_logical_type *member_types,
  const char **member_names,
  idx_t member_count
);

##### Parameters

* `member_types`: The array of types of the struct members.
* `member_names`: The array of names of the struct members.
* `member_count`: The number of members of the struct.

##### Return Value

The logical type.

#### `duckdb_create_enum_type`

Creates an ENUM type from the passed member name array.
The resulting type should be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_create_enum_type(
  const char **member_names,
  idx_t member_count
);

##### Parameters

* `member_names`: The array of names that the enum should consist of.
* `member_count`: The number of elements that were specified in the array.

##### Return Value

The logical type.

#### `duckdb_create_decimal_type`

Creates a DECIMAL type with the specified width and scale.
The resulting type should be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_create_decimal_type(
  uint8_t width,
  uint8_t scale
);

##### Parameters

* `width`: The width of the decimal type
* `scale`: The scale of the decimal type

##### Return Value

The logical type.

#### `duckdb_get_type_id`

Retrieves the enum `duckdb_type` of a `duckdb_logical_type`.

##### Syntax

duckdb_type duckdb_get_type_id(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type.

##### Return Value

The `duckdb_type` id.

#### `duckdb_decimal_width`

Retrieves the width of a decimal type.

##### Syntax

uint8_t duckdb_decimal_width(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The width of the decimal type

#### `duckdb_decimal_scale`

Retrieves the scale of a decimal type.

##### Syntax

uint8_t duckdb_decimal_scale(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The scale of the decimal type

#### `duckdb_decimal_internal_type`

Retrieves the internal storage type of a decimal type.

##### Syntax

duckdb_type duckdb_decimal_internal_type(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The internal type of the decimal type

#### `duckdb_enum_internal_type`

Retrieves the internal storage type of an enum type.

##### Syntax

duckdb_type duckdb_enum_internal_type(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The internal type of the enum type

#### `duckdb_enum_dictionary_size`

Retrieves the dictionary size of the enum type.

##### Syntax

uint32_t duckdb_enum_dictionary_size(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The dictionary size of the enum type

#### `duckdb_enum_dictionary_value`

Retrieves the dictionary value at the specified position from the enum.

The result must be freed with `duckdb_free`.

##### Syntax

char *duckdb_enum_dictionary_value(
  duckdb_logical_type type,
  idx_t index
);

##### Parameters

* `type`: The logical type object
* `index`: The index in the dictionary

##### Return Value

The string value of the enum type. Must be freed with `duckdb_free`.

#### `duckdb_list_type_child_type`

Retrieves the child type of the given LIST type. Also accepts MAP types.
The result must be freed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_list_type_child_type(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type, either LIST or MAP.

##### Return Value

The child type of the LIST or MAP type.

#### `duckdb_array_type_child_type`

Retrieves the child type of the given ARRAY type.

The result must be freed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_array_type_child_type(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type. Must be ARRAY.

##### Return Value

The child type of the ARRAY type.

#### `duckdb_array_type_array_size`

Retrieves the array size of the given array type.

##### Syntax

idx_t duckdb_array_type_array_size(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The fixed number of elements the values of this array type can store.

#### `duckdb_map_type_key_type`

Retrieves the key type of the given map type.

The result must be freed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_map_type_key_type(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The key type of the map type. Must be destroyed with `duckdb_destroy_logical_type`.

#### `duckdb_map_type_value_type`

Retrieves the value type of the given map type.

The result must be freed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_map_type_value_type(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The value type of the map type. Must be destroyed with `duckdb_destroy_logical_type`.

#### `duckdb_struct_type_child_count`

Returns the number of children of a struct type.

##### Syntax

idx_t duckdb_struct_type_child_count(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type object

##### Return Value

The number of children of a struct type.

#### `duckdb_struct_type_child_name`

Retrieves the name of the struct child.

The result must be freed with `duckdb_free`.

##### Syntax

char *duckdb_struct_type_child_name(
  duckdb_logical_type type,
  idx_t index
);

##### Parameters

* `type`: The logical type object
* `index`: The child index

##### Return Value

The name of the struct type. Must be freed with `duckdb_free`.

#### `duckdb_struct_type_child_type`

Retrieves the child type of the given struct type at the specified index.

The result must be freed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_struct_type_child_type(
  duckdb_logical_type type,
  idx_t index
);

##### Parameters

* `type`: The logical type object
* `index`: The child index

##### Return Value

The child type of the struct type. Must be destroyed with `duckdb_destroy_logical_type`.

#### `duckdb_union_type_member_count`

Returns the number of members that the union type has.

##### Syntax

idx_t duckdb_union_type_member_count(
  duckdb_logical_type type
);

##### Parameters

* `type`: The logical type (union) object

##### Return Value

The number of members of a union type.

#### `duckdb_union_type_member_name`

Retrieves the name of the union member.

The result must be freed with `duckdb_free`.

##### Syntax

char *duckdb_union_type_member_name(
  duckdb_logical_type type,
  idx_t index
);

##### Parameters

* `type`: The logical type object
* `index`: The child index

##### Return Value

The name of the union member. Must be freed with `duckdb_free`.

#### `duckdb_union_type_member_type`

Retrieves the child type of the given union member at the specified index.

The result must be freed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_union_type_member_type(
  duckdb_logical_type type,
  idx_t index
);

##### Parameters

* `type`: The logical type object
* `index`: The child index

##### Return Value

The child type of the union member. Must be destroyed with `duckdb_destroy_logical_type`.

#### `duckdb_destroy_logical_type`

Destroys the logical type and de-allocates all memory allocated for that type.

##### Syntax

void duckdb_destroy_logical_type(
  duckdb_logical_type *type
);

##### Parameters

* `type`: The logical type to destroy.

#### `duckdb_register_logical_type`

Registers a custom type within the given connection.
The type must have an alias

##### Syntax

duckdb_state duckdb_register_logical_type(
  duckdb_connection con,
  duckdb_logical_type type,
  duckdb_create_type_info info
);

##### Parameters

* `con`: The connection to use
* `type`: The custom type to register

##### Return Value

Whether or not the registration was successful.

#### `duckdb_create_data_chunk`

Creates an empty data chunk with the specified column types.
The result must be destroyed with `duckdb_destroy_data_chunk`.

##### Syntax

duckdb_data_chunk duckdb_create_data_chunk(
  duckdb_logical_type *types,
  idx_t column_count
);

##### Parameters

* `types`: An array of column types. Column types can not contain ANY and INVALID types.
* `column_count`: The number of columns.

##### Return Value

The data chunk.

#### `duckdb_destroy_data_chunk`

Destroys the data chunk and de-allocates all memory allocated for that chunk.

##### Syntax

void duckdb_destroy_data_chunk(
  duckdb_data_chunk *chunk
);

##### Parameters

* `chunk`: The data chunk to destroy.

#### `duckdb_data_chunk_reset`

Resets a data chunk, clearing the validity masks and setting the cardinality of the data chunk to 0.
After calling this method, you must call `duckdb_vector_get_validity` and `duckdb_vector_get_data` to obtain current
data and validity pointers

##### Syntax

void duckdb_data_chunk_reset(
  duckdb_data_chunk chunk
);

##### Parameters

* `chunk`: The data chunk to reset.

#### `duckdb_data_chunk_get_column_count`

Retrieves the number of columns in a data chunk.

##### Syntax

idx_t duckdb_data_chunk_get_column_count(
  duckdb_data_chunk chunk
);

##### Parameters

* `chunk`: The data chunk to get the data from

##### Return Value

The number of columns in the data chunk

#### `duckdb_data_chunk_get_vector`

Retrieves the vector at the specified column index in the data chunk.

The pointer to the vector is valid for as long as the chunk is alive.
It does NOT need to be destroyed.

##### Syntax

duckdb_vector duckdb_data_chunk_get_vector(
  duckdb_data_chunk chunk,
  idx_t col_idx
);

##### Parameters

* `chunk`: The data chunk to get the data from

##### Return Value

The vector

#### `duckdb_data_chunk_get_size`

Retrieves the current number of tuples in a data chunk.

##### Syntax

idx_t duckdb_data_chunk_get_size(
  duckdb_data_chunk chunk
);

##### Parameters

* `chunk`: The data chunk to get the data from

##### Return Value

The number of tuples in the data chunk

#### `duckdb_data_chunk_set_size`

Sets the current number of tuples in a data chunk.

##### Syntax

void duckdb_data_chunk_set_size(
  duckdb_data_chunk chunk,
  idx_t size
);

##### Parameters

* `chunk`: The data chunk to set the size in
* `size`: The number of tuples in the data chunk

#### `duckdb_create_vector`

Creates a flat vector. Must be destroyed with `duckdb_destroy_vector`.

##### Syntax

duckdb_vector duckdb_create_vector(
  duckdb_logical_type type,
  idx_t capacity
);

##### Parameters

* `type`: The logical type of the vector.
* `capacity`: The capacity of the vector.

##### Return Value

The vector.

#### `duckdb_destroy_vector`

Destroys the vector and de-allocates its memory.

##### Syntax

void duckdb_destroy_vector(
  duckdb_vector *vector
);

##### Parameters

* `vector`: A pointer to the vector.

#### `duckdb_vector_get_column_type`

Retrieves the column type of the specified vector.

The result must be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_vector_get_column_type(
  duckdb_vector vector
);

##### Parameters

* `vector`: The vector get the data from

##### Return Value

The type of the vector

#### `duckdb_vector_get_data`

Retrieves the data pointer of the vector.

The data pointer can be used to read or write values from the vector.
How to read or write values depends on the type of the vector.

##### Syntax

void *duckdb_vector_get_data(
  duckdb_vector vector
);

##### Parameters

* `vector`: The vector to get the data from

##### Return Value

The data pointer

#### `duckdb_vector_get_validity`

Retrieves the validity mask pointer of the specified vector.

If all values are valid, this function MIGHT return NULL!

The validity mask is a bitset that signifies null-ness within the data chunk.
It is a series of uint64_t values, where each uint64_t value contains validity for 64 tuples.
The bit is set to 1 if the value is valid (i.e., not NULL) or 0 if the value is invalid (i.e., NULL).

Validity of a specific value can be obtained like this:

idx_t entry_idx = row_idx / 64;
idx_t idx_in_entry = row_idx % 64;
bool is_valid = validity_mask[entry_idx] & (1 << idx_in_entry);

Alternatively, the (slower) duckdb_validity_row_is_valid function can be used.

##### Syntax

uint64_t *duckdb_vector_get_validity(
  duckdb_vector vector
);

##### Parameters

* `vector`: The vector to get the data from

##### Return Value

The pointer to the validity mask, or NULL if no validity mask is present

#### `duckdb_vector_ensure_validity_writable`

Ensures the validity mask is writable by allocating it.

After this function is called, `duckdb_vector_get_validity` will ALWAYS return non-NULL.
This allows NULL values to be written to the vector, regardless of whether a validity mask was present before.

##### Syntax

void duckdb_vector_ensure_validity_writable(
  duckdb_vector vector
);

##### Parameters

* `vector`: The vector to alter

#### `duckdb_vector_assign_string_element`

Assigns a string element in the vector at the specified location.

##### Syntax

void duckdb_vector_assign_string_element(
  duckdb_vector vector,
  idx_t index,
  const char *str
);

##### Parameters

* `vector`: The vector to alter
* `index`: The row position in the vector to assign the string to
* `str`: The null-terminated string

#### `duckdb_vector_assign_string_element_len`

Assigns a string element in the vector at the specified location. You may also use this function to assign BLOBs.

##### Syntax

void duckdb_vector_assign_string_element_len(
  duckdb_vector vector,
  idx_t index,
  const char *str,
  idx_t str_len
);

##### Parameters

* `vector`: The vector to alter
* `index`: The row position in the vector to assign the string to
* `str`: The string
* `str_len`: The length of the string (in bytes)

#### `duckdb_list_vector_get_child`

Retrieves the child vector of a list vector.

The resulting vector is valid as long as the parent vector is valid.

##### Syntax

duckdb_vector duckdb_list_vector_get_child(
  duckdb_vector vector
);

##### Parameters

* `vector`: The vector

##### Return Value

The child vector

#### `duckdb_list_vector_get_size`

Returns the size of the child vector of the list.

##### Syntax

idx_t duckdb_list_vector_get_size(
  duckdb_vector vector
);

##### Parameters

* `vector`: The vector

##### Return Value

The size of the child list

#### `duckdb_list_vector_set_size`

Sets the total size of the underlying child-vector of a list vector.

##### Syntax

duckdb_state duckdb_list_vector_set_size(
  duckdb_vector vector,
  idx_t size
);

##### Parameters

* `vector`: The list vector.
* `size`: The size of the child list.

##### Return Value

The duckdb state. Returns DuckDBError if the vector is nullptr.

#### `duckdb_list_vector_reserve`

Sets the total capacity of the underlying child-vector of a list.

After calling this method, you must call `duckdb_vector_get_validity` and `duckdb_vector_get_data` to obtain current
data and validity pointers

##### Syntax

duckdb_state duckdb_list_vector_reserve(
  duckdb_vector vector,
  idx_t required_capacity
);

##### Parameters

* `vector`: The list vector.
* `required_capacity`: the total capacity to reserve.

##### Return Value

The duckdb state. Returns DuckDBError if the vector is nullptr.

#### `duckdb_struct_vector_get_child`

Retrieves the child vector of a struct vector.
The resulting vector is valid as long as the parent vector is valid.

##### Syntax

duckdb_vector duckdb_struct_vector_get_child(
  duckdb_vector vector,
  idx_t index
);

##### Parameters

* `vector`: The vector
* `index`: The child index

##### Return Value

The child vector

#### `duckdb_array_vector_get_child`

Retrieves the child vector of an array vector.
The resulting vector is valid as long as the parent vector is valid.
The resulting vector has the size of the parent vector multiplied by the array size.

##### Syntax

duckdb_vector duckdb_array_vector_get_child(
  duckdb_vector vector
);

##### Parameters

* `vector`: The vector

##### Return Value

The child vector

#### `duckdb_slice_vector`

Slice a vector with a selection vector.
The length of the selection vector must be less than or equal to the length of the vector.
Turns the vector into a dictionary vector.

##### Syntax

void duckdb_slice_vector(
  duckdb_vector vector,
  duckdb_selection_vector sel,
  idx_t len
);

##### Parameters

* `vector`: The vector to slice.
* `sel`: The selection vector.
* `len`: The length of the selection vector.

#### `duckdb_vector_copy_sel`

Copy the src vector to the dst with a selection vector that identifies which indices to copy.

##### Syntax

void duckdb_vector_copy_sel(
  duckdb_vector src,
  duckdb_vector dst,
  duckdb_selection_vector sel,
  idx_t src_count,
  idx_t src_offset,
  idx_t dst_offset
);

##### Parameters

* `src`: The vector to copy from.
* `dst`: The vector to copy to.
* `sel`: The selection vector. The length of the selection vector should not be more than the length of the src
vector
* `src_count`: The number of entries from selection vector to copy. Think of this as the effective length of the
selection vector starting from index 0
* `src_offset`: The offset in the selection vector to copy from (important: actual number of items copied =
src_count - src_offset).
* `dst_offset`: The offset in the dst vector to start copying to.

#### `duckdb_vector_reference_value`

Copies the value from `value` to `vector`.

##### Syntax

void duckdb_vector_reference_value(
  duckdb_vector vector,
  duckdb_value value
);

##### Parameters

* `vector`: The receiving vector.
* `value`: The value to copy into the vector.

#### `duckdb_vector_reference_vector`

Changes `to_vector` to reference `from_vector. After, the vectors share ownership of the data.

##### Syntax

void duckdb_vector_reference_vector(
  duckdb_vector to_vector,
  duckdb_vector from_vector
);

##### Parameters

* `to_vector`: The receiving vector.
* `from_vector`: The vector to reference.

#### `duckdb_validity_row_is_valid`

Returns whether or not a row is valid (i.e., not NULL) in the given validity mask.

##### Syntax

bool duckdb_validity_row_is_valid(
  uint64_t *validity,
  idx_t row
);

##### Parameters

* `validity`: The validity mask, as obtained through `duckdb_vector_get_validity`
* `row`: The row index

##### Return Value

true if the row is valid, false otherwise

#### `duckdb_validity_set_row_validity`

In a validity mask, sets a specific row to either valid or invalid.

Note that `duckdb_vector_ensure_validity_writable` should be called before calling `duckdb_vector_get_validity`,
to ensure that there is a validity mask to write to.

##### Syntax

void duckdb_validity_set_row_validity(
  uint64_t *validity,
  idx_t row,
  bool valid
);

##### Parameters

* `validity`: The validity mask, as obtained through `duckdb_vector_get_validity`.
* `row`: The row index
* `valid`: Whether or not to set the row to valid, or invalid

#### `duckdb_validity_set_row_invalid`

In a validity mask, sets a specific row to invalid.

Equivalent to `duckdb_validity_set_row_validity` with valid set to false.

##### Syntax

void duckdb_validity_set_row_invalid(
  uint64_t *validity,
  idx_t row
);

##### Parameters

* `validity`: The validity mask
* `row`: The row index

#### `duckdb_validity_set_row_valid`

In a validity mask, sets a specific row to valid.

Equivalent to `duckdb_validity_set_row_validity` with valid set to true.

##### Syntax

void duckdb_validity_set_row_valid(
  uint64_t *validity,
  idx_t row
);

##### Parameters

* `validity`: The validity mask
* `row`: The row index

#### `duckdb_create_scalar_function`

Creates a new empty scalar function.

The return value must be destroyed with `duckdb_destroy_scalar_function`.

##### Return Value

The scalar function object.

##### Syntax

duckdb_scalar_function duckdb_create_scalar_function(

);

#### `duckdb_destroy_scalar_function`

Destroys the given scalar function object.

##### Syntax

void duckdb_destroy_scalar_function(
  duckdb_scalar_function *scalar_function
);

##### Parameters

* `scalar_function`: The scalar function to destroy

#### `duckdb_scalar_function_set_name`

Sets the name of the given scalar function.

##### Syntax

void duckdb_scalar_function_set_name(
  duckdb_scalar_function scalar_function,
  const char *name
);

##### Parameters

* `scalar_function`: The scalar function
* `name`: The name of the scalar function

#### `duckdb_scalar_function_set_varargs`

Sets the parameters of the given scalar function to varargs. Does not require adding parameters with
duckdb_scalar_function_add_parameter.

##### Syntax

void duckdb_scalar_function_set_varargs(
  duckdb_scalar_function scalar_function,
  duckdb_logical_type type
);

##### Parameters

* `scalar_function`: The scalar function.
* `type`: The type of the arguments.

##### Return Value

The parameter type. Cannot contain INVALID.

#### `duckdb_scalar_function_set_special_handling`

Sets the scalar function's null-handling behavior to special.

##### Syntax

void duckdb_scalar_function_set_special_handling(
  duckdb_scalar_function scalar_function
);

##### Parameters

* `scalar_function`: The scalar function.

#### `duckdb_scalar_function_set_volatile`

Sets the Function Stability of the scalar function to VOLATILE, indicating the function should be re-run for every row.
This limits optimization that can be performed for the function.

##### Syntax

void duckdb_scalar_function_set_volatile(
  duckdb_scalar_function scalar_function
);

##### Parameters

* `scalar_function`: The scalar function.

#### `duckdb_scalar_function_add_parameter`

Adds a parameter to the scalar function.

##### Syntax

void duckdb_scalar_function_add_parameter(
  duckdb_scalar_function scalar_function,
  duckdb_logical_type type
);

##### Parameters

* `scalar_function`: The scalar function.
* `type`: The parameter type. Cannot contain INVALID.

#### `duckdb_scalar_function_set_return_type`

Sets the return type of the scalar function.

##### Syntax

void duckdb_scalar_function_set_return_type(
  duckdb_scalar_function scalar_function,
  duckdb_logical_type type
);

##### Parameters

* `scalar_function`: The scalar function
* `type`: Cannot contain INVALID or ANY.

#### `duckdb_scalar_function_set_extra_info`

Assigns extra information to the scalar function that can be fetched during binding, etc.

##### Syntax

void duckdb_scalar_function_set_extra_info(
  duckdb_scalar_function scalar_function,
  void *extra_info,
  duckdb_delete_callback_t destroy
);

##### Parameters

* `scalar_function`: The scalar function
* `extra_info`: The extra information
* `destroy`: The callback that will be called to destroy the extra information (if any)

#### `duckdb_scalar_function_set_bind`

Sets the (optional) bind function of the scalar function.

##### Syntax

void duckdb_scalar_function_set_bind(
  duckdb_scalar_function scalar_function,
  duckdb_scalar_function_bind_t bind
);

##### Parameters

* `scalar_function`: The scalar function.
* `bind`: The bind function.

#### `duckdb_scalar_function_set_bind_data`

Sets the user-provided bind data in the bind object of the scalar function.
The bind data object can be retrieved again during execution.
In most case, you also need to set the copy-callback of your bind data via duckdb_scalar_function_set_bind_data_copy.

##### Syntax

void duckdb_scalar_function_set_bind_data(
  duckdb_bind_info info,
  void *bind_data,
  duckdb_delete_callback_t destroy
);

##### Parameters

* `info`: The bind info of the scalar function.
* `bind_data`: The bind data object.
* `destroy`: The callback to destroy the bind data (if any).

#### `duckdb_scalar_function_set_bind_data_copy`

Sets the copy-callback for the user-provided bind data in the bind object of the scalar function.

##### Syntax

void duckdb_scalar_function_set_bind_data_copy(
  duckdb_bind_info info,
  duckdb_copy_callback_t copy
);

##### Parameters

* `info`: The bind info of the scalar function.
* `copy`: The callback to copy the bind data (if any).

#### `duckdb_scalar_function_bind_set_error`

Report that an error has occurred while calling bind on a scalar function.

##### Syntax

void duckdb_scalar_function_bind_set_error(
  duckdb_bind_info info,
  const char *error
);

##### Parameters

* `info`: The bind info object.
* `error`: The error message.

#### `duckdb_scalar_function_set_function`

Sets the main function of the scalar function.

##### Syntax

void duckdb_scalar_function_set_function(
  duckdb_scalar_function scalar_function,
  duckdb_scalar_function_t function
);

##### Parameters

* `scalar_function`: The scalar function
* `function`: The function

#### `duckdb_register_scalar_function`

Register the scalar function object within the given connection.

The function requires at least a name, a function and a return type.

If the function is incomplete or a function with this name already exists DuckDBError is returned.

##### Syntax

duckdb_state duckdb_register_scalar_function(
  duckdb_connection con,
  duckdb_scalar_function scalar_function
);

##### Parameters

* `con`: The connection to register it in.
* `scalar_function`: The function pointer

##### Return Value

Whether or not the registration was successful.

#### `duckdb_scalar_function_get_extra_info`

Retrieves the extra info of the function as set in `duckdb_scalar_function_set_extra_info`.

##### Syntax

void *duckdb_scalar_function_get_extra_info(
  duckdb_function_info info
);

##### Parameters

* `info`: The info object.

##### Return Value

The extra info.

#### `duckdb_scalar_function_bind_get_extra_info`

Retrieves the extra info of the function as set in the bind info.

##### Syntax

void *duckdb_scalar_function_bind_get_extra_info(
  duckdb_bind_info info
);

##### Parameters

* `info`: The info object.

##### Return Value

The extra info.

#### `duckdb_scalar_function_get_bind_data`

Gets the scalar function's bind data set by `duckdb_scalar_function_set_bind_data`.
Note that the bind data is read-only.

##### Syntax

void *duckdb_scalar_function_get_bind_data(
  duckdb_function_info info
);

##### Parameters

* `info`: The function info.

##### Return Value

The bind data object.

#### `duckdb_scalar_function_get_client_context`

Retrieves the client context of the bind info of a scalar function.

##### Syntax

void duckdb_scalar_function_get_client_context(
  duckdb_bind_info info,
  duckdb_client_context *out_context
);

##### Parameters

* `info`: The bind info object of the scalar function.
* `out_context`: The client context of the bind info. Must be destroyed with `duckdb_destroy_client_context`.

#### `duckdb_scalar_function_set_error`

Report that an error has occurred while executing the scalar function.

##### Syntax

void duckdb_scalar_function_set_error(
  duckdb_function_info info,
  const char *error
);

##### Parameters

* `info`: The info object.
* `error`: The error message

#### `duckdb_create_scalar_function_set`

Creates a new empty scalar function set.

The return value must be destroyed with `duckdb_destroy_scalar_function_set`.

##### Return Value

The scalar function set object.

##### Syntax

duckdb_scalar_function_set duckdb_create_scalar_function_set(
  const char *name
);

#### `duckdb_destroy_scalar_function_set`

Destroys the given scalar function set object.

##### Syntax

void duckdb_destroy_scalar_function_set(
  duckdb_scalar_function_set *scalar_function_set
);

#### `duckdb_add_scalar_function_to_set`

Adds the scalar function as a new overload to the scalar function set.

Returns DuckDBError if the function could not be added, for example if the overload already exists.

##### Syntax

duckdb_state duckdb_add_scalar_function_to_set(
  duckdb_scalar_function_set set,
  duckdb_scalar_function function
);

##### Parameters

* `set`: The scalar function set
* `function`: The function to add

#### `duckdb_register_scalar_function_set`

Register the scalar function set within the given connection.

The set requires at least a single valid overload.

If the set is incomplete or a function with this name already exists DuckDBError is returned.

##### Syntax

duckdb_state duckdb_register_scalar_function_set(
  duckdb_connection con,
  duckdb_scalar_function_set set
);

##### Parameters

* `con`: The connection to register it in.
* `set`: The function set to register

##### Return Value

Whether or not the registration was successful.

#### `duckdb_scalar_function_bind_get_argument_count`

Returns the number of input arguments of the scalar function.

##### Syntax

idx_t duckdb_scalar_function_bind_get_argument_count(
  duckdb_bind_info info
);

##### Parameters

* `info`: The bind info.

##### Return Value

The number of input arguments.

#### `duckdb_scalar_function_bind_get_argument`

Returns the input argument at index of the scalar function.

##### Syntax

duckdb_expression duckdb_scalar_function_bind_get_argument(
  duckdb_bind_info info,
  idx_t index
);

##### Parameters

* `info`: The bind info.
* `index`: The argument index.

##### Return Value

The input argument at index. Must be destroyed with `duckdb_destroy_expression`.

#### `duckdb_create_selection_vector`

Creates a new selection vector of size `size`.
Must be destroyed with `duckdb_destroy_selection_vector`.

##### Syntax

duckdb_selection_vector duckdb_create_selection_vector(
  idx_t size
);

##### Parameters

* `size`: The size of the selection vector.

##### Return Value

The selection vector.

#### `duckdb_destroy_selection_vector`

Destroys the selection vector and de-allocates its memory.

##### Syntax

void duckdb_destroy_selection_vector(
  duckdb_selection_vector sel
);

##### Parameters

* `sel`: The selection vector.

#### `duckdb_selection_vector_get_data_ptr`

Access the data pointer of a selection vector.

##### Syntax

sel_t *duckdb_selection_vector_get_data_ptr(
  duckdb_selection_vector sel
);

##### Parameters

* `sel`: The selection vector.

##### Return Value

The data pointer.

#### `duckdb_create_aggregate_function`

Creates a new empty aggregate function.

The return value should be destroyed with `duckdb_destroy_aggregate_function`.

##### Return Value

The aggregate function object.

##### Syntax

duckdb_aggregate_function duckdb_create_aggregate_function(

);

#### `duckdb_destroy_aggregate_function`

Destroys the given aggregate function object.

##### Syntax

void duckdb_destroy_aggregate_function(
  duckdb_aggregate_function *aggregate_function
);

#### `duckdb_aggregate_function_set_name`

Sets the name of the given aggregate function.

##### Syntax

void duckdb_aggregate_function_set_name(
  duckdb_aggregate_function aggregate_function,
  const char *name
);

##### Parameters

* `aggregate_function`: The aggregate function
* `name`: The name of the aggregate function

#### `duckdb_aggregate_function_add_parameter`

Adds a parameter to the aggregate function.

##### Syntax

void duckdb_aggregate_function_add_parameter(
  duckdb_aggregate_function aggregate_function,
  duckdb_logical_type type
);

##### Parameters

* `aggregate_function`: The aggregate function.
* `type`: The parameter type. Cannot contain INVALID.

#### `duckdb_aggregate_function_set_return_type`

Sets the return type of the aggregate function.

##### Syntax

void duckdb_aggregate_function_set_return_type(
  duckdb_aggregate_function aggregate_function,
  duckdb_logical_type type
);

##### Parameters

* `aggregate_function`: The aggregate function.
* `type`: The return type. Cannot contain INVALID or ANY.

#### `duckdb_aggregate_function_set_functions`

Sets the main functions of the aggregate function.

##### Syntax

void duckdb_aggregate_function_set_functions(
  duckdb_aggregate_function aggregate_function,
  duckdb_aggregate_state_size state_size,
  duckdb_aggregate_init_t state_init,
  duckdb_aggregate_update_t update,
  duckdb_aggregate_combine_t combine,
  duckdb_aggregate_finalize_t finalize
);

##### Parameters

* `aggregate_function`: The aggregate function
* `state_size`: state size
* `state_init`: state init function
* `update`: update states
* `combine`: combine states
* `finalize`: finalize states

#### `duckdb_aggregate_function_set_destructor`

Sets the state destructor callback of the aggregate function (optional)

##### Syntax

void duckdb_aggregate_function_set_destructor(
  duckdb_aggregate_function aggregate_function,
  duckdb_aggregate_destroy_t destroy
);

##### Parameters

* `aggregate_function`: The aggregate function
* `destroy`: state destroy callback

#### `duckdb_register_aggregate_function`

Register the aggregate function object within the given connection.

The function requires at least a name, functions and a return type.

If the function is incomplete or a function with this name already exists DuckDBError is returned.

##### Syntax

duckdb_state duckdb_register_aggregate_function(
  duckdb_connection con,
  duckdb_aggregate_function aggregate_function
);

##### Parameters

* `con`: The connection to register it in.

##### Return Value

Whether or not the registration was successful.

#### `duckdb_aggregate_function_set_special_handling`

Sets the NULL handling of the aggregate function to SPECIAL_HANDLING.

##### Syntax

void duckdb_aggregate_function_set_special_handling(
  duckdb_aggregate_function aggregate_function
);

##### Parameters

* `aggregate_function`: The aggregate function

#### `duckdb_aggregate_function_set_extra_info`

Assigns extra information to the scalar function that can be fetched during binding, etc.

##### Syntax

void duckdb_aggregate_function_set_extra_info(
  duckdb_aggregate_function aggregate_function,
  void *extra_info,
  duckdb_delete_callback_t destroy
);

##### Parameters

* `aggregate_function`: The aggregate function
* `extra_info`: The extra information
* `destroy`: The callback that will be called to destroy the extra information (if any)

#### `duckdb_aggregate_function_get_extra_info`

Retrieves the extra info of the function as set in `duckdb_aggregate_function_set_extra_info`.

##### Syntax

void *duckdb_aggregate_function_get_extra_info(
  duckdb_function_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The extra info

#### `duckdb_aggregate_function_set_error`

Report that an error has occurred while executing the aggregate function.

##### Syntax

void duckdb_aggregate_function_set_error(
  duckdb_function_info info,
  const char *error
);

##### Parameters

* `info`: The info object
* `error`: The error message

#### `duckdb_create_aggregate_function_set`

Creates a new empty aggregate function set.

The return value should be destroyed with `duckdb_destroy_aggregate_function_set`.

##### Return Value

The aggregate function set object.

##### Syntax

duckdb_aggregate_function_set duckdb_create_aggregate_function_set(
  const char *name
);

#### `duckdb_destroy_aggregate_function_set`

Destroys the given aggregate function set object.

##### Syntax

void duckdb_destroy_aggregate_function_set(
  duckdb_aggregate_function_set *aggregate_function_set
);

#### `duckdb_add_aggregate_function_to_set`

Adds the aggregate function as a new overload to the aggregate function set.

Returns DuckDBError if the function could not be added, for example if the overload already exists.

##### Syntax

duckdb_state duckdb_add_aggregate_function_to_set(
  duckdb_aggregate_function_set set,
  duckdb_aggregate_function function
);

##### Parameters

* `set`: The aggregate function set
* `function`: The function to add

#### `duckdb_register_aggregate_function_set`

Register the aggregate function set within the given connection.

The set requires at least a single valid overload.

If the set is incomplete or a function with this name already exists DuckDBError is returned.

##### Syntax

duckdb_state duckdb_register_aggregate_function_set(
  duckdb_connection con,
  duckdb_aggregate_function_set set
);

##### Parameters

* `con`: The connection to register it in.
* `set`: The function set to register

##### Return Value

Whether or not the registration was successful.

#### `duckdb_create_table_function`

Creates a new empty table function.

The return value should be destroyed with `duckdb_destroy_table_function`.

##### Return Value

The table function object.

##### Syntax

duckdb_table_function duckdb_create_table_function(

);

#### `duckdb_destroy_table_function`

Destroys the given table function object.

##### Syntax

void duckdb_destroy_table_function(
  duckdb_table_function *table_function
);

##### Parameters

* `table_function`: The table function to destroy

#### `duckdb_table_function_set_name`

Sets the name of the given table function.

##### Syntax

void duckdb_table_function_set_name(
  duckdb_table_function table_function,
  const char *name
);

##### Parameters

* `table_function`: The table function
* `name`: The name of the table function

#### `duckdb_table_function_add_parameter`

Adds a parameter to the table function.

##### Syntax

void duckdb_table_function_add_parameter(
  duckdb_table_function table_function,
  duckdb_logical_type type
);

##### Parameters

* `table_function`: The table function.
* `type`: The parameter type. Cannot contain INVALID.

#### `duckdb_table_function_add_named_parameter`

Adds a named parameter to the table function.

##### Syntax

void duckdb_table_function_add_named_parameter(
  duckdb_table_function table_function,
  const char *name,
  duckdb_logical_type type
);

##### Parameters

* `table_function`: The table function.
* `name`: The parameter name.
* `type`: The parameter type. Cannot contain INVALID.

#### `duckdb_table_function_set_extra_info`

Assigns extra information to the table function that can be fetched during binding, etc.

##### Syntax

void duckdb_table_function_set_extra_info(
  duckdb_table_function table_function,
  void *extra_info,
  duckdb_delete_callback_t destroy
);

##### Parameters

* `table_function`: The table function
* `extra_info`: The extra information
* `destroy`: The callback that will be called to destroy the extra information (if any)

#### `duckdb_table_function_set_bind`

Sets the bind function of the table function.

##### Syntax

void duckdb_table_function_set_bind(
  duckdb_table_function table_function,
  duckdb_table_function_bind_t bind
);

##### Parameters

* `table_function`: The table function
* `bind`: The bind function

#### `duckdb_table_function_set_init`

Sets the init function of the table function.

##### Syntax

void duckdb_table_function_set_init(
  duckdb_table_function table_function,
  duckdb_table_function_init_t init
);

##### Parameters

* `table_function`: The table function
* `init`: The init function

#### `duckdb_table_function_set_local_init`

Sets the thread-local init function of the table function.

##### Syntax

void duckdb_table_function_set_local_init(
  duckdb_table_function table_function,
  duckdb_table_function_init_t init
);

##### Parameters

* `table_function`: The table function
* `init`: The init function

#### `duckdb_table_function_set_function`

Sets the main function of the table function.

##### Syntax

void duckdb_table_function_set_function(
  duckdb_table_function table_function,
  duckdb_table_function_t function
);

##### Parameters

* `table_function`: The table function
* `function`: The function

#### `duckdb_table_function_supports_projection_pushdown`

Sets whether or not the given table function supports projection pushdown.

If this is set to true, the system will provide a list of all required columns in the `init` stage through
the `duckdb_init_get_column_count` and `duckdb_init_get_column_index` functions.
If this is set to false (the default), the system will expect all columns to be projected.

##### Syntax

void duckdb_table_function_supports_projection_pushdown(
  duckdb_table_function table_function,
  bool pushdown
);

##### Parameters

* `table_function`: The table function
* `pushdown`: True if the table function supports projection pushdown, false otherwise.

#### `duckdb_register_table_function`

Register the table function object within the given connection.

The function requires at least a name, a bind function, an init function and a main function.

If the function is incomplete or a function with this name already exists DuckDBError is returned.

##### Syntax

duckdb_state duckdb_register_table_function(
  duckdb_connection con,
  duckdb_table_function function
);

##### Parameters

* `con`: The connection to register it in.
* `function`: The function pointer

##### Return Value

Whether or not the registration was successful.

#### `duckdb_bind_get_extra_info`

Retrieves the extra info of the function as set in `duckdb_table_function_set_extra_info`.

##### Syntax

void *duckdb_bind_get_extra_info(
  duckdb_bind_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The extra info

#### `duckdb_table_function_get_client_context`

Retrieves the client context of the bind info of a table function.

##### Syntax

void duckdb_table_function_get_client_context(
  duckdb_bind_info info,
  duckdb_client_context *out_context
);

##### Parameters

* `info`: The bind info object of the table function.
* `out_context`: The client context of the bind info. Must be destroyed with `duckdb_destroy_client_context`.

#### `duckdb_bind_add_result_column`

Adds a result column to the output of the table function.

##### Syntax

void duckdb_bind_add_result_column(
  duckdb_bind_info info,
  const char *name,
  duckdb_logical_type type
);

##### Parameters

* `info`: The table function's bind info.
* `name`: The column name.
* `type`: The logical column type.

#### `duckdb_bind_get_parameter_count`

Retrieves the number of regular (non-named) parameters to the function.

##### Syntax

idx_t duckdb_bind_get_parameter_count(
  duckdb_bind_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The number of parameters

#### `duckdb_bind_get_parameter`

Retrieves the parameter at the given index.

The result must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_bind_get_parameter(
  duckdb_bind_info info,
  idx_t index
);

##### Parameters

* `info`: The info object
* `index`: The index of the parameter to get

##### Return Value

The value of the parameter. Must be destroyed with `duckdb_destroy_value`.

#### `duckdb_bind_get_named_parameter`

Retrieves a named parameter with the given name.

The result must be destroyed with `duckdb_destroy_value`.

##### Syntax

duckdb_value duckdb_bind_get_named_parameter(
  duckdb_bind_info info,
  const char *name
);

##### Parameters

* `info`: The info object
* `name`: The name of the parameter

##### Return Value

The value of the parameter. Must be destroyed with `duckdb_destroy_value`.

#### `duckdb_bind_set_bind_data`

Sets the user-provided bind data in the bind object of the table function.
This object can be retrieved again during execution.

##### Syntax

void duckdb_bind_set_bind_data(
  duckdb_bind_info info,
  void *bind_data,
  duckdb_delete_callback_t destroy
);

##### Parameters

* `info`: The bind info of the table function.
* `bind_data`: The bind data object.
* `destroy`: The callback to destroy the bind data (if any).

#### `duckdb_bind_set_cardinality`

Sets the cardinality estimate for the table function, used for optimization.

##### Syntax

void duckdb_bind_set_cardinality(
  duckdb_bind_info info,
  idx_t cardinality,
  bool is_exact
);

##### Parameters

* `info`: The bind data object.
* `is_exact`: Whether or not the cardinality estimate is exact, or an approximation

#### `duckdb_bind_set_error`

Report that an error has occurred while calling bind on a table function.

##### Syntax

void duckdb_bind_set_error(
  duckdb_bind_info info,
  const char *error
);

##### Parameters

* `info`: The info object
* `error`: The error message

#### `duckdb_init_get_extra_info`

Retrieves the extra info of the function as set in `duckdb_table_function_set_extra_info`.

##### Syntax

void *duckdb_init_get_extra_info(
  duckdb_init_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The extra info

#### `duckdb_init_get_bind_data`

Gets the bind data set by `duckdb_bind_set_bind_data` during the bind.

Note that the bind data should be considered as read-only.
For tracking state, use the init data instead.

##### Syntax

void *duckdb_init_get_bind_data(
  duckdb_init_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The bind data object

#### `duckdb_init_set_init_data`

Sets the user-provided init data in the init object. This object can be retrieved again during execution.

##### Syntax

void duckdb_init_set_init_data(
  duckdb_init_info info,
  void *init_data,
  duckdb_delete_callback_t destroy
);

##### Parameters

* `info`: The info object
* `init_data`: The init data object.
* `destroy`: The callback that will be called to destroy the init data (if any)

#### `duckdb_init_get_column_count`

Returns the number of projected columns.

This function must be used if projection pushdown is enabled to figure out which columns to emit.

##### Syntax

idx_t duckdb_init_get_column_count(
  duckdb_init_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The number of projected columns.

#### `duckdb_init_get_column_index`

Returns the column index of the projected column at the specified position.

This function must be used if projection pushdown is enabled to figure out which columns to emit.

##### Syntax

idx_t duckdb_init_get_column_index(
  duckdb_init_info info,
  idx_t column_index
);

##### Parameters

* `info`: The info object
* `column_index`: The index at which to get the projected column index, from 0..duckdb_init_get_column_count(info)

##### Return Value

The column index of the projected column.

#### `duckdb_init_set_max_threads`

Sets how many threads can process this table function in parallel (default: 1)

##### Syntax

void duckdb_init_set_max_threads(
  duckdb_init_info info,
  idx_t max_threads
);

##### Parameters

* `info`: The info object
* `max_threads`: The maximum amount of threads that can process this table function

#### `duckdb_init_set_error`

Report that an error has occurred while calling init.

##### Syntax

void duckdb_init_set_error(
  duckdb_init_info info,
  const char *error
);

##### Parameters

* `info`: The info object
* `error`: The error message

#### `duckdb_function_get_extra_info`

Retrieves the extra info of the function as set in `duckdb_table_function_set_extra_info`.

##### Syntax

void *duckdb_function_get_extra_info(
  duckdb_function_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The extra info

#### `duckdb_function_get_bind_data`

Gets the table function's bind data set by `duckdb_bind_set_bind_data`.

Note that the bind data is read-only.
For tracking state, use the init data instead.

##### Syntax

void *duckdb_function_get_bind_data(
  duckdb_function_info info
);

##### Parameters

* `info`: The function info object.

##### Return Value

The bind data object.

#### `duckdb_function_get_init_data`

Gets the init data set by `duckdb_init_set_init_data` during the init.

##### Syntax

void *duckdb_function_get_init_data(
  duckdb_function_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The init data object

#### `duckdb_function_get_local_init_data`

Gets the thread-local init data set by `duckdb_init_set_init_data` during the local_init.

##### Syntax

void *duckdb_function_get_local_init_data(
  duckdb_function_info info
);

##### Parameters

* `info`: The info object

##### Return Value

The init data object

#### `duckdb_function_set_error`

Report that an error has occurred while executing the function.

##### Syntax

void duckdb_function_set_error(
  duckdb_function_info info,
  const char *error
);

##### Parameters

* `info`: The info object
* `error`: The error message

#### `duckdb_add_replacement_scan`

Add a replacement scan definition to the specified database.

##### Syntax

void duckdb_add_replacement_scan(
  duckdb_database db,
  duckdb_replacement_callback_t replacement,
  void *extra_data,
  duckdb_delete_callback_t delete_callback
);

##### Parameters

* `db`: The database object to add the replacement scan to
* `replacement`: The replacement scan callback
* `extra_data`: Extra data that is passed back into the specified callback
* `delete_callback`: The delete callback to call on the extra data, if any

#### `duckdb_replacement_scan_set_function_name`

Sets the replacement function name. If this function is called in the replacement callback,
the replacement scan is performed. If it is not called, the replacement callback is not performed.

##### Syntax

void duckdb_replacement_scan_set_function_name(
  duckdb_replacement_scan_info info,
  const char *function_name
);

##### Parameters

* `info`: The info object
* `function_name`: The function name to substitute.

#### `duckdb_replacement_scan_add_parameter`

Adds a parameter to the replacement scan function.

##### Syntax

void duckdb_replacement_scan_add_parameter(
  duckdb_replacement_scan_info info,
  duckdb_value parameter
);

##### Parameters

* `info`: The info object
* `parameter`: The parameter to add.

#### `duckdb_replacement_scan_set_error`

Report that an error has occurred while executing the replacement scan.

##### Syntax

void duckdb_replacement_scan_set_error(
  duckdb_replacement_scan_info info,
  const char *error
);

##### Parameters

* `info`: The info object
* `error`: The error message

#### `duckdb_get_profiling_info`

Returns the root node of the profiling information. Returns nullptr, if profiling is not enabled.

##### Syntax

duckdb_profiling_info duckdb_get_profiling_info(
  duckdb_connection connection
);

##### Parameters

* `connection`: A connection object.

##### Return Value

A profiling information object.

#### `duckdb_profiling_info_get_value`

Returns the value of the metric of the current profiling info node. Returns nullptr, if the metric does
 not exist or is not enabled. Currently, the value holds a string, and you can retrieve the string
 by calling the corresponding function: char *duckdb_get_varchar(duckdb_value value).

##### Syntax

duckdb_value duckdb_profiling_info_get_value(
  duckdb_profiling_info info,
  const char *key
);

##### Parameters

* `info`: A profiling information object.
* `key`: The name of the requested metric.

##### Return Value

The value of the metric. Must be freed with `duckdb_destroy_value`

#### `duckdb_profiling_info_get_metrics`

Returns the key-value metric map of this profiling node as a MAP duckdb_value.
The individual elements are accessible via the duckdb_value MAP functions.

##### Syntax

duckdb_value duckdb_profiling_info_get_metrics(
  duckdb_profiling_info info
);

##### Parameters

* `info`: A profiling information object.

##### Return Value

The key-value metric map as a MAP duckdb_value.

#### `duckdb_profiling_info_get_child_count`

Returns the number of children in the current profiling info node.

##### Syntax

idx_t duckdb_profiling_info_get_child_count(
  duckdb_profiling_info info
);

##### Parameters

* `info`: A profiling information object.

##### Return Value

The number of children in the current node.

#### `duckdb_profiling_info_get_child`

Returns the child node at the specified index.

##### Syntax

duckdb_profiling_info duckdb_profiling_info_get_child(
  duckdb_profiling_info info,
  idx_t index
);

##### Parameters

* `info`: A profiling information object.
* `index`: The index of the child node.

##### Return Value

The child node at the specified index.

#### `duckdb_appender_create`

Creates an appender object.

Note that the object must be destroyed with `duckdb_appender_destroy`.

##### Syntax

duckdb_state duckdb_appender_create(
  duckdb_connection connection,
  const char *schema,
  const char *table,
  duckdb_appender *out_appender
);

##### Parameters

* `connection`: The connection context to create the appender in.
* `schema`: The schema of the table to append to, or `nullptr` for the default schema.
* `table`: The table name to append to.
* `out_appender`: The resulting appender object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_create_ext`

Creates an appender object.

Note that the object must be destroyed with `duckdb_appender_destroy`.

##### Syntax

duckdb_state duckdb_appender_create_ext(
  duckdb_connection connection,
  const char *catalog,
  const char *schema,
  const char *table,
  duckdb_appender *out_appender
);

##### Parameters

* `connection`: The connection context to create the appender in.
* `catalog`: The catalog of the table to append to, or `nullptr` for the default catalog.
* `schema`: The schema of the table to append to, or `nullptr` for the default schema.
* `table`: The table name to append to.
* `out_appender`: The resulting appender object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_create_query`

Creates an appender object that executes the given query with any data appended to it.

Note that the object must be destroyed with `duckdb_appender_destroy`.

##### Syntax

duckdb_state duckdb_appender_create_query(
  duckdb_connection connection,
  const char *query,
  idx_t column_count,
  duckdb_logical_type *types,
  const char *table_name,
  const char **column_names,
  duckdb_appender *out_appender
);

##### Parameters

* `connection`: The connection context to create the appender in.
* `query`: The query to execute, can be an INSERT, DELETE, UPDATE or MERGE INTO statement.
* `column_count`: The number of columns to append.
* `types`: The types of the columns to append.
* `table_name`: (optionally) the table name used to refer to the appended data, defaults to "appended_data".
* `column_names`: (optionally) the list of column names, defaults to "col1", "col2", ...
* `out_appender`: The resulting appender object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_column_count`

Returns the number of columns that belong to the appender.
If there is no active column list, then this equals the table's physical columns.

##### Syntax

idx_t duckdb_appender_column_count(
  duckdb_appender appender
);

##### Parameters

* `appender`: The appender to get the column count from.

##### Return Value

The number of columns in the data chunks.

#### `duckdb_appender_column_type`

Returns the type of the column at the specified index. This is either a type in the active column list, or the same type
as a column in the receiving table.

Note: The resulting type must be destroyed with `duckdb_destroy_logical_type`.

##### Syntax

duckdb_logical_type duckdb_appender_column_type(
  duckdb_appender appender,
  idx_t col_idx
);

##### Parameters

* `appender`: The appender to get the column type from.
* `col_idx`: The index of the column to get the type of.

##### Return Value

The `duckdb_logical_type` of the column.

#### `duckdb_appender_error`

> Warning Deprecation notice. This method is scheduled for removal in a future release.
Use duckdb_appender_error_data instead.

Returns the error message associated with the appender.
If the appender has no error message, this returns `nullptr` instead.

The error message should not be freed. It will be de-allocated when `duckdb_appender_destroy` is called.

##### Syntax

const char *duckdb_appender_error(
  duckdb_appender appender
);

##### Parameters

* `appender`: The appender to get the error from.

##### Return Value

The error message, or `nullptr` if there is none.

#### `duckdb_appender_error_data`

Returns the error data associated with the appender.
Must be destroyed with duckdb_destroy_error_data.

##### Syntax

duckdb_error_data duckdb_appender_error_data(
  duckdb_appender appender
);

##### Parameters

* `appender`: The appender to get the error data from.

##### Return Value

The error data.

#### `duckdb_appender_flush`

Flush the appender to the table, forcing the cache of the appender to be cleared. If flushing the data triggers a
constraint violation or any other error, then all data is invalidated, and this function returns DuckDBError.
It is not possible to append more values. Call duckdb_appender_error_data to obtain the error data followed by
duckdb_appender_destroy to destroy the invalidated appender.

##### Syntax

duckdb_state duckdb_appender_flush(
  duckdb_appender appender
);

##### Parameters

* `appender`: The appender to flush.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_close`

Closes the appender by flushing all intermediate states and closing it for further appends. If flushing the data
triggers a constraint violation or any other error, then all data is invalidated, and this function returns DuckDBError.
Call duckdb_appender_error_data to obtain the error data followed by duckdb_appender_destroy to destroy the invalidated
appender.

##### Syntax

duckdb_state duckdb_appender_close(
  duckdb_appender appender
);

##### Parameters

* `appender`: The appender to flush and close.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_destroy`

Closes the appender by flushing all intermediate states to the table and destroying it. By destroying it, this function
de-allocates all memory associated with the appender. If flushing the data triggers a constraint violation,
then all data is invalidated, and this function returns DuckDBError. Due to the destruction of the appender, it is no
longer possible to obtain the specific error message with duckdb_appender_error. Therefore, call duckdb_appender_close
before destroying the appender, if you need insights into the specific error.

##### Syntax

duckdb_state duckdb_appender_destroy(
  duckdb_appender *appender
);

##### Parameters

* `appender`: The appender to flush, close and destroy.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_add_column`

Appends a column to the active column list of the appender. Immediately flushes all previous data.

The active column list specifies all columns that are expected when flushing the data. Any non-active columns are filled
with their default values, or NULL.

##### Syntax

duckdb_state duckdb_appender_add_column(
  duckdb_appender appender,
  const char *name
);

##### Parameters

* `appender`: The appender to add the column to.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_clear_columns`

Removes all columns from the active column list of the appender, resetting the appender to treat all columns as active.
Immediately flushes all previous data.

##### Syntax

duckdb_state duckdb_appender_clear_columns(
  duckdb_appender appender
);

##### Parameters

* `appender`: The appender to clear the columns from.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_appender_begin_row`

A nop function, provided for backwards compatibility reasons. Does nothing. Only `duckdb_appender_end_row` is required.

##### Syntax

duckdb_state duckdb_appender_begin_row(
  duckdb_appender appender
);

#### `duckdb_appender_end_row`

Finish the current row of appends. After end_row is called, the next row can be appended.

##### Syntax

duckdb_state duckdb_appender_end_row(
  duckdb_appender appender
);

##### Parameters

* `appender`: The appender.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_append_default`

Append a DEFAULT value (NULL if DEFAULT not available for column) to the appender.

##### Syntax

duckdb_state duckdb_append_default(
  duckdb_appender appender
);

#### `duckdb_append_default_to_chunk`

Append a DEFAULT value, at the specified row and column, (NULL if DEFAULT not available for column) to the chunk created
from the specified appender. The default value of the column must be a constant value. Non-deterministic expressions
like nextval('seq') or random() are not supported.

##### Syntax

duckdb_state duckdb_append_default_to_chunk(
  duckdb_appender appender,
  duckdb_data_chunk chunk,
  idx_t col,
  idx_t row
);

##### Parameters

* `appender`: The appender to get the default value from.
* `chunk`: The data chunk to append the default value to.
* `col`: The chunk column index to append the default value to.
* `row`: The chunk row index to append the default value to.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_append_bool`

Append a bool value to the appender.

##### Syntax

duckdb_state duckdb_append_bool(
  duckdb_appender appender,
  bool value
);

#### `duckdb_append_int8`

Append an int8_t value to the appender.

##### Syntax

duckdb_state duckdb_append_int8(
  duckdb_appender appender,
  int8_t value
);

#### `duckdb_append_int16`

Append an int16_t value to the appender.

##### Syntax

duckdb_state duckdb_append_int16(
  duckdb_appender appender,
  int16_t value
);

#### `duckdb_append_int32`

Append an int32_t value to the appender.

##### Syntax

duckdb_state duckdb_append_int32(
  duckdb_appender appender,
  int32_t value
);

#### `duckdb_append_int64`

Append an int64_t value to the appender.

##### Syntax

duckdb_state duckdb_append_int64(
  duckdb_appender appender,
  int64_t value
);

#### `duckdb_append_hugeint`

Append a duckdb_hugeint value to the appender.

##### Syntax

duckdb_state duckdb_append_hugeint(
  duckdb_appender appender,
  duckdb_hugeint value
);

#### `duckdb_append_uint8`

Append a uint8_t value to the appender.

##### Syntax

duckdb_state duckdb_append_uint8(
  duckdb_appender appender,
  uint8_t value
);

#### `duckdb_append_uint16`

Append a uint16_t value to the appender.

##### Syntax

duckdb_state duckdb_append_uint16(
  duckdb_appender appender,
  uint16_t value
);

#### `duckdb_append_uint32`

Append a uint32_t value to the appender.

##### Syntax

duckdb_state duckdb_append_uint32(
  duckdb_appender appender,
  uint32_t value
);

#### `duckdb_append_uint64`

Append a uint64_t value to the appender.

##### Syntax

duckdb_state duckdb_append_uint64(
  duckdb_appender appender,
  uint64_t value
);

#### `duckdb_append_uhugeint`

Append a duckdb_uhugeint value to the appender.

##### Syntax

duckdb_state duckdb_append_uhugeint(
  duckdb_appender appender,
  duckdb_uhugeint value
);

#### `duckdb_append_float`

Append a float value to the appender.

##### Syntax

duckdb_state duckdb_append_float(
  duckdb_appender appender,
  float value
);

#### `duckdb_append_double`

Append a double value to the appender.

##### Syntax

duckdb_state duckdb_append_double(
  duckdb_appender appender,
  double value
);

#### `duckdb_append_date`

Append a duckdb_date value to the appender.

##### Syntax

duckdb_state duckdb_append_date(
  duckdb_appender appender,
  duckdb_date value
);

#### `duckdb_append_time`

Append a duckdb_time value to the appender.

##### Syntax

duckdb_state duckdb_append_time(
  duckdb_appender appender,
  duckdb_time value
);

#### `duckdb_append_timestamp`

Append a duckdb_timestamp value to the appender.

##### Syntax

duckdb_state duckdb_append_timestamp(
  duckdb_appender appender,
  duckdb_timestamp value
);

#### `duckdb_append_interval`

Append a duckdb_interval value to the appender.

##### Syntax

duckdb_state duckdb_append_interval(
  duckdb_appender appender,
  duckdb_interval value
);

#### `duckdb_append_varchar`

Append a varchar value to the appender.

##### Syntax

duckdb_state duckdb_append_varchar(
  duckdb_appender appender,
  const char *val
);

#### `duckdb_append_varchar_length`

Append a varchar value to the appender.

##### Syntax

duckdb_state duckdb_append_varchar_length(
  duckdb_appender appender,
  const char *val,
  idx_t length
);

#### `duckdb_append_blob`

Append a blob value to the appender.

##### Syntax

duckdb_state duckdb_append_blob(
  duckdb_appender appender,
  const void *data,
  idx_t length
);

#### `duckdb_append_null`

Append a NULL value to the appender (of any type).

##### Syntax

duckdb_state duckdb_append_null(
  duckdb_appender appender
);

#### `duckdb_append_value`

Append a duckdb_value to the appender.

##### Syntax

duckdb_state duckdb_append_value(
  duckdb_appender appender,
  duckdb_value value
);

#### `duckdb_append_data_chunk`

Appends a pre-filled data chunk to the specified appender.
 Attempts casting, if the data chunk types do not match the active appender types.

##### Syntax

duckdb_state duckdb_append_data_chunk(
  duckdb_appender appender,
  duckdb_data_chunk chunk
);

##### Parameters

* `appender`: The appender to append to.
* `chunk`: The data chunk to append.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_table_description_create`

Creates a table description object. Note that `duckdb_table_description_destroy` should always be called on the
resulting table_description, even if the function returns `DuckDBError`.

##### Syntax

duckdb_state duckdb_table_description_create(
  duckdb_connection connection,
  const char *schema,
  const char *table,
  duckdb_table_description *out
);

##### Parameters

* `connection`: The connection context.
* `schema`: The schema of the table, or `nullptr` for the default schema.
* `table`: The table name.
* `out`: The resulting table description object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_table_description_create_ext`

Creates a table description object. Note that `duckdb_table_description_destroy` must be called on the resulting
table_description, even if the function returns `DuckDBError`.

##### Syntax

duckdb_state duckdb_table_description_create_ext(
  duckdb_connection connection,
  const char *catalog,
  const char *schema,
  const char *table,
  duckdb_table_description *out
);

##### Parameters

* `connection`: The connection context.
* `catalog`: The catalog (database) name of the table, or `nullptr` for the default catalog.
* `schema`: The schema of the table, or `nullptr` for the default schema.
* `table`: The table name.
* `out`: The resulting table description object.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_table_description_destroy`

Destroy the TableDescription object.

##### Syntax

void duckdb_table_description_destroy(
  duckdb_table_description *table_description
);

##### Parameters

* `table_description`: The table_description to destroy.

#### `duckdb_table_description_error`

Returns the error message associated with the given table_description.
If the table_description has no error message, this returns `nullptr` instead.
The error message should not be freed. It will be de-allocated when `duckdb_table_description_destroy` is called.

##### Syntax

const char *duckdb_table_description_error(
  duckdb_table_description table_description
);

##### Parameters

* `table_description`: The table_description to get the error from.

##### Return Value

The error message, or `nullptr` if there is none.

#### `duckdb_column_has_default`

Check if the column at 'index' index of the table has a DEFAULT expression.

##### Syntax

duckdb_state duckdb_column_has_default(
  duckdb_table_description table_description,
  idx_t index,
  bool *out
);

##### Parameters

* `table_description`: The table_description to query.
* `index`: The index of the column to query.
* `out`: The out-parameter used to store the result.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_table_description_get_column_name`

Obtain the column name at 'index'.
The out result must be destroyed with `duckdb_free`.

##### Syntax

char *duckdb_table_description_get_column_name(
  duckdb_table_description table_description,
  idx_t index
);

##### Parameters

* `table_description`: The table_description to query.
* `index`: The index of the column to query.

##### Return Value

The column name.

#### `duckdb_to_arrow_schema`

Transforms a DuckDB Schema into an Arrow Schema

##### Syntax

duckdb_error_data duckdb_to_arrow_schema(
  duckdb_arrow_options arrow_options,
  duckdb_logical_type *types,
  const char **names,
  idx_t column_count,
  struct ArrowSchema *out_schema
);

##### Parameters

* `arrow_options`: The Arrow settings used to produce arrow.
* `types`: The DuckDB logical types for each column in the schema.
* `names`: The names for each column in the schema.
* `column_count`: The number of columns that exist in the schema.
* `out_schema`: The resulting arrow schema. Must be destroyed with `out_schema->release(out_schema)`.

##### Return Value

The error data. Must be destroyed with `duckdb_destroy_error_data`.

#### `duckdb_data_chunk_to_arrow`

Transforms a DuckDB data chunk into an Arrow array.

##### Syntax

duckdb_error_data duckdb_data_chunk_to_arrow(
  duckdb_arrow_options arrow_options,
  duckdb_data_chunk chunk,
  struct ArrowArray *out_arrow_array
);

##### Parameters

* `arrow_options`: The Arrow settings used to produce arrow.
* `chunk`: The DuckDB data chunk to convert.
* `out_arrow_array`: The output Arrow structure that will hold the converted data. Must be released with
`out_arrow_array->release(out_arrow_array)`

##### Return Value

The error data. Must be destroyed with `duckdb_destroy_error_data`.

#### `duckdb_schema_from_arrow`

Transforms an Arrow Schema into a DuckDB Schema.

##### Syntax

duckdb_error_data duckdb_schema_from_arrow(
  duckdb_connection connection,
  struct ArrowSchema *schema,
  duckdb_arrow_converted_schema *out_types
);

##### Parameters

* `connection`: The connection to get the transformation settings from.
* `schema`: The input Arrow schema. Must be released with `schema->release(schema)`.
* `out_types`: The Arrow converted schema with extra information about the arrow types. Must be destroyed with
`duckdb_destroy_arrow_converted_schema`.

##### Return Value

The error data. Must be destroyed with `duckdb_destroy_error_data`.

#### `duckdb_data_chunk_from_arrow`

Transforms an Arrow array into a DuckDB data chunk. The data chunk will retain ownership of the underlying Arrow data.

##### Syntax

duckdb_error_data duckdb_data_chunk_from_arrow(
  duckdb_connection connection,
  struct ArrowArray *arrow_array,
  duckdb_arrow_converted_schema converted_schema,
  duckdb_data_chunk *out_chunk
);

##### Parameters

* `connection`: The connection to get the transformation settings from.
* `arrow_array`: The input Arrow array. Data ownership is passed on to DuckDB's DataChunk, the underlying object
does not need to be released and won't have ownership of the data.
* `converted_schema`: The Arrow converted schema with extra information about the arrow types.
* `out_chunk`: The resulting DuckDB data chunk. Must be destroyed by duckdb_destroy_data_chunk.

##### Return Value

The error data. Must be destroyed with `duckdb_destroy_error_data`.

#### `duckdb_destroy_arrow_converted_schema`

Destroys the arrow converted schema and de-allocates all memory allocated for that arrow converted schema.

##### Syntax

void duckdb_destroy_arrow_converted_schema(
  duckdb_arrow_converted_schema *arrow_converted_schema
);

##### Parameters

* `arrow_converted_schema`: The arrow converted schema to destroy.

#### `duckdb_query_arrow`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Executes a SQL query within a connection and stores the full (materialized) result in an arrow structure.
If the query fails to execute, DuckDBError is returned and the error message can be retrieved by calling
`duckdb_query_arrow_error`.

Note that after running `duckdb_query_arrow`, `duckdb_destroy_arrow` must be called on the result object even if the
query fails, otherwise the error stored within the result will not be freed correctly.

##### Syntax

duckdb_state duckdb_query_arrow(
  duckdb_connection connection,
  const char *query,
  duckdb_arrow *out_result
);

##### Parameters

* `connection`: The connection to perform the query in.
* `query`: The SQL query to run.
* `out_result`: The query result.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_query_arrow_schema`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Fetch the internal arrow schema from the arrow result. Remember to call release on the respective
ArrowSchema object.

##### Syntax

duckdb_state duckdb_query_arrow_schema(
  duckdb_arrow result,
  duckdb_arrow_schema *out_schema
);

##### Parameters

* `result`: The result to fetch the schema from.
* `out_schema`: The output schema.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_prepared_arrow_schema`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Fetch the internal arrow schema from the prepared statement. Remember to call release on the respective
ArrowSchema object.

##### Syntax

duckdb_state duckdb_prepared_arrow_schema(
  duckdb_prepared_statement prepared,
  duckdb_arrow_schema *out_schema
);

##### Parameters

* `prepared`: The prepared statement to fetch the schema from.
* `out_schema`: The output schema.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_result_arrow_array`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Convert a data chunk into an arrow struct array. Remember to call release on the respective
ArrowArray object.

##### Syntax

void duckdb_result_arrow_array(
  duckdb_result result,
  duckdb_data_chunk chunk,
  duckdb_arrow_array *out_array
);

##### Parameters

* `result`: The result object the data chunk have been fetched from.
* `chunk`: The data chunk to convert.
* `out_array`: The output array.

#### `duckdb_query_arrow_array`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Fetch an internal arrow struct array from the arrow result. Remember to call release on the respective
ArrowArray object.

This function can be called multiple time to get next chunks, which will free the previous out_array.
So consume the out_array before calling this function again.

##### Syntax

duckdb_state duckdb_query_arrow_array(
  duckdb_arrow result,
  duckdb_arrow_array *out_array
);

##### Parameters

* `result`: The result to fetch the array from.
* `out_array`: The output array.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_arrow_column_count`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Returns the number of columns present in the arrow result object.

##### Syntax

idx_t duckdb_arrow_column_count(
  duckdb_arrow result
);

##### Parameters

* `result`: The result object.

##### Return Value

The number of columns present in the result object.

#### `duckdb_arrow_row_count`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Returns the number of rows present in the arrow result object.

##### Syntax

idx_t duckdb_arrow_row_count(
  duckdb_arrow result
);

##### Parameters

* `result`: The result object.

##### Return Value

The number of rows present in the result object.

#### `duckdb_arrow_rows_changed`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Returns the number of rows changed by the query stored in the arrow result. This is relevant only for
INSERT/UPDATE/DELETE queries. For other queries the rows_changed will be 0.

##### Syntax

idx_t duckdb_arrow_rows_changed(
  duckdb_arrow result
);

##### Parameters

* `result`: The result object.

##### Return Value

The number of rows changed.

#### `duckdb_query_arrow_error`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

 Returns the error message contained within the result. The error is only set if `duckdb_query_arrow` returns
`DuckDBError`.

The error message should not be freed. It will be de-allocated when `duckdb_destroy_arrow` is called.

##### Syntax

const char *duckdb_query_arrow_error(
  duckdb_arrow result
);

##### Parameters

* `result`: The result object to fetch the error from.

##### Return Value

The error of the result.

#### `duckdb_destroy_arrow`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Closes the result and de-allocates all memory allocated for the arrow result.

##### Syntax

void duckdb_destroy_arrow(
  duckdb_arrow *result
);

##### Parameters

* `result`: The result to destroy.

#### `duckdb_destroy_arrow_stream`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Releases the arrow array stream and de-allocates its memory.

##### Syntax

void duckdb_destroy_arrow_stream(
  duckdb_arrow_stream *stream_p
);

##### Parameters

* `stream_p`: The arrow array stream to destroy.

#### `duckdb_execute_prepared_arrow`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Executes the prepared statement with the given bound parameters, and returns an arrow query result.
Note that after running `duckdb_execute_prepared_arrow`, `duckdb_destroy_arrow` must be called on the result object.

##### Syntax

duckdb_state duckdb_execute_prepared_arrow(
  duckdb_prepared_statement prepared_statement,
  duckdb_arrow *out_result
);

##### Parameters

* `prepared_statement`: The prepared statement to execute.
* `out_result`: The query result.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_arrow_scan`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Scans the Arrow stream and creates a view with the given name.

##### Syntax

duckdb_state duckdb_arrow_scan(
  duckdb_connection connection,
  const char *table_name,
  duckdb_arrow_stream arrow
);

##### Parameters

* `connection`: The connection on which to execute the scan.
* `table_name`: Name of the temporary view to create.
* `arrow`: Arrow stream wrapper.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_arrow_array_scan`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Scans the Arrow array and creates a view with the given name.
Note that after running `duckdb_arrow_array_scan`, `duckdb_destroy_arrow_stream` must be called on the out stream.

##### Syntax

duckdb_state duckdb_arrow_array_scan(
  duckdb_connection connection,
  const char *table_name,
  duckdb_arrow_schema arrow_schema,
  duckdb_arrow_array arrow_array,
  duckdb_arrow_stream *out_stream
);

##### Parameters

* `connection`: The connection on which to execute the scan.
* `table_name`: Name of the temporary view to create.
* `arrow_schema`: Arrow schema wrapper.
* `arrow_array`: Arrow array wrapper.
* `out_stream`: Output array stream that wraps around the passed schema, for releasing/deleting once done.

##### Return Value

`DuckDBSuccess` on success or `DuckDBError` on failure.

#### `duckdb_execute_tasks`

Execute DuckDB tasks on this thread.

Will return after `max_tasks` have been executed, or if there are no more tasks present.

##### Syntax

void duckdb_execute_tasks(
  duckdb_database database,
  idx_t max_tasks
);

##### Parameters

* `database`: The database object to execute tasks for
* `max_tasks`: The maximum amount of tasks to execute

#### `duckdb_create_task_state`

Creates a task state that can be used with duckdb_execute_tasks_state to execute tasks until
`duckdb_finish_execution` is called on the state.

`duckdb_destroy_state` must be called on the result.

##### Syntax

duckdb_task_state duckdb_create_task_state(
  duckdb_database database
);

##### Parameters

* `database`: The database object to create the task state for

##### Return Value

The task state that can be used with duckdb_execute_tasks_state.

#### `duckdb_execute_tasks_state`

Execute DuckDB tasks on this thread.

The thread will keep on executing tasks forever, until duckdb_finish_execution is called on the state.
Multiple threads can share the same duckdb_task_state.

##### Syntax

void duckdb_execute_tasks_state(
  duckdb_task_state state
);

##### Parameters

* `state`: The task state of the executor

#### `duckdb_execute_n_tasks_state`

Execute DuckDB tasks on this thread.

The thread will keep on executing tasks until either duckdb_finish_execution is called on the state,
max_tasks tasks have been executed or there are no more tasks to be executed.

Multiple threads can share the same duckdb_task_state.

##### Syntax

idx_t duckdb_execute_n_tasks_state(
  duckdb_task_state state,
  idx_t max_tasks
);

##### Parameters

* `state`: The task state of the executor
* `max_tasks`: The maximum amount of tasks to execute

##### Return Value

The amount of tasks that have actually been executed

#### `duckdb_finish_execution`

Finish execution on a specific task.

##### Syntax

void duckdb_finish_execution(
  duckdb_task_state state
);

##### Parameters

* `state`: The task state to finish execution

#### `duckdb_task_state_is_finished`

Check if the provided duckdb_task_state has finished execution

##### Syntax

bool duckdb_task_state_is_finished(
  duckdb_task_state state
);

##### Parameters

* `state`: The task state to inspect

##### Return Value

Whether or not duckdb_finish_execution has been called on the task state

#### `duckdb_destroy_task_state`

Destroys the task state returned from duckdb_create_task_state.

Note that this should not be called while there is an active duckdb_execute_tasks_state running
on the task state.

##### Syntax

void duckdb_destroy_task_state(
  duckdb_task_state state
);

##### Parameters

* `state`: The task state to clean up

#### `duckdb_execution_is_finished`

Returns true if the execution of the current query is finished.

##### Syntax

bool duckdb_execution_is_finished(
  duckdb_connection con
);

##### Parameters

* `con`: The connection on which to check

#### `duckdb_stream_fetch_chunk`

> Warning Deprecation notice. This method is scheduled for removal in a future release.

Fetches a data chunk from the (streaming) duckdb_result. This function should be called repeatedly until the result is
exhausted.

The result must be destroyed with `duckdb_destroy_data_chunk`.

This function can only be used on duckdb_results created with 'duckdb_pending_prepared_streaming'

If this function is used, none of the other result functions can be used and vice versa (i.e., this function cannot be
mixed with the legacy result functions or the materialized result functions).

It is not known beforehand how many chunks will be returned by this result.

##### Syntax

duckdb_data_chunk duckdb_stream_fetch_chunk(
  duckdb_result result
);

##### Parameters

* `result`: The result object to fetch the data chunk from.

##### Return Value

The resulting data chunk. Returns `NULL` if the result has an error.

#### `duckdb_fetch_chunk`

Fetches a data chunk from a duckdb_result. This function should be called repeatedly until the result is exhausted.

The result must be destroyed with `duckdb_destroy_data_chunk`.

It is not known beforehand how many chunks will be returned by this result.

##### Syntax

duckdb_data_chunk duckdb_fetch_chunk(
  duckdb_result result
);

##### Parameters

* `result`: The result object to fetch the data chunk from.

##### Return Value

The resulting data chunk. Returns `NULL` if the result has an error.

#### `duckdb_create_cast_function`

Creates a new cast function object.

##### Return Value

The cast function object.

##### Syntax

duckdb_cast_function duckdb_create_cast_function(

);

#### `duckdb_cast_function_set_source_type`

Sets the source type of the cast function.

##### Syntax

void duckdb_cast_function_set_source_type(
  duckdb_cast_function cast_function,
  duckdb_logical_type source_type
);

##### Parameters

* `cast_function`: The cast function object.
* `source_type`: The source type to set.

#### `duckdb_cast_function_set_target_type`

Sets the target type of the cast function.

##### Syntax

void duckdb_cast_function_set_target_type(
  duckdb_cast_function cast_function,
  duckdb_logical_type target_type
);

##### Parameters

* `cast_function`: The cast function object.
* `target_type`: The target type to set.

#### `duckdb_cast_function_set_implicit_cast_cost`

Sets the "cost" of implicitly casting the source type to the target type using this function.

##### Syntax

void duckdb_cast_function_set_implicit_cast_cost(
  duckdb_cast_function cast_function,
  int64_t cost
);

##### Parameters

* `cast_function`: The cast function object.
* `cost`: The cost to set.

#### `duckdb_cast_function_set_function`

Sets the actual cast function to use.

##### Syntax

void duckdb_cast_function_set_function(
  duckdb_cast_function cast_function,
  duckdb_cast_function_t function
);

##### Parameters

* `cast_function`: The cast function object.
* `function`: The function to set.

#### `duckdb_cast_function_set_extra_info`

Assigns extra information to the cast function that can be fetched during execution, etc.

##### Syntax

void duckdb_cast_function_set_extra_info(
  duckdb_cast_function cast_function,
  void *extra_info,
  duckdb_delete_callback_t destroy
);

##### Parameters

* `extra_info`: The extra information
* `destroy`: The callback that will be called to destroy the extra information (if any)

#### `duckdb_cast_function_get_extra_info`

Retrieves the extra info of the function as set in `duckdb_cast_function_set_extra_info`.

##### Syntax

void *duckdb_cast_function_get_extra_info(
  duckdb_function_info info
);

##### Parameters

* `info`: The info object.

##### Return Value

The extra info.

#### `duckdb_cast_function_get_cast_mode`

Get the cast execution mode from the given function info.

##### Syntax

duckdb_cast_mode duckdb_cast_function_get_cast_mode(
  duckdb_function_info info
);

##### Parameters

* `info`: The info object.

##### Return Value

The cast mode.

#### `duckdb_cast_function_set_error`

Report that an error has occurred while executing the cast function.

##### Syntax

void duckdb_cast_function_set_error(
  duckdb_function_info info,
  const char *error
);

##### Parameters

* `info`: The info object.
* `error`: The error message.

#### `duckdb_cast_function_set_row_error`

Report that an error has occurred while executing the cast function, setting the corresponding output row to NULL.

##### Syntax

void duckdb_cast_function_set_row_error(
  duckdb_function_info info,
  const char *error,
  idx_t row,
  duckdb_vector output
);

##### Parameters

* `info`: The info object.
* `error`: The error message.
* `row`: The index of the row within the output vector to set to NULL.
* `output`: The output vector.

#### `duckdb_register_cast_function`

Registers a cast function within the given connection.

##### Syntax

duckdb_state duckdb_register_cast_function(
  duckdb_connection con,
  duckdb_cast_function cast_function
);

##### Parameters

* `con`: The connection to use.
* `cast_function`: The cast function to register.

##### Return Value

Whether or not the registration was successful.

#### `duckdb_destroy_cast_function`

Destroys the cast function object.

##### Syntax

void duckdb_destroy_cast_function(
  duckdb_cast_function *cast_function
);

##### Parameters

* `cast_function`: The cast function object.

#### `duckdb_destroy_expression`

Destroys the expression and de-allocates its memory.

##### Syntax

void duckdb_destroy_expression(
  duckdb_expression *expr
);

##### Parameters

* `expr`: A pointer to the expression.

#### `duckdb_expression_return_type`

Returns the return type of an expression.

##### Syntax

duckdb_logical_type duckdb_expression_return_type(
  duckdb_expression expr
);

##### Parameters

* `expr`: The expression.

##### Return Value

The return type. Must be destroyed with `duckdb_destroy_logical_type`.

#### `duckdb_expression_is_foldable`

Returns whether the expression is foldable into a value or not.

##### Syntax

bool duckdb_expression_is_foldable(
  duckdb_expression expr
);

##### Parameters

* `expr`: The expression.

##### Return Value

True, if the expression is foldable, else false.

#### `duckdb_expression_fold`

Folds an expression creating a folded value.

##### Syntax

duckdb_error_data duckdb_expression_fold(
  duckdb_client_context context,
  duckdb_expression expr,
  duckdb_value *out_value
);

##### Parameters

* `context`: The client context.
* `expr`: The expression. Must be foldable.
* `out_value`: The folded value, if folding was successful. Must be destroyed with `duckdb_destroy_value`.

##### Return Value

The error data. Must be destroyed with `duckdb_destroy_error_data`.
