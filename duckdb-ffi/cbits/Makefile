# DuckDB library fetch and install
DUCKDB_VERSION = 1.5.0
DUCKDB_URL = https://github.com/duckdb/duckdb/releases/download/v$(DUCKDB_VERSION)/libduckdb-linux-amd64.zip
EXPECTED_SHA256 = e9b2adb9dc5348da57eee9562782e0e7f543e74ff26d634bc7c208a5b6c59a1c
ARCHIVE = libduckdb.zip
EXTRACT_DIR = duckdb
INSTALL_DIR = /usr/lib

.PHONY: all fetch install clean distclean

all: fetch

fetch: $(EXTRACT_DIR)/libduckdb.so

$(ARCHIVE):
	@echo "Downloading libduckdb-linux-amd64.zip..."
	curl -fsSL -o $(ARCHIVE) "$(DUCKDB_URL)"

$(EXTRACT_DIR)/libduckdb.so: $(ARCHIVE)
	@echo "Verifying SHA256 checksum..."
	@ACTUAL_SHA256=$$(sha256sum $(ARCHIVE) | cut -d' ' -f1); \
	if [ "$$ACTUAL_SHA256" != "$(EXPECTED_SHA256)" ]; then \
		echo "ERROR: SHA256 mismatch!" >&2; \
		echo "  Expected: $(EXPECTED_SHA256)" >&2; \
		echo "  Actual:   $$ACTUAL_SHA256" >&2; \
		exit 1; \
	fi
	@echo "Extracting archive..."
	unzip $(ARCHIVE) -d $(EXTRACT_DIR)/
	@touch $(EXTRACT_DIR)/libduckdb.so

install: $(EXTRACT_DIR)/libduckdb.so
	@echo "Installing DuckDB libraries to $(INSTALL_DIR)..."
	sudo cp $(EXTRACT_DIR)/libduckdb.so.1.5.0 $(INSTALL_DIR)/
	sudo ln -sf libduckdb.so.1.5.0 $(INSTALL_DIR)/libduckdb.so.1.5
	sudo ln -sf libduckdb.so.1.5 $(INSTALL_DIR)/libduckdb.so
	sudo ldconfig
	@echo "DuckDB libraries installed successfully."

clean:
	rm -f $(ARCHIVE)

distclean: clean
	rm -rf $(EXTRACT_DIR)

# Show current status
status:
	@echo "Archive: $(if $(wildcard $(ARCHIVE)),present,missing)"
	@echo "Extracted: $(if $(wildcard $(EXTRACT_DIR)/libduckdb.so),yes,no)"
	@echo "Installed: $(if $(wildcard $(INSTALL_DIR)/libduckdb.so),yes,no)"
