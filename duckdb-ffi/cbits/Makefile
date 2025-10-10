# DuckDB library fetch and install
DUCKDB_VERSION = 1.4.0
DUCKDB_URL = https://github.com/duckdb/duckdb/releases/download/v$(DUCKDB_VERSION)/libduckdb-linux-amd64.zip
EXPECTED_SHA256 = 3e7a902ee6cddf7d55035903cf26f14a9a45302bed6fbd379cf34e1c9cc9983f
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
	sudo cp $(EXTRACT_DIR)/libduckdb.so.1.4.0 $(INSTALL_DIR)/
	sudo ln -sf libduckdb.so.1.4.0 $(INSTALL_DIR)/libduckdb.so.1.4
	sudo ln -sf libduckdb.so.1.4 $(INSTALL_DIR)/libduckdb.so
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
