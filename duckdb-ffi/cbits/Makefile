# DuckDB library fetch and install
DUCKDB_VERSION = 1.5.6
DUCKDB_URL = https://github.com/duckdb/duckdb/releases/download/v$(DUCKDB_VERSION)/libduckdb-linux-amd64.zip
EXPECTED_SHA256 = b845005f5132a7d8180057c35e14a7626632258782f871a90861b19c1c03841b
ARCHIVE = libduckdb-$(DUCKDB_VERSION)-linux-amd64.zip
EXTRACT_DIR = duckdb/$(DUCKDB_VERSION)/linux-amd64
INSTALL_DIR ?= $(HOME)/.local/lib

.PHONY: all fetch install clean distclean

all: fetch

fetch: $(EXTRACT_DIR)/libduckdb.so

$(ARCHIVE):
	@echo "Downloading libduckdb-linux-amd64.zip..."
	curl --proto '=https' --proto-redir '=https' -fsSL -o $(ARCHIVE).tmp "$(DUCKDB_URL)"
	mv $(ARCHIVE).tmp $(ARCHIVE)

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
	mkdir -p $(EXTRACT_DIR)
	unzip -o $(ARCHIVE) -d $(EXTRACT_DIR)/
	@touch $(EXTRACT_DIR)/libduckdb.so

install: $(EXTRACT_DIR)/libduckdb.so
	@echo "Installing DuckDB libraries to $(INSTALL_DIR)..."
	mkdir -p $(INSTALL_DIR)
	install -m 755 $(EXTRACT_DIR)/libduckdb.so $(INSTALL_DIR)/libduckdb.so.$(DUCKDB_VERSION)
	ln -sf libduckdb.so.$(DUCKDB_VERSION) $(INSTALL_DIR)/libduckdb.so.1.5
	ln -sf libduckdb.so.1.5 $(INSTALL_DIR)/libduckdb.so
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
