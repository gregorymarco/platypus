# Platypus Compiler Makefile
CC = gcc
CFLAGS = -Wall -Wextra -std=c99 -pedantic
DEBUG_FLAGS = -g -DDEBUG
RELEASE_FLAGS = -O2

SRC_DIR = compiler
BUILD_DIR = build

# Source files
COMMON_SRC = $(SRC_DIR)/buffer.c $(SRC_DIR)/scanner.c
PARSER_SRC = $(COMMON_SRC) $(SRC_DIR)/parser.c $(SRC_DIR)/platy.c
SCANNER_SRC = $(COMMON_SRC) $(SRC_DIR)/platy_st.c

# Targets
.PHONY: all clean debug release scanner parser test

all: release

release: CFLAGS += $(RELEASE_FLAGS)
release: parser scanner

debug: CFLAGS += $(DEBUG_FLAGS)
debug: parser scanner

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

parser: $(BUILD_DIR)
	$(CC) $(CFLAGS) -o $(BUILD_DIR)/platypus $(PARSER_SRC)
	@echo "Built: $(BUILD_DIR)/platypus (parser)"

scanner: $(BUILD_DIR)
	$(CC) $(CFLAGS) -o $(BUILD_DIR)/platy_scanner $(SCANNER_SRC)
	@echo "Built: $(BUILD_DIR)/platy_scanner (scanner only)"

test: release
	@echo "=== Running test files ==="
	@for f in test-files/source/*.pls; do \
		echo "\n--- Testing $$f ---"; \
		$(BUILD_DIR)/platypus "$$f" || true; \
	done

clean:
	rm -rf $(BUILD_DIR)

# Individual object compilation (for incremental builds)
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

help:
	@echo "Platypus Compiler Build System"
	@echo ""
	@echo "Targets:"
	@echo "  make          - Build release binaries"
	@echo "  make debug    - Build with debug symbols"
	@echo "  make scanner  - Build scanner-only executable"
	@echo "  make parser   - Build full parser executable"
	@echo "  make test     - Run all test files"
	@echo "  make clean    - Remove build artifacts"

