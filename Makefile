# Platypus Compiler Makefile
CC = gcc
CFLAGS = -Wall -Wextra -std=c99
DEBUG_FLAGS = -g -DDEBUG
RELEASE_FLAGS = -O2

SRC_DIR = compiler
BUILD_DIR = build

# Source files
COMMON_SRC = $(SRC_DIR)/buffer.c $(SRC_DIR)/scanner.c
PARSER_SRC = $(COMMON_SRC) $(SRC_DIR)/parser.c $(SRC_DIR)/platy.c
TRANS_SRC = $(COMMON_SRC) $(SRC_DIR)/codegen.c $(SRC_DIR)/transpiler.c

# Targets
.PHONY: all clean debug release scanner parser transpiler test

all: release

release: CFLAGS += $(RELEASE_FLAGS)
release: parser scanner transpiler

debug: CFLAGS += $(DEBUG_FLAGS)
debug: parser scanner transpiler

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

parser: $(BUILD_DIR)
	$(CC) $(CFLAGS) -o $(BUILD_DIR)/platypus $(PARSER_SRC)
	@echo "Built: $(BUILD_DIR)/platypus (syntax checker)"

scanner: $(BUILD_DIR)
	$(CC) $(CFLAGS) -o $(BUILD_DIR)/platy_scanner $(SCANNER_SRC)
	@echo "Built: $(BUILD_DIR)/platy_scanner (tokenizer)"

transpiler: $(BUILD_DIR)
	$(CC) $(CFLAGS) -o $(BUILD_DIR)/platy_trans $(TRANS_SRC)
	@echo "Built: $(BUILD_DIR)/platy_trans (C transpiler)"

test: release
	@echo "=== Transpiling and running test files ==="
	@for f in test-files/source/*.pls; do \
		echo "\n--- Testing $$f ---"; \
		$(BUILD_DIR)/platy_trans "$$f" && \
		gcc -o /tmp/platy_test "$${f%.pls}.c" -w && \
		echo "Compiled successfully!" || true; \
	done

clean:
	rm -rf $(BUILD_DIR)
	rm -f test-files/source/*.c

help:
	@echo "Platypus Compiler Build System"
	@echo ""
	@echo "Targets:"
	@echo "  make            - Build all tools (release mode)"
	@echo "  make debug      - Build with debug symbols"
	@echo "  make parser     - Build syntax checker"
	@echo "  make scanner    - Build tokenizer"
	@echo "  make transpiler - Build C transpiler"
	@echo "  make test       - Transpile and compile all test files"
	@echo "  make clean      - Remove build artifacts"
	@echo ""
	@echo "Usage:"
	@echo "  ./platyc source.pls [output]  - Compile PLATYPUS to executable"

