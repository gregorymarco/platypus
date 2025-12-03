#!/bin/bash
# Quick build script for Platypus compiler

set -e

cd "$(dirname "$0")"

echo "=== Building Platypus Compiler ==="

mkdir -p build

# Compile parser (full compiler)
echo "Compiling parser..."
gcc -Wall -Wextra -std=c99 -O2 \
    compiler/buffer.c \
    compiler/scanner.c \
    compiler/parser.c \
    compiler/platy.c \
    -o build/platypus

# Compile scanner-only tool
echo "Compiling scanner..."
gcc -Wall -Wextra -std=c99 -O2 \
    compiler/buffer.c \
    compiler/scanner.c \
    compiler/platy_st.c \
    -o build/platy_scanner

echo ""
echo "Build complete!"
echo "  Parser:  ./build/platypus <source.pls>"
echo "  Scanner: ./build/platy_scanner <source.pls>"

