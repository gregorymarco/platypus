/********************************************************************************
Filename: codegen.c
Purpose: C code generator implementation for the PLATYPUS transpiler. This module
         emits valid C99 source code from PLATYPUS program constructs. Handles
         variable declarations, expressions, control flow, and I/O operations.

Compiler: GCC / Clang (C99 compatible)
Author: Marco Gregory
Course: Compilers - PLATYPUS Transpiler Extension
Version: 1.0
Date: December 2025

Design Decisions:
    1. Single-pass emission: Code is emitted directly during parsing rather than
       building an AST. This simplifies implementation but prevents optimization.
    
    2. Lazy variable declaration: Variables are declared in C on first use rather
       than collected and declared at function start. This matches PLATYPUS
       semantics where variables don't need explicit declaration.
    
    3. String concatenation strategy: Uses a rotating buffer pool (STR_BUFS=8)
       to safely handle nested str_concat() calls like str_concat(str_concat(a,b),c).
       Each call gets a fresh buffer, preventing overwrites.
    
    4. All arithmetic as double: To avoid int/float ambiguity and match PLATYPUS
       mixed arithmetic semantics, all numeric variables are C doubles.
    
    5. Inline declarations: Variables declared at point of first use rather than
       hoisted to function start. Valid C99 but not C89.

Edge Cases Handled:
    - Empty program (just PLATYPUS{}) - outputs valid empty main()
    - Variables with $ suffix - correctly identified as strings
    - Nested string concatenations - rotating buffers prevent corruption
    - Empty WRITE() - outputs just newline
    - Missing variable declaration - lazily declared on first use

Known Limitations:
    - Symbol table is O(n) linear search - acceptable for small programs
    - No escape sequence handling in string literals
    - String buffer overflow possible with very long concatenations
    - No support for string comparison in relational expressions

Function List:
    emit_indent, find_or_add_symbol, find_symbol, cg_init, cg_finish,
    cg_program_start, cg_program_end, cg_declare_var, cg_assign_start,
    cg_assign_end, cg_emit_int, cg_emit_float, cg_emit_string, cg_emit_var,
    cg_emit_op, cg_emit_unary, cg_emit_lparen, cg_emit_rparen, cg_emit_concat,
    cg_if_start, cg_if_condition_end, cg_if_then, cg_if_else, cg_if_end,
    cg_while_start, cg_while_condition_end, cg_while_body_start, cg_while_end,
    cg_emit_relop, cg_emit_logop, cg_read_start, cg_read_var, cg_read_next,
    cg_read_end, cg_write_start, cg_write_string, cg_write_var, cg_write_next,
    cg_write_end, cg_write_newline, cg_get_var_type, cg_stmt_end

********************************************************************************/

#include "codegen.h"
#include <string.h>
#include <stdlib.h>

/********************************************************************************
 * STATIC VARIABLES (Module State)
 ********************************************************************************/

static FILE *outfile = NULL;      /* Output file handle for generated C code */
static Symbol symbols[MAX_VARS];  /* Symbol table for variable tracking */
static int sym_count = 0;         /* Current number of symbols in table */
static int indent_level = 1;      /* Current indentation depth (1 = inside main) */
static int in_write = 0;          /* Flag: currently processing WRITE statement */
static int write_count = 0;       /* Number of items in current WRITE statement */

/********************************************************************************
 * HELPER FUNCTIONS
 ********************************************************************************/

/********************************************************
Purpose: Emit indentation spaces to output file. Uses 4 spaces per indent level.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: fprintf
Parameters: None (uses static indent_level)
Return Value: None
Algorithm: Loops indent_level times, emitting 4 spaces each iteration.
Edge Case: indent_level of 0 emits nothing (valid for global scope if needed)
*********************************************************/
static void emit_indent(void) {
    for (int i = 0; i < indent_level; i++) {
        fprintf(outfile, "    ");
    }
}

/********************************************************
Purpose: Find existing symbol or add new one to symbol table.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: strcmp, strncpy
Parameters: 
    name - variable identifier to find/add (null-terminated)
    type - VarType for new symbols (ignored if symbol exists)
Return Value: 
    Pointer to Symbol entry (existing or new), or NULL if table full
Algorithm:
    1. Linear search through existing symbols
    2. If found, return pointer to existing entry
    3. If not found and space available, add new entry and return pointer
    4. If table full, return NULL (caller should handle error)
Edge Cases:
    - Duplicate names: Returns existing entry, ignoring new type
    - Table full: Returns NULL (MAX_VARS exceeded)
    - Name longer than MAX_VAR_NAME-1: Truncated with null terminator
*********************************************************/
static Symbol* find_or_add_symbol(const char *name, VarType type) {
    /* Search existing symbols - O(n) linear scan */
    for (int i = 0; i < sym_count; i++) {
        if (strcmp(symbols[i].name, name) == 0) {
            return &symbols[i];
        }
    }
    
    /* Add new symbol if space available */
    if (sym_count < MAX_VARS) {
        strncpy(symbols[sym_count].name, name, MAX_VAR_NAME - 1);
        symbols[sym_count].name[MAX_VAR_NAME - 1] = '\0'; /* Ensure termination */
        symbols[sym_count].type = type;
        symbols[sym_count].declared = 0; /* Not yet emitted to C code */
        return &symbols[sym_count++];
    }
    
    /* Table full - should emit error but return NULL for now */
    return NULL;
}

/********************************************************
Purpose: Find existing symbol in table without adding.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: strcmp
Parameters: name - variable identifier to find
Return Value: Pointer to Symbol if found, NULL otherwise
*********************************************************/
static Symbol* find_symbol(const char *name) {
    for (int i = 0; i < sym_count; i++) {
        if (strcmp(symbols[i].name, name) == 0) {
            return &symbols[i];
        }
    }
    return NULL;
}

/********************************************************************************
 * INITIALIZATION AND CLEANUP
 ********************************************************************************/

/********************************************************
Purpose: Initialize code generator state for new translation.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: None
Parameters: out - FILE pointer for C code output (must be open for writing)
Return Value: None
Edge Case: NULL file pointer causes undefined behavior on first write
*********************************************************/
void cg_init(FILE *out) {
    outfile = out;
    sym_count = 0;          /* Reset symbol table */
    indent_level = 1;       /* Start inside main() */
    in_write = 0;
    write_count = 0;
}

/********************************************************
Purpose: Finalize code generation, close output file if needed.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: fclose
Parameters: None
Return Value: None
Design Choice: Does not close stdout to allow console output mode
*********************************************************/
void cg_finish(void) {
    if (outfile && outfile != stdout) {
        fclose(outfile);
    }
    outfile = NULL;
}

/********************************************************************************
 * PROGRAM STRUCTURE EMISSION
 ********************************************************************************/

/********************************************************
Purpose: Emit C program header including includes, helpers, and main() opening.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: fprintf
Parameters: None
Return Value: None
Algorithm:
    1. Emit file header comment
    2. Emit required #includes (stdio, string, stdlib)
    3. Emit STR_MAX constant for string buffer size
    4. Emit str_concat helper function with rotating buffers
    5. Emit main() signature and opening brace

Design Decisions:
    - str_concat uses rotating buffers (STR_BUFS=8) to safely handle nesting
    - Uses snprintf for buffer overflow protection
    - Returns pointer to static buffer (no allocation, but not thread-safe)
*********************************************************/
void cg_program_start(void) {
    fprintf(outfile, "/* Generated C code from PLATYPUS */\n");
    fprintf(outfile, "#include <stdio.h>\n");
    fprintf(outfile, "#include <string.h>\n");
    fprintf(outfile, "#include <stdlib.h>\n\n");
    
    /* String buffer size - matches PLATYPUS practical string limits */
    fprintf(outfile, "#define STR_MAX 256\n");
    
    /* Number of rotating buffers for nested concatenation
     * 8 buffers supports up to 8 levels of nesting like:
     * str_concat(str_concat(str_concat(...)))
     */
    fprintf(outfile, "#define STR_BUFS 8\n\n");
    
    /* String concatenation helper function
     * Design: Uses rotating buffer pool to prevent overwrites in nested calls.
     * Each call uses next buffer in rotation, wrapping at STR_BUFS.
     * Example: str_concat(str_concat("a","b"),"c") uses buffers 0 and 1.
     */
    fprintf(outfile, "/* String concatenation helper - uses rotating buffers for nesting */\n");
    fprintf(outfile, "static char* str_concat(const char *a, const char *b) {\n");
    fprintf(outfile, "    static char bufs[STR_BUFS][STR_MAX];\n");
    fprintf(outfile, "    static int idx = 0;\n");
    fprintf(outfile, "    char *buf = bufs[idx];\n");
    fprintf(outfile, "    idx = (idx + 1) %% STR_BUFS;\n");
    fprintf(outfile, "    snprintf(buf, STR_MAX, \"%%s%%s\", a, b);\n");
    fprintf(outfile, "    return buf;\n");
    fprintf(outfile, "}\n\n");
    
    fprintf(outfile, "int main(void) {\n");
}

/********************************************************
Purpose: Emit main() closing with return statement.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: emit_indent, fprintf
Parameters: None
Return Value: None
Note: Variable declarations are emitted inline (lazy declaration)
      rather than collected and emitted here.
*********************************************************/
void cg_program_end(void) {
    emit_indent();
    fprintf(outfile, "return 0;\n");
    fprintf(outfile, "}\n");
}

/********************************************************************************
 * VARIABLE DECLARATION
 ********************************************************************************/

/********************************************************
Purpose: Emit C variable declaration if not already declared.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: find_or_add_symbol, emit_indent, fprintf
Parameters:
    name - variable identifier (null-terminated, max 8 chars significant)
    type - VAR_INT, VAR_FLOAT, or VAR_STRING
Return Value: None
Algorithm:
    1. Find or add symbol to table
    2. If already declared, do nothing (idempotent)
    3. If new, emit appropriate C declaration with initialization
Design Choices:
    - VAR_INT: int with 0 initialization
    - VAR_FLOAT: double with 0.0 initialization (handles mixed arithmetic)
    - VAR_STRING: char array[STR_MAX] with empty string initialization
Edge Case: Symbol table full - silently fails (should add error handling)
*********************************************************/
void cg_declare_var(const char *name, VarType type) {
    Symbol *sym = find_or_add_symbol(name, type);
    
    if (sym && !sym->declared) {
        emit_indent();
        switch (type) {
            case VAR_INT:
                fprintf(outfile, "int %s = 0;\n", name);
                break;
            case VAR_FLOAT:
                /* Use double for all arithmetic to handle mixed int/float */
                fprintf(outfile, "double %s = 0.0;\n", name);
                break;
            case VAR_STRING:
                /* Fixed-size buffer with empty string initialization */
                fprintf(outfile, "char %s[STR_MAX] = \"\";\n", name);
                break;
        }
        sym->declared = 1; /* Mark as declared to prevent duplicates */
    }
}

/********************************************************************************
 * ASSIGNMENT EMISSION
 * Note: These functions are partially implemented. The transpiler.c module
 * handles most assignment logic directly for better control over output format.
 ********************************************************************************/

/********************************************************
Purpose: Begin emitting an assignment statement.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: find_symbol, emit_indent, fprintf
Parameters: varname - target variable for assignment
Return Value: None
Note: String assignments use strcpy(), arithmetic use direct assignment.
*********************************************************/
void cg_assign_start(const char *varname) {
    Symbol *sym = find_symbol(varname);
    emit_indent();
    if (sym && sym->type == VAR_STRING) {
        fprintf(outfile, "strcpy(%s, ", varname);
    } else {
        fprintf(outfile, "%s = ", varname);
    }
}

/********************************************************
Purpose: End assignment statement (close strcpy paren if needed).
Author: Marco Gregory  
History/Versions: 1.0
Note: Simplified - assumes caller handles context correctly.
*********************************************************/
void cg_assign_end(void) {
    fprintf(outfile, ");\n");
}

/********************************************************************************
 * EXPRESSION VALUE EMISSION
 ********************************************************************************/

/********************************************************
Purpose: Emit integer literal value.
Author: Marco Gregory
History/Versions: 1.0
*********************************************************/
void cg_emit_int(int value) {
    fprintf(outfile, "%d", value);
}

/********************************************************
Purpose: Emit floating-point literal value.
Author: Marco Gregory
History/Versions: 1.0
Note: Cast to double for consistent output precision.
*********************************************************/
void cg_emit_float(float value) {
    fprintf(outfile, "%f", (double)value);
}

/********************************************************
Purpose: Emit string literal with quotes.
Author: Marco Gregory
History/Versions: 1.0
Edge Case: No escape sequence processing - assumes clean input.
*********************************************************/
void cg_emit_string(const char *str) {
    fprintf(outfile, "\"%s\"", str);
}

/********************************************************
Purpose: Emit variable reference.
Author: Marco Gregory
History/Versions: 1.0
*********************************************************/
void cg_emit_var(const char *name) {
    fprintf(outfile, "%s", name);
}

/********************************************************
Purpose: Emit binary operator with spacing.
Author: Marco Gregory
History/Versions: 1.0
*********************************************************/
void cg_emit_op(char op) {
    fprintf(outfile, " %c ", op);
}

/********************************************************
Purpose: Emit unary operator (no spacing).
Author: Marco Gregory
History/Versions: 1.0
*********************************************************/
void cg_emit_unary(char op) {
    fprintf(outfile, "%c", op);
}

void cg_emit_lparen(void) {
    fprintf(outfile, "(");
}

void cg_emit_rparen(void) {
    fprintf(outfile, ")");
}

/********************************************************
Purpose: Emit comma separator for str_concat arguments.
Author: Marco Gregory
History/Versions: 1.0
Note: Caller is responsible for str_concat( wrapper.
*********************************************************/
void cg_emit_concat(void) {
    fprintf(outfile, ", ");
}

/********************************************************************************
 * CONTROL FLOW - IF STATEMENT
 ********************************************************************************/

/********************************************************
Purpose: Begin IF statement with optional condition negation.
Author: Marco Gregory
History/Versions: 1.0
Parameters: precond_true - 1 for IF TRUE(...), 0 for IF FALSE(...)
Design: PLATYPUS IF FALSE(x) maps to C if(!(x))
*********************************************************/
void cg_if_start(int precond_true) {
    emit_indent();
    if (precond_true) {
        fprintf(outfile, "if (");
    } else {
        fprintf(outfile, "if (!(");
    }
}

/********************************************************
Purpose: End IF condition, open body block.
Author: Marco Gregory
History/Versions: 1.0
Note: Closes extra paren if precond was FALSE.
*********************************************************/
void cg_if_condition_end(void) {
    fprintf(outfile, ")) {\n");
    indent_level++;
}

void cg_if_then(void) {
    /* Body handling done by condition_end */
}

void cg_if_else(void) {
    indent_level--;
    emit_indent();
    fprintf(outfile, "} else {\n");
    indent_level++;
}

void cg_if_end(void) {
    indent_level--;
    emit_indent();
    fprintf(outfile, "}\n");
}

/********************************************************************************
 * CONTROL FLOW - WHILE STATEMENT
 ********************************************************************************/

/********************************************************
Purpose: Begin WHILE statement with optional condition negation.
Author: Marco Gregory
History/Versions: 1.0
Parameters: precond_true - 1 for WHILE TRUE(...), 0 for WHILE FALSE(...)
Design: PLATYPUS WHILE FALSE(x) loops until x becomes true -> while(!(x))
*********************************************************/
void cg_while_start(int precond_true) {
    emit_indent();
    if (precond_true) {
        fprintf(outfile, "while (");
    } else {
        fprintf(outfile, "while (!(");
    }
}

void cg_while_condition_end(void) {
    fprintf(outfile, ")) {\n");
    indent_level++;
}

void cg_while_body_start(void) {
    /* Handled by condition_end */
}

void cg_while_end(void) {
    indent_level--;
    emit_indent();
    fprintf(outfile, "}\n");
}

/********************************************************************************
 * RELATIONAL AND LOGICAL OPERATORS
 ********************************************************************************/

/********************************************************
Purpose: Emit relational operator.
Author: Marco Gregory
History/Versions: 1.0
Parameters: op - 0=EQ(==), 1=NE(<>), 2=GT(>), 3=LT(<)
Note: Maps PLATYPUS <> to C !=
*********************************************************/
void cg_emit_relop(int op) {
    switch (op) {
        case 0: fprintf(outfile, " == "); break;  /* EQ: == */
        case 1: fprintf(outfile, " != "); break;  /* NE: <> */
        case 2: fprintf(outfile, " > "); break;   /* GT: > */
        case 3: fprintf(outfile, " < "); break;   /* LT: < */
    }
}

/********************************************************
Purpose: Emit logical operator.
Author: Marco Gregory
History/Versions: 1.0
Parameters: op - 0=AND(.AND.), 1=OR(.OR.)
Note: Maps PLATYPUS .AND./.OR. to C &&/||
*********************************************************/
void cg_emit_logop(int op) {
    switch (op) {
        case 0: fprintf(outfile, " && "); break;  /* AND */
        case 1: fprintf(outfile, " || "); break;  /* OR */
    }
}

/********************************************************************************
 * I/O OPERATIONS - READ STATEMENT
 ********************************************************************************/

void cg_read_start(void) {
    /* Individual scanf calls emitted per variable */
}

/********************************************************
Purpose: Emit scanf call for single variable.
Author: Marco Gregory
History/Versions: 1.0
Parameters:
    name - variable to read into
    type - determines format specifier (%d, %lf, %s)
Edge Cases:
    - String reads limited to 255 chars to prevent overflow
    - String variables don't need & (array decays to pointer)
*********************************************************/
void cg_read_var(const char *name, VarType type) {
    emit_indent();
    switch (type) {
        case VAR_INT:
            fprintf(outfile, "scanf(\"%%d\", &%s);\n", name);
            break;
        case VAR_FLOAT:
            fprintf(outfile, "scanf(\"%%lf\", &%s);\n", name);
            break;
        case VAR_STRING:
            /* Width specifier prevents buffer overflow */
            fprintf(outfile, "scanf(\"%%255s\", %s);\n", name);
            break;
    }
}

void cg_read_next(void) {
    /* Multiple reads are individual scanf calls */
}

void cg_read_end(void) {
    /* Nothing needed */
}

/********************************************************************************
 * I/O OPERATIONS - WRITE STATEMENT
 * Note: These are partially implemented. Transpiler.c handles WRITE directly
 * for simpler individual printf calls per item.
 ********************************************************************************/

void cg_write_start(void) {
    in_write = 1;
    write_count = 0;
    emit_indent();
    fprintf(outfile, "printf(\"");
}

void cg_write_string(const char *str) {
    fprintf(outfile, "%s", str);
    write_count++;
}

void cg_write_var(const char *name, VarType type) {
    (void)name; /* Unused - format only */
    switch (type) {
        case VAR_INT:   fprintf(outfile, "%%d"); break;
        case VAR_FLOAT: fprintf(outfile, "%%f"); break;
        case VAR_STRING: fprintf(outfile, "%%s"); break;
    }
    write_count++;
}

void cg_write_next(void) {
    fprintf(outfile, " ");
}

void cg_write_end(void) {
    fprintf(outfile, "\\n\");\n");
    in_write = 0;
}

void cg_write_newline(void) {
    emit_indent();
    fprintf(outfile, "printf(\"\\n\");\n");
}

/********************************************************************************
 * UTILITY FUNCTIONS
 ********************************************************************************/

/********************************************************
Purpose: Determine variable type from name or symbol table.
Author: Marco Gregory
History/Versions: 1.0
Parameters: name - variable identifier
Return Value: VarType (VAR_STRING if $ suffix, else VAR_INT default)
Algorithm:
    1. Look up in symbol table
    2. If found, return stored type
    3. If not found, infer from naming convention ($ = string)
Design Choice: Default to VAR_INT for unknown arithmetic variables.
               Transpiler.c uses VAR_FLOAT for better mixed arithmetic.
*********************************************************/
VarType cg_get_var_type(const char *name) {
    Symbol *sym = find_symbol(name);
    if (sym) return sym->type;
    
    /* Infer type from name: $ suffix indicates string variable */
    size_t len = strlen(name);
    if (len > 0 && name[len-1] == '$') {
        return VAR_STRING;
    }
    return VAR_INT;
}

/********************************************************
Purpose: Emit statement terminator (semicolon + newline).
Author: Marco Gregory
History/Versions: 1.0
*********************************************************/
void cg_stmt_end(void) {
    fprintf(outfile, ";\n");
}
