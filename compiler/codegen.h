/********************************************************************************
Filename: codegen.h
Purpose: Header file for the PLATYPUS C code generator module. Defines the 
         interface for transpiling PLATYPUS intermediate representation to 
         valid C99 source code. Provides symbol table structures, type 
         definitions, and function declarations for code emission.

Compiler: GCC / Clang (C99 compatible)
Author: Marco Gregory
Course: Compilers - PLATYPUS Transpiler Extension
Version: 1.0
Date: December 
5

Design Decisions:
    - Uses a simple linear symbol table (O(n) lookup) since PLATYPUS programs
      are typically small. For larger programs, a hash table would be better.
    - Variable types are inferred from naming convention ($ suffix = string)
      rather than explicit declarations, matching PLATYPUS semantics.
    - Code is emitted directly during parsing (single-pass) rather than
      building an AST first. This is simpler but limits optimization.
    - All arithmetic variables use 'double' in C to handle both PLATYPUS
      integers and floats without precision loss.

Limitations:
    - MAX_VARS (256) limits total unique variables per program
    - MAX_VAR_NAME (9) matches PLATYPUS 8-char limit + null terminator
    - No support for nested scopes (PLATYPUS has flat scope)

********************************************************************************/

#ifndef CODEGEN_H_
#define CODEGEN_H_

#include <stdio.h>

/********************************************************************************
 * CONSTANTS AND LIMITS
 ********************************************************************************/

#define MAX_VARS 256      /* Maximum unique variables in a PLATYPUS program.
                             Edge case: exceeding this causes silent failure
                             in symbol table insertion. Consider error handling. */

#define MAX_VAR_NAME 9    /* 8 significant chars + null terminator.
                             Matches PLATYPUS VID_LEN specification. */

/********************************************************************************
 * TYPE DEFINITIONS
 ********************************************************************************/

/*
 * VarType - PLATYPUS variable type enumeration
 * 
 * Design choice: VAR_FLOAT is used for all arithmetic to avoid int/float
 * ambiguity. PLATYPUS spec allows mixed arithmetic, so we use double for all.
 * VAR_INT is retained for potential future optimization (int-only expressions).
 */
typedef enum { 
    VAR_INT,      /* Integer variable (currently unused - all arith uses FLOAT) */
    VAR_FLOAT,    /* Floating-point variable (default for arithmetic) */
    VAR_STRING    /* String variable (identified by $ suffix) */
} VarType;

/*
 * Symbol - Symbol table entry for variable tracking
 *
 * Tracks declared variables to:
 *   1. Emit C declarations on first use (lazy declaration)
 *   2. Determine correct format specifiers for I/O
 *   3. Handle string vs arithmetic differently
 *
 * Edge case: If same variable used with different implied types,
 * first usage wins. PLATYPUS doesn't allow this anyway.
 */
typedef struct {
    char name[MAX_VAR_NAME];  /* Variable identifier (null-terminated) */
    VarType type;             /* Inferred type from name/context */
    int declared;             /* 1 if C declaration already emitted, 0 otherwise */
} Symbol;

/********************************************************************************
 * FUNCTION DECLARATIONS
 ********************************************************************************/

/*
 * cg_init - Initialize code generator with output file
 * Parameters: out - FILE pointer for C code output (must be open for writing)
 * Edge case: NULL file pointer causes undefined behavior
 */
void cg_init(FILE *out);

/*
 * cg_finish - Finalize code generation and close output
 * Closes output file unless it's stdout
 */
void cg_finish(void);

/*
 * cg_program_start - Emit C program header and main() opening
 * Outputs: #includes, helper functions, main() signature, opening brace
 */
void cg_program_start(void);

/*
 * cg_program_end - Emit main() closing (return 0, closing brace)
 */
void cg_program_end(void);

/*
 * cg_declare_var - Declare a variable in generated C code
 * Parameters: name - variable identifier, type - VAR_INT/VAR_FLOAT/VAR_STRING
 * Design: Only emits declaration on first call for each variable (idempotent)
 */
void cg_declare_var(const char *name, VarType type);

/* Assignment emission (currently simplified - see transpiler.c for actual use) */
void cg_assign_start(const char *varname);
void cg_assign_end(void);

/* Expression value emission */
void cg_emit_int(int value);
void cg_emit_float(float value);
void cg_emit_string(const char *str);
void cg_emit_var(const char *name);
void cg_emit_op(char op);
void cg_emit_unary(char op);
void cg_emit_lparen(void);
void cg_emit_rparen(void);
void cg_emit_concat(void);

/* Control flow - IF statement */
void cg_if_start(int precond_true);
void cg_if_condition_end(void);
void cg_if_then(void);
void cg_if_else(void);
void cg_if_end(void);

/* Control flow - WHILE statement */
void cg_while_start(int precond_true);
void cg_while_condition_end(void);
void cg_while_body_start(void);
void cg_while_end(void);

/*
 * cg_emit_relop - Emit relational operator
 * Parameters: op - 0=EQ(==), 1=NE(<>), 2=GT(>), 3=LT(<)
 * Maps PLATYPUS operators to C equivalents
 */
void cg_emit_relop(int op);

/*
 * cg_emit_logop - Emit logical operator  
 * Parameters: op - 0=AND(.AND.), 1=OR(.OR.)
 * Maps PLATYPUS .AND./.OR. to C &&/||
 */
void cg_emit_logop(int op);

/* I/O operations - READ statement */
void cg_read_start(void);
void cg_read_var(const char *name, VarType type);
void cg_read_next(void);
void cg_read_end(void);

/* I/O operations - WRITE statement */
void cg_write_start(void);
void cg_write_string(const char *str);
void cg_write_var(const char *name, VarType type);
void cg_write_next(void);
void cg_write_end(void);
void cg_write_newline(void);

/*
 * cg_get_var_type - Look up variable type from symbol table
 * Parameters: name - variable identifier to look up
 * Returns: VarType of variable, or inferred type if not in table
 * Design: Infers VAR_STRING if name ends with '$', else VAR_FLOAT
 */
VarType cg_get_var_type(const char *name);

/* Statement terminator (emits semicolon + newline) */
void cg_stmt_end(void);

#endif /* CODEGEN_H_ */

