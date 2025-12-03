/********************************************************************************
Filename: transpiler.c
Purpose: PLATYPUS to C transpiler - main driver module. Performs syntax-directed
         translation from PLATYPUS source code to equivalent C99 source code.
         Combines parsing with direct code emission (single-pass translation).

Compiler: GCC / Clang (C99 compatible)
Author: Marco Gregory
Course: Compilers - PLATYPUS Transpiler Extension
Version: 1.0
Date: December 2025

Architecture:
    This module implements a recursive-descent parser that directly emits C code
    during parsing, rather than building an intermediate AST. This single-pass
    approach is simpler but prevents optimization passes.
    
    Token Stream (Scanner) --> Parser/Transpiler --> C Source File
    
    The scanner (scanner.c) provides tokens via malar_next_token().
    This module consumes tokens and emits C code to the output file.

Design Decisions:
    1. Single-pass translation: C code emitted during parsing for simplicity.
       Trade-off: No optimization, but faster development and smaller memory.
    
    2. Direct emission to FILE: Uses fprintf() directly rather than building
       strings. Simpler and more memory-efficient for large programs.
    
    3. Error recovery: Minimal - reports first error and continues.
       Could be enhanced with panic-mode recovery for better diagnostics.
    
    4. Variable typing: Inferred from name ($ suffix = string, else float).
       All arithmetic uses double to handle mixed int/float expressions.
    
    5. String concatenation: Collects all parts first, then emits nested
       str_concat() calls. Supports up to MAX_STR_PARTS (16) operands.

Edge Cases Handled:
    - Empty program: PLATYPUS{} produces valid empty main()
    - Missing semicolons: Descriptive error with line number
    - Unexpected tokens: Shows what was expected vs. what was found
    - Nested control structures: Proper indentation in output
    - Multiple string concatenations: Proper nesting of str_concat()
    - Empty WRITE(): Produces printf("\n")
    - Empty READ variable list: Syntax error (caught by parser)

Known Limitations:
    - No error recovery after first syntax error
    - String literals cannot contain escape sequences or embedded quotes
    - Maximum 16 operands in single string concatenation expression
    - No support for nested function calls (PLATYPUS doesn't have functions)
    - Line numbers may be slightly off for multi-line constructs

Function List:
    token_name, keyword_name, trans_error, trans_error_expected, match,
    get_var_type, ensure_var_declared, trans_program, trans_opt_statements,
    trans_statements, trans_statement, trans_assignment_statement,
    trans_primary_arith, trans_unary_arith, trans_mult_arith, trans_additive_arith,
    trans_arithmetic_expression, collect_primary_string, emit_str_part,
    trans_string_expression, trans_primary_rel, trans_relational_expression,
    trans_logical_and, trans_logical_or, trans_conditional_expression,
    trans_selection_statement, trans_iteration_statement, trans_input_statement,
    trans_variable_list_read, trans_output_statement, trans_variable_list_write,
    err_printf, main

********************************************************************************/

#define _CRT_SECURE_NO_WARNINGS  /* Suppress MSVC warnings for standard functions */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "buffer.h"
#include "token.h"
#include "codegen.h"

/********************************************************************************
 * CONSTANTS
 ********************************************************************************/

#define INIT_CAPACITY 200      /* Initial source buffer capacity in bytes */
#define INC_FACTOR 15          /* Buffer increment factor (multiplicative mode) */
#define STR_INIT_CAPACITY 100  /* Initial string literal table capacity */

/********************************************************************************
 * GLOBAL VARIABLES
 ********************************************************************************/

static Buffer *sc_buf;         /* Source code input buffer */
Buffer *str_LTBL;              /* String Literal Table - repository for string constants.
                                  Shared with scanner for string literal storage. */
int scerrnum;                  /* Scanner runtime error number (0 = no error) */
extern int line;               /* Current source line number (defined in scanner.c).
                                  Used for error messages to indicate error location. */

/********************************************************************************
 * EXTERNAL DECLARATIONS
 ********************************************************************************/

extern int scanner_init(Buffer *psc_buf);  /* Initialize scanner with source buffer */
extern Token malar_next_token(void);       /* Get next token from scanner */
extern char *kw_table[];                   /* Keyword string table (from table.h) */

/********************************************************************************
 * PARSER STATE VARIABLES
 ********************************************************************************/

static Token lookahead;   /* Current lookahead token for predictive parsing.
                             All parsing decisions based on this token's code. */
static int synerrno = 0;  /* Syntax error count. Non-zero indicates parse failure. */
static FILE *cg_out = NULL; /* Output file for generated C code */

/********************************************************************************
 * FORWARD DECLARATIONS
 * Recursive descent parser functions - one per grammar production
 ********************************************************************************/

static void trans_program(void);
static void trans_statements(void);
static void trans_statement(void);
static void trans_opt_statements(void);
static void trans_assignment_statement(void);
static void trans_selection_statement(void);
static void trans_iteration_statement(void);
static void trans_input_statement(void);
static void trans_output_statement(void);
static void trans_arithmetic_expression(void);
static void trans_string_expression(void);
static void trans_conditional_expression(void);
static void trans_variable_list_read(void);
static void trans_variable_list_write(void);

/********************************************************************************
 * ERROR HANDLING UTILITIES
 ********************************************************************************/

/********************************************************
Purpose: Convert token code to human-readable name for error messages.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: None
Parameters: code - token code from Token.code field
Return Value: Constant string describing the token type
Design Choice: Returns descriptive strings rather than internal names
               for better user experience in error messages.
*********************************************************/
static const char* token_name(int code) {
    switch (code) {
        case ERR_T:    return "error";
        case SEOF_T:   return "end-of-file";
        case AVID_T:   return "arithmetic variable";
        case SVID_T:   return "string variable";
        case FPL_T:    return "floating-point literal";
        case INL_T:    return "integer literal";
        case STR_T:    return "string literal";
        case SCC_OP_T: return "'#'";
        case ASS_OP_T: return "'='";
        case ART_OP_T: return "arithmetic operator";
        case REL_OP_T: return "relational operator";
        case LOG_OP_T: return "logical operator";
        case LPR_T:    return "'('";
        case RPR_T:    return "')'";
        case LBR_T:    return "'{'";
        case RBR_T:    return "'}'";
        case KW_T:     return "keyword";
        case COM_T:    return "','";
        case EOS_T:    return "';'";
        default:       return "unknown token";
    }
}

/********************************************************
Purpose: Convert keyword index to keyword string for error messages.
Author: Marco Gregory
History/Versions: 1.0
Parameters: idx - index into kw_table (matches KW_* constants)
Return Value: Keyword string (e.g., "PLATYPUS", "IF", etc.)
Note: Must match order in kw_table from table.h
*********************************************************/
static const char* keyword_name(int idx) {
    switch (idx) {
        case 0: return "ELSE";
        case 1: return "FALSE";
        case 2: return "IF";
        case 3: return "PLATYPUS";
        case 4: return "READ";
        case 5: return "REPEAT";
        case 6: return "THEN";
        case 7: return "TRUE";
        case 8: return "WHILE";
        case 9: return "WRITE";
        default: return "unknown";
    }
}

/********************************************************
Purpose: Report generic syntax error with line number.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: fprintf
Parameters: msg - descriptive error message
Return Value: None (increments synerrno)
Design Choice: Uses ANSI color codes for visibility in terminal.
               \033[1;31m = bold red, \033[0m = reset
*********************************************************/
static void trans_error(const char *msg) {
    fprintf(stderr, "\033[1;31mError\033[0m at line %d: %s\n", line, msg);
    synerrno++;
}

/********************************************************
Purpose: Report "expected X, found Y" style error with full context.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: fprintf, token_name, keyword_name
Parameters: expected - string describing what was expected
Return Value: None (increments synerrno)
Algorithm:
    1. Print error with line number
    2. Print expected vs actual token type
    3. If keyword or variable, also print actual value
Design Choice: Shows variable names/keyword values for easier debugging
*********************************************************/
static void trans_error_expected(const char *expected) {
    fprintf(stderr, "\033[1;31mError\033[0m at line %d: expected %s, found %s", 
            line, expected, token_name(lookahead.code));
    if (lookahead.code == KW_T) {
        fprintf(stderr, " '%s'", keyword_name(lookahead.attribute.get_int));
    } else if (lookahead.code == AVID_T || lookahead.code == SVID_T) {
        fprintf(stderr, " '%s'", lookahead.attribute.vid_lex);
    }
    fprintf(stderr, "\n");
    synerrno++;
}

/********************************************************************************
 * TOKEN MATCHING
 ********************************************************************************/

/********************************************************
Purpose: Match expected token and advance to next token.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: malar_next_token, trans_error_expected, snprintf
Parameters:
    code - expected token code
    attr - expected attribute (only checked for KW_T, LOG_OP_T, ART_OP_T, REL_OP_T)
Return Value: None (advances lookahead on success, reports error on failure)
Algorithm:
    1. Check if lookahead.code matches expected code
    2. For tokens with attributes, also check attribute value
    3. On match: advance to next token
    4. On mismatch: report error with context
Edge Cases:
    - Attribute mismatch: Reports specific expected attribute
    - End of file: Properly reports SEOF_T in error
*********************************************************/
static void match(int code, int attr) {
    if (lookahead.code == code) {
        if (code == KW_T || code == LOG_OP_T || code == ART_OP_T || code == REL_OP_T) {
            if (lookahead.attribute.get_int != attr) {
                char buf[64];
                if (code == KW_T) {
                    snprintf(buf, sizeof(buf), "keyword '%s'", keyword_name(attr));
                } else {
                    snprintf(buf, sizeof(buf), "operator with attribute %d", attr);
                }
                trans_error_expected(buf);
                return;
            }
        }
        lookahead = malar_next_token();
    } else {
        char buf[64];
        if (code == KW_T) {
            snprintf(buf, sizeof(buf), "keyword '%s'", keyword_name(attr));
        } else {
            snprintf(buf, sizeof(buf), "%s", token_name(code));
        }
        trans_error_expected(buf);
    }
}

/********************************************************************************
 * KEYWORD INDEX CONSTANTS
 * Must match order in kw_table[] from table.h exactly!
 * These are used to check lookahead.attribute.get_int for keywords.
 ********************************************************************************/
#define KW_ELSE 0
#define KW_FALSE 1
#define KW_IF 2
#define KW_PLATYPUS 3
#define KW_READ 4
#define KW_REPEAT 5
#define KW_THEN 6
#define KW_TRUE 7
#define KW_WHILE 8
#define KW_WRITE 9

/********************************************************************************
 * TYPE INFERENCE UTILITIES
 ********************************************************************************/

/********************************************************
Purpose: Infer PLATYPUS variable type from identifier name.
Author: Marco Gregory
History/Versions: 1.0
Parameters: name - variable identifier (null-terminated)
Return Value: VarType - VAR_STRING if ends with $, else VAR_FLOAT
Design Choice: Use VAR_FLOAT (C double) for all arithmetic to handle
               PLATYPUS's mixed int/float arithmetic without precision loss.
               VAR_INT could be used if integer-only optimization desired.
Edge Case: Empty name returns VAR_FLOAT (shouldn't happen with valid tokens)
*********************************************************/
static VarType get_var_type(const char *name) {
    size_t len = strlen(name);
    if (len > 0 && name[len - 1] == '$') {
        return VAR_STRING;
    }
    return VAR_FLOAT;
}

/********************************************************
Purpose: Ensure variable has been declared in generated C code.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: cg_declare_var, get_var_type
Parameters: name - variable identifier to declare
Return Value: None
Design: Lazy declaration - emits C declaration on first use.
        cg_declare_var is idempotent, so safe to call multiple times.
*********************************************************/
static void ensure_var_declared(const char *name) {
    cg_declare_var(name, get_var_type(name));
}

/********************************************************************************
 * GRAMMAR PRODUCTIONS - PROGRAM STRUCTURE
 ********************************************************************************/

/********************************************************
Purpose: Parse and transpile PLATYPUS program.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: cg_program_start, match, trans_opt_statements, cg_program_end
Parameters: None (uses lookahead)
Return Value: None
Grammar:
    <program> -> PLATYPUS { <opt_statements> }
First Set: { PLATYPUS }
Algorithm:
    1. Emit C program header (includes, main signature)
    2. Match PLATYPUS keyword
    3. Match opening brace
    4. Parse optional statements
    5. Match closing brace
    6. Emit C program footer (return 0, closing brace)
*********************************************************/
static void trans_program(void) {
    cg_program_start();
    match(KW_T, KW_PLATYPUS);
    match(LBR_T, 0);
    trans_opt_statements();
    match(RBR_T, 0);
    cg_program_end();
}

/********************************************************
Purpose: Parse optional statements (may be empty).
Author: Marco Gregory
History/Versions: 1.0
Grammar:
    <opt_statements> -> <statements> | ε
First Set: { AVID_T, SVID_T, IF, WHILE, READ, WRITE, ε }
Design: Checks FIRST set to decide whether statements follow.
        If not in FIRST(statements), produces empty (epsilon).
*********************************************************/
static void trans_opt_statements(void) {
    switch (lookahead.code) {
        case AVID_T:
        case SVID_T:
            trans_statements();
            break;
        case KW_T:
            if (lookahead.attribute.get_int == KW_IF ||
                lookahead.attribute.get_int == KW_WHILE ||
                lookahead.attribute.get_int == KW_READ ||
                lookahead.attribute.get_int == KW_WRITE) {
                trans_statements();
            }
            /* Else: empty (ε production) - valid for empty blocks */
            break;
        /* Default: empty block - no statements */
    }
}

/********************************************************
Purpose: Parse one or more statements.
Author: Marco Gregory
History/Versions: 1.0
Grammar:
    <statements> -> <statement> <statements'>
    <statements'> -> <statement> <statements'> | ε
First Set: { AVID_T, SVID_T, IF, WHILE, READ, WRITE }
Algorithm: Parse first statement, then loop while more statements follow.
*********************************************************/
static void trans_statements(void) {
    trans_statement();
    /* Continue while lookahead is in FIRST(statement) */
    while (lookahead.code == AVID_T || lookahead.code == SVID_T ||
           (lookahead.code == KW_T && 
            (lookahead.attribute.get_int == KW_IF ||
             lookahead.attribute.get_int == KW_WHILE ||
             lookahead.attribute.get_int == KW_READ ||
             lookahead.attribute.get_int == KW_WRITE))) {
        trans_statement();
    }
}

/********************************************************
Purpose: Parse single statement (assignment or control).
Author: Marco Gregory
History/Versions: 1.0
Grammar:
    <statement> -> <assignment_statement>
                 | <selection_statement>
                 | <iteration_statement>
                 | <input_statement>
                 | <output_statement>
First Set: { AVID_T, SVID_T, IF, WHILE, READ, WRITE }
Design: Switch on lookahead to select appropriate production.
*********************************************************/
static void trans_statement(void) {
    switch (lookahead.code) {
        case AVID_T:
        case SVID_T:
            trans_assignment_statement();
            break;
        case KW_T:
            switch (lookahead.attribute.get_int) {
                case KW_IF:
                    trans_selection_statement();
                    break;
                case KW_WHILE:
                    trans_iteration_statement();
                    break;
                case KW_READ:
                    trans_input_statement();
                    break;
                case KW_WRITE:
                    trans_output_statement();
                    break;
                default:
                    trans_error_expected("statement (IF, WHILE, READ, WRITE, or assignment)");
                    break;
            }
            break;
        default:
            trans_error_expected("statement (IF, WHILE, READ, WRITE, or assignment)");
    }
}

/********************************************************************************
 * ASSIGNMENT STATEMENT
 ********************************************************************************/

/********************************************************
Purpose: Parse and transpile assignment statement.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: ensure_var_declared, match, trans_arithmetic_expression,
                  trans_string_expression, fprintf
Grammar:
    <assignment_statement> -> <assignment_expression> ;
    <assignment_expression> -> AVID = <arithmetic_expression>
                             | SVID = <string_expression>
First Set: { AVID_T, SVID_T }
Algorithm:
    1. Extract variable name from current token
    2. Ensure variable is declared in generated C
    3. Determine if string or arithmetic assignment
    4. Emit appropriate C assignment syntax
    5. Match semicolon terminator
Design Choices:
    - String assignment uses strcpy() for proper C semantics
    - Arithmetic assignment uses direct = operator
    - Variables declared lazily on first use
Edge Cases:
    - Variable name truncated to 8 chars (VID_LEN)
    - String expression may involve concatenation
*********************************************************/
static void trans_assignment_statement(void) {
    char varname[9];
    int is_string = (lookahead.code == SVID_T);
    
    /* Extract variable name (max 8 significant chars) */
    strncpy(varname, lookahead.attribute.vid_lex, 8);
    varname[8] = '\0';
    
    /* Emit C declaration if first use of this variable */
    ensure_var_declared(varname);
    
    if (is_string) {
        /* String assignment: strcpy(var, expr); */
        match(SVID_T, 0);
        match(ASS_OP_T, 0);
        fprintf(cg_out, "    strcpy(%s, ", varname);
        trans_string_expression();
        fprintf(cg_out, ");\n");
    } else {
        /* Arithmetic assignment: var = expr; */
        match(AVID_T, 0);
        match(ASS_OP_T, 0);
        fprintf(cg_out, "    %s = ", varname);
        trans_arithmetic_expression();
        fprintf(cg_out, ";\n");
    }
    match(EOS_T, 0);
}

/********************************************************************************
 * ARITHMETIC EXPRESSIONS
 * Implements operator precedence via recursive descent:
 *   Highest: unary +/-, parentheses
 *   Middle:  *, /
 *   Lowest:  +, - (binary)
 ********************************************************************************/

/********************************************************
Purpose: Parse and emit primary arithmetic expression.
Author: Marco Gregory
History/Versions: 1.0
Grammar:
    <primary_arithmetic_expression> -> AVID_T | FPL_T | INL_T
                                     | ( <arithmetic_expression> )
First Set: { AVID_T, FPL_T, INL_T, ( }
Design: Emits C equivalent directly to output.
*********************************************************/
static void trans_primary_arith(void) {
    switch (lookahead.code) {
        case AVID_T:
            ensure_var_declared(lookahead.attribute.vid_lex);
            fprintf(cg_out, "%s", lookahead.attribute.vid_lex);
            match(AVID_T, 0);
            break;
        case FPL_T:
            fprintf(cg_out, "%f", lookahead.attribute.flt_value);
            match(FPL_T, 0);
            break;
        case INL_T:
            fprintf(cg_out, "%d", lookahead.attribute.get_int);
            match(INL_T, 0);
            break;
        case LPR_T:
            fprintf(cg_out, "(");
            match(LPR_T, 0);
            trans_arithmetic_expression();
            fprintf(cg_out, ")");
            match(RPR_T, 0);
            break;
    }
}

static void trans_unary_arith(void) {
    if (lookahead.code == ART_OP_T) {
        if (lookahead.attribute.arr_op == 0) { /* PLUS */
            fprintf(cg_out, "+");
            match(ART_OP_T, 0);
        } else if (lookahead.attribute.arr_op == 1) { /* MINUS */
            fprintf(cg_out, "-");
            match(ART_OP_T, 1);
        }
    }
    trans_primary_arith();
}

static void trans_mult_arith(void) {
    trans_primary_arith();
    while (lookahead.code == ART_OP_T && 
           (lookahead.attribute.arr_op == 2 || lookahead.attribute.arr_op == 3)) {
        if (lookahead.attribute.arr_op == 2) { /* MULT */
            fprintf(cg_out, " * ");
            match(ART_OP_T, 2);
        } else { /* DIV */
            fprintf(cg_out, " / ");
            match(ART_OP_T, 3);
        }
        trans_primary_arith();
    }
}

static void trans_additive_arith(void) {
    trans_mult_arith();
    while (lookahead.code == ART_OP_T && 
           (lookahead.attribute.arr_op == 0 || lookahead.attribute.arr_op == 1)) {
        if (lookahead.attribute.arr_op == 0) { /* PLUS */
            fprintf(cg_out, " + ");
            match(ART_OP_T, 0);
        } else { /* MINUS */
            fprintf(cg_out, " - ");
            match(ART_OP_T, 1);
        }
        trans_mult_arith();
    }
}

static void trans_arithmetic_expression(void) {
    /* Check for unary +/- */
    if (lookahead.code == ART_OP_T && 
        (lookahead.attribute.arr_op == 0 || lookahead.attribute.arr_op == 1)) {
        trans_unary_arith();
    } else {
        trans_additive_arith();
    }
}

/********************************************************************************
 * STRING EXPRESSIONS WITH CONCATENATION
 * 
 * Design: String concatenation (# operator) requires collecting all operands
 * first, then emitting nested str_concat() calls. This is because we emit
 * left-to-right but str_concat nesting is right-associative for evaluation:
 * 
 *   a$ # b$ # c$  -->  str_concat(str_concat(a$, b$), c$)
 * 
 * Algorithm:
 *   1. Collect all string parts into str_parts array
 *   2. Count total parts
 *   3. If 1 part: emit directly
 *   4. If 2+ parts: emit nested str_concat calls
 ********************************************************************************/

#define MAX_STR_PARTS 16  /* Maximum operands in single concatenation.
                             Edge case: exceeding this causes error message. */

/*
 * StrPart - Represents one operand in a string concatenation expression.
 * Used to collect all parts before emitting nested str_concat calls.
 */
typedef struct {
    int is_literal;   /* 1 for string literal (needs quotes), 0 for variable */
    char value[256];  /* Variable name OR literal content (without quotes) */
} StrPart;

static StrPart str_parts[MAX_STR_PARTS];  /* Collected string operands */
static int str_part_count = 0;            /* Number of operands collected */

/********************************************************
Purpose: Collect one primary string operand into str_parts array.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: ensure_var_declared, strncpy, match, b_location
Parameters: None (uses lookahead, modifies str_parts)
Return Value: None
Grammar:
    <primary_string_expression> -> SVID_T | STR_T
Edge Cases:
    - str_part_count >= MAX_STR_PARTS: emits error, doesn't add
    - Value truncated at 255 chars to prevent overflow
*********************************************************/
static void collect_primary_string(void) {
    if (str_part_count >= MAX_STR_PARTS) {
        trans_error("Too many string concatenation operands");
        return;
    }
    
    if (lookahead.code == SVID_T) {
        ensure_var_declared(lookahead.attribute.vid_lex);
        str_parts[str_part_count].is_literal = 0;
        strncpy(str_parts[str_part_count].value, lookahead.attribute.vid_lex, 255);
        str_parts[str_part_count].value[255] = '\0';
        str_part_count++;
        match(SVID_T, 0);
    } else if (lookahead.code == STR_T) {
        str_parts[str_part_count].is_literal = 1;
        strncpy(str_parts[str_part_count].value, 
                b_location(str_LTBL, lookahead.attribute.str_offset), 255);
        str_parts[str_part_count].value[255] = '\0';
        str_part_count++;
        match(STR_T, 0);
    } else {
        trans_error_expected("string variable or string literal");
    }
}

/********************************************************
Purpose: Emit one collected string part to output.
Author: Marco Gregory
History/Versions: 1.0
Parameters: idx - index into str_parts array (0 to str_part_count-1)
Return Value: None
Design: Literals wrapped in quotes, variables emitted as-is.
*********************************************************/
static void emit_str_part(int idx) {
    if (str_parts[idx].is_literal) {
        fprintf(cg_out, "\"%s\"", str_parts[idx].value);
    } else {
        fprintf(cg_out, "%s", str_parts[idx].value);
    }
}

/********************************************************
Purpose: Parse and emit string expression with optional concatenation.
Author: Marco Gregory
History/Versions: 1.0
Called Functions: collect_primary_string, match, emit_str_part
Grammar:
    <string_expression> -> <primary_string_expression>
                         | <string_expression> # <primary_string_expression>
First Set: { SVID_T, STR_T }
Algorithm:
    1. Reset str_parts collector
    2. Collect first operand
    3. While # operator follows, collect additional operands
    4. Emit based on operand count:
       - 1 part: emit directly (no function call)
       - 2 parts: str_concat(a, b)
       - 3+ parts: nested str_concat(str_concat(a, b), c)
Example:
    a$ # b$ # c$  -->  str_concat(str_concat(a$, b$), c$)
*********************************************************/
static void trans_string_expression(void) {
    /* Reset parts collector */
    str_part_count = 0;
    
    /* Collect first part */
    collect_primary_string();
    
    /* Collect remaining parts (after # operators) */
    while (lookahead.code == SCC_OP_T) {
        match(SCC_OP_T, 0);
        collect_primary_string();
    }
    
    /* Now emit the expression */
    if (str_part_count == 1) {
        /* No concatenation needed */
        emit_str_part(0);
    } else if (str_part_count == 2) {
        /* Simple: str_concat(a, b) */
        fprintf(cg_out, "str_concat(");
        emit_str_part(0);
        fprintf(cg_out, ", ");
        emit_str_part(1);
        fprintf(cg_out, ")");
    } else {
        /* Multiple parts: nest str_concat calls left-to-right
         * a # b # c -> str_concat(str_concat(a, b), c) 
         */
        /* Open nested str_concat calls */
        for (int i = 0; i < str_part_count - 1; i++) {
            fprintf(cg_out, "str_concat(");
        }
        /* Emit first part */
        emit_str_part(0);
        /* Emit remaining parts with closing parens */
        for (int i = 1; i < str_part_count; i++) {
            fprintf(cg_out, ", ");
            emit_str_part(i);
            fprintf(cg_out, ")");
        }
    }
}

/* Relational expression */
static void trans_primary_rel(void) {
    switch (lookahead.code) {
        case AVID_T:
            fprintf(cg_out, "%s", lookahead.attribute.vid_lex);
            match(AVID_T, 0);
            break;
        case FPL_T:
            fprintf(cg_out, "%f", lookahead.attribute.flt_value);
            match(FPL_T, 0);
            break;
        case INL_T:
            fprintf(cg_out, "%d", lookahead.attribute.get_int);
            match(INL_T, 0);
            break;
        case SVID_T:
            fprintf(cg_out, "%s", lookahead.attribute.vid_lex);
            match(SVID_T, 0);
            break;
        case STR_T:
            fprintf(cg_out, "\"%s\"", b_location(str_LTBL, lookahead.attribute.str_offset));
            match(STR_T, 0);
            break;
    }
}

static void trans_relational_expression(void) {
    trans_primary_rel();
    if (lookahead.code == REL_OP_T) {
        switch (lookahead.attribute.rel_op) {
            case 0: fprintf(cg_out, " == "); match(REL_OP_T, 0); break; /* EQ */
            case 1: fprintf(cg_out, " != "); match(REL_OP_T, 1); break; /* NE */
            case 2: fprintf(cg_out, " > ");  match(REL_OP_T, 2); break; /* GT */
            case 3: fprintf(cg_out, " < ");  match(REL_OP_T, 3); break; /* LT */
        }
        trans_primary_rel();
    }
}

static void trans_logical_and(void) {
    trans_relational_expression();
    while (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == 0) { /* AND */
        fprintf(cg_out, " && ");
        match(LOG_OP_T, 0);
        trans_relational_expression();
    }
}

static void trans_logical_or(void) {
    trans_logical_and();
    while (lookahead.code == LOG_OP_T && lookahead.attribute.log_op == 1) { /* OR */
        fprintf(cg_out, " || ");
        match(LOG_OP_T, 1);
        trans_logical_and();
    }
}

static void trans_conditional_expression(void) {
    trans_logical_or();
}

/* IF statement */
static void trans_selection_statement(void) {
    int precond_true = 1;
    
    match(KW_T, KW_IF);
    
    /* Pre-condition: TRUE or FALSE */
    if (lookahead.code == KW_T) {
        if (lookahead.attribute.get_int == KW_TRUE) {
            precond_true = 1;
            match(KW_T, KW_TRUE);
        } else if (lookahead.attribute.get_int == KW_FALSE) {
            precond_true = 0;
            match(KW_T, KW_FALSE);
        }
    }
    
    if (precond_true) {
        fprintf(cg_out, "    if (");
    } else {
        fprintf(cg_out, "    if (!(");
    }
    match(LPR_T, 0);
    trans_conditional_expression();
    match(RPR_T, 0);
    if (precond_true) {
        fprintf(cg_out, ") {\n");
    } else {
        fprintf(cg_out, ")) {\n");
    }
    
    match(KW_T, KW_THEN);
    match(LBR_T, 0);
    trans_opt_statements();
    match(RBR_T, 0);
    
    fprintf(cg_out, "    } else {\n");
    match(KW_T, KW_ELSE);
    match(LBR_T, 0);
    trans_opt_statements();
    match(RBR_T, 0);
    fprintf(cg_out, "    }\n");
    
    match(EOS_T, 0);
}

/* WHILE statement */
static void trans_iteration_statement(void) {
    int precond_true = 1;
    
    match(KW_T, KW_WHILE);
    
    if (lookahead.code == KW_T) {
        if (lookahead.attribute.get_int == KW_TRUE) {
            precond_true = 1;
            match(KW_T, KW_TRUE);
        } else if (lookahead.attribute.get_int == KW_FALSE) {
            precond_true = 0;
            match(KW_T, KW_FALSE);
        }
    }
    
    if (precond_true) {
        fprintf(cg_out, "    while (");
    } else {
        fprintf(cg_out, "    while (!(");
    }
    match(LPR_T, 0);
    trans_conditional_expression();
    match(RPR_T, 0);
    if (precond_true) {
        fprintf(cg_out, ") {\n");
    } else {
        fprintf(cg_out, ")) {\n");
    }
    
    match(KW_T, KW_REPEAT);
    match(LBR_T, 0);
    trans_statements();
    match(RBR_T, 0);
    fprintf(cg_out, "    }\n");
    
    match(EOS_T, 0);
}

/* READ statement */
static void trans_input_statement(void) {
    match(KW_T, KW_READ);
    match(LPR_T, 0);
    trans_variable_list_read();
    match(RPR_T, 0);
    match(EOS_T, 0);
}

static void trans_variable_list_read(void) {
    char varname[9];
    VarType vt;
    
    strncpy(varname, lookahead.attribute.vid_lex, 8);
    varname[8] = '\0';
    vt = get_var_type(varname);
    ensure_var_declared(varname);
    
    if (lookahead.code == AVID_T) {
        fprintf(cg_out, "    scanf(\"%%lf\", &%s);\n", varname);
        match(AVID_T, 0);
    } else if (lookahead.code == SVID_T) {
        fprintf(cg_out, "    scanf(\"%%255s\", %s);\n", varname);
        match(SVID_T, 0);
    }
    
    while (lookahead.code == COM_T) {
        match(COM_T, 0);
        strncpy(varname, lookahead.attribute.vid_lex, 8);
        varname[8] = '\0';
        vt = get_var_type(varname);
        ensure_var_declared(varname);
        
        if (lookahead.code == AVID_T) {
            fprintf(cg_out, "    scanf(\"%%lf\", &%s);\n", varname);
            match(AVID_T, 0);
        } else if (lookahead.code == SVID_T) {
            fprintf(cg_out, "    scanf(\"%%255s\", %s);\n", varname);
            match(SVID_T, 0);
        }
    }
}

/* WRITE statement */
static void trans_output_statement(void) {
    match(KW_T, KW_WRITE);
    match(LPR_T, 0);
    
    if (lookahead.code == RPR_T) {
        /* Empty write - newline */
        fprintf(cg_out, "    printf(\"\\n\");\n");
    } else if (lookahead.code == STR_T) {
        /* String literal */
        fprintf(cg_out, "    printf(\"%s\\n\");\n", 
                b_location(str_LTBL, lookahead.attribute.str_offset));
        match(STR_T, 0);
    } else {
        /* Variable list */
        trans_variable_list_write();
    }
    
    match(RPR_T, 0);
    match(EOS_T, 0);
}

static void trans_variable_list_write(void) {
    char varname[9];
    
    strncpy(varname, lookahead.attribute.vid_lex, 8);
    varname[8] = '\0';
    
    if (lookahead.code == AVID_T) {
        fprintf(cg_out, "    printf(\"%%g\", %s);\n", varname);
        match(AVID_T, 0);
    } else if (lookahead.code == SVID_T) {
        fprintf(cg_out, "    printf(\"%%s\", %s);\n", varname);
        match(SVID_T, 0);
    }
    
    while (lookahead.code == COM_T) {
        match(COM_T, 0);
        strncpy(varname, lookahead.attribute.vid_lex, 8);
        varname[8] = '\0';
        
        if (lookahead.code == AVID_T) {
            fprintf(cg_out, "    printf(\" %%g\", %s);\n", varname);
            match(AVID_T, 0);
        } else if (lookahead.code == SVID_T) {
            fprintf(cg_out, "    printf(\" %%s\", %s);\n", varname);
            match(SVID_T, 0);
        }
    }
    fprintf(cg_out, "    printf(\"\\n\");\n");
}

/* Error printing */
static void err_printf(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    if (strchr(fmt, '\n') == NULL)
        fprintf(stderr, "\n");
}

int main(int argc, char **argv) {
    FILE *fi;
    int loadsize;
    char outname[256];
    
    if (argc < 2) {
        fprintf(stderr, "Usage: %s source.pls [output.c]\n", argv[0]);
        return 1;
    }
    
    /* Create source buffer */
    sc_buf = b_allocate(INIT_CAPACITY, INC_FACTOR, 'm');
    if (!sc_buf) {
        fprintf(stderr, "Could not create source buffer\n");
        return 1;
    }
    
    /* Open and load source file */
    if ((fi = fopen(argv[1], "r")) == NULL) {
        fprintf(stderr, "Cannot open file: %s\n", argv[1]);
        return 1;
    }
    
    printf("Reading %s...\n", argv[1]);
    loadsize = b_load(fi, sc_buf);
    fclose(fi);
    
    if (loadsize == RT_FAIL_1) {
        fprintf(stderr, "Error loading source file\n");
        return 1;
    }
    
    b_compact(sc_buf, '\0');
    
    /* Create string literal table */
    str_LTBL = b_allocate(STR_INIT_CAPACITY, INC_FACTOR, 'a');
    if (!str_LTBL) {
        fprintf(stderr, "Could not create string literal table\n");
        return 1;
    }
    
    /* Determine output filename */
    if (argc >= 3) {
        strncpy(outname, argv[2], 255);
    } else {
        strncpy(outname, argv[1], 250);
        char *dot = strrchr(outname, '.');
        if (dot) *dot = '\0';
        strcat(outname, ".c");
    }
    
    /* Open output file */
    cg_out = fopen(outname, "w");
    if (!cg_out) {
        fprintf(stderr, "Cannot create output file: %s\n", outname);
        return 1;
    }
    
    /* Initialize scanner */
    if (scanner_init(sc_buf)) {
        fprintf(stderr, "Scanner initialization failed\n");
        return 1;
    }
    
    /* Initialize codegen */
    cg_init(cg_out);
    
    /* Parse and transpile */
    printf("Transpiling to %s...\n", outname);
    lookahead = malar_next_token();
    trans_program();
    
    if (lookahead.code != SEOF_T) {
        fprintf(stderr, "Warning: Extra tokens after program end\n");
    }
    
    fclose(cg_out);
    
    /* Cleanup */
    b_free(sc_buf);
    b_free(str_LTBL);
    
    if (synerrno == 0) {
        printf("Transpilation successful!\n");
        printf("Compile with: gcc -o program %s\n", outname);
        return 0;
    } else {
        printf("Transpilation completed with %d error(s)\n", synerrno);
        return 1;
    }
}

