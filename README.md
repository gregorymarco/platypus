# PLATYPUS Compiler

A compiler for the PLATYPUS programming language that transpiles to C. Originally developed as a compilers course project (CST8152), extended with a full C code generator.

```
   ___  __    __  ____  _  _  ___  _  _  ___ 
  / __)(  )  (  )(_  _)( \/ )/ __)( )( )/ __)
 ( (__  )(__  )(__ _)(_  \  /\__ \ )()(  \__ \
  \___)(____)(____|____) (__)(___/(__)\_)(___/
        PLATYPUS → C Transpiler
```

## Quick Start

```bash
# Build the compiler
./build.sh

# Compile a PLATYPUS program to executable
./platyc program.pls

# Run it
./program
```

## Repository Structure

```
platypus/
├── compiler/           # Source code
│   ├── buffer.c/h      # Input buffer management
│   ├── scanner.c       # Lexical analyzer (tokenizer)
│   ├── table.h         # Scanner transition tables & token definitions
│   ├── token.h         # Token type definitions
│   ├── parser.c/h      # Syntax analyzer (validates grammar)
│   ├── codegen.c/h     # C code generator module
│   ├── transpiler.c    # Main transpiler (parser + code emission)
│   ├── platy.c         # Parser-only driver (syntax checking)
│
├── spec/               # Language specification
│   ├── language-spec.md    # Full grammar and semantics
│   ├── syntax-spec.md      # Syntax details
│   └── transition-diagram.png  # Scanner state machine
│
├── test-files/         # Test programs
│   ├── source/         # .pls source files
│   └── result/         # Expected outputs
│
├── build/              # Compiled binaries (gitignored)
├── build.sh            # Build script
├── Makefile            # Alternative build system
└── platyc              # Compiler driver script
```

## Build Tools

| Command | Description |
|---------|-------------|
| `./build.sh` | Build all tools |
| `make` | Same as build.sh |
| `make clean` | Remove build artifacts |

### Generated Executables

| Binary | Purpose |
|--------|---------|
| `build/platy_trans` | Transpiler: PLATYPUS → C source |
| `build/platypus` | Syntax checker only |
| `build/platy_scanner` | Tokenizer only |

## Usage

### Compile and Run (Recommended)

```bash
# One-step compile to executable
./platyc myprogram.pls

# With custom output name
./platyc myprogram.pls myapp

# Run
./myprogram   # or ./myapp
```

### Manual Steps

```bash
# Step 1: Transpile to C
./build/platy_trans myprogram.pls

# Step 2: Compile C to executable  
gcc -o myprogram myprogram.c

# Step 3: Run
./myprogram
```

### Syntax Check Only

```bash
./build/platypus myprogram.pls
```

---

# PLATYPUS Language Reference

## Program Structure

Every PLATYPUS program is wrapped in `PLATYPUS { }`:

```platypus
PLATYPUS {
    !! Your code here
}
```

## Comments

Single-line comments start with `!!`:

```platypus
!! This is a comment
a = 10;  !! Inline comment
```

## Variables

Variables are **implicitly declared** on first use. Type is inferred from name:

| Suffix | Type | Example |
|--------|------|---------|
| (none) | Numeric (float) | `count`, `total` |
| `$` | String | `name$`, `msg$` |

```platypus
count = 42;           !! Numeric variable
pi = 3.14159;         !! Also numeric
greeting$ = "Hello";  !! String variable
```

**Naming rules:**
- First character must be a letter (a-z, A-Z)
- Followed by letters or digits
- Maximum 8 significant characters
- String variables end with `$`

## Data Types

| Type | Literals | Examples |
|------|----------|----------|
| Integer | Decimal digits | `0`, `42`, `999` |
| Float | Decimal with `.` | `3.14`, `0.5`, `100.` |
| String | Double-quoted | `"Hello"`, `"World"` |

## Operators

### Arithmetic
| Operator | Meaning |
|----------|---------|
| `+` | Addition / Unary plus |
| `-` | Subtraction / Unary minus |
| `*` | Multiplication |
| `/` | Division |

### String
| Operator | Meaning |
|----------|---------|
| `#` | Concatenation |

```platypus
full$ = first$ # " " # last$;
```

### Relational
| Operator | Meaning |
|----------|---------|
| `==` | Equal |
| `<>` | Not equal |
| `<` | Less than |
| `>` | Greater than |

### Logical
| Operator | Meaning |
|----------|---------|
| `.AND.` | Logical AND |
| `.OR.` | Logical OR |

## Statements

### Assignment

```platypus
variable = expression;
string$ = string_expression;
```

### Input (READ)

```platypus
READ(variable);
READ(a, b, c);  !! Multiple variables
```

### Output (WRITE)

```platypus
WRITE("Hello World!");     !! String literal
WRITE(variable);           !! Single variable
WRITE(a, b, c);            !! Multiple variables
WRITE();                   !! Empty line
```

### Selection (IF)

PLATYPUS has a unique IF syntax with a **pre-condition** (`TRUE` or `FALSE`):

```platypus
IF TRUE(condition) THEN {
    !! Executes when condition is TRUE
} ELSE {
    !! Executes when condition is FALSE
};
```

```platypus
IF FALSE(x == 0) THEN {
    !! Executes when x == 0 is FALSE (i.e., x != 0)
} ELSE {
    !! Executes when x == 0 is TRUE (i.e., x == 0)
};
```

**Note:** Both THEN and ELSE blocks are required (can be empty).

### Iteration (WHILE)

Similar pre-condition syntax:

```platypus
WHILE TRUE(condition) REPEAT {
    !! Loop while condition is TRUE
};

WHILE FALSE(x > 10) REPEAT {
    !! Loop while x > 10 is FALSE (i.e., x <= 10)
};
```

## Complete Example

```platypus
!! Factorial calculator
PLATYPUS {
    WRITE("Enter a number:");
    READ(n);
    
    result = 1;
    counter = 1;
    
    WHILE TRUE(counter < n .OR. counter == n) REPEAT {
        result = result * counter;
        counter = counter + 1;
    };
    
    WRITE("Factorial is:");
    WRITE(result);
}
```

## Grammar Summary

```
<program>       → PLATYPUS { <opt_statements> }
<statements>    → <statement> | <statements> <statement>
<statement>     → <assignment> | <selection> | <iteration> 
                | <input> | <output>

<assignment>    → AVID = <arith_expr> ; | SVID = <str_expr> ;
<selection>     → IF <pre> ( <cond_expr> ) THEN { <opt_stmts> } 
                  ELSE { <opt_stmts> } ;
<iteration>     → WHILE <pre> ( <cond_expr> ) REPEAT { <stmts> } ;
<input>         → READ ( <var_list> ) ;
<output>        → WRITE ( <output_list> ) ;

<pre>           → TRUE | FALSE
<arith_expr>    → <term> | <arith_expr> +/- <term>
<str_expr>      → <str_primary> | <str_expr> # <str_primary>
<cond_expr>     → <log_or_expr>
```

## Error Messages

The transpiler provides detailed error messages with line numbers:

```
Error at line 5: expected ';', found arithmetic variable 'x'
Error at line 12: expected keyword 'THEN', found '}'
```

## Limitations

- No functions or procedures
- No arrays
- No nested scopes (all variables global)
- String literals cannot contain `"` or newlines
- Maximum 8 significant characters in identifiers

---

## License

Educational project. Original course materials by Svillen Ranev (Algonquin College).
Transpiler extension by Marco Gregory.

