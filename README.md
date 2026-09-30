# Mash

A small, statically typed programming language and compiler written in Rust. Mash parses, type-checks, and optimizes source code, then emits x86-64 assembly for NASM. A small C runtime handles printing, and GCC links the final executable.

```text
Source → Tokens → AST → Typed AST → Constant folding → IR → Assembly → Executable
```

## A taste of Mash

```text
let n = 5;
let result = 1;

while n > 1 {
    result *= n;
    n -= 1;
}

print(result);
```

Output:

```text
120
```

## Getting started

You will need:

- Rust and Cargo with support for the Rust 2024 edition.
- NASM, GCC, GNU Make, and a linker (`ld`).
- An x86-64 build environment. The Makefile includes ELF64 settings for Linux and Mach-O settings for macOS.

From the repository root, create the source and output directories:

```sh
mkdir -p progs out
```

Save the example above as `progs/factorial.msh`, then build and run it:

```sh
make out/factorial
./out/factorial
```

Make runs the compiler, assembles `out/factorial.s`, builds the runtime as needed, and links `out/factorial`.

To generate assembly without assembling or linking:

```sh
cargo run -- progs/factorial.msh
```

The compiler accepts one source-file argument and writes `out/<source-name>.s` relative to the current directory. The `out` directory must already exist. To inspect a compiled executable on Linux:

```sh
objdump -d out/factorial
```

## Language features

- **Types:** signed 64-bit integers, booleans, characters, and unit (`()`). Variable types are inferred from their initializers and checked on assignment.
- **Variables:** `let x = 10;`, reassignment, and compound assignments such as `+=`, `*=`, and `<<=`.
- **Control flow:** `if` / `else if` / `else`, `while`, nested blocks, `break;`, and `break n;` to exit multiple enclosing loops.
- **Expressions:** arithmetic, comparisons, logical operations, bitwise operations, shifts, and ternaries (`condition ? a : b`).
- **Integer literals:** decimal, hexadecimal (`0xff`), binary (`0b1010`), and octal (`0o17`).

```text
let score = 85;
let passed = score >= 70;
let grade = score >= 90 ? 4 : (score >= 80 ? 3 : 2);

if passed {
    print(grade);
} else {
    print(0);
}
```

### Built-in functions

| Function | Purpose |
| --- | --- |
| `print(value)` | Print an integer, boolean, or character. |
| `abs(x)` | Absolute value of an integer. |
| `sgn(x)` | Sign of an integer: `-1`, `0`, or `1`. |
| `min(a, ...)` | Smallest of one or more integers. |
| `max(a, ...)` | Largest of one or more integers. |

## Inside the compiler

| Stage | Source | Responsibility |
| --- | --- | --- |
| Lexing | [lexer.rs](src/lexer.rs) | Tokenization with Logos and lookahead. |
| Parsing | [parser.rs](src/parser.rs) | Build the abstract syntax tree. |
| AST and types | [types.rs](src/types.rs) | Shared expression, statement, and type definitions. |
| Type checking | [type_check.rs](src/type_check.rs) | Validate scopes, operators, conditions, and built-in arguments. |
| Constant folding | [folding.rs](src/folding.rs) | Simplify expressions and constant control flow. |
| IR generation | [compile_to_ir.rs](src/compile_to_ir.rs) | Lower the typed program into an assembly-oriented intermediate representation. |
| IR definitions | [inter_rep.rs](src/inter_rep.rs) | Represent instructions, registers, operands, and labels. |
| Assembly optimization | [optimize_asm.rs](src/optimize_asm.rs) | Simplify instruction sequences before emission. |
| Assembly emission | [ir_asm.rs](src/ir_asm.rs) | Render NASM assembly. |

[main.rs](src/main.rs) connects the pipeline, [constructors.rs](src/constructors.rs) provides construction macros, and [runtime/](runtime/) contains the C printing routines.

## Development

Build the compiler:

```sh
cargo build
```

The [tests/](tests/) directory contains lexer, parser, type-checking, and compilation tests. Compilation tests use Make, NASM, and GCC to build and run generated programs.

```sh
mkdir -p out
cargo test
```

Some tests still use older language syntax and need updating to match the current parser.

## Project status

Mash is an experimental compiler. Parsing, type checking, constant folding, and native assembly generation are implemented. The current backend targets x86-64; strings, lists, and user-defined functions are not yet implemented. Error reporting and test coverage are still evolving.
