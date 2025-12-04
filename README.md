# Mash

- Compiling will support only x86_64 assembly (for now, I may implement LLVM later).
- Type checking coming soon!
- Compiles down to NASM, from there NASM and GCC are used.
- Use `objdump -d` to see the disassembled output.
- This repository will be updating frequently as I progress in the project!
- Proper testing coming soon!


## Documentation coming soon!

## If you are a Recruiter Viewing my Work
Please see:
- [parser.rs](src/parser.rs)
- [types.rs](src/types.rs)
- [ir_asm.rs](src/ir_asm.rs)
- [inter_rep.rs](src/inter_rep.rs)
- [constructors.rs](src/constructors.rs)
- [lexer.rs](src/lexer.rs)
- [type_check.rs](src/type_check.rs)
- [compile_to_ir](src/compile_to_ir.rs)

## Todo
- Replace null type with unit
- Remove let statement(?)
- Allow shadowing(?)
- Make print a function instead of a keyword