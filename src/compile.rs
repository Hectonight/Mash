use std::fs::write;
use std::path::Path;
use std::process::Command;
use crate::compile_to_ir::compile_to_ir;
use crate::ir_asm::ir_to_asm;
use crate::types::TypedProgram;

pub fn compile(program: TypedProgram, filename: &Path) -> std::io::Result<()> {
    write_s(program, filename)
}


fn write_s(program: TypedProgram, filename: &Path) -> Result<(), std::io::Error> {
    let ir = compile_to_ir(&program);
    let asm = ir_to_asm(ir);
    let path = format!("./out/{}.s", filename.file_stem().unwrap().to_str().unwrap());
    // Write the assembly to disk
    write(path, asm)
}

fn compile_o(file: &str) {
    // Assemble using NASM
    Command::new("nasm")
        .args(["-f", "elf64", "out/out.s", "-o", "out/out.o"])
        .status()
        .unwrap();
}

fn compile_out(file: &str) {
    // Link using gcc
    Command::new("gcc")
        .args(["out/out.o", "-o", "out/out"])
        .status()
        .unwrap();
}


