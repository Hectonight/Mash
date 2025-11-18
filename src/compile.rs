use std::fs::write;
use std::process::Command;
use crate::compile_to_ir::compile_to_ir;
use crate::ir_asm::ir_to_asm;
use crate::types::Program;

pub fn compile(program: Program) {
    write_s(program);
    compile_o("");
    compile_out("");
}


fn write_s(program: Program) {
    let ir = compile_to_ir(&program);
    let asm = ir_to_asm(ir);

    // Write the assembly to disk
    write("./out/out.s", asm).unwrap();
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


