use crate::compile_to_ir::compile_to_ir;
use crate::ir_asm::ir_to_asm;
use crate::types::TypedProgram;
use std::fs::write;
use std::path::Path;
use crate::inter_rep::AsmProg;
use crate::optimize_asm::optimize_asm;

pub fn compile(program: TypedProgram, filename: &Path) -> std::io::Result<()> {
    let ir = compile_to_ir(&program);
    let opt_prog = optimize_asm(ir);
    write_s(opt_prog, filename)
}

fn write_s(program: AsmProg, filename: &Path) -> Result<(), std::io::Error> {
    let asm = ir_to_asm(program);
    let path = format!(
        "./out/{}.s",
        filename.file_stem().unwrap().to_str().unwrap()
    );
    // Write the assembly to disk
    write(path, asm)
}
