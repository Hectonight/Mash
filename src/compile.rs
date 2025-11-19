use crate::compile_to_ir::compile_to_ir;
use crate::ir_asm::ir_to_asm;
use crate::types::TypedProgram;
use std::fs::write;
use std::path::Path;

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



