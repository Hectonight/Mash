#![allow(unused_variables, dead_code, unused_imports, unused_mut)]
mod lexer;
mod parser;
mod types;
mod interp;
mod compile;
mod folding;
mod inter_rep;
mod ir_asm;
mod compile_to_ir;

#[macro_use]
mod constructors;

use interp::interp;
use lexer::{Token, PeekableLexer};

use logos::Logos;
use std::{env, fs};
use crate::compile::compile;
use crate::inter_rep::IRInst::Mov;
use crate::inter_rep::Operand::{Imm, Reg};
// use crate::inter_rep::Register::RAX;

fn main() {
    // dbg!(ir_asm::instr_to_asm(&Mov(Reg(RAX), Imm(2))));




    let filename = env::args().nth(1).expect("Expected file argument");
    let src = fs::read_to_string(&filename).expect("Failed to read file");
    let lexer = Token::lexer(&src);


    let mut peeker = PeekableLexer::new(lexer);

    let parsed = parser::parser(&mut peeker);
    compile(parsed.unwrap());

    //
    // interp(parsed.expect("Err")).unwrap();


}