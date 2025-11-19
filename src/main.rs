#![allow(dead_code)]
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
mod type_check;

use lexer::{PeekableLexer, Token};

use crate::compile::compile;
use logos::Logos;
use std::path::Path;
use std::{env, fs};

fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");
    let src = fs::read_to_string(&filename).expect("Failed to read file");
    let lexer = Token::lexer(&src);

    let mut peeker = PeekableLexer::new(lexer);

    let parsed = parser::parser(&mut peeker);
    let t = type_check::typify(parsed.unwrap());
    let path = Path::new(&filename);
    compile(t.unwrap(), path).expect("Failed to compile");
}