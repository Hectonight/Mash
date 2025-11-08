#![allow(unused_variables, unused)]
mod lexer;
mod parser;
mod types;
mod interp;

use interp::interp;
use lexer::Tokens;

use logos::Logos;
use std::{env, fs};


fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");
    let src = fs::read_to_string(&filename).expect("Failed to read file");
    let mut lexer = <Tokens as Logos>::lexer(&src);

    let parsed = parser::parser(&mut lexer);

    // println!("{:?}", e.expect("Err"));
    interp(&parsed.expect("Err"));
}
