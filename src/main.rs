#![allow(unused_variables, dead_code)]
mod lexer;
mod parser;
mod types;
mod interp;

use interp::interp;
use lexer::{Tokens, PeekableLexer};

use logos::Logos;
use std::{env, fs};

fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");
    let src = fs::read_to_string(&filename).expect("Failed to read file");
    let lexer = <Tokens as Logos>::lexer(&src);
    let mut peeker = PeekableLexer::new(lexer);

    let parsed = parser::parser(&mut peeker);

    // println!("{:?}", e.expect("Err"));
    interp(&parsed.expect("Err")).unwrap();
}
