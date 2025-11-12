#![allow(unused_variables, dead_code, unused_imports, unused_mut)]
mod lexer;
mod parser;
mod types;
mod interp;
mod compile;
mod folding;

use interp::interp;
use lexer::{Token, PeekableLexer};

use logos::Logos;
use std::{env, fs};

fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");
    let src = fs::read_to_string(&filename).expect("Failed to read file");
    let lexer = Token::lexer(&src);


    let mut peeker = PeekableLexer::new(lexer);

    let parsed = parser::parser(&mut peeker);

    interp(parsed.expect("Err")).unwrap();
    // parse_str()

    // dbg!(unexpected_token(&mut peeker)).expect("panic message");

}

fn parse_str(s: &str) {
    let lexer = Token::lexer(s);
    let mut peeker = PeekableLexer::new(lexer);
    // while let Some(peek) = peeker.next() {
    //     println!("{:?}", peek);
    // }
    let parsed = parser::parser(&mut peeker);
    // let _ = dbg!(parsed);
}