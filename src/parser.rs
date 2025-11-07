use std::ops::Range;
use logos::Lexer;
use crate::lexer::Tokens;
use crate::types::Datum::{Bool, Int, Null};
use crate::types::{Expr, Program, Statement};
use crate::types::Expr::{Datum};

type Lex<'a> = Lexer<'a, Tokens>;
type ParseResult<T> = Result<T, (String, Range<usize>)>;


pub fn parser(lexer: &mut Lex) -> ParseResult<Program> {
    let mut program = Program::new();
    while let Some(statement) = parse_statement(lexer) {
        program.push(statement?)
    }
    Ok(program)
}

fn parse_statement(lexer: &mut Lex) -> Option<ParseResult<Statement>> {
    match lexer.next()? {
        Err(_) => Some(Err(("Unrecognized token".to_owned(), lexer.span()))),
        Ok(Tokens::Print) => Some(parse_print(lexer)),
        _ => { Some(parse_statement_expr(lexer)) }
    }
}

fn parse_print(lexer: &mut Lex) -> ParseResult<Statement> {
    let x = parse_expr(lexer)?;
    assert_token(lexer, Tokens::Semicolon, ";")?;
    Ok(Statement::Print(x))
}

fn parse_statement_expr(lexer: &mut Lex) -> ParseResult<Statement> {
    let x = parse_expr(lexer)?;
    assert_token(lexer, Tokens::Semicolon, ";")?;
    Ok(Statement::Expr(x))
}

fn parse_expr(lexer: &mut Lex) -> ParseResult<Expr> {
    match lexer.next() {
        Some(Ok(Tokens::LParen)) => {
            let expr = parse_expr(lexer)?;
            assert_token(lexer, Tokens::RParen, ")")?;
            Ok(expr)
        },
        Some(Ok(Tokens::Int(x))) => Ok(Datum(Int(x))),
        Some(Ok(Tokens::Bool(x))) => Ok(Datum(Bool(x))),
        Some(Ok(Tokens::Null)) => Ok(Datum(Null)),
        None => Err(("Unexpected end of input".to_owned(), lexer.span())),
        _ =>    Err(("Unrecognized token".to_owned(), lexer.span()))
    }
}


fn assert_token(lexer: &mut Lex, token: Tokens, s: &str) -> ParseResult<()> {
    match lexer.next() {
        Some(Ok(t)) if t == token => Ok(()),
        Some(Err(_)) => Err(("Unrecognized token".to_owned(), lexer.span())),
        _ => Err((("Expected ".to_owned() + s), lexer.span()))
    }
}

