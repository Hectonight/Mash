use crate::lexer::{PeekableLexer, Tokens};
use crate::types::{Datum, Expr, Ops, Program, Statement, Value};
use std::ops::Range;


type ParseResult<T> = Result<T, (String, Range<usize>)>;


fn unrecognized_token<T>(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<T> {
    Err(("Unrecognized token".to_owned(), lexer.span()))
}


fn unexpected_eof<T>(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<T> {
    Err(("Unexpected end of input".to_owned(), lexer.span()))
}


pub fn parser(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Program> {
    let mut program = Program::new();
    while let Some(statement) = parse_statement(lexer) {
        program.push(statement?)
    }
    Ok(program)
}

fn parse_statement(lexer: &mut PeekableLexer<Tokens>) -> Option<ParseResult<Statement>> {
    match lexer.peek() {
        None => None,
        Some(Err(_)) => Some(unrecognized_token(lexer)),
        Some(Ok(Tokens::Print)) => { lexer.next(); Some(parse_print(lexer)) },
        Some(Ok(Tokens::If)) => { lexer.next(); Some(parse_if(lexer)) }
        _ => { Some(parse_statement_expr(lexer)) }
    }
}

fn parse_if(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Statement> {

    todo!()
}

fn parse_print(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Statement> {
    let x = parse_expr(lexer)?;
    assert_token(lexer, Tokens::Semicolon, ";")?;
    Ok(Statement::Print(x))
}

fn parse_statement_expr(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Statement> {
    let x = parse_expr(lexer)?;
    assert_token(lexer, Tokens::Semicolon, ";")?;
    Ok(Statement::Expr(x))
}

fn parse_expr(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Expr> {
    parse_ternary(lexer)
}

fn parse_ternary(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Expr> {
    let mut lhs = parse_or(lexer)?;
    while let Some(Ok(Tokens::QuestionMark)) = lexer.peek() {
        lexer.next();
        let mid = parse_ternary(lexer)?;
        assert_token(lexer, Tokens::Colon, ":")?;
        let rhs = parse_ternary(lexer)?;
        lhs = Expr::Op(Ops::Ternary(Box::new(lhs), Box::new(mid), Box::new(rhs)));
    }
    Ok(lhs)
}

fn parse_or(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Expr> {
    let mut lhs = parse_and(lexer)?;
    while let Some(Ok(Tokens::Or)) = lexer.peek() {
        lexer.next();
        let rhs = parse_and(lexer)?;
        lhs = Expr::Op(Ops::Or(Box::new(lhs), Box::new(rhs)));
    }
    Ok(lhs)
}

fn parse_and(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Expr> {
    let mut lhs = parse_bitor(lexer)?;
    while let Some(Ok(Tokens::Ampersand)) = lexer.peek() {
        lexer.next();
        let rhs = parse_bitor(lexer)?;
        lhs = Expr::Op(Ops::And(Box::new(lhs), Box::new(rhs)));
    }
    Ok(lhs)
}

fn parse_bitor(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Expr> {
    let mut lhs = parse_bitxor(lexer)?;
    while let Some(Ok(Tokens::VBar)) = lexer.peek() {
        lexer.next();
        let rhs = parse_bitxor(lexer)?;
        lhs = Expr::Op(Ops::BitOr(Box::new(lhs), Box::new(rhs)));
    }
    Ok(lhs)
}


fn parse_bitxor(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Expr> {
    let mut lhs = parse_bitand(lexer)?;
    while let Some(Ok(Tokens::Carrot)) = lexer.peek() {
        lexer.next();
        let rhs = parse_bitand(lexer)?;
        lhs = Expr::Op(Ops::BitXor(Box::new(lhs), Box::new(rhs)));
    }
    Ok(lhs)
}


fn parse_bitand(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Expr> {
    let mut lhs = parse_equality(lexer)?;
    while let Some(Ok(Tokens::Ampersand)) = lexer.peek() {
        let token = lexer.next();
        let rhs = parse_equality(lexer)?;
        lhs = Expr::Op(Ops::BitAnd(Box::new(lhs), Box::new(rhs)));
    }
    Ok(lhs)
}


fn parse_equality(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Expr> {
    let mut lhs = parse_relational(lexer)?;
    while let Some(Ok(Tokens::Equal | Tokens::NotEqual)) = lexer.peek() {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_relational(lexer)?;
        lhs = match token {
            Tokens::Equal => Expr::Op(Ops::Eq(Box::new(lhs), Box::new(rhs))),
            Tokens::NotEqual => Expr::Op(Ops::Neq(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!()
        }
    }
    Ok(lhs)
}

fn parse_relational(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Expr> {
    let mut lhs = parse_bitshift(lexer)?;
    while let Some(Ok(Tokens::Less | Tokens::Greater | Tokens::GreaterEqual | Tokens::LessEqual)) = lexer.peek() {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_bitshift(lexer)?;
        lhs = match token {
            Tokens::GreaterEqual => Expr::Op(Ops::Geq(Box::new(lhs), Box::new(rhs))),
            Tokens::LessEqual => Expr::Op(Ops::Leq(Box::new(lhs), Box::new(rhs))),
            Tokens::Greater => Expr::Op(Ops::Gt(Box::new(lhs), Box::new(rhs))),
            Tokens::Less => Expr::Op(Ops::Lt(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!()
        }
    }
    Ok(lhs)
}

fn parse_bitshift(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Expr> {
    let mut lhs = parse_addsub(lexer)?;
    while let Some(Ok(Tokens::ShiftLeft | Tokens::ShiftRight)) = lexer.peek() {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_addsub(lexer)?;
        lhs = match token {
            Tokens::ShiftRight => Expr::Op(Ops::BitShiftRight(Box::new(lhs), Box::new(rhs))),
            Tokens::ShiftLeft => Expr::Op(Ops::BitShiftLeft(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!()
        }
    }
    Ok(lhs)
}

fn parse_addsub(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Expr> {
    let mut lhs = parse_muldivmod(lexer)?;
    while let Some(Ok(Tokens::Plus | Tokens::Minus)) = lexer.peek() {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_muldivmod(lexer)?;
        lhs = match token {
            Tokens::Plus => Expr::Op(Ops::Plus(Box::new(lhs), Box::new(rhs))),
            Tokens::Minus => Expr::Op(Ops::Minus(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!()
        }
    }
    Ok(lhs)
}

fn parse_muldivmod(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Expr> {
    let mut lhs = parse_prefix(lexer)?;
    while let Some(Ok(Tokens::Star | Tokens::Slash | Tokens::Percent)) = lexer.peek() {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_prefix(lexer)?;
        lhs = match token {
            Tokens::Star => Expr::Op(Ops::Mul(Box::new(lhs), Box::new(rhs))),
            Tokens::Slash => Expr::Op(Ops::Div(Box::new(lhs), Box::new(rhs))),
            Tokens::Percent => Expr::Op(Ops::Mod(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!()
        }
    }
    Ok(lhs)
}

fn parse_prefix(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Expr> {
    match lexer.peek() {
        Some(Ok(Tokens::ExclamationPoint)) => {
            lexer.next();
            Ok(Expr::Op(Ops::Not(Box::new(after_ops(lexer)?))))
        }
        Some(Ok(Tokens::Tilde)) => {
            lexer.next();
            Ok(Expr::Op(Ops::BitNot(Box::new(after_ops(lexer)?))))
        }
        Some(Ok(Tokens::Plus)) => {
            lexer.next();
            Ok(Expr::Op(Ops::Pos(Box::new(after_ops(lexer)?))))
        }
        Some(Ok(Tokens::Minus)) => {
            lexer.next();
            Ok(Expr::Op(Ops::Neg(Box::new(after_ops(lexer)?))))
        }
        _ => Ok(after_ops(lexer)?),
    }
}

fn after_ops(lexer: &mut PeekableLexer<Tokens>) -> ParseResult<Expr> {
    match lexer.next() {
        Some(Ok(Tokens::LParen)) => {
            let expr = parse_expr(lexer)?;
            assert_token(lexer, Tokens::RParen, ")")?;
            Ok(expr)
        },
        Some(Ok(Tokens::Identifier(x))) => Ok(Expr::Identifier(x)),
        Some(Ok(Tokens::Int(x)))        => Ok(Expr::Datum(Datum::Int(x))),
        Some(Ok(Tokens::Bool(x)))       => Ok(Expr::Datum(Datum::Bool(x))),
        Some(Ok(Tokens::Null))          => Ok(Expr::Datum(Datum::Null)),
        None => Err(("Unexpected end of input".to_owned(), lexer.span())),
        Some(Err(_)) => Err(("Unrecognized token".to_owned(), lexer.span())),
        Some(v) => { Err(("Unexpected token".to_owned(), lexer.span()))},

    }
}



fn assert_token(lexer: &mut PeekableLexer<Tokens>, token: Tokens, s: &str) -> ParseResult<()> {
    match lexer.next() {
        Some(Ok(t)) if t == token => Ok(()),
        Some(Err(_)) => unrecognized_token(lexer),
        _ => Err(("Expected ".to_owned() + s, lexer.span()))
    }
}

