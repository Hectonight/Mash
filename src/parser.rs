use crate::lexer::{PeekableLexer, Token};
use crate::types::{Datum, Expr, Ops, Program, Statement};
use std::ops::Range;
use crate::types::Statement::Assignment;

type ParseResult<T> = Result<T, (String, Range<usize>)>;


fn unrecognized_token<T>(lexer: &mut PeekableLexer<Token>) -> ParseResult<T> {
    Err(("Unrecognized token".to_owned(), lexer.span()))
}

fn unexpected_eof<T>(lexer: &mut PeekableLexer<Token>) -> ParseResult<T> {
    Err(("Unexpected end of input".to_owned(), lexer.span()))
}
fn unexpected_token<T>(lexer: &mut PeekableLexer<Token>, token: Token) -> ParseResult<T> {
    Err((format!("Unexpected token {:?}", token), lexer.span()))
}


pub fn parser(lexer: &mut PeekableLexer<Token>) -> ParseResult<Program> {
    let mut program = Program::new();
    while let Some(statement) = parse_statement_option(lexer) {
        program.push(statement?)
    }
    Ok(program)
}

fn parse_statement_option(lexer: &mut PeekableLexer<Token>) -> Option<ParseResult<Statement>> {
    match lexer.peek() {
        Some(_) => Some(parse_statement(lexer)),
        None => None
    }

}

fn parse_statement(lexer: &mut PeekableLexer<Token>) -> ParseResult<Statement> {
    match (lexer.peek(), lexer.peek2()) {
        | (Some(Err(_)), _) => unrecognized_token(lexer),
        | (Some(Ok(Token::Print)), _) => { lexer.next(); parse_print(lexer) },
        | (Some(Ok(Token::If)), _) => { lexer.next(); parse_if(lexer) }
        | (Some(Ok(Token::Let)), _) => {lexer.next(); parse_let(lexer) }

        | (Some(Ok(Token::Identifier(_))),
            Some(Ok(Token::Equal
                     | Token::PlusEqual
                     | Token::MinusEqual
                     | Token::AmpersandEqual
                     | Token::StarEqual
                     | Token::SlashEqual
                     | Token::PercentEqual
                     | Token::ShiftLeftEqual
                     | Token::ShiftRightEqual
                     | Token::VBarEqual
                     | Token::CarrotEqual
                 ))) => {
            parse_assignment(lexer)
        },
        (None, _) => unexpected_eof(lexer),
        _ => parse_statement_expr(lexer)
    }
}

fn parse_assignment(lexer: &mut PeekableLexer<Token>) -> ParseResult<Statement> {
    let assignment = match (lexer.next(), lexer.next()) {
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::Equal))) => Ok(Assignment(s, parse_expr(lexer)?)),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::PlusEqual)))       => Ok(Assignment(s.clone(), Expr::Op(Ops::Plus( Box::new(Expr::Identifier(s)), Box::new(parse_expr(lexer)?))))),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::MinusEqual)))      => Ok(Assignment(s.clone(), Expr::Op(Ops::Minus(Box::new(Expr::Identifier(s)), Box::new(parse_expr(lexer)?))))),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::StarEqual)))       => Ok(Assignment(s.clone(), Expr::Op(Ops::Mul(Box::new(Expr::Identifier(s)), Box::new(parse_expr(lexer)?))))),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::SlashEqual)))      => Ok(Assignment(s.clone(), Expr::Op(Ops::Div(Box::new(Expr::Identifier(s)), Box::new(parse_expr(lexer)?))))),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::PercentEqual)))    => Ok(Assignment(s.clone(), Expr::Op(Ops::Mod(Box::new(Expr::Identifier(s)), Box::new(parse_expr(lexer)?))))),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::AmpersandEqual)))  => Ok(Assignment(s.clone(), Expr::Op(Ops::BitAnd(Box::new(Expr::Identifier(s)), Box::new(parse_expr(lexer)?))))),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::VBarEqual)))       => Ok(Assignment(s.clone(), Expr::Op(Ops::BitOr(Box::new(Expr::Identifier(s)), Box::new(parse_expr(lexer)?))))),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::CarrotEqual)))     => Ok(Assignment(s.clone(), Expr::Op(Ops::BitXor(Box::new(Expr::Identifier(s)), Box::new(parse_expr(lexer)?))))),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::ShiftLeftEqual)))  => Ok(Assignment(s.clone(), Expr::Op(Ops::BitShiftLeft(Box::new(Expr::Identifier(s)), Box::new(parse_expr(lexer)?))))),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::ShiftRightEqual))) => Ok(Assignment(s.clone(), Expr::Op(Ops::BitShiftRight(Box::new(Expr::Identifier(s)), Box::new(parse_expr(lexer)?))))),
        _ => unreachable!()
    };
    assert_token(lexer, Token::Semicolon, ";")?;
    assignment
}

fn parse_let(lexer: &mut PeekableLexer<Token>) -> ParseResult<Statement> {
    match lexer.next() {
        Some(Ok(Token::Identifier(s))) => {
            assert_token(lexer, Token::Equal, "=")?;
            let e = parse_expr(lexer)?;
            assert_token(lexer, Token::Semicolon, ";")?;
            Ok(Statement::Let(s, e))
        }
        None => unexpected_eof(lexer),
        Some(Err(_)) => unrecognized_token(lexer),
        Some(Ok(t)) => unexpected_token(lexer, t),
    }
}

fn parse_if(lexer: &mut PeekableLexer<Token>) -> ParseResult<Statement> {
    let e = parse_expr(lexer)?;
    let mut elif = vec![];
    let codeblock_initial = parse_codeblock_no_semi(lexer)?;
    while let Some(Ok(Token::Else)) = lexer.peek() {
        lexer.next();
        match lexer.next() {
            Some(Ok(Token::If)) => elif.push((parse_expr(lexer)?, parse_codeblock_no_semi(lexer)?)),
            Some(Ok(Token::LCurly)) => return Ok(Statement::If(e,codeblock_initial,elif,Some(parse_code_block_rest(lexer)?))),
            None => return unexpected_eof(lexer),
            Some(Err(_)) => return unrecognized_token(lexer),
            Some(Ok(t)) => return unexpected_token(lexer, t),
        }
    }
    optional_token(lexer, Token::Semicolon);
    Ok(Statement::If(e,codeblock_initial,elif,None))
}

#[allow(dead_code)]
fn parse_codeblock(lexer: &mut PeekableLexer<Token>) -> ParseResult<Vec<Statement>> {
    assert_token(lexer, Token::LCurly, "{")?;
    parse_code_block_rest(lexer)
}

fn parse_code_block_rest(lexer: &mut PeekableLexer<Token>) -> ParseResult<Vec<Statement>> {
    let parsed = parse_codeblock_no_semi_rest(lexer)?;
    optional_token(lexer, Token::Semicolon);
    Ok(parsed)
}

fn parse_codeblock_no_semi(lexer: &mut PeekableLexer<Token>) -> ParseResult<Vec<Statement>> {
    assert_token(lexer, Token::LCurly, "{")?;
    parse_codeblock_no_semi_rest(lexer)
}

fn parse_codeblock_no_semi_rest(lexer: &mut PeekableLexer<Token>) -> ParseResult<Vec<Statement>> {
    let mut statements = vec![];
    loop {
        match lexer.peek() {
            None => return unexpected_eof(lexer),
            Some(Err(_)) => return unrecognized_token(lexer),
            Some(Ok(Token::RCurly)) => {
                lexer.next();

                return Ok(statements);
            }
            Some(Ok(_)) => statements.push(parse_statement(lexer)?)
        }
    }
}

fn parse_print(lexer: &mut PeekableLexer<Token>) -> ParseResult<Statement> {
    let x = parse_expr(lexer)?;
    assert_token(lexer, Token::Semicolon, ";")?;
    Ok(Statement::Print(x))
}

fn parse_statement_expr(lexer: &mut PeekableLexer<Token>) -> ParseResult<Statement> {
    let x = parse_expr(lexer)?;
    assert_token(lexer, Token::Semicolon, ";")?;
    Ok(Statement::Expr(x))
}

fn parse_expr(lexer: &mut PeekableLexer<Token>) -> ParseResult<Expr> {
    parse_ternary(lexer)
}

fn parse_ternary(lexer: &mut PeekableLexer<Token>) -> ParseResult<Expr> {
    let mut lhs = parse_or(lexer)?;
    while let Some(Ok(Token::QuestionMark)) = lexer.peek() {
        lexer.next();
        let mid = parse_ternary(lexer)?;
        assert_token(lexer, Token::Colon, ":")?;
        let rhs = parse_ternary(lexer)?;
        lhs = Expr::Op(Ops::Ternary(Box::new(lhs), Box::new(mid), Box::new(rhs)));
    }
    Ok(lhs)
}

fn parse_or(lexer: &mut PeekableLexer<Token>) -> ParseResult<Expr> {
    let mut lhs = parse_and(lexer)?;
    while let Some(Ok(Token::Or)) = lexer.peek() {
        lexer.next();
        let rhs = parse_and(lexer)?;
        lhs = Expr::Op(Ops::Or(Box::new(lhs), Box::new(rhs)));
    }
    Ok(lhs)
}

fn parse_and(lexer: &mut PeekableLexer<Token>) -> ParseResult<Expr> {
    let mut lhs = parse_bitor(lexer)?;
    while let Some(Ok(Token::Ampersand)) = lexer.peek() {
        lexer.next();
        let rhs = parse_bitor(lexer)?;
        lhs = Expr::Op(Ops::And(Box::new(lhs), Box::new(rhs)));
    }
    Ok(lhs)
}

fn parse_bitor(lexer: &mut PeekableLexer<Token>) -> ParseResult<Expr> {
    let mut lhs = parse_bitxor(lexer)?;
    while let Some(Ok(Token::VBar)) = lexer.peek() {
        lexer.next();
        let rhs = parse_bitxor(lexer)?;
        lhs = Expr::Op(Ops::BitOr(Box::new(lhs), Box::new(rhs)));
    }
    Ok(lhs)
}


fn parse_bitxor(lexer: &mut PeekableLexer<Token>) -> ParseResult<Expr> {
    let mut lhs = parse_bitand(lexer)?;
    while let Some(Ok(Token::Carrot)) = lexer.peek() {
        lexer.next();
        let rhs = parse_bitand(lexer)?;
        lhs = Expr::Op(Ops::BitXor(Box::new(lhs), Box::new(rhs)));
    }
    Ok(lhs)
}


fn parse_bitand(lexer: &mut PeekableLexer<Token>) -> ParseResult<Expr> {
    let mut lhs = parse_equality(lexer)?;
    while let Some(Ok(Token::Ampersand)) = lexer.peek() {
        lexer.next();
        let rhs = parse_equality(lexer)?;
        lhs = Expr::Op(Ops::BitAnd(Box::new(lhs), Box::new(rhs)));
    }
    Ok(lhs)
}


fn parse_equality(lexer: &mut PeekableLexer<Token>) -> ParseResult<Expr> {
    let mut lhs = parse_relational(lexer)?;
    while let Some(Ok(Token::DoubleEqual | Token::NotEqual)) = lexer.peek() {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_relational(lexer)?;
        lhs = match token {
            Token::DoubleEqual => Expr::Op(Ops::Eq(Box::new(lhs), Box::new(rhs))),
            Token::NotEqual => Expr::Op(Ops::Neq(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!()
        }
    }
    Ok(lhs)
}

fn parse_relational(lexer: &mut PeekableLexer<Token>) -> ParseResult<Expr> {
    let mut lhs = parse_bitshift(lexer)?;
    while let Some(Ok(Token::Less | Token::Greater | Token::GreaterEqual | Token::LessEqual)) = lexer.peek() {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_bitshift(lexer)?;
        lhs = match token {
            Token::GreaterEqual => Expr::Op(Ops::Geq(Box::new(lhs), Box::new(rhs))),
            Token::LessEqual => Expr::Op(Ops::Leq(Box::new(lhs), Box::new(rhs))),
            Token::Greater => Expr::Op(Ops::Gt(Box::new(lhs), Box::new(rhs))),
            Token::Less => Expr::Op(Ops::Lt(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!()
        }
    }
    Ok(lhs)
}

fn parse_bitshift(lexer: &mut PeekableLexer<Token>) -> ParseResult<Expr> {
    let mut lhs = parse_addsub(lexer)?;
    while let Some(Ok(Token::ShiftLeft | Token::ShiftRight)) = lexer.peek() {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_addsub(lexer)?;
        lhs = match token {
            Token::ShiftRight => Expr::Op(Ops::BitShiftRight(Box::new(lhs), Box::new(rhs))),
            Token::ShiftLeft => Expr::Op(Ops::BitShiftLeft(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!()
        }
    }
    Ok(lhs)
}

fn parse_addsub(lexer: &mut PeekableLexer<Token>) -> ParseResult<Expr> {
    let mut lhs = parse_muldivmod(lexer)?;
    while let Some(Ok(Token::Plus | Token::Minus)) = lexer.peek() {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_muldivmod(lexer)?;
        lhs = match token {
            Token::Plus => Expr::Op(Ops::Plus(Box::new(lhs), Box::new(rhs))),
            Token::Minus => Expr::Op(Ops::Minus(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!()
        }
    }
    Ok(lhs)
}

fn parse_muldivmod(lexer: &mut PeekableLexer<Token>) -> ParseResult<Expr> {
    let mut lhs = parse_prefix(lexer)?;
    while let Some(Ok(Token::Star | Token::Slash | Token::Percent)) = lexer.peek() {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_prefix(lexer)?;
        lhs = match token {
            Token::Star => Expr::Op(Ops::Mul(Box::new(lhs), Box::new(rhs))),
            Token::Slash => Expr::Op(Ops::Div(Box::new(lhs), Box::new(rhs))),
            Token::Percent => Expr::Op(Ops::Mod(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!()
        }
    }
    Ok(lhs)
}

fn parse_prefix(lexer: &mut PeekableLexer<Token>) -> ParseResult<Expr> {
    match lexer.peek() {
        Some(Ok(Token::ExclamationPoint)) => {
            lexer.next();
            Ok(Expr::Op(Ops::Not(Box::new(after_ops(lexer)?))))
        }
        Some(Ok(Token::Tilde)) => {
            lexer.next();
            Ok(Expr::Op(Ops::BitNot(Box::new(after_ops(lexer)?))))
        }
        Some(Ok(Token::Plus)) => {
            lexer.next();
            Ok(Expr::Op(Ops::Pos(Box::new(after_ops(lexer)?))))
        }
        Some(Ok(Token::Minus)) => {
            lexer.next();
            Ok(Expr::Op(Ops::Neg(Box::new(after_ops(lexer)?))))
        }
        _ => Ok(after_ops(lexer)?),
    }
}

fn after_ops(lexer: &mut PeekableLexer<Token>) -> ParseResult<Expr> {
    match lexer.next() {
        Some(Ok(Token::LParen)) => {
            let expr = parse_expr(lexer)?;
            assert_token(lexer, Token::RParen, ")")?;
            Ok(expr)
        },
        Some(Ok(Token::Identifier(x))) => Ok(Expr::Identifier(x)),
        Some(Ok(Token::Int(x)))        => Ok(Expr::Datum(Datum::Int(x))),
        Some(Ok(Token::Bool(x)))       => Ok(Expr::Datum(Datum::Bool(x))),
        Some(Ok(Token::Char(x)))       => Ok(Expr::Datum(Datum::Char(x))),
        Some(Ok(Token::Null))          => Ok(Expr::Datum(Datum::Null)),
        None => unexpected_eof(lexer),
        Some(Err(_)) => unrecognized_token(lexer),
        Some(Ok(t)) => unexpected_token(lexer, t),
    }
}



fn assert_token(lexer: &mut PeekableLexer<Token>, token: Token, s: &str) -> ParseResult<()> {
    match lexer.next() {
        Some(Ok(t)) if t == token => Ok(()),
        Some(Err(_)) => unrecognized_token(lexer),
        _ => Err(("Expected ".to_owned() + s, lexer.span()))
    }
}

fn optional_token(lexer: &mut PeekableLexer<Token>, token: Token) {
    if *lexer.peek() == Some(Ok(token)) {
        lexer.next();
    }
}
