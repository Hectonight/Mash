use crate::lexer::{PeekableLexer, Token};
use crate::types::{
    BuiltIn, Datum, Expr, Ops, Program, Statement, UntypedCodeBlock, UntypedExpr, UntypedOps,
    UntypedStatement,
};
use std::ops::Range;

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

fn parse_statement_option(
    lexer: &mut PeekableLexer<Token>,
) -> Option<ParseResult<UntypedStatement>> {
    match lexer.peek() {
        Some(_) => Some(parse_statement(lexer)),
        None => None,
    }
}

fn parse_statement(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedStatement> {
    match (lexer.peek(), lexer.peek2()) {
        (Some(Err(_)), _) => unrecognized_token(lexer),
        (Some(Ok(Token::If)), _) => {
            lexer.next();
            parse_if(lexer)
        }
        (Some(Ok(Token::Let)), _) => {
            lexer.next();
            parse_let(lexer)
        }
        (
            Some(Ok(Token::Identifier(_))),
            Some(Ok(
                Token::Equal
                | Token::PlusEqual
                | Token::MinusEqual
                | Token::AmpersandEqual
                | Token::StarEqual
                | Token::SlashEqual
                | Token::PercentEqual
                | Token::ShiftLeftEqual
                | Token::ShiftRightEqual
                | Token::VBarEqual
                | Token::CarrotEqual,
            )),
        ) => parse_assignment(lexer),
        (Some(Ok(Token::While)), _) => {
            lexer.next();
            parse_while(lexer)
        }
        (Some(Ok(Token::Break)), _) => {
            lexer.next();
            parse_break(lexer)
        }
        (Some(Ok(Token::Continue)), _) => {
            lexer.next();
            Ok(Statement::Continue)
        }
        (Some(Ok(Token::LCurly)), _) => Ok(Statement::CodeBlock(parse_codeblock(lexer)?)),
        (None, _) => unexpected_eof(lexer),
        _ => parse_statement_expr(lexer),
    }
}

fn parse_break(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedStatement> {
    match lexer.next() {
        Some(Ok(Token::Semicolon)) => Ok(Statement::Break(1)),
        Some(Ok(Token::Int(n))) => {
            if n > 0 {
                assert_token(lexer, Token::Semicolon, "")?;
                Ok(Statement::Break(n as usize))
            } else {
                Err((
                    format!("Break expects positive integer, found {}", n),
                    lexer.span(),
                ))
            }
        }
        None => unexpected_eof(lexer),
        Some(Ok(t)) => unexpected_token(lexer, t),
        Some(Err(_)) => unrecognized_token(lexer),
    }
}

fn parse_while(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedStatement> {
    let e = parse_expr(lexer)?;
    let cb = parse_codeblock(lexer)?;
    Ok(Statement::While(e, cb))
}

fn parse_assignment(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedStatement> {
    let assignment = match (lexer.next(), lexer.next()) {
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::Equal))) => {
            Ok(Statement::Assignment(s, parse_expr(lexer)?))
        }
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::PlusEqual))) => Ok(Statement::Assignment(
            s.clone(),
            UntypedExpr(Expr::Op(Ops::Plus(
                Box::new(UntypedExpr(Expr::Identifier(s))),
                Box::new(parse_expr(lexer)?),
            ))),
        )),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::MinusEqual))) => Ok(Statement::Assignment(
            s.clone(),
            UntypedExpr(Expr::Op(Ops::Minus(
                Box::new(UntypedExpr(Expr::Identifier(s))),
                Box::new(parse_expr(lexer)?),
            ))),
        )),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::StarEqual))) => Ok(Statement::Assignment(
            s.clone(),
            UntypedExpr(Expr::Op(Ops::Mul(
                Box::new(UntypedExpr(Expr::Identifier(s))),
                Box::new(parse_expr(lexer)?),
            ))),
        )),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::SlashEqual))) => Ok(Statement::Assignment(
            s.clone(),
            UntypedExpr(Expr::Op(Ops::Div(
                Box::new(UntypedExpr(Expr::Identifier(s))),
                Box::new(parse_expr(lexer)?),
            ))),
        )),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::PercentEqual))) => {
            Ok(Statement::Assignment(
                s.clone(),
                UntypedExpr(Expr::Op(Ops::Mod(
                    Box::new(UntypedExpr(Expr::Identifier(s))),
                    Box::new(parse_expr(lexer)?),
                ))),
            ))
        }
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::AmpersandEqual))) => {
            Ok(Statement::Assignment(
                s.clone(),
                UntypedExpr(Expr::Op(Ops::BitAnd(
                    Box::new(UntypedExpr(Expr::Identifier(s))),
                    Box::new(parse_expr(lexer)?),
                ))),
            ))
        }
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::VBarEqual))) => Ok(Statement::Assignment(
            s.clone(),
            UntypedExpr(Expr::Op(Ops::BitOr(
                Box::new(UntypedExpr(Expr::Identifier(s))),
                Box::new(parse_expr(lexer)?),
            ))),
        )),
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::CarrotEqual))) => {
            Ok(Statement::Assignment(
                s.clone(),
                UntypedExpr(Expr::Op(Ops::BitXor(
                    Box::new(UntypedExpr(Expr::Identifier(s))),
                    Box::new(parse_expr(lexer)?),
                ))),
            ))
        }
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::ShiftLeftEqual))) => {
            Ok(Statement::Assignment(
                s.clone(),
                UntypedExpr(Expr::Op(Ops::BitShiftLeft(
                    Box::new(UntypedExpr(Expr::Identifier(s))),
                    Box::new(parse_expr(lexer)?),
                ))),
            ))
        }
        (Some(Ok(Token::Identifier(s))), Some(Ok(Token::ShiftRightEqual))) => {
            Ok(Statement::Assignment(
                s.clone(),
                UntypedExpr(Expr::Op(Ops::BitShiftRight(
                    Box::new(UntypedExpr(Expr::Identifier(s))),
                    Box::new(parse_expr(lexer)?),
                ))),
            ))
        }
        _ => unreachable!(),
    };
    assert_token(lexer, Token::Semicolon, ";")?;
    assignment
}

fn parse_let(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedStatement> {
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

fn parse_if(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedStatement> {
    let e = parse_expr(lexer)?;
    let mut elif = vec![];
    let codeblock_initial = parse_codeblock_no_semi(lexer)?;
    while let Some(Ok(Token::Else)) = lexer.peek() {
        lexer.next();
        match lexer.next() {
            Some(Ok(Token::If)) => elif.push((parse_expr(lexer)?, parse_codeblock_no_semi(lexer)?)),
            Some(Ok(Token::LCurly)) => {
                return Ok(Statement::If(
                    e,
                    codeblock_initial,
                    elif,
                    Some(parse_code_block_rest(lexer)?),
                ));
            }
            None => return unexpected_eof(lexer),
            Some(Err(_)) => return unrecognized_token(lexer),
            Some(Ok(t)) => return unexpected_token(lexer, t),
        }
    }
    optional_token(lexer, Token::Semicolon);
    Ok(Statement::If(e, codeblock_initial, elif, None))
}

fn parse_codeblock(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedCodeBlock> {
    assert_token(lexer, Token::LCurly, "{")?;
    parse_code_block_rest(lexer)
}

fn parse_code_block_rest(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedCodeBlock> {
    let parsed = parse_codeblock_no_semi_rest(lexer)?;
    optional_token(lexer, Token::Semicolon);
    Ok(parsed)
}

fn parse_codeblock_no_semi(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedCodeBlock> {
    assert_token(lexer, Token::LCurly, "{")?;
    parse_codeblock_no_semi_rest(lexer)
}

fn parse_codeblock_no_semi_rest(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedCodeBlock> {
    let mut statements = vec![];
    loop {
        match lexer.peek() {
            None => return unexpected_eof(lexer),
            Some(Err(_)) => return unrecognized_token(lexer),
            Some(Ok(Token::RCurly)) => {
                lexer.next();

                return Ok(statements);
            }
            Some(Ok(_)) => statements.push(parse_statement(lexer)?),
        }
    }
}

fn parse_statement_expr(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedStatement> {
    let x = parse_expr(lexer)?;
    assert_token(lexer, Token::Semicolon, ";")?;
    Ok(Statement::Expr(x))
}

fn parse_expr(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    parse_ternary(lexer)
}

fn parse_builtin(ident: String, lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    assert_token(lexer, Token::LParen, "(")?;

    let builtin = match ident.as_str() {
        "abs" => BuiltIn::Abs,
        "max" => BuiltIn::Max,
        "min" => BuiltIn::Min,
        "sgn" => BuiltIn::Sgn,
        "print" => BuiltIn::Print,
        _ => return Err(("Unknown Function Name".to_string(), lexer.span())),
    };

    let mut params = Vec::new();

    if let Some(Ok(Token::RParen)) = lexer.peek() {
        lexer.next();
        return Ok(UntypedExpr(Expr::BuiltIn(builtin, params)));
    }

    params.push(parse_expr(lexer)?);

    while let Some(Ok(Token::Comma)) = lexer.peek() {
        lexer.next();
        params.push(parse_expr(lexer)?);
    }
    assert_token(lexer, Token::RParen, ")")?;

    Ok(UntypedExpr(Expr::BuiltIn(builtin, params)))
}

fn parse_ternary(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    let mut lhs = parse_or(lexer)?;
    while let Some(Ok(Token::QuestionMark)) = lexer.peek() {
        lexer.next();
        let mid = parse_ternary(lexer)?;
        assert_token(lexer, Token::Colon, ":")?;
        let rhs = parse_ternary(lexer)?;
        lhs = UntypedExpr(Expr::Op(Ops::Ternary(
            Box::new(lhs),
            Box::new(mid),
            Box::new(rhs),
        )));
    }
    Ok(lhs)
}

fn parse_or(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    let mut lhs = parse_and(lexer)?;
    while let Some(Ok(Token::Or)) = lexer.peek() {
        lexer.next();
        let rhs = parse_and(lexer)?;
        lhs = UntypedExpr(Expr::Op(Ops::Or(Box::new(lhs), Box::new(rhs))));
    }
    Ok(lhs)
}

fn parse_and(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    let mut lhs = parse_bitor(lexer)?;
    while let Some(Ok(Token::And)) = lexer.peek() {
        lexer.next();
        let rhs = parse_bitor(lexer)?;
        lhs = UntypedExpr(Expr::Op(UntypedOps::And(Box::new(lhs), Box::new(rhs))));
    }
    Ok(lhs)
}

fn parse_bitor(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    let mut lhs = parse_bitxor(lexer)?;
    while let Some(Ok(Token::VBar)) = lexer.peek() {
        lexer.next();
        let rhs = parse_bitxor(lexer)?;
        lhs = UntypedExpr(Expr::Op(Ops::BitOr(Box::new(lhs), Box::new(rhs))));
    }
    Ok(lhs)
}

fn parse_bitxor(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    let mut lhs = parse_bitand(lexer)?;
    while let Some(Ok(Token::Carrot)) = lexer.peek() {
        lexer.next();
        let rhs = parse_bitand(lexer)?;
        lhs = UntypedExpr(Expr::Op(Ops::BitXor(Box::new(lhs), Box::new(rhs))));
    }
    Ok(lhs)
}

fn parse_bitand(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    let mut lhs = parse_equality(lexer)?;
    while let Some(Ok(Token::Ampersand)) = lexer.peek() {
        lexer.next();
        let rhs = parse_equality(lexer)?;
        lhs = UntypedExpr(Expr::Op(Ops::BitAnd(Box::new(lhs), Box::new(rhs))));
    }
    Ok(lhs)
}

fn parse_equality(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    let mut lhs = parse_relational(lexer)?;
    while let Some(Ok(Token::DoubleEqual | Token::NotEqual)) = lexer.peek() {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_relational(lexer)?;
        lhs = UntypedExpr(match token {
            Token::DoubleEqual => Expr::Op(Ops::Eq(Box::new(lhs), Box::new(rhs))),
            Token::NotEqual => Expr::Op(Ops::Neq(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!(),
        })
    }
    Ok(lhs)
}

fn parse_relational(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    let mut lhs = parse_bitshift(lexer)?;
    while let Some(Ok(Token::Less | Token::Greater | Token::GreaterEqual | Token::LessEqual)) =
        lexer.peek()
    {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_bitshift(lexer)?;
        lhs = UntypedExpr(match token {
            Token::GreaterEqual => Expr::Op(Ops::Geq(Box::new(lhs), Box::new(rhs))),
            Token::LessEqual => Expr::Op(Ops::Leq(Box::new(lhs), Box::new(rhs))),
            Token::Greater => Expr::Op(Ops::Gt(Box::new(lhs), Box::new(rhs))),
            Token::Less => Expr::Op(Ops::Lt(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!(),
        })
    }
    Ok(lhs)
}

fn parse_bitshift(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    let mut lhs = parse_addsub(lexer)?;
    while let Some(Ok(Token::ShiftLeft | Token::ShiftRight)) = lexer.peek() {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_addsub(lexer)?;
        lhs = UntypedExpr(match token {
            Token::ShiftRight => Expr::Op(Ops::BitShiftRight(Box::new(lhs), Box::new(rhs))),
            Token::ShiftLeft => Expr::Op(Ops::BitShiftLeft(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!(),
        })
    }
    Ok(lhs)
}

fn parse_addsub(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    let mut lhs = parse_muldivmod(lexer)?;
    while let Some(Ok(Token::Plus | Token::Minus)) = lexer.peek() {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_muldivmod(lexer)?;
        lhs = UntypedExpr(match token {
            Token::Plus => Expr::Op(Ops::Plus(Box::new(lhs), Box::new(rhs))),
            Token::Minus => Expr::Op(Ops::Minus(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!(),
        })
    }
    Ok(lhs)
}

fn parse_muldivmod(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    let mut lhs = parse_prefix(lexer)?;
    while let Some(Ok(Token::Star | Token::Slash | Token::Percent)) = lexer.peek() {
        let token = lexer.next().unwrap().unwrap();
        let rhs = parse_prefix(lexer)?;
        lhs = UntypedExpr(match token {
            Token::Star => Expr::Op(Ops::Mul(Box::new(lhs), Box::new(rhs))),
            Token::Slash => Expr::Op(Ops::Div(Box::new(lhs), Box::new(rhs))),
            Token::Percent => Expr::Op(Ops::Mod(Box::new(lhs), Box::new(rhs))),
            _ => unreachable!(),
        })
    }
    Ok(lhs)
}

fn parse_prefix(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    match lexer.peek() {
        Some(Ok(Token::ExclamationPoint)) => {
            lexer.next();
            Ok(UntypedExpr(Expr::Op(Ops::Not(Box::new(parse_prefix(
                lexer,
            )?)))))
        }
        Some(Ok(Token::Tilde)) => {
            lexer.next();
            Ok(UntypedExpr(Expr::Op(Ops::BitNot(Box::new(parse_prefix(
                lexer,
            )?)))))
        }
        Some(Ok(Token::Plus)) => {
            lexer.next();
            Ok(UntypedExpr(Expr::Op(Ops::Pos(Box::new(parse_prefix(
                lexer,
            )?)))))
        }
        Some(Ok(Token::Minus)) => {
            lexer.next();
            Ok(UntypedExpr(Expr::Op(Ops::Neg(Box::new(parse_prefix(
                lexer,
            )?)))))
        }
        _ => Ok(after_ops(lexer)?),
    }
}

fn after_ops(lexer: &mut PeekableLexer<Token>) -> ParseResult<UntypedExpr> {
    match (lexer.next(), lexer.peek()) {
        (Some(Ok(Token::LParen)), Some(Ok(Token::RParen))) => { lexer. next(); Ok(UntypedExpr(Expr::Datum(Datum::Unit))) },
        (Some(Ok(Token::LParen)), _) => {
            let expr = parse_expr(lexer)?;
            assert_token(lexer, Token::RParen, ")")?;
            Ok(expr)
        }
        (Some(Ok(Token::Identifier(f))), Some(Ok(Token::LParen))) => parse_builtin(f, lexer),
        (Some(Ok(Token::Identifier(x))), _) => Ok(UntypedExpr(Expr::Identifier(x))),
        (Some(Ok(Token::Int(x))), _) => Ok(UntypedExpr(Expr::Datum(Datum::Int(x)))),
        (Some(Ok(Token::Bool(x))), _) => Ok(UntypedExpr(Expr::Datum(Datum::Bool(x)))),
        (Some(Ok(Token::Char(x))), _) => Ok(UntypedExpr(Expr::Datum(Datum::Char(x)))),
        (None, _) => unexpected_eof(lexer),
        (Some(Err(_)), _) => unrecognized_token(lexer),
        (Some(Ok(t)), _) => unexpected_token(lexer, t),
    }
}

fn assert_token(lexer: &mut PeekableLexer<Token>, token: Token, s: &str) -> ParseResult<()> {
    match lexer.next() {
        Some(Ok(t)) if t == token => Ok(()),
        Some(Err(_)) => unrecognized_token(lexer),
        _ => Err(("Expected ".to_owned() + s, lexer.span())),
    }
}

fn optional_token(lexer: &mut PeekableLexer<Token>, token: Token) {
    if *lexer.peek() == Some(Ok(token)) {
        lexer.next();
    }
}
