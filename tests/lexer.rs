use logos::Logos;
use mash::lexer::Token;

#[test]
fn let_tokens() {
    let input = "let x = 42;";
    let lexer = Token::lexer(input);

    let expected_tokens = vec![
        Token::Let,
        Token::Identifier("x".into()),
        Token::Equal,
        Token::Int(42),
        Token::Semicolon,
    ];

    let tokens: Vec<Token> = lexer
        .into_iter()
        .map(|r| r.expect("Lexer returned an error"))
        .collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn if_tokens() {
    let input = "if { ; 2 222 true false ;";
    let lexer = Token::lexer(input);

    let expected_tokens = vec![
        Token::If,
        Token::LCurly,
        Token::Semicolon,
        Token::Int(2),
        Token::Int(222),
        Token::Bool(true),
        Token::Bool(false),
        Token::Semicolon,
    ];

    let tokens: Vec<Token> = lexer
        .into_iter()
        .map(|r| r.expect("Lexer returned an error"))
        .collect();

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn no_tokens() {
    let input = "";
    let lexer = Token::lexer(input);

    let expected_tokens = vec![];

    let tokens: Vec<Token> = lexer
        .into_iter()
        .map(|r| r.expect("Lexer returned an error"))
        .collect();

    assert_eq!(tokens, expected_tokens);
}