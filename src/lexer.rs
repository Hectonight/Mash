use std::mem::replace;
use logos::{Lexer, Logos, Span};


pub struct PeekableLexer<'a, T: Logos<'a>> {
    lexer: Lexer<'a, T>,
    peeker1: Option<Result<T, T::Error>>,
    peeker2: Option<Result<T, T::Error>>,
    span1: Span,
    span2: Span,
}

impl<'a, T: Logos<'a>> PeekableLexer<'a, T> {
    pub fn new(mut lexer: Lexer<'a, T>) -> Self {
        let span1 = lexer.span();
        let peeker1 = lexer.next();
        let span2 = lexer.span();
        let peeker2 = lexer.next();
        Self { lexer, peeker1, peeker2, span1, span2 }
    }

    pub fn next(&mut self) -> Option<Result<T, T::Error>> {
        let peeked = self.peeker1.take();
        self.peeker1 = self.peeker2.take();
        self.peeker2 = self.lexer.next();

        self.span1 = replace(&mut self.span2, self.lexer.span());
        peeked
    }
    pub fn span(&self) -> Span {
        self.span1.clone()
    }

    pub fn peek(&self) -> &Option<Result<T, T::Error>> {
        &self.peeker1
    }

    pub fn peek2(&self) -> &Option<Result<T, T::Error>> {
        &self.peeker2
    }

}


#[derive(Logos, Debug, PartialEq, Eq, Clone)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
pub enum Token {

    #[regex(r"\d+", |lex| lex.slice().parse::<i64>().unwrap())]
    #[regex(r"0[xX][\da-fA-F]+", |lex| i64::from_str_radix(&lex.slice()[2..], 16).unwrap())]
    #[regex(r"0[bB][01]+", |lex| i64::from_str_radix(&lex.slice()[2..], 2).unwrap())]
    #[regex(r"0[oO][0-7]+", |lex| i64::from_str_radix(&lex.slice()[2..], 8).unwrap())]
    Int(i64),

    // #[regex(r"'([^'\\\x00\t\xa0\xd0]|\\x[0-7][\da-fA-F]|\\[\\nrt0]|\\u\{[\da-fA-F]{1,6}\})'", |lex|
    // {
    // dbg!(&lex.slice());
    // lex.slice()[1..lex.span().end - lex.span().start - 1].parse::<char>().unwrap()
    // }
    // )]
    // Char(char),

    #[regex(r"'[^'\\\x00\t\xa0\xd0]'", |lex| lex.slice()[1..2].parse::<char>().unwrap())]
    #[token("'\\n'", |_| '\n')]
    #[token("'\\0'", |_| '\0')]
    #[token("'\\t'", |_| '\t')]
    #[token("'\\\\'", |_| '\\')]
    #[token("'\\r'", |_| '\r')]
    #[token("'\\''", |_| '\'')]
    // #[regex(r"'\\u\{[\da-fA-F]{1,6}\}'", |lex| char::from_u32(lex.slice()[3..lex.span().end - lex.span().start - 2].parse::<u32>().unwrap()))]
    Char(char),
    // String(String),

    #[token("print")]
    Print,

    #[token("null")]
    Null,

    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Bool(bool),

    #[regex(r"[a-zA-Z_]\w*", |lex| lex.slice().to_owned())]
    Identifier(String),

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("=")]
    Equal,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("[")]
    LBrack,

    #[token("]")]
    RBrack,

    #[token("{")]
    LCurly,

    #[token("}")]
    RCurly,

    #[token("?")]
    QuestionMark,

    #[token("&")]
    Ampersand,

    #[token("|")]
    VBar,

    #[token("!")]
    ExclamationPoint,

    #[token("~")]
    Tilde,

    #[token("^")]
    Carrot,

    #[token("%")]
    Percent,

    #[token("<")]
    Less,

    #[token(">")]
    Greater,

    #[token(",")]
    Comma,

    #[token(".")]
    Period,

    #[token("\"")]
    DoubleQuote,

    #[token("'")]
    Quote,

    #[token("==")]
    DoubleEqual,

    #[token("!=")]
    NotEqual,

    #[token("++")]
    PlusPlus,

    #[token("--")]
    MinusMinus,

    #[token("<=")]
    LessEqual,

    #[token(">=")]
    GreaterEqual,

    #[token("<<")]
    ShiftLeft,

    #[token(">>")]
    ShiftRight,

    #[token("+=")]
    PlusEqual,

    #[token("-=")]
    MinusEqual,

    #[token("*=")]
    StarEqual,

    #[token("/=")]
    SlashEqual,

    #[token("%=")]
    PercentEqual,

    #[token("&=")]
    AmpersandEqual,

    #[token("|=")]
    VBarEqual,

    #[token("^=")]
    CarrotEqual,

    #[token("&&")]
    And,

    #[token("||")]
    Or,

    #[token("<<=")]
    ShiftLeftEqual,

    #[token(">>=")]
    ShiftRightEqual,

    #[token(";")]
    Semicolon,

    #[token(":")]
    Colon,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("let")]
    Let,

    #[token("for")]
    For,

    #[token("while")]
    While,

    #[token("break")]
    Break,

    #[token("return")]
    Return,

    #[token("fun")]
    Function


}