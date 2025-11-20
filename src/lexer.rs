use logos::{Lexer, Logos, Span};


pub struct PeekableLexer<'a, T: Logos<'a>> {
    lexer: Lexer<'a, T>,
    peeker: Option<Result<T, T::Error>>
}

impl<'a, T: Logos<'a>> PeekableLexer<'a, T> {
    pub fn new(mut lexer: Lexer<'a, T>) -> Self {
        let peeker = lexer.next();
        Self { lexer, peeker }
    }

    pub fn next(&mut self) -> Option<Result<T, T::Error>> {
        let peeked = self.peeker.take();
        self.peeker = self.lexer.next();
        peeked
    }
    pub fn span(&self) -> Span {
        self.lexer.span()
    }

    pub fn peek(&self) -> &Option<Result<T, T::Error>> {
        &self.peeker
    }
}




#[derive(Logos, Debug, PartialEq, Eq, Clone)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
pub enum Token {

    #[regex(r"\d+", |lex| lex.slice().parse::<i64>().unwrap())]
    Int(i64),

    // Char(char),
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