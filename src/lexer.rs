use logos::Logos;



#[derive(Logos, Debug, PartialEq, Eq)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
pub enum Tokens {

    #[regex(r"[+-]?\d+", |lex| lex.slice().parse::<i64>().unwrap())]
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
    Id(String),

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("=")]
    Equals,

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