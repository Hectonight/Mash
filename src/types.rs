/*
x* Implies 0 or more
[[ x ]] Implies optional
[[ x ]]* Implies 0 or more for entire group


Program = Statement*

Statement =
| Expr ;
| print Expr ;
| let Id = Expr ;
| if Expr CodeBlock [[ else if CodeBlock ]]* [[ else CodeBlock ]] [[ ; ]]
| Id Assignment Expr ;

CodeBlock =
| { Statement* }


Assignment =
| =
| +=
| -=
| |=
| *=
| /=
| ^=
| &=
| %=
| <<=
| >>=

Datum =
| Bool
| Int
| Null

Expr =
| Ternary

Ternary =
| Ternary ? Ternary : Ternary

Or =
| Or || Or
| And

And =
| And && And
| BitOr

BitOr =
| BitOr | BitOr
| BitXor

BitXor =
| BitXor ^ BitXor
| BitAnd

BitAnd =
| BitAnd & BitAnd
| Equality

Equality =
| Equality == Equality
| Equality != Equality
| Relational

Relational =
| Relational < Relational
| Relational > Relational
| Relational <= Relational
| Relational >= Relational
| BitShift

BitShift =
| BitShift << BitShift
| BitShift >> BitShift

AddSub =
| AddSub + AddSub
| AddSub - AddSub

MultDivMod =
| MultDivMod * MultDivMod
| MultDivMod / MultDivMod
| MultDivMod % MultDivMod
| Prefix

Prefix =
| ~ Prefix
| + Prefix
| - Prefix
| ! Prefix
| AfterOps

AfterOps =
| ( Expr )
| Datum
| Id

 */
use std::cmp::Ordering;
use crate::lexer::Token;

pub type CodeBlock = Vec<Statement>;
pub type Program = CodeBlock;


#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Expr(Expr),
    If(Expr, Box<CodeBlock>, Option<Box<CodeBlock>>),
    Print(Expr)
}

// All recursive Expr will be in a box
#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Datum(Datum),
    Identifier(String),
    Op(Ops)
}

#[derive(Debug, PartialEq, Eq)]
pub enum Ops {
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    BitNot(Box<Expr>),
    Neg(Box<Expr>),
    Pos(Box<Expr>),
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Neq(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Leq(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Geq(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    BitAnd(Box<Expr>, Box<Expr>),
    BitOr(Box<Expr>, Box<Expr>),
    BitXor(Box<Expr>, Box<Expr>),
    BitShiftLeft(Box<Expr>, Box<Expr>),
    BitShiftRight(Box<Expr>, Box<Expr>),
}


#[derive(Debug, PartialEq, Eq)]
pub enum Datum {
    Int(i64),
    Bool(bool),
    Null
}


#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Null,
    Void
}


// impl PartialOrd for Tokens {
//     fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//         // match  {  }
//     }
// }
