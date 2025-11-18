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
use std::fmt::{Display, Formatter};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub};
use crate::lexer::Token;



#[derive(Debug)]
pub enum Statement {
    Expr(Expr),
    If(Expr, CodeBlock, Vec<(Expr, CodeBlock)>, Option<CodeBlock>),
    Print(Expr),
    Let(String, Expr)
}

#[derive(Debug)]
pub enum TypedStatement {
    Expr(TypedExpr),
    If(TypedExpr, TypedCodeBlock, Vec<(TypedExpr, TypedCodeBlock)>, Option<TypedCodeBlock>),
    Print(TypedExpr),
    Let(String, TypedExpr)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Type {
    Int,
    Bool,
    Null,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Type::Int => "int",
            Type::Bool => "bool",
            Type::Null => "null"
        })
    }
}

pub type Program = CodeBlock;
pub type TypedProgram = TypedCodeBlock;
pub type CodeBlock = Vec<Statement>;
pub type TypedCodeBlock = Vec<TypedStatement>;
pub type TypedExpr = (Expr, Type);


// All recursive Expr will be in a box
#[derive(Debug, Clone)]
pub enum Expr {
    Datum(Datum),
    Identifier(String),
    Op(Ops)
}

#[derive(Debug, Clone)]
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


#[derive(Debug, PartialEq, Eq, Clone)]
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

pub(crate) type ResultValue = Result<Value, String>;
pub(crate) type ResultUnit = Result<(), String>;

impl Add for Value {
    type Output = ResultValue;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) =>  Ok(Value::Int(a + b)),
            _ => Err("Type Error".to_owned())
        }
    }
}

impl Sub for Value {
    type Output = ResultValue;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) =>  Ok(Value::Int(a - b)),
            _ => Err("Type Error".to_owned())
        }
    }
}


impl Mul for Value {
    type Output = ResultValue;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) =>  Ok(Value::Int(a * b)),
            _ => Err("Type Error".to_owned())
        }
    }
}

impl Div for Value {
    type Output = ResultValue;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) =>  Ok(Value::Int(a / b)),
            _ => Err("Type Error".to_owned())
        }
    }
}

impl Not for Value {
    type Output = ResultValue;
    fn not(self) -> Self::Output {
        match self {
            Value::Int(a) =>  Ok(Value::Int(!a)),
            Value::Bool(a) =>  Ok(Value::Bool(!a)),
            _ => Err("Type Error".to_owned())
        }
    }
}

impl Neg for Value {
    type Output = ResultValue;
    fn neg(self) -> Self::Output {
        match self {
            Value::Int(a) => Ok(Value::Int(-a)),
            _ => Err("Type Error".to_owned())
        }
    }
}

impl BitOr for Value {
    type Output = ResultValue;
    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) =>  Ok(Value::Int(a | b)),
            _ => Err("Type Error".to_owned())
        }
    }
}

impl BitXor for Value {
    type Output = ResultValue;
    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) =>  Ok(Value::Int(a ^ b)),
            _ => Err("Type Error".to_owned())
        }
    }
}

impl BitAnd for Value {
    type Output = ResultValue;
    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) =>  Ok(Value::Int(a & b)),
            _ => Err("Type Error".to_owned())
        }
    }
}

impl Rem for Value {
    type Output = ResultValue;
    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) =>  Ok(Value::Int(a % b)),
            _ => Err("Type Error".to_owned())
        }
    }
}

impl Shl for Value {
    type Output = ResultValue;

    fn shl(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) =>  Ok(Value::Int(a << b)),
            _ => Err("Type Error".to_owned())
        }
    }
}

impl Shr for Value {
    type Output = ResultValue;

    fn shr(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) =>  Ok(Value::Int(a >> b)),
            _ => Err("Type Error".to_owned())
        }
    }
}

impl Value {
    pub(crate) fn equal(self, rhs: Self) -> ResultValue {
        Ok(Value::Bool(self == rhs))
    }

    pub(crate) fn not_equal(self, rhs: Self) -> ResultValue {
        Ok(Value::Bool(self != rhs))
    }

    pub(crate) fn less(self, rhs: Self) -> ResultValue {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) =>  Ok(Value::Bool(a < b)),
            _ => Err("Type Error".to_owned())
        }
    }

    pub(crate) fn less_eq(self, rhs: Self) -> ResultValue {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) =>  Ok(Value::Bool(a <= b)),
            _ => Err("Type Error".to_owned())
        }
    }

    pub(crate) fn greater(self, rhs: Self) -> ResultValue {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) =>  Ok(Value::Bool(a > b)),
            _ => Err("Type Error".to_owned())
        }
    }

    pub(crate) fn greater_eq(self, rhs: Self) -> ResultValue {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) =>  Ok(Value::Bool(a >= b)),
            _ => Err("Type Error".to_owned())
        }
    }

    pub(crate) fn and(self, rhs: Self) -> ResultValue {
        match (self, rhs) {
            (Value::Bool(a), Value::Bool(b)) =>  Ok(Value::Bool(a && b)),
            _ => Err("Type Error".to_owned())
        }
    }

    pub(crate) fn or(self, rhs: Self) -> ResultValue {
        match (self, rhs) {
            (Value::Bool(a), Value::Bool(b)) =>  Ok(Value::Bool(a || b)),
            _ => Err("Type Error".to_owned())
        }
    }
}
