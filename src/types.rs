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
| while Expr Codeblock [[ ; ]]
| break ;
| continue ;

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
| Or

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



Future Optimizations

a [+-*|&^] b => c
[+-!~] a => b
a || b => c
a && b => c
true || x => true
false && x => false
x / 1 => x
x / -1 => - x
x * 1 or 1 * x => x
x + 0 or 0 + x => x
x - 0 => x
0 - x => - x
x ^ x => 0
x & -1 or -1 & x => x
x ^ -1 or -1 ^ x => ~ x
x | 0 or 0 | x => x
x & 0 or 0 & x => 0
x | -1 or -1 | x => -1
x | ~x or ~x | x => -1
x << 0 => 0
x >> 0 => 0
x / 2**n => x >> n
x * 2**n => x << n
x * -1 or -1 * x => - x
~ x + 1 or 1 + ~ x => - x

x = x => delete (after parsing += and similar operators do not exist)
a [%/] b_nonzero => c
if true { s* } else if ... else ... => s*
if false { ... } else if ... else if true { s* } else if ... else ... => s*
if false { ... } else if false { ... } ... else { s* } => s*
{ ...; break n?; ... } => { ...; break n?; }
{ ...; continue; ... } => { ...; continue; }
while false { ... } => delete

Be careful on optimizing asm because of flags, to optimize add and others
I need to make some rules when writing asm or require more complex logic than simple rules
Semicolons denote new lines

push reg; pop X => mov X, reg
push X; pop reg => mov reg, X
mov X, X => delete (this should not ever be written anyway)
cmovcc X, X => delete (this should not ever be written anyway)

move extern to start and remove duplicates (assume this for future optimizations)

jmp X; ... ; Label Y => jmp X; Label Y
... is not Label and Extern (we know it's not extern due to previous optimizations)

add case for while true which simplifies the code significantly (only if no break statement in while)
add cases in conditionals no reason i need to actually return 1 or 0 I can just cmp X, Y for x == y
 */
use std::fmt::{Display, Formatter};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub};

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expr),
    If(Expr, CodeBlock, Vec<(Expr, CodeBlock)>, Option<CodeBlock>),
    Print(Expr),
    Let(String, Expr),
    Assignment(String, Expr),
    While(Expr, CodeBlock),
    Break(usize),
    Continue,
}

#[derive(Debug, Clone)]
pub enum TypedStatement {
    Expr(TypedExpr),
    If(
        TypedExpr,
        TypedCodeBlock,
        Vec<(TypedExpr, TypedCodeBlock)>,
        Option<TypedCodeBlock>,
    ),
    Print(TypedExpr),
    Let(String, TypedExpr),
    Assignment(String, TypedExpr),
    While(TypedExpr, TypedCodeBlock),
    Break(usize),
    Continue,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Type {
    Int,
    Bool,
    Char,
    Null,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Int => "int",
                Type::Bool => "bool",
                Type::Null => "null",
                Type::Char => "char",
            }
        )
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
    Op(Ops),
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
    Char(char),
    Null,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Char(char),
    Null,
}

pub(crate) type ResultValue = Result<Value, String>;
pub(crate) type ResultUnit = Result<(), String>;

impl Add for Value {
    type Output = ResultValue;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            _ => Err("Type Error".to_owned()),
        }
    }
}

impl Sub for Value {
    type Output = ResultValue;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            _ => Err("Type Error".to_owned()),
        }
    }
}

impl Mul for Value {
    type Output = ResultValue;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            _ => Err("Type Error".to_owned()),
        }
    }
}

impl Div for Value {
    type Output = ResultValue;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a / b)),
            _ => Err("Type Error".to_owned()),
        }
    }
}

impl Not for Value {
    type Output = ResultValue;
    fn not(self) -> Self::Output {
        match self {
            Value::Int(a) => Ok(Value::Int(!a)),
            Value::Bool(a) => Ok(Value::Bool(!a)),
            _ => Err("Type Error".to_owned()),
        }
    }
}

impl Neg for Value {
    type Output = ResultValue;
    fn neg(self) -> Self::Output {
        match self {
            Value::Int(a) => Ok(Value::Int(-a)),
            _ => Err("Type Error".to_owned()),
        }
    }
}

impl BitOr for Value {
    type Output = ResultValue;
    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
            _ => Err("Type Error".to_owned()),
        }
    }
}

impl BitXor for Value {
    type Output = ResultValue;
    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
            _ => Err("Type Error".to_owned()),
        }
    }
}

impl BitAnd for Value {
    type Output = ResultValue;
    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a & b)),
            _ => Err("Type Error".to_owned()),
        }
    }
}

impl Rem for Value {
    type Output = ResultValue;
    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
            _ => Err("Type Error".to_owned()),
        }
    }
}

impl Shl for Value {
    type Output = ResultValue;

    fn shl(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a << b)),
            _ => Err("Type Error".to_owned()),
        }
    }
}

impl Shr for Value {
    type Output = ResultValue;

    fn shr(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a >> b)),
            _ => Err("Type Error".to_owned()),
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
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
            _ => Err("Type Error".to_owned()),
        }
    }

    pub(crate) fn less_eq(self, rhs: Self) -> ResultValue {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
            _ => Err("Type Error".to_owned()),
        }
    }

    pub(crate) fn greater(self, rhs: Self) -> ResultValue {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
            _ => Err("Type Error".to_owned()),
        }
    }

    pub(crate) fn greater_eq(self, rhs: Self) -> ResultValue {
        match (self, rhs) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
            _ => Err("Type Error".to_owned()),
        }
    }

    pub(crate) fn and(self, rhs: Self) -> ResultValue {
        match (self, rhs) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
            _ => Err("Type Error".to_owned()),
        }
    }

    pub(crate) fn or(self, rhs: Self) -> ResultValue {
        match (self, rhs) {
            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
            _ => Err("Type Error".to_owned()),
        }
    }
}
