/*

Program = Statement*

Statement =
| Expr ;
| print Expr ;
| let Id Assignment Expr ;

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

Expr =
| ( Expr )
| Datum
| Id
| Or

Datum =
| Bool
| Int
| Null

Ternary =
| Or ? Or : Or

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

 */


pub type Program = Vec<Statement>;

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Expr(Expr),
    Print(Expr)
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Datum(Datum),
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
