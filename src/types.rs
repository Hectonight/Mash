/*

Program = Statement*

Statement =
| Expr ;
| print Expr ;

Expr =
| ( Expr )
| Datum

Datum =
| Bool
| Int
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
