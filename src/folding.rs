use crate::types::{
    Datum, Expr, Ops, TypedCodeBlock as CodeBlock, TypedExpr, TypedProgram as Program,
    TypedStatement as Statement,
};
use crate::{bool, datum, ident, int};

pub fn fold(program: Program) -> Program {
    program.into_iter().map(|s| fold_statement(s)).collect()
}

fn fold_codeblock(code_block: CodeBlock) -> CodeBlock {
    code_block.into_iter().map(|s| fold_statement(s)).collect()
}

fn fold_statement(s: Statement) -> Statement {
    match s {
        Statement::Expr(e) => Statement::Expr(fold_texpr(e)),
        Statement::If(e, cb, v, el) => Statement::If(
            fold_texpr(e),
            fold_codeblock(cb),
            v.into_iter()
                .map(|(ex, cbb)| (fold_texpr(ex), fold_codeblock(cbb)))
                .collect(),
            el.map(fold_codeblock),
        ),
        Statement::Print(e) => Statement::Print(fold_texpr(e)),
        Statement::Let(s, e) => Statement::Let(s, e),
        Statement::Assignment(s, e) => Statement::Assignment(s, e),
        Statement::While(e, cb) => Statement::While(fold_texpr(e), fold_codeblock(cb)),
        Statement::Break(_) | Statement::Continue => s,
    }
}

fn fold_texpr(e: TypedExpr) -> TypedExpr {
    (fold_expr(e.0), e.1)
}

fn fold_expr(expr: Expr) -> Expr {
    match expr {
        Expr::Op(Ops::Neg(ex)) => {
            let e = fold_expr(*ex);
            match e {
                int!(v) => int!(v.wrapping_neg()),
                _ => Expr::Op(Ops::Neg(Box::new(e))),
            }
        }
        Expr::Op(Ops::Not(ex)) => {
            let e = fold_expr(*ex);
            match e {
                bool!(v) => bool!(!v),
                _ => Expr::Op(Ops::Not(Box::new(e))),
            }
        }

        Expr::Op(Ops::BitNot(ex)) => {
            let e = fold_expr(*ex);
            match e {
                int!(v) => int!(!v),
                _ => Expr::Op(Ops::BitNot(Box::new(e))),
            }
        }

        Expr::Op(Ops::Pos(e)) => fold_expr(*e),
        Expr::Op(Ops::Or(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (bool!(true), _) | (bool!(false), bool!(true)) => bool!(true),
                (bool!(false), bool!(false)) => bool!(true),
                (e1, e2) => Expr::Op(Ops::Or(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::And(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (bool!(false), _) | (bool!(true), bool!(false)) => bool!(false),
                (bool!(true), bool!(true)) => bool!(true),
                (e1, e2) => Expr::Op(Ops::Or(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::Eq(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (datum!(d1), datum!(d2)) => bool!(d1 == d2),
                (ident!(s1), ident!(s2)) if s1 == s2 => bool!(true),
                (e1, e2) => Expr::Op(Ops::Eq(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::Neq(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (datum!(d1), datum!(d2)) => bool!(d1 != d2),
                (ident!(s1), ident!(s2)) if s1 == s2 => bool!(false),
                (e1, e2) => Expr::Op(Ops::Neq(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::Lt(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => Expr::Datum(Datum::Bool(d1 < d2)),
                (ident!(s1), ident!(s2)) if s1 == s2 => bool!(false),
                (e1, e2) => Expr::Op(Ops::Lt(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::Gt(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => Expr::Datum(Datum::Bool(d1 > d2)),
                (ident!(s1), ident!(s2)) if s1 == s2 => bool!(false),
                (e1, e2) => Expr::Op(Ops::Gt(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::Leq(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => Expr::Datum(Datum::Bool(d1 <= d2)),
                (ident!(s1), ident!(s2)) if s1 == s2 => bool!(true),
                (e1, e2) => Expr::Op(Ops::Leq(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::Geq(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => Expr::Datum(Datum::Bool(d1 >= d2)),
                (Expr::Identifier(s1), Expr::Identifier(s2)) if s1 == s2 => bool!(true),
                (e1, e2) => Expr::Op(Ops::Geq(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::Plus(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (e, int!(0)) | (int!(0), e) => e,
                (int!(d1), int!(d2)) => int!(d1.wrapping_add(d2)),
                (e1, e2) => Expr::Op(Ops::Plus(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::Minus(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (e, int!(0)) => e,
                (int!(0), e) => Expr::Op(Ops::Neg(Box::new(e))),
                (int!(d1), int!(d2)) => int!(d1.wrapping_sub(d2)),
                (e1, e2) => Expr::Op(Ops::Minus(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::Mul(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => int!(d1.wrapping_mul(d2)),
                (int!(0), ident!(_)) | (ident!(_), int!(0)) => int!(0),
                (int!(-1), e) | (e, int!(-1)) => Expr::Op(Ops::Neg(Box::new(e))),
                (int!(1), e) | (e, int!(1)) => e,
                (e, int!(d)) | (int!(d), e) if d.count_ones() == 1 && d < 0 => Expr::Op(
                    Ops::BitShiftLeft(Box::new(e), Box::new(int!(d.abs().ilog2() as i64))),
                ),
                (e, int!(d)) | (int!(d), e) if d.count_ones() == 1 && d > 0 => Expr::Op(
                    Ops::Neg(Box::new(Expr::Op(Ops::BitShiftLeft(Box::new(e), Box::new(int!(d.abs().ilog2() as i64)))))),
                ),
                (e1, e2) => Expr::Op(Ops::Mul(Box::new(e1), Box::new(e2))),
            }
        }


        Expr::Op(Ops::Div(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) if d2 != 0 => int!(d1.wrapping_div(d2)),
                (e, int!(-1)) => Expr::Op(Ops::Neg(Box::new(e))),
                (e, int!(1)) => e,
                (e1, e2) => Expr::Op(Ops::Div(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::Mod(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) if d2 != 0 => int!(d1 % d2),
                (ident!(_) , int!(1) | int!(-1)) => int!(0),
                (e1, e2) => Expr::Op(Ops::Mod(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::BitAnd(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => int!(d1 & d2),
                (ident!(_) , int!(0)) => int!(0),
                (e, int!(-1)) => e,
                (e1, e2) => Expr::Op(Ops::BitAnd(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::BitOr(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => int!(d1 | d2),
                (ident!(_) , int!(-1)) => int!(-1),
                (e, int!(-1)) => e,
                (e1, e2) => Expr::Op(Ops::BitOr(Box::new(e1), Box::new(e2))),
            }
        }


        Expr::Op(Ops::BitXor(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => int!(d1 ^ d2),
                (ident!(x) , ident!(y)) if x == y => int!(0),
                (e, int!(0)) => e,
                (e, int!(-1)) => Expr::Op(Ops::Neg(Box::new(e))),
                (e1, e2) => Expr::Op(Ops::BitXor(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::BitShiftRight(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => int!(d1 >> d2),
                (e, int!(0)) => e,
                (e1, e2) => Expr::Op(Ops::BitShiftRight(Box::new(e1), Box::new(e2))),
            }
        }

        Expr::Op(Ops::BitShiftLeft(e1, e2)) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => int!(d1 << d2),
                (e, int!(0)) => e,
                (e1, e2) => Expr::Op(Ops::BitShiftLeft(Box::new(e1), Box::new(e2))),
            }
        }

        _ => expr,
    }
}
