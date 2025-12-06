use crate::types::{TypedCodeBlock, TypedExpr, TypedProgram, TypedStatement};
use crate::{bool, datum, ident, int, match_op, op};

pub fn fold(program: TypedProgram) -> TypedProgram {
    program.into_iter().map(|s| fold_statement(s)).collect()
}

fn fold_codeblock(code_block: TypedCodeBlock) -> TypedCodeBlock {
    code_block.into_iter().map(|s| fold_statement(s)).collect()
}

fn fold_statement(s: TypedStatement) -> TypedStatement {
    match s {
        TypedStatement::Expr(e) => TypedStatement::Expr(fold_expr(e)),
        TypedStatement::If(e, cb, v, el) => TypedStatement::If(
            fold_expr(e),
            fold_codeblock(cb),
            v.into_iter()
                .map(|(ex, cbb)| (fold_expr(ex), fold_codeblock(cbb)))
                .collect(),
            el.map(fold_codeblock),
        ),
        TypedStatement::Let(s, e) => TypedStatement::Let(s, e),
        TypedStatement::Assignment(s, e) => TypedStatement::Assignment(s, e),
        TypedStatement::While(e, cb) => TypedStatement::While(fold_expr(e), fold_codeblock(cb)),
        TypedStatement::Break(_) | TypedStatement::Continue => s,
    }
}

fn fold_expr(expr: TypedExpr) -> TypedExpr {
    match expr {
        match_op!(Int, Neg, ex) => {
            let e = fold_expr(*ex);
            match e {
                int!(v) => int!(v.wrapping_neg()),
                _ => op!(Int, Neg, e),
            }
        }
        match_op!(Bool, Not, ex) => {
            let e = fold_expr(*ex);
            match e {
                bool!(v) => bool!(!v),
                _ => op!(Bool, Not, e),
            }
        }

        match_op!(Int, BitNot, ex) => {
            let e = fold_expr(*ex);
            match e {
                int!(v) => int!(!v),
                _ => op!(Int, BitNot, e),
            }
        }

        match_op!(Int, Pos, e) => fold_expr(*e),
        match_op!(Bool, Or, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (bool!(true), _) | (bool!(false), bool!(true)) => bool!(true),
                (bool!(false), bool!(false)) => bool!(true),
                (e1, e2) => op!(Bool, Or, e1, e2),
            }
        }

        match_op!(Bool, And, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (bool!(false), _) | (bool!(true), bool!(false)) => bool!(false),
                (bool!(true), bool!(true)) => bool!(true),
                (e1, e2) => op!(Bool, And, e1, e2),
            }
        }

        match_op!(Bool, Eq, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (datum!(d1), datum!(d2)) => bool!(d1 == d2),
                (ident!(s1), ident!(s2)) if s1 == s2 => bool!(true),
                (e1, e2) => op!(Bool, Eq, e1, e2),
            }
        }

        match_op!(Bool, Neq, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (datum!(d1), datum!(d2)) => bool!(d1 != d2),
                (ident!(s1), ident!(s2)) if s1 == s2 => bool!(false),
                (e1, e2) => op!(Bool, Neq, e1, e2),
            }
        }

        match_op!(Bool, Lt, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => bool!(d1 < d2),
                (ident!(s1), ident!(s2)) if s1 == s2 => bool!(false),
                (e1, e2) => op!(Bool, Lt, e1, e2),
            }
        }

        match_op!(Bool, Gt, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => bool!(d1 > d2),
                (ident!(s1), ident!(s2)) if s1 == s2 => bool!(false),
                (e1, e2) => op!(Bool, Gt, e1, e2),
            }
        }

        match_op!(Bool, Leq, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => bool!(d1 <= d2),
                (ident!(s1), ident!(s2)) if s1 == s2 => bool!(true),
                (e1, e2) => op!(Bool, Leq, e1, e2),
            }
        }

        match_op!(Bool, Geq, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => bool!(d1 >= d2),
                (ident!(s1), ident!(s2)) if s1 == s2 => bool!(true),
                (e1, e2) => op!(Bool, Geq, e1, e2),
            }
        }

        match_op!(Int, Plus, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (e, int!(0)) | (int!(0), e) => e,
                (int!(d1), int!(d2)) => int!(d1.wrapping_add(d2)),
                (e1, e2) => op!(Int, Plus, e1, e2),
            }
        }

        match_op!(Int, Minus, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (e, int!(0)) => e,
                (int!(0), e) => op!(Int, Neg, e),
                (int!(d1), int!(d2)) => int!(d1.wrapping_sub(d2)),
                (e1, e2) => op!(Int, Minus, e1, e2),
            }
        }

        match_op!(Int, Mul, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => int!(d1.wrapping_mul(d2)),
                (int!(0), ident!(_)) | (ident!(_), int!(0)) => int!(0),
                (int!(-1), e) | (e, int!(-1)) => op!(Int, Neg, e),
                (int!(1), e) | (e, int!(1)) => e,
                (e, int!(d)) | (int!(d), e) if d.count_ones() == 1 && d < 0 => {
                    op!(Int, BitShiftLeft, e, int!(d.abs().ilog2() as i64))
                }
                (e, int!(d)) | (int!(d), e) if d.count_ones() == 1 && d > 0 => op!(
                    Int,
                    Neg,
                    op!(Int, BitShiftLeft, e, int!(d.abs().ilog2() as i64))
                ),
                (e1, e2) => op!(Int, Mul, e1, e2),
            }
        }

        match_op!(Int, Div, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) if d2 != 0 => int!(d1.wrapping_div(d2)),
                (e, int!(-1)) => op!(Int, Neg, e),
                (e, int!(1)) => e,
                (e1, e2) => op!(Int, Div, e1, e2),
            }
        }

        match_op!(Int, Mod, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) if d2 != 0 => int!(d1 % d2),
                (ident!(_), int!(1) | int!(-1)) => int!(0),
                (e1, e2) => op!(Int, Mod, e1, e2),
            }
        }

        match_op!(Int, BitAnd, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => int!(d1 & d2),
                (ident!(_), int!(0)) => int!(0),
                (e, int!(-1)) => e,
                (e1, e2) => op!(Int, BitAnd, e1, e2),
            }
        }

        match_op!(Int, BitOr, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => int!(d1 | d2),
                (ident!(_), int!(-1)) => int!(-1),
                (e, int!(-1)) => e,
                (e1, e2) => op!(Int, BitOr, e1, e2),
            }
        }

        match_op!(Int, BitXor, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => int!(d1 ^ d2),
                (ident!(x), ident!(y)) if x == y => int!(0),
                (e, int!(0)) => e,
                (e, int!(-1)) => op!(Int, Neg, e),
                (e1, e2) => op!(Int, BitXor, e1, e2),
            }
        }

        match_op!(Int, BitShiftRight, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => int!(d1 >> d2),
                (e, int!(0)) => e,
                (e1, e2) => op!(Int, BitShiftRight, e1, e2),
            }
        }

        match_op!(Int, BitShiftLeft, e1, e2) => {
            let e1 = fold_expr(*e1);
            let e2 = fold_expr(*e2);
            match (e1, e2) {
                (int!(d1), int!(d2)) => int!(d1 << d2),
                (e, int!(0)) => e,
                (e1, e2) => op!(Int, BitShiftLeft, e1, e2),
            }
        }

        _ => expr,
    }
}
