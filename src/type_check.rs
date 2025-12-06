use crate::op;
use crate::types::{
    BuiltIn, Datum, Expr, Program, Type, TypedCodeBlock, TypedExpr, TypedOps, TypedProgram,
    TypedStatement, UntypedCodeBlock, UntypedExpr, UntypedOps, UntypedStatement,
};
use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::RangeBounds;

struct TEnv {
    environment: Vec<HashMap<String, Type>>,
    control_flow: usize,
}

impl TEnv {
    fn new() -> Self {
        Self {
            environment: vec![HashMap::new()],
            control_flow: 0,
        }
    }

    fn new_environment(&mut self) {
        self.environment.push(HashMap::new());
    }

    fn pop_environment(&mut self) {
        self.environment.pop();
    }

    fn lookup(&self, key: &String) -> Option<&Type> {
        for env in self.environment.iter().rev() {
            if let Some(val) = env.get(key) {
                return Some(val);
            }
        }
        None
    }

    fn insert(&mut self, s: String, t: Type) {
        let l = self.environment.len();
        self.environment[l - 1].insert(s, t);
    }

    fn member(&self, key: &String) -> bool {
        self.environment.iter().any(|env| env.contains_key(key))
    }
}

pub fn typify(program: Program) -> Result<TypedProgram, String> {
    let mut tenv = TEnv::new();
    program
        .into_iter()
        .map(|s| typify_statement(s, &mut tenv))
        .collect()
}

fn typify_codeblock(
    codeblock: UntypedCodeBlock,
    tenv: &mut TEnv,
) -> Result<TypedCodeBlock, String> {
    tenv.new_environment();
    let tcb = codeblock
        .into_iter()
        .map(|s| typify_statement(s, tenv))
        .collect();
    tenv.pop_environment();
    tcb
}

fn typify_statement(
    statement: UntypedStatement,
    tenv: &mut TEnv,
) -> Result<TypedStatement, String> {
    match statement {
        UntypedStatement::Expr(e) => Ok(TypedStatement::Expr(typify_expr(e, tenv)?)),
        UntypedStatement::If(e, cb, elifs, eb) => {
            let s = "conditional if".to_owned();
            Ok(TypedStatement::If(
                assert_expr_type(e, Type::Bool, s.clone(), tenv)?,
                typify_codeblock(cb, tenv)?,
                elifs
                    .into_iter()
                    .map(|(ex, tcb)| {
                        Ok((
                            assert_expr_type(ex, Type::Bool, s.clone(), tenv)?,
                            typify_codeblock(tcb, tenv)?,
                        ))
                    })
                    .collect::<Result<Vec<_>, String>>()?,
                match eb {
                    Some(cb) => Some(typify_codeblock(cb, tenv)?),
                    None => None,
                },
            ))
        }
        UntypedStatement::Let(id, e) => {
            let ex = typify_expr(e, tenv)?;
            if tenv.member(&id) {
                return Err(format!("{} defined multiple times", id));
            }
            tenv.insert(id.clone(), ex.typ);
            Ok(TypedStatement::Let(id.clone(), ex))
        }
        UntypedStatement::Assignment(id, e) => {
            let right = typify_expr(e, tenv)?;
            let tleft = tenv.lookup(&id).ok_or(format!("{} is not defined", id))?;
            if *tleft != right.typ {
                Err(format!(
                    "Assignment type mismatch {} and {}",
                    *tleft, right.typ
                ))
            } else {
                Ok(TypedStatement::Assignment(id, right))
            }
        }
        UntypedStatement::While(e, cb) => {
            tenv.control_flow += 1;
            let ex = typify_expr(e, tenv)?;
            if ex.typ != Type::Bool {
                Err(format!(
                    "While type mismatch, found {} expected bool",
                    ex.typ
                ))
            } else {
                Ok(TypedStatement::While(ex, typify_codeblock(cb, tenv)?))
            }
        }
        UntypedStatement::Break(n) => {
            if tenv.control_flow >= n {
                tenv.control_flow -= n;
                Ok(TypedStatement::Break(n))
            } else {
                Err(format!(
                    "Found break of {} in depth {}",
                    n, tenv.control_flow
                ))
            }
        }
        UntypedStatement::Continue => Ok(TypedStatement::Continue),
    }
}

fn typify_expr(expr: UntypedExpr, tenv: &TEnv) -> Result<TypedExpr, String> {
    match expr.0 {
        Expr::Datum(d) => Ok(TypedExpr {
            typ: datum_type(&d),
            expr: Expr::Datum(d),
        }),
        Expr::Identifier(id) => match tenv.lookup(&id) {
            Some(t) => Ok(TypedExpr {
                typ: *t,
                expr: Expr::Identifier(id),
            }),
            None => Err(format!("Unknown identifier {}", id)),
        },
        Expr::Op(op) => op_type(op, tenv),
        Expr::BuiltIn(f, p) => builtin_type(f, p, tenv),
    }
}

fn arity_error(builtin: &BuiltIn, expected: usize, found: usize) -> Result<TypedExpr, String> {
    Err(format!(
        "{} expected {} parameter. Found {}",
        builtin, expected, found
    ))
}

fn arity_range_error<R: RangeBounds<usize> + Debug>(
    builtin: &BuiltIn,
    expected: R,
    found: usize,
) -> Result<TypedExpr, String> {
    Err(format!(
        "{} expected {:?} parameter. Found {}",
        builtin, expected, found
    ))
}

fn assert_type_builtin(builtin: &BuiltIn, expected: Type, actual: Type) -> Result<(), String> {
    if expected == actual {
        Ok(())
    } else {
        Err(format!(
            "{} expected {}. Found {}",
            builtin, expected, actual
        ))
    }
}

fn builtin_type(
    builtin: BuiltIn,
    mut params: Vec<UntypedExpr>,
    tenv: &TEnv,
) -> Result<TypedExpr, String> {
    match builtin {
        BuiltIn::Abs | BuiltIn::Sgn => {
            if params.len() != 1 {
                arity_error(&builtin, 1, params.len())
            } else {
                let e = typify_expr(params.pop().unwrap(), tenv)?;
                assert_type_builtin(&builtin, Type::Int, e.typ)?;
                Ok(TypedExpr {
                    typ: Type::Int,
                    expr: Expr::BuiltIn(builtin, vec![e]),
                })
            }
        }

        BuiltIn::Max | BuiltIn::Min => {
            if params.len() == 0 {
                arity_range_error(&builtin, 1.., 0)
            } else {
                let tparams = params
                    .into_iter()
                    .map(|p| {
                        let e = typify_expr(p, tenv)?;
                        assert_type_builtin(&builtin, Type::Int, e.typ)?;
                        Ok::<TypedExpr, String>(e)
                    })
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(TypedExpr {
                    typ: Type::Int,
                    expr: Expr::BuiltIn(builtin, tparams),
                })
            }
        }
        BuiltIn::Print => {
            if params.len() > 1 {
                arity_range_error(&builtin, 0..=1, params.len())
            } else {
                let tparams =
                    if params.len() == 0 {
                        vec![]
                    } else {
                        vec![typify_expr(params.pop().unwrap(), tenv)?]
                    };
                Ok(TypedExpr {
                    typ: Type::Unit,
                    expr: Expr::BuiltIn(builtin, tparams),
                })
            }
        }
    }
}

fn datum_type(datum: &Datum) -> Type {
    match datum {
        Datum::Int(_) => Type::Int,
        Datum::Bool(_) => Type::Bool,
        Datum::Char(_) => Type::Char,
        Datum::Unit => Type::Unit,
    }
}

fn op_type(ops: UntypedOps, tenv: &TEnv) -> Result<TypedExpr, String> {
    match ops {
        UntypedOps::Ternary(cond, a, b) => {
            let c = assert_expr_type(*cond, Type::Bool, "ternary".to_string(), tenv)?;
            let e1 = typify_expr(*a, tenv)?;
            let e2 = typify_expr(*b, tenv)?;
            if e1.typ != e2.typ {
                return Err(format!(
                    "Both sides of the result of a ternary expression must have the same type. Found {} and {}.",
                    e1.typ, e2.typ
                ));
            }
            Ok(TypedExpr {
                typ: e1.typ,
                expr: Expr::Op(TypedOps::Ternary(Box::new(c), Box::new(e1), Box::new(e2))),
            })
        }
        UntypedOps::Not(v) => assert_expr_type(*v, Type::Bool, "not".to_string(), tenv),
        UntypedOps::BitNot(v) => assert_expr_type(*v, Type::Int, "bitwise not".to_string(), tenv),
        UntypedOps::Neg(v) => assert_expr_type(*v, Type::Int, "negation".to_string(), tenv),
        UntypedOps::Pos(v) => assert_expr_type(*v, Type::Int, "pos".to_string(), tenv),
        UntypedOps::Plus(a, b) => {
            let s = "addition".to_string();
            let e1 = assert_expr_type(*a, Type::Int, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Int, s, tenv)?;
            Ok(op!(Int, Plus, e1, e2))
        }
        UntypedOps::Minus(a, b) => {
            let s = "subtraction".to_string();
            let e1 = assert_expr_type(*a, Type::Int, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Int, s, tenv)?;
            Ok(op!(Int, Minus, e1, e2))
        }
        UntypedOps::Mul(a, b) => {
            let s = "multiplication".to_string();
            let e1 = assert_expr_type(*a, Type::Int, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Int, s, tenv)?;
            Ok(op!(Int, Mul, e1, e2))
        }
        UntypedOps::Div(a, b) => {
            let s = "division".to_string();
            let e1 = assert_expr_type(*a, Type::Int, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Int, s, tenv)?;
            Ok(op!(Int, Div, e1, e2))
        }
        UntypedOps::Mod(a, b) => {
            let s = "modulo".to_string();
            let e1 = assert_expr_type(*a, Type::Int, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Int, s, tenv)?;
            Ok(op!(Int, Mod, e1, e2))
        }
        UntypedOps::Eq(a, b) => {
            let e1 = typify_expr(*a, tenv)?;
            let e2 = typify_expr(*b, tenv)?;
            if e1.typ != e2.typ {
                return Err(format!(
                    "Both sides of an equal expression must have the same type. Found {} and {}.",
                    e1.typ, e2.typ
                ));
            }
            Ok(op!(Bool, Eq, e1, e2))
        }
        UntypedOps::Neq(a, b) => {
            let e1 = typify_expr(*a, tenv)?;
            let e2 = typify_expr(*b, tenv)?;
            if e1.typ != e2.typ {
                return Err(format!(
                    "Both sides of an not equal expression must have the same type. Found {} and {}.",
                    e1.typ, e2.typ
                ));
            }
            Ok(op!(Bool, Neq, e1, e2))
        }
        UntypedOps::Lt(a, b) => {
            let s = "less than".to_string();
            let e1 = assert_expr_type(*a, Type::Int, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Int, s, tenv)?;
            Ok(op!(Bool, Lt, e1, e2))
        }
        UntypedOps::Leq(a, b) => {
            let s = "less than or equal to".to_string();
            let e1 = assert_expr_type(*a, Type::Int, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Int, s, tenv)?;
            Ok(op!(Bool, Leq, e1, e2))
        }
        UntypedOps::Gt(a, b) => {
            let s = "greater than".to_string();
            let e1 = assert_expr_type(*a, Type::Int, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Int, s, tenv)?;
            Ok(op!(Bool, Gt, e1, e2))
        }
        UntypedOps::Geq(a, b) => {
            let s = "greater than or equal to".to_string();
            let e1 = assert_expr_type(*a, Type::Int, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Int, s, tenv)?;
            Ok(op!(Bool, Geq, e1, e2))
        }
        UntypedOps::And(a, b) => {
            let s = "and".to_string();
            let e1 = assert_expr_type(*a, Type::Bool, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Bool, s, tenv)?;
            Ok(op!(Bool, And, e1, e2))
        }
        UntypedOps::Or(a, b) => {
            let s = "or".to_string();
            let e1 = assert_expr_type(*a, Type::Bool, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Bool, s, tenv)?;
            Ok(op!(Bool, Or, e1, e2))
        }
        UntypedOps::BitAnd(a, b) => {
            let s = "bitwise and".to_string();
            let e1 = assert_expr_type(*a, Type::Int, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Int, s, tenv)?;
            Ok(op!(Int, BitAnd, e1, e2))
        }
        UntypedOps::BitOr(a, b) => {
            let s = "bitwise or".to_string();
            let e1 = assert_expr_type(*a, Type::Int, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Int, s, tenv)?;
            Ok(op!(Int, BitOr, e1, e2))
        }
        UntypedOps::BitXor(a, b) => {
            let s = "bitwise xor".to_string();
            let e1 = assert_expr_type(*a, Type::Int, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Int, s, tenv)?;
            Ok(op!(Int, BitXor, e1, e2))
        }
        UntypedOps::BitShiftLeft(a, b) => {
            let s = "bit shift left".to_string();
            let e1 = assert_expr_type(*a, Type::Int, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Int, s, tenv)?;
            Ok(op!(Int, BitShiftLeft, e1, e2))
        }
        UntypedOps::BitShiftRight(a, b) => {
            let s = "bit shift right".to_string();
            let e1 = assert_expr_type(*a, Type::Int, s.clone(), tenv)?;
            let e2 = assert_expr_type(*b, Type::Int, s, tenv)?;
            Ok(op!(Int, BitShiftRight, e1, e2))
        }
    }
}

fn assert_expr_type(
    expr: UntypedExpr,
    t: Type,
    msg: String,
    tenv: &TEnv,
) -> Result<TypedExpr, String> {
    let ty = typify_expr(expr, tenv)?;
    if t != ty.typ {
        return Err(format!(
            "Expected type {} in {} expression. Found {}.",
            t, msg, ty.typ
        ));
    }
    Ok(ty)
}
