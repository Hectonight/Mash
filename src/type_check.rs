use crate::types::{BuiltIn, CodeBlock, Datum, Expr, Ops, Program, Statement, Type, TypedCodeBlock, TypedExpr, TypedProgram, TypedStatement};
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

fn typify_codeblock(codeblock: CodeBlock, tenv: &mut TEnv) -> Result<TypedCodeBlock, String> {
    tenv.new_environment();
    let tcb = codeblock
        .into_iter()
        .map(|s| typify_statement(s, tenv))
        .collect();
    tenv.pop_environment();
    tcb
}

fn typify_statement(statement: Statement, tenv: &mut TEnv) -> Result<TypedStatement, String> {
    match statement {
        Statement::Expr(e) => Ok(TypedStatement::Expr(typify_expr(e, tenv)?)),
        Statement::If(e, cb, elifs, eb) => {
            let s = "conditional if".to_owned();
            Ok(TypedStatement::If(
                assert_typify_expr(e, Type::Bool, s.clone(), tenv)?,
                typify_codeblock(cb, tenv)?,
                elifs
                    .into_iter()
                    .map(|(ex, tcb)| {
                        Ok((
                            assert_typify_expr(ex, Type::Bool, s.clone(), tenv)?,
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
        Statement::Print(e) => Ok(TypedStatement::Print(typify_expr(e, tenv)?)),
        Statement::Let(id, e) => {
            let (ex, t) = typify_expr(e, tenv)?;
            if tenv.member(&id) {
                return Err(format!("{} defined multiple times", id));
            }
            tenv.insert(id.clone(), t);
            Ok(TypedStatement::Let(id.clone(), (ex, t)))
        }
        Statement::Assignment(id, e) => {
            let (ex, tright) = typify_expr(e, tenv)?;
            let tleft = tenv.lookup(&id).ok_or(format!("{} is not defined", id))?;
            if *tleft != tright {
                Err(format!(
                    "Assignment type mismatch {} and {}",
                    *tleft, tright
                ))
            } else {
                Ok(TypedStatement::Assignment(id, (ex, tright)))
            }
        }
        Statement::While(e, cb) => {
            tenv.control_flow += 1;
            let (ex, t) = typify_expr(e, tenv)?;
            if t != Type::Bool {
                Err(format!("While type mismatch, found {} expected bool", t))
            } else {
                Ok(TypedStatement::While(
                    (ex, Type::Bool),
                    typify_codeblock(cb, tenv)?,
                ))
            }
        }
        Statement::Break(n) => {
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
        Statement::Continue => Ok(TypedStatement::Continue),
    }
}

fn typify_expr(expr: Expr, tenv: &TEnv) -> Result<TypedExpr, String> {
    let t = expr_type(&expr, tenv)?;
    Ok((expr, t))
}

fn expr_type(expr: &Expr, tenv: &TEnv) -> Result<Type, String> {
    match expr {
        Expr::Datum(d) => Ok(datum_type(&d)),
        Expr::Identifier(id) => match tenv.lookup(&id) {
            Some(t) => Ok(*t),
            None => Err(format!("Unknown identifier {}", id)),
        },
        Expr::Op(op) => op_type(op, tenv),
        Expr::BuiltIn(f, p) => builtin_type(f, p, tenv)
    }
}

fn arity_error(builtin: &BuiltIn, expected: usize, found: usize) -> Result<Type, String> {
    Err(format!("{} expected {} parameter. Found {}", builtin, expected, found))
}

fn arity_range_error<R: RangeBounds<usize> + Debug>(builtin: &BuiltIn, expected: R, found: usize) -> Result<Type, String> {
    Err(format!("{} expected {:?} parameter. Found {}", builtin, expected, found))
}

fn assert_type_builtin(builtin: &BuiltIn, expected: Type, actual: Type) -> Result<(), String> {
    if expected == actual {
        Ok(())
    } else {
        Err(format!("{} expected {}. Found {}", builtin, expected, actual))
    }
}

fn builtin_type(builtin: &BuiltIn, params: &Vec<Expr>, tenv: &TEnv) -> Result<Type, String> {
    match builtin {
        BuiltIn::Abs | BuiltIn::Sgn => {
            if params.len() != 1 {
                arity_error(builtin, 1, params.len())
            } else {
                let t = expr_type(&params[0], tenv)?;
                assert_type_builtin(builtin, t, Type::Int)?;
                Ok(Type::Int)
            }
        }

        BuiltIn::Max | BuiltIn::Min => {
            if params.len() == 0 {
                arity_range_error(builtin, 1.., 0)
            } else {
                for param in params {
                    let t = expr_type(&param, tenv)?;
                    assert_type_builtin(builtin, Type::Int, t)?;
                }
                Ok(Type::Int)
            }
        }
    }
}

fn datum_type(datum: &Datum) -> Type {
    match datum {
        Datum::Int(_) => Type::Int,
        Datum::Bool(_) => Type::Bool,
        Datum::Char(_) => Type::Char,
        Datum::Null => Type::Null,
    }
}

fn op_type(ops: &Ops, tenv: &TEnv) -> Result<Type, String> {
    match ops {
        Ops::Ternary(cond, a, b) => {
            assert_expr_type(cond, Type::Bool, "ternary".to_string(), tenv)?;
            let t1 = expr_type(a, tenv)?;
            let t2 = expr_type(b, tenv)?;
            if t1 != t2 {
                return Err(format!(
                    "Both sides of the result of a ternary expression must have the same type. Found {} and {}.",
                    t1, t2
                ));
            }
            Ok(t1)
        }
        Ops::Not(v) => Ok(assert_expr_type(v, Type::Bool, "not".to_string(), tenv)?),
        Ops::BitNot(v) => Ok(assert_expr_type(
            v,
            Type::Int,
            "bitwise not".to_string(),
            tenv,
        )?),
        Ops::Neg(v) => Ok(assert_expr_type(
            v,
            Type::Int,
            "negation".to_string(),
            tenv,
        )?),
        Ops::Pos(v) => Ok(assert_expr_type(v, Type::Int, "pos".to_string(), tenv)?),
        Ops::Plus(a, b) => {
            let s = "addition".to_string();
            assert_expr_type(a, Type::Int, s.clone(), tenv)?;
            Ok(assert_expr_type(b, Type::Int, s, tenv)?)
        }
        Ops::Minus(a, b) => {
            let s = "subtraction".to_string();
            assert_expr_type(a, Type::Int, s.clone(), tenv)?;
            Ok(assert_expr_type(b, Type::Int, s, tenv)?)
        }
        Ops::Mul(a, b) => {
            let s = "multiplication".to_string();
            assert_expr_type(a, Type::Int, s.clone(), tenv)?;
            Ok(assert_expr_type(b, Type::Int, s, tenv)?)
        }
        Ops::Div(a, b) => {
            let s = "division".to_string();
            assert_expr_type(a, Type::Int, s.clone(), tenv)?;
            Ok(assert_expr_type(b, Type::Int, s, tenv)?)
        }
        Ops::Mod(a, b) => {
            let s = "modulo".to_string();
            assert_expr_type(a, Type::Int, s.clone(), tenv)?;
            Ok(assert_expr_type(b, Type::Int, s, tenv)?)
        }
        Ops::Eq(a, b) => {
            let t1 = expr_type(a, tenv)?;
            let t2 = expr_type(b, tenv)?;
            if t1 != t2 {
                return Err(format!(
                    "Both sides of an equal expression must have the same type. Found {} and {}.",
                    t1, t2
                ));
            }
            Ok(Type::Bool)
        }
        Ops::Neq(a, b) => {
            let t1 = expr_type(a, tenv)?;
            let t2 = expr_type(b, tenv)?;
            if t1 != t2 {
                return Err(format!(
                    "Both sides of an not equal expression must have the same type. Found {} and {}.",
                    t1, t2
                ));
            }
            Ok(Type::Bool)
        }
        Ops::Lt(a, b) => {
            let s = "less than".to_string();
            assert_expr_type(a, Type::Int, s.clone(), tenv)?;
            assert_expr_type(b, Type::Int, s, tenv)?;
            Ok(Type::Bool)
        }
        Ops::Leq(a, b) => {
            let s = "less than or equal to".to_string();
            assert_expr_type(a, Type::Int, s.clone(), tenv)?;
            assert_expr_type(b, Type::Int, s, tenv)?;
            Ok(Type::Bool)
        }
        Ops::Gt(a, b) => {
            let s = "greater than".to_string();
            assert_expr_type(a, Type::Int, s.clone(), tenv)?;
            assert_expr_type(b, Type::Int, s, tenv)?;
            Ok(Type::Bool)
        }
        Ops::Geq(a, b) => {
            let s = "greater than or equal to".to_string();
            assert_expr_type(a, Type::Int, s.clone(), tenv)?;
            assert_expr_type(b, Type::Int, s, tenv)?;
            Ok(Type::Bool)
        }
        Ops::And(a, b) => {
            let s = "and".to_string();
            assert_expr_type(a, Type::Bool, s.clone(), tenv)?;
            assert_expr_type(b, Type::Bool, s, tenv)?;
            Ok(Type::Bool)
        }
        Ops::Or(a, b) => {
            let s = "or".to_string();
            assert_expr_type(a, Type::Bool, s.clone(), tenv)?;
            Ok(assert_expr_type(b, Type::Bool, s, tenv)?)
        }
        Ops::BitAnd(a, b) => {
            let s = "bitwise and".to_string();
            assert_expr_type(a, Type::Int, s.clone(), tenv)?;
            Ok(assert_expr_type(b, Type::Int, s, tenv)?)
        }
        Ops::BitOr(a, b) => {
            let s = "bitwise or".to_string();
            assert_expr_type(a, Type::Int, s.clone(), tenv)?;
            Ok(assert_expr_type(b, Type::Int, s, tenv)?)
        }
        Ops::BitXor(a, b) => {
            let s = "bitwise xor".to_string();
            assert_expr_type(a, Type::Int, s.clone(), tenv)?;
            Ok(assert_expr_type(b, Type::Int, s, tenv)?)
        }
        Ops::BitShiftLeft(a, b) => {
            let s = "bit shift left".to_string();
            assert_expr_type(a, Type::Int, s.clone(), tenv)?;
            Ok(assert_expr_type(b, Type::Int, s, tenv)?)
        }
        Ops::BitShiftRight(a, b) => {
            let s = "bit shift right".to_string();
            assert_expr_type(a, Type::Int, s.clone(), tenv)?;
            Ok(assert_expr_type(b, Type::Int, s, tenv)?)
        }
    }
}

fn assert_typify_expr(expr: Expr, t: Type, msg: String, tenv: &TEnv) -> Result<TypedExpr, String> {
    let (e, ty) = typify_expr(expr, tenv)?;
    if t != ty {
        return Err(format!(
            "Expected type {} in {} expression. Found {}.",
            t, msg, ty
        ));
    }
    Ok((e, ty))
}

fn assert_expr_type(expr: &Expr, t: Type, msg: String, tenv: &TEnv) -> Result<Type, String> {
    let ty = expr_type(expr, tenv)?;
    if t != ty {
        return Err(format!(
            "Expected type {} in {} expression. Found {}.",
            t, msg, ty
        ));
    }
    Ok(t)
}
