use crate::types::{
    Datum, Expr, Ops, ResultUnit, ResultValue, TypedCodeBlock as CodeBlock, TypedExpr,
    TypedProgram as Program, TypedStatement as Statement, Value,
};
use std::collections::HashMap;
use std::io::{Write, stderr, stdout};
use std::ops::{Neg, Not};

#[allow(dead_code)]
struct Environment<'a, W1: Write, W2: Write> {
    environment: Vec<HashMap<String, Value>>,
    out: &'a mut W1,
    err: &'a mut W2,
}

impl<'a, W1: Write, W2: Write> Environment<'a, W1, W2> {
    fn new(out: &'a mut W1, err: &'a mut W2) -> Self {
        Self {
            environment: vec![],
            out,
            err,
        }
    }

    fn new_environment(&mut self) {
        self.environment.push(HashMap::new());
    }

    fn pop_environment(&mut self) {
        self.environment.pop();
    }

    fn lookup(&self, key: &String) -> Option<&Value> {
        for env in self.environment.iter().rev() {
            if let Some(val) = env.get(key) {
                return Some(val);
            }
        }
        None
    }

    fn update(&mut self, key: String, val: Value) {
        for env in self.environment.iter_mut().rev() {
            if env.contains_key(&key) {
                env.insert(key, val);
                return;
            }
        }
    }

    fn insert(&mut self, s: String, v: Value) {
        let l = self.environment.len();
        self.environment[l - 1].insert(s, v);
    }
}

pub fn interp(program: &Program) -> Result<i32, String> {
    interp_test(program, &mut stdout(), &mut stderr())
}

pub fn interp_test<'a, W1: Write + 'static, W2: Write + 'static>(
    program: &Program,
    out: &'a mut W1,
    err: &'a mut W2,
) -> Result<i32, String> {
    let mut interpreter = Environment::new(out, err);
    interp_codeblock(program, &mut interpreter)?;
    Ok(0)
}

fn interp_codeblock<W1: Write, W2: Write>(
    codeblock: &CodeBlock,
    env: &mut Environment<W1, W2>,
) -> ResultUnit {
    env.new_environment();
    for statement in codeblock {
        interp_statement(statement, env)?;
    }
    env.pop_environment();
    Ok(())
}

fn interp_statement<W1: Write, W2: Write>(
    statement: &Statement,
    env: &mut Environment<W1, W2>,
) -> ResultUnit {
    match statement {
        Statement::Expr((e, _)) => {
            interp_expr(e, env)?;
            Ok(())
        }
        Statement::Print((e, _)) => {
            let x = interp_expr(e, env)?;
            printer(&x, env)
        }
        Statement::If(e, then, elifs, else_block) => interp_if(e, then, elifs, else_block, env),
        Statement::Let(s, (e, _)) => interp_let(s, e, env),
        Statement::Assignment(s, (e, _)) => interp_assignment(s, e, env),
        Statement::While((e, _), cb) => interp_while(e, cb, env),
        _ => unimplemented!(),
    }
}

fn interp_while<W1: Write, W2: Write>(
    e: &Expr,
    code_block: &CodeBlock,
    env: &mut Environment<W1, W2>,
) -> ResultUnit {
    while let Value::Bool(true) = interp_expr(e, env)? {
        interp_codeblock(code_block, env)?;
    }
    Ok(())
}

fn interp_assignment<W1: Write, W2: Write>(
    s: &String,
    expr: &Expr,
    env: &mut Environment<W1, W2>,
) -> ResultUnit {
    let x = interp_expr(expr, env)?;
    env.update(s.clone(), x);
    Ok(())
}

fn interp_let<W1: Write, W2: Write>(
    s: &String,
    expr: &Expr,
    env: &mut Environment<W1, W2>,
) -> ResultUnit {
    let v = interp_expr(expr, env)?;
    env.insert(s.clone(), v);
    Ok(())
}

fn interp_if<W1: Write, W2: Write>(
    typed_expr: &TypedExpr,
    codeblock: &CodeBlock,
    elifs: &Vec<(TypedExpr, CodeBlock)>,
    else_block: &Option<CodeBlock>,
    env: &mut Environment<W1, W2>,
) -> ResultUnit {
    let (expr, _) = typed_expr;
    match interp_expr(expr, env)? {
        Value::Bool(true) => interp_codeblock(codeblock, env),
        Value::Bool(false) => {
            for ((e, _), block) in elifs {
                match interp_expr(e, env)? {
                    Value::Bool(false) => (),
                    Value::Bool(true) => return interp_codeblock(block, env),
                    _ => return Err("Type Error".to_owned()),
                }
            }

            if let Some(block) = else_block {
                interp_codeblock(block, env)
            } else {
                Ok(())
            }
        }
        _ => Err("Type Error".to_owned()),
    }
}

fn printer<W1: Write, W2: Write>(value: &Value, env: &mut Environment<W1, W2>) -> ResultUnit {
    match value {
        Value::Int(x) => writeln!(env.out, "{}", x).map_err(|e| e.to_string()),
        Value::Bool(x) => writeln!(env.out, "{}", x).map_err(|e| e.to_string()),
        Value::Char(x) => writeln!(env.out, "{}", x).map_err(|e| e.to_string()),
        Value::Null => writeln!(env.out, "null").map_err(|e| e.to_string()),
    }
}

fn interp_expr<W1: Write, W2: Write>(expr: &Expr, env: &Environment<W1, W2>) -> ResultValue {
    match expr {
        Expr::Datum(x) => Ok(interp_datum(&x)),
        Expr::Identifier(s) => {
            if let Some(v) = env.lookup(&s) {
                Ok(*v)
            } else {
                Err("Undefined Identifier ".to_owned() + &*s)
            }
        }
        Expr::Op(op) => interp_ops(op, env),
        _ => unimplemented!()
    }
}

fn interp_ops<W1: Write, W2: Write>(op: &Ops, env: &Environment<W1, W2>) -> ResultValue {
    match op {
        Ops::Ternary(a, b, c) => match interp_expr(&**a, env)? {
            Value::Bool(true) => interp_expr(&**b, env),
            Value::Bool(false) => interp_expr(&**c, env),
            _ => Err("Type Error".to_owned()),
        },
        Ops::Pos(a) => interp_expr(&**a, env),
        Ops::Neg(a) => interp_expr(&**a, env)?.neg(),
        Ops::Not(a) | Ops::BitNot(a) => interp_expr(&**a, env)?.not(),
        Ops::Plus(a, b) => interp_expr(&**a, env)? + interp_expr(&**b, env)?,
        Ops::Minus(a, b) => interp_expr(&**a, env)? - interp_expr(&**b, env)?,
        Ops::Mul(a, b) => interp_expr(&**a, env)? * interp_expr(&**b, env)?,
        Ops::Div(a, b) => interp_expr(&**a, env)? / interp_expr(&**b, env)?,
        Ops::Mod(a, b) => interp_expr(&**a, env)? % interp_expr(&**b, env)?,
        Ops::Gt(a, b) => interp_expr(&**a, env)?.greater(interp_expr(&**b, env)?),
        Ops::Lt(a, b) => interp_expr(&**a, env)?.less(interp_expr(&**b, env)?),
        Ops::Geq(a, b) => interp_expr(&**a, env)?.greater_eq(interp_expr(&**b, env)?),
        Ops::Leq(a, b) => interp_expr(&**a, env)?.less_eq(interp_expr(&**b, env)?),
        Ops::Neq(a, b) => interp_expr(&**a, env)?.not_equal(interp_expr(&**b, env)?),
        Ops::Eq(a, b) => interp_expr(&**a, env)?.equal(interp_expr(&**b, env)?),
        Ops::And(a, b) => interp_expr(&**a, env)?.and(interp_expr(&**b, env)?),
        Ops::Or(a, b) => interp_expr(&**a, env)?.or(interp_expr(&**b, env)?),
        Ops::BitAnd(a, b) => interp_expr(&**a, env)? & interp_expr(&**b, env)?,
        Ops::BitOr(a, b) => interp_expr(&**a, env)? | interp_expr(&**b, env)?,
        Ops::BitXor(a, b) => interp_expr(&**a, env)? ^ interp_expr(&**b, env)?,
        Ops::BitShiftLeft(a, b) => interp_expr(&**a, env)? << interp_expr(&**b, env)?,
        Ops::BitShiftRight(a, b) => interp_expr(&**a, env)? >> interp_expr(&**b, env)?,
    }
}

fn interp_datum(dat: &Datum) -> Value {
    match dat {
        Datum::Int(x) => Value::Int(*x),
        Datum::Bool(x) => Value::Bool(*x),
        Datum::Char(x) => Value::Char(*x),
        Datum::Null => Value::Null,
    }
}
