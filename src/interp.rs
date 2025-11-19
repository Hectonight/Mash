use crate::types::{Datum, Expr, Ops, ResultUnit, ResultValue, TypedCodeBlock, TypedExpr, TypedProgram, TypedStatement, Value};
use std::collections::HashMap;
use std::ops::{Neg, Not};


struct Environment {
    environment: Vec<HashMap<String, Value>>,
}

impl Environment {
    fn new() -> Self {
        Self { environment: vec![] }
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

    fn insert(&mut self, s: String, v: Value) {
        let l = self.environment.len();
        self.environment[l - 1].insert(s, v);
    }
}

pub fn interp(program: TypedProgram) -> Result<i64, String> {
    let mut interpreter = Environment::new();
    interp_codeblock(program, &mut interpreter)?;
    Ok(0)
}

fn interp_codeblock(codeblock: TypedCodeBlock, env: &mut Environment) -> ResultUnit {
    env.new_environment();
    for statement in codeblock {
        interp_statement(statement, env)?;
    }
    env.pop_environment();
    Ok(())
}

fn interp_statement(statement: TypedStatement, env: &mut Environment) -> ResultUnit {
    match statement {
        TypedStatement::Expr((e, _)) => { interp_expr(e, env)?; Ok(()) },
        TypedStatement::Print((e, _)) => {
            let x = interp_expr(e, env)?;
            printer(&x);
            Ok(())
        }
        TypedStatement::If(e,then,elifs,else_block) => interp_if(e, then, elifs, else_block, env),
        TypedStatement::Let(s, (e, _)) => interp_let(s, e, env),
    }
}

fn interp_let(s: String, expr: Expr, env: &mut Environment) -> ResultUnit {
    let v = interp_expr(expr, env)?;
    env.insert(s, v);
    Ok(())
}


fn interp_if(typed_expr: TypedExpr, codeblock: TypedCodeBlock, elifs: Vec<(TypedExpr,TypedCodeBlock)>,
             else_block: Option<TypedCodeBlock>, env: &mut Environment) -> ResultUnit {
    let (expr, _) = typed_expr;
    match interp_expr(expr, env)? {
        Value::Bool(true) => interp_codeblock(codeblock, env),
        Value::Bool(false) => {

            for ((e, _), block) in elifs {
                match interp_expr(e, env)? {
                    Value::Bool(false) => (),
                    Value::Bool(true) => return interp_codeblock(block, env),
                    _ => return Err("Type Error".to_owned())
                }
            }

            if let Some(block) = else_block {
                interp_codeblock(block, env)
            } else {
                Ok(())
            }
        }
        _ => Err("Type Error".to_owned())
    }
}

fn printer(value: &Value) {
    match value {
        Value::Int(x) => println!("{}", x),
        Value::Bool(x) => println!("{}", x),
        Value::Null => println!("null"),
        Value::Void => ()
    }
}


fn interp_expr(expr: Expr, env: &Environment) -> ResultValue {
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
    }
}

fn interp_ops(op: Ops, env: &Environment) -> ResultValue {
    match op {
        Ops::Ternary(a, b, c) => {
            match interp_expr(*a, env)? {
                Value::Bool(true) => interp_expr(*b, env),
                Value::Bool(false) => interp_expr(*c, env),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::Pos(a) => interp_expr(*a, env),
        Ops::Neg(a) => interp_expr(*a, env)?.neg(),
        Ops::Not(a) | Ops::BitNot(a) => interp_expr(*a, env)?.not(),
        Ops::Plus(a, b) => interp_expr(*a, env)? + interp_expr(*b, env)?,
        Ops::Minus(a, b) => interp_expr(*a, env)? - interp_expr(*b, env)?,
        Ops::Mul(a,b) => interp_expr(*a, env)? * interp_expr(*b, env)?,
        Ops::Div(a,b) => interp_expr(*a, env)? / interp_expr(*b, env)?,
        Ops::Mod(a,b) => interp_expr(*a, env)? % interp_expr(*b, env)?,
        Ops::Gt(a,b) => interp_expr(*a, env)?.greater(interp_expr(*b, env)?),
        Ops::Lt(a,b) => interp_expr(*a, env)?.less(interp_expr(*b, env)?),
        Ops::Geq(a,b) => interp_expr(*a, env)?.greater_eq(interp_expr(*b, env)?),
        Ops::Leq(a,b) => interp_expr(*a, env)?.less_eq(interp_expr(*b, env)?),
        Ops::Neq(a,b) => interp_expr(*a, env)?.not_equal(interp_expr(*b, env)?),
        Ops::Eq(a,b) => interp_expr(*a, env)?.equal(interp_expr(*b, env)?),
        Ops::And(a,b) => interp_expr(*a, env)?.and(interp_expr(*b, env)?),
        Ops::Or(a,b) => interp_expr(*a, env)?.or(interp_expr(*b, env)?),
        Ops::BitAnd(a,b) => interp_expr(*a, env)? & interp_expr(*b, env)?,
        Ops::BitOr(a,b) => interp_expr(*a, env)? | interp_expr(*b, env)?,
        Ops::BitXor(a,b) => interp_expr(*a, env)? ^ interp_expr(*b, env)?,
        Ops::BitShiftLeft(a,b) => interp_expr(*a, env)? << interp_expr(*b, env)?,
        Ops::BitShiftRight(a,b) => interp_expr(*a, env)? >> interp_expr(*b, env)?,
    }
}

fn interp_datum(dat: &Datum) -> Value {
    match dat {
        Datum::Int(x) => Value::Int(*x),
        Datum::Bool(x) => Value::Bool(*x),
        Datum::Null => Value::Null
    }
}

