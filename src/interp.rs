use crate::types::{Datum, Expr, Ops, Program, Statement, Value};

type ResultValue = Result<Value, String>;

pub fn interp(program: Program) -> ResultValue {
    for statement in program {
        interp_statement(statement)?;
    }
    Ok(Value::Int(0))
}

fn interp_statement(statement: Statement) -> ResultValue {
    match statement {
        Statement::Expr(e) => interp_expr(e),
        Statement::Print(e) => {
            let x = interp_expr(e)?;
            printer(&x);
            Ok(x)
        }
        _ => todo!()
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


fn interp_expr(expr: Expr) -> ResultValue {
    match expr {
        Expr::Datum(x) => Ok(interp_datum(&x)),
        Expr::Op(op) => interp_ops(op),
        _ => Err("Undefined expression".to_owned())
    }
}

fn interp_ops(op: Ops) -> ResultValue {
    match op {

        Ops::Ternary(a, b, c) => {
            match interp_expr(*a)? {
                Value::Bool(true) => interp_expr(*b),
                Value::Bool(false) => interp_expr(*c),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::Not(a) => {
            match interp_expr(*a)? {
                Value::Bool(a) => Ok(Value::Bool(!a)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::BitNot(a) => {
            match interp_expr(*a)? {
                Value::Int(a) => Ok(Value::Int(!a)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::Neg(a) => {
            match interp_expr(*a)? {
                Value::Int(a) => Ok(Value::Int(-a)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::Pos(a) => {
            match interp_expr(*a)? {
                Value::Int(a) => Ok(Value::Int(a)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::Plus(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::Minus(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::Mul(a, b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::Div(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a / b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::Mod(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::Eq(a,b) => {
            Ok(Value::Bool(interp_expr(*a)? == interp_expr(*b)?))
        }
        Ops::Neq(a,b) => {
            Ok(Value::Bool(interp_expr(*a)? != interp_expr(*b)?))
        }
        Ops::Lt(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::Leq(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::Gt(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::Geq(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::And(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::Or(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::BitAnd(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a & b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::BitOr(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::BitXor(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::BitShiftLeft(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a << b)),
                _ => Err("Type Error".to_owned())
            }
        }
        Ops::BitShiftRight(a,b) => {
            match (interp_expr(*a)?, interp_expr(*b)?) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a >> b)),
                _ => Err("Type Error".to_owned())
            }
        }
    }
}

fn interp_datum(dat: &Datum) -> Value {
    match dat {
        Datum::Int(x) => Value::Int(*x),
        Datum::Bool(x) => Value::Bool(*x),
        Datum::Null => Value::Null
    }
}

