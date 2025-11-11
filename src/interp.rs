use crate::types::{CodeBlock, Datum, Expr, Ops, Program, Statement, Value};

type ResultValue = Result<Value, String>;
type ResultUnit = Result<(), String>;


pub fn interp(program: Program) -> Result<i64, String> {
    interp_codeblock(program)?;
    Ok(0)
}

fn interp_codeblock(codeblock: CodeBlock) -> ResultUnit {
    for statement in codeblock {
        interp_statement(statement)?;
    }
    Ok(())
}

fn interp_statement(statement: Statement) -> ResultUnit {
    match statement {
        Statement::Expr(e) => { interp_expr(e)?; Ok(()) },
        Statement::Print(e) => {
            let x = interp_expr(e)?;
            printer(&x);
            Ok(())
        }
        Statement::If(e,then,elifs,else_block) => interp_if(e, then, elifs, else_block),
    }
}

fn interp_if(expr: Expr, codeblock: CodeBlock, elifs: Vec<(Expr,CodeBlock)>, else_block: Option<CodeBlock>) -> ResultUnit {
    match interp_expr(expr)? {
        Value::Bool(true) => interp_codeblock(codeblock),
        Value::Bool(false) => {

            for (e, block) in elifs {
                match interp_expr(e)? {
                    Value::Bool(false) => (),
                    Value::Bool(true) => return interp_codeblock(block),
                    _ => return Err("Type Error".to_owned())
                }
            }

            if let Some(block) = else_block {
                interp_codeblock(block)
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


fn interp_expr(expr: Expr) -> ResultValue {
    match expr {
        Expr::Datum(x) => Ok(interp_datum(&x)),
        Expr::Op(op) => interp_ops(op),
        _ => Err("Undefined Expression".to_owned())
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

