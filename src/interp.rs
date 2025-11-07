use crate::types::{Datum, Expr, Program, Statement, Value};

type ResultValue = Result<Value, String>;

pub fn interp(program: &Program) -> ResultValue {
    for statement in program {
        interp_statement(statement);
    }
    Ok(Value::Int(0))
}

fn interp_statement(statement: &Statement) -> ResultValue {
    match statement {
        Statement::Expr(e) => interp_expr(e),
        Statement::Print(e) => {
            let x = interp_expr(e)?;
            printer(&x);
            Ok(x)
        }
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


fn interp_expr(expr: &Expr) -> ResultValue {
    match expr {
        Expr::Datum(x) => Ok(match_datum(x))
    }
}

fn match_datum(dat: &Datum) -> Value {
    match dat {
        Datum::Int(x) => Value::Int(*x),
        Datum::Bool(x) => Value::Bool(*x),
        Datum::Null => Value::Null
    }
}

