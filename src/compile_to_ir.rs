use crate::inter_rep::{AsmProg, IRInst};
use crate::inter_rep::Imm::Int;
use crate::inter_rep::IRInst::{Mov, Push, Ret, Section};
use crate::inter_rep::Operand::{Imm, Reg};
use crate::inter_rep::R64::{R15, RAX, RDI, RSP};
use crate::constructors::{add, and, call, external, global, label, mov, pop, push, section, sub, xor};
use crate::types::{Datum, Expr, TypedExpr, TypedStatement as Statement, TypedProgram as Program, Type};


pub fn compile_to_ir(program: &Program) -> AsmProg {
    let mut asm = vec![
        section(".note.GNU-stack"),
        global("main"),
        section(".text"),
        label("main"),
    ];
    asm.append(
        &mut program.iter()
            .flat_map(|s| compile_statement(s))
            .collect::<AsmProg>()
    );
    asm.append(&mut vec![
        xor(RAX, RAX),
        Ret
    ]);
    asm
}

fn compile_statement(statement: &Statement) -> AsmProg {
    match statement {
        Statement::Expr((e, _)) => compile_expr(e),
        Statement::If(_, _, _, _) => todo!(),
        Statement::Print(e) => compile_print(e),
        Statement::Let(_, _) => todo!(),
    }
}

fn compile_print(te: &TypedExpr) -> AsmProg {
    let (e, t) = te;
    match t {
        Type::Int => compile_print_int(e),
        Type::Bool => todo!(),
        Type::Null => todo!()
    }
}

fn compile_print_int(e: &Expr) -> AsmProg {
    let mut asm = compile_expr(e);
    asm.append(&mut vec![
            push(R15),
            mov(R15, RSP),
            and(R15, 0b1000),
            sub(RSP, R15),
            external("print_int"),
            mov(RDI,RAX),
            call("print_int"),
            add(RSP, R15),
            pop(R15)
    ]);
    asm
}

fn compile_expr(expr: &Expr) -> AsmProg {
    match expr {
        Expr::Datum(d) => compile_datum(d),
        Expr::Identifier(_) => todo!(),
        Expr::Op(_) => todo!()
    }
}

fn compile_datum(datum: &Datum) -> AsmProg {
    vec![match datum {
        Datum::Int(i) => mov(RAX, *i),
        Datum::Bool(b) => todo!(),
        Datum::Null => todo!()
    }]
}

