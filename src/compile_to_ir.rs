use crate::inter_rep::{AsmProg, IRInst};
use crate::inter_rep::Imm::Int;
use crate::inter_rep::IRInst::{Mov, Ret, Section};
use crate::inter_rep::Operand::{Imm, Reg};
use crate::inter_rep::R64::RAX;
use crate::constructors::{global, label, mov, section, xor};
use crate::types::{Datum, Expr, Program, Statement};


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
        Statement::Expr(e) => compile_expr(e),
        Statement::If(_, _, _, _) => todo!(),
        Statement::Print(_) => todo!(),
        Statement::Let(_, _) => todo!(),
    }
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

