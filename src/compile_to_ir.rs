use crate::inter_rep::{AsmProg, IRInst};
use crate::inter_rep::Imm::Int;
use crate::inter_rep::IRInst::Mov;
use crate::inter_rep::Operand::{Imm, Reg};
use crate::inter_rep::R64::RAX;
use crate::reg;
use crate::types::{Datum, Expr, Program, Statement};

pub fn compile_to_ir(program: &Program) -> AsmProg {
    program.iter()
        .flat_map(|s| compile_statement(s))
        .collect::<AsmProg>()
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
    match datum {
        Datum::Int(i) => vec![Mov(Reg(reg!(RAX)),Imm(Int(*i as i128)))],
        Datum::Bool(_) => todo!(),
        Datum::Null => todo!()
    }
}

