use crate::inter_rep::{AsmProg, IRInst};
use crate::inter_rep::Imm::Int;
use crate::inter_rep::IRInst::{Mov, Push, Ret, Section};
use crate::inter_rep::Operand::{Imm, Reg};
use crate::inter_rep::R64::{R15, RAX, RDI, RSP};
use crate::constructors::{add, and, call, external, global, label, mov, or, pop, push, section, sub, xor};
use crate::types::{Datum, Expr, TypedExpr, TypedStatement as Statement, TypedProgram as Program, Type, Ops};


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
        Expr::Op(op) => compile_op(op),
    }
}


fn compile_op2(e1: &Expr, e2: &Expr) -> AsmProg {
    let mut asm = compile_expr(e1);
    asm.push(push(RAX));
    asm.append(&mut compile_expr(e2));
    asm
}

fn compile_op(op: &Ops) -> AsmProg {
    match op {
        Ops::Ternary(_, _, _) => todo!(),
        Ops::Not(_) => todo!(),
        Ops::BitNot(_) => todo!(),
        Ops::Neg(_) => todo!(),
        Ops::Pos(_) => todo!(),
        Ops::Plus(e1, e2) => {
            let mut asm = compile_op2(e1, e2);
            asm.append(&mut vec![
                pop(RDI),
                add(RAX, RDI),
            ]);
            asm
        },
        Ops::Minus(e1, e2) => {
            let mut asm = compile_op2(e1, e2);
            asm.append(&mut vec![
                pop(RDI),
                sub(RDI, RAX),
                mov(RAX, RDI)
            ]);
            asm
        },
        Ops::Mul(_, _) => todo!(),
        Ops::Div(_, _) => todo!(),
        Ops::Mod(_, _) => todo!(),
        Ops::Eq(_, _) => todo!(),
        Ops::Neq(_, _) => todo!(),
        Ops::Lt(_, _) => todo!(),
        Ops::Leq(_, _) => todo!(),
        Ops::Gt(_, _) => todo!(),
        Ops::Geq(_, _) => todo!(),
        Ops::BitAnd(e1, e2) | Ops::And(e1, e2) => {
            let mut asm = compile_op2(e1, e2);
            asm.append(&mut vec![
                pop(RDI),
                and(RAX, RDI),
            ]);
            asm
        },
        Ops::BitOr(e1, e2) | Ops::Or(e1, e2) => {
            let mut asm = compile_op2(e1, e2);
            asm.append(&mut vec![
                pop(RDI),
                or(RAX, RDI),
            ]);
            asm
        },
        Ops::BitXor(e1, e2) => {
            let mut asm = compile_op2(e1, e2);
            asm.append(&mut vec![
                pop(RDI),
                xor(RAX, RDI),
            ]);
            asm
        },
        Ops::BitShiftLeft(_, _) => todo!(),
        Ops::BitShiftRight(_, _) => todo!(),
    }
}

fn compile_datum(datum: &Datum) -> AsmProg {
    vec![match datum {
        Datum::Int(i) => mov(RAX, *i),
        Datum::Bool(b) => mov(RAX, 1),
        Datum::Null => todo!()
    }]
}

