use std::collections::HashMap;
use std::ops::Index;
use crate::inter_rep::{AsmProg, IRInst, Label, Mem};
use crate::inter_rep::Imm::Int;
use crate::inter_rep::IRInst::{Mov, Push, Ret, Section};
use crate::inter_rep::Operand::{Imm, Reg};
use crate::inter_rep::R64::{R15, RAX, RCX, RDI, RSP};
use crate::constructors::{add, and, call, external, global, imul, imul2, label, mov, neg, not, or, pop, push, section, shl, shr, sub, xor};
use crate::inter_rep::R32::{EAX, EDI};
use crate::inter_rep::R8::CL;
use crate::{mem, reg};
use crate::types::{Datum, Expr, TypedExpr, TypedStatement as Statement, TypedProgram as Program, Type, Ops};

struct CEnv {
    variables: Vec<HashMap<String, usize>>,
    stack_sizes: Vec<usize>,
    label_count: usize,
}

impl CEnv {

    fn new() -> Self {
        CEnv {
            variables: vec![HashMap::new()],
            stack_sizes: vec![0],
            label_count: 0
        }
    }
    fn new_env(&mut self) {
        self.variables.push(HashMap::new());
        self.stack_sizes.push(0);
    }

    fn pop_env(&mut self) -> usize {
        self.variables.pop();
        self.stack_sizes.pop().unwrap() * 8
    }

    // Assume that the variable is defined due to type checking
    fn locate(&self, name: &String) -> usize {
        let mut size = 0;
        for i in (0..self.stack_sizes.len()).rev() {
            size += self.stack_sizes[i];
            if let Some(loc) = self.variables[i].get(name) {
                return (size - loc) * 8;
            }
        }
        unreachable!("All identifiers should be defined.")
    }

    fn insert(&mut self, name: String) {
        let last = self.stack_sizes.len() - 1;
        self.stack_sizes[last] += 1;
        let loc = self.stack_sizes[last];
        self.variables[last].insert(name, loc);
    }

    fn increment_stack(&mut self, size: usize) {
        match self.stack_sizes.last_mut() {
            Some(x) => { *x += size;}
            None => unreachable!("Stack sizes is never empty.")
        }
    }

    fn decrement_stack(&mut self, size: usize) {
        match self.stack_sizes.last_mut() {
            Some(x) => { *x -= size;}
            None => unreachable!("Stack sizes is never empty.")
        }
    }

}


pub fn compile_to_ir(program: &Program) -> AsmProg {
    let mut cenv = CEnv::new();
    let mut asm = vec![
        section(".note.GNU-stack"),
        global("main"),
        section(".text"),
        label("main"),
    ];
    asm.append(
        &mut program.iter()
            .flat_map(|s| compile_statement(s, &mut cenv))
            .collect::<AsmProg>()
    );

    let size = cenv.pop_env();
    if size > 0 {
        asm.push(add(RSP, size))
    }

    asm.append(&mut vec![
        xor(RAX, RAX),
        Ret
    ]);
    asm
}

fn compile_statement(statement: &Statement, cenv: &mut CEnv) -> AsmProg {
    match statement {
        Statement::Expr((e, _)) => compile_expr(e, cenv),
        Statement::If(_, _, _, _) => todo!(),
        Statement::Print((e, t)) => compile_print(e, t, cenv),
        Statement::Let(s, (e, _)) => compile_let(s, e, cenv),
    }
}

fn compile_let(s: &String, e: &Expr, cenv: &mut CEnv) -> AsmProg {
    let mut asm = compile_expr(e, cenv);
    asm.push(push(RAX));
    cenv.insert(s.clone());
    asm
}

fn compile_print(e: &Expr, t: &Type, cenv: &mut CEnv) -> AsmProg {
    match t {
        Type::Int => compile_print_int(e, cenv),
        Type::Bool => compile_print_bool(e, cenv),
        Type::Null => compile_print_null()
    }
}

fn compile_print_bool(e: &Expr, cenv: &mut CEnv) -> AsmProg {
    let mut asm = compile_expr(e, cenv);
    asm.append(&mut vec![
        push(R15),
        mov(R15, RSP),
        and(R15, 0b1000),
        sub(RSP, R15),
        external("print_bool"),
        mov(EDI,EAX),
        call("print_bool"),
        add(RSP, R15),
        pop(R15)
    ]);
    asm
}

fn compile_print_null() -> AsmProg {
    vec![
        push(R15),
        mov(R15, RSP),
        and(R15, 0b1000),
        sub(RSP, R15),
        external("print_null"),
        call("print_null"),
        add(RSP, R15),
        pop(R15)
    ]
}

fn compile_print_int(e: &Expr, cenv: &mut CEnv) -> AsmProg {
    let mut asm = compile_expr(e, cenv);
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

fn compile_expr(expr: &Expr, cenv: &mut CEnv) -> AsmProg {
    match expr {
        Expr::Datum(d) => compile_datum(d),
        Expr::Identifier(s) => compile_ident(s, cenv),
        Expr::Op(op) => compile_op(op, cenv),
    }
}

fn compile_ident(s: &String, cenv: &CEnv) -> AsmProg {
    vec![mov(RAX, mem![RSP + cenv.locate(s)])]
}


fn compile_op2(e1: &Expr, e2: &Expr, cenv: &mut CEnv) -> AsmProg {
    let mut asm = compile_expr(e1, cenv);
    asm.push(push(RAX));
    cenv.increment_stack(1);
    asm.append(&mut compile_expr(e2, cenv));
    asm
}

fn compile_op(op: &Ops, cenv: &mut CEnv) -> AsmProg {
    match op {
        Ops::Ternary(_, _, _) => todo!(),
        Ops::Not(_) => todo!(),
        Ops::BitNot(e) => {
            let mut asm = compile_expr(e, cenv);
            asm.push(not(RAX));
            asm
        },
        Ops::Neg(e) => {
            let mut asm = compile_expr(e, cenv);
            asm.push(neg(RAX));
            asm
        },
        Ops::Pos(e) => compile_expr(e, cenv),
        Ops::Plus(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                pop(RDI),
                add(RAX, RDI),
            ]);
            cenv.decrement_stack(1);
            asm
        },
        Ops::Minus(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                pop(RDI),
                sub(RDI, RAX),
                mov(RAX, RDI)
            ]);
            cenv.decrement_stack(1);
            asm
        },
        Ops::Mul(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                pop(RDI),
                imul2(RAX, Some(RDI)),
            ]);
            cenv.decrement_stack(1);
            asm
        },
        Ops::Div(_, _) => todo!(),
        Ops::Mod(_, _) => todo!(),
        Ops::Eq(_, _) => todo!(),
        Ops::Neq(_, _) => todo!(),
        Ops::Lt(_, _) => todo!(),
        Ops::Leq(_, _) => todo!(),
        Ops::Gt(_, _) => todo!(),
        Ops::Geq(_, _) => todo!(),
        Ops::BitAnd(e1, e2) | Ops::And(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                pop(RDI),
                and(RAX, RDI),
            ]);
            cenv.decrement_stack(1);
            asm
        },
        Ops::BitOr(e1, e2) | Ops::Or(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                pop(RDI),
                or(RAX, RDI),
            ]);
            cenv.decrement_stack(1);
            asm
        },
        Ops::BitXor(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                pop(RDI),
                xor(RAX, RDI),
            ]);
            cenv.decrement_stack(1);
            asm
        },
        Ops::BitShiftLeft(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                mov(RCX, RAX),
                pop(RAX),
                shl(RAX, CL),
            ]);
            cenv.decrement_stack(1);
            asm
        },
        Ops::BitShiftRight(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                mov(RCX, RAX),
                pop(RAX),
                shr(RAX, CL),
            ]);
            cenv.decrement_stack(1);
            asm
        },
    }
}

fn compile_datum(datum: &Datum) -> AsmProg {
    match datum {
        Datum::Int(i) => vec![mov(RAX, *i)],
        Datum::Bool(b) => vec![mov(EAX, if *b { 1 } else { 0 })],
        Datum::Null => vec![]
    }
}

