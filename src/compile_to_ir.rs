use crate::constructors::{
    add, and, call, cmove, cmovg, cmovge, cmovl, cmovle, cmovne, cmp, external, global, idiv,
    imul2, je, jg, jge, jmp, jne, label, mov, neg, not, or, pop, push, sar, section, shl, sub,
    test, xor,
};
use crate::inter_rep::IRInst::{Cqo, Ret, Syscall};
use crate::inter_rep::Label;
use crate::inter_rep::R8::CL;
use crate::inter_rep::R32::{EAX, EDI};
use crate::inter_rep::R64::{R8, R15, RAX, RCX, RDI, RDX, RSI, RSP};
use crate::inter_rep::{AsmProg, Mem};
use crate::mem;
use crate::types::{
    BuiltIn, Datum, Expr, Type, TypedCodeBlock, TypedExpr, TypedOps, TypedProgram, TypedStatement,
};
use std::collections::HashMap;

#[allow(dead_code)]
struct CEnv {
    variables: Vec<HashMap<String, usize>>,
    stack_sizes: Vec<usize>,
    label_count: usize,
    control_flow: Vec<(Label, Label, usize)>,
}

impl CEnv {
    fn new() -> Self {
        CEnv {
            variables: vec![HashMap::new()],
            stack_sizes: vec![0],
            label_count: 0,
            control_flow: vec![],
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
            Some(x) => {
                *x += size;
            }
            None => unreachable!("Stack sizes is never empty."),
        }
    }

    fn decrement_stack(&mut self, size: usize) {
        match self.stack_sizes.last_mut() {
            Some(x) => {
                *x -= size;
            }
            None => unreachable!("Stack sizes is never empty."),
        }
    }

    fn genlabel(&mut self, name: &str) -> Label {
        let s = format!("{}_{}", name, self.label_count);
        self.label_count += 1;
        s
    }

    fn breaker(&self, loops: usize) -> (&Label, usize) {
        let (_, end, size) = &self.control_flow[self.control_flow.len() - loops];
        let s: usize = self.stack_sizes[*size..].iter().sum();
        (end, s * 8)
    }

    fn continuer(&self) -> (&Label, usize) {
        let (start, _, _) = &self.control_flow[self.control_flow.len() - 1];
        let s = self.stack_sizes[self.control_flow.len() - 1];
        (start, s * 8)
    }

    fn incr_control_flow(&mut self, start: String, end: String) {
        self.control_flow.push((start, end, self.variables.len()));
    }

    fn decr_control_flow(&mut self) {
        self.control_flow.pop();
    }
}

pub fn compile_to_ir(program: &TypedProgram) -> AsmProg {
    let mut cenv = CEnv::new();
    let mut asm = vec![
        section(".note.GNU-stack"),
        global("main"),
        section(".text"),
        label("main"),
    ];
    asm.append(
        &mut program
            .iter()
            .flat_map(|s| compile_statement(s, &mut cenv))
            .collect::<AsmProg>(),
    );

    let size = cenv.pop_env();
    if size > 0 {
        asm.push(add(RSP, size))
    }

    asm.append(&mut vec![xor(RAX, RAX), Ret]);
    asm
}

fn compile_statement(statement: &TypedStatement, cenv: &mut CEnv) -> AsmProg {
    match statement {
        TypedStatement::Expr(e) => compile_expr(e, cenv),
        TypedStatement::If(e, then, elifs, el) => compile_if(
            e,
            then,
            elifs.iter().map(|(ex, cb)| (ex, cb)).collect(),
            el,
            cenv,
        ),
        TypedStatement::Let(s, e) => compile_let(s, e, cenv),
        TypedStatement::Assignment(s, e) => compile_assignment(s, e, cenv),
        TypedStatement::While(e, cb) => compile_while(e, cb, cenv),
        TypedStatement::Break(n) => compile_break(*n, cenv),
        TypedStatement::Continue => compile_continue(cenv),
    }
}

fn compile_continue(cenv: &mut CEnv) -> AsmProg {
    let (start, adder) = cenv.continuer();
    if adder != 0 {
        vec![add(RSP, adder), jmp(start.clone())]
    } else {
        vec![jmp(start.clone())]
    }
}

fn compile_break(loops: usize, cenv: &mut CEnv) -> AsmProg {
    let (end, adder) = cenv.breaker(loops);
    if adder != 0 {
        vec![add(RSP, adder), jmp(end.clone())]
    } else {
        vec![jmp(end.clone())]
    }
}

fn compile_while(e: &TypedExpr, code_block: &TypedCodeBlock, cenv: &mut CEnv) -> AsmProg {
    let while_label = cenv.genlabel("while");
    let done = cenv.genlabel("done");
    cenv.incr_control_flow(while_label.clone(), done.clone());
    let mut asm = vec![label(while_label.clone())];
    asm.append(&mut compile_expr(e, cenv));
    asm.append(&mut vec![test(RAX, RAX), je(done.clone())]);
    asm.append(&mut compile_codeblock(code_block, cenv));
    asm.push(jmp(while_label));
    asm.push(label(done));
    cenv.decr_control_flow();
    asm
}

fn compile_assignment(ident: &String, expr: &TypedExpr, cenv: &mut CEnv) -> AsmProg {
    let mut asm = compile_expr(expr, cenv);
    asm.push(mov(mem![RSP + cenv.locate(ident)], RAX));
    asm
}

fn compile_if(
    expr: &TypedExpr,
    then: &TypedCodeBlock,
    elifs: Vec<(&TypedExpr, &TypedCodeBlock)>,
    el: &Option<TypedCodeBlock>,
    cenv: &mut CEnv,
) -> AsmProg {
    let mut asm = compile_expr(&expr, cenv);
    let done = cenv.genlabel("done");
    let skip_main = cenv.genlabel("skip");

    asm.append(&mut vec![test(RAX, RAX), je(skip_main.clone())]);
    asm.append(&mut compile_codeblock(then, cenv));
    asm.append(&mut vec![jmp(done.clone()), label(skip_main)]);

    for (e, cb) in elifs {
        asm.append(&mut compile_expr(&e, cenv));
        let skip = cenv.genlabel("skip");
        asm.append(&mut vec![test(RAX, RAX), je(skip.clone())]);
        asm.append(&mut compile_codeblock(cb, cenv));
        asm.append(&mut vec![jmp(done.clone()), label(skip)]);
    }

    if let Some(els) = el {
        asm.append(&mut compile_codeblock(els, cenv));
    }
    asm.push(label(done));
    asm
}

fn compile_codeblock(block: &TypedCodeBlock, cenv: &mut CEnv) -> AsmProg {
    cenv.new_env();
    let mut asm = block
        .iter()
        .flat_map(|s| compile_statement(s, cenv))
        .collect::<AsmProg>();
    let adder = cenv.pop_env();
    if adder > 0 {
        asm.push(add(RSP, adder));
    }
    asm
}

fn compile_let(s: &String, e: &TypedExpr, cenv: &mut CEnv) -> AsmProg {
    let mut asm = compile_expr(e, cenv);
    asm.push(push(RAX));
    cenv.insert(s.clone());
    asm
}

fn compile_expr(expr: &TypedExpr, cenv: &mut CEnv) -> AsmProg {
    match &expr.expr {
        Expr::Datum(d) => compile_datum(&d),
        Expr::Identifier(s) => compile_ident(&s, cenv),
        Expr::Op(op) => compile_op(&op, cenv),
        Expr::BuiltIn(f, p) => compile_builtin(&f, &p, cenv),
    }
}

fn compile_builtin(builtin: &BuiltIn, params: &Vec<TypedExpr>, cenv: &mut CEnv) -> AsmProg {
    let mut asm = if params.len() > 0 {
        let mut s: Vec<_> = params[..params.len() - 1]
            .iter()
            .flat_map(|p| {
                let mut v = compile_expr(p, cenv);
                v.push(push(RAX));
                v
            })
            .collect();
        s.append(&mut compile_expr(&params[params.len() - 1], cenv));
        s
    } else {
        vec![]
    };

    match builtin {
        BuiltIn::Abs => {
            let skip = cenv.genlabel("skip");
            asm.append(&mut vec![
                cmp(RAX, 0),
                jge(skip.clone()),
                neg(RAX),
                label(skip),
            ])
        }
        BuiltIn::Max => {
            for _ in 0..params.len() - 1 {
                asm.append(&mut vec![pop(RDI), cmp(RDI, RAX), cmovg(RAX, RDI)])
            }
        }
        BuiltIn::Min => {
            for _ in 0..params.len() - 1 {
                asm.append(&mut vec![pop(RDI), cmp(RDI, RAX), cmovl(RAX, RDI)])
            }
        }
        BuiltIn::Sgn => {
            let pos = cenv.genlabel("pos");
            let done = cenv.genlabel("done");
            asm.append(&mut vec![
                cmp(RAX, 0),
                jg(pos.clone()),
                je(done.clone()),
                mov(RAX, -1),
                jmp(done.clone()),
                label(pos),
                mov(RAX, 1),
                label(done),
            ])
        }
        BuiltIn::Print => {
            asm.append(&mut
                if params.len() == 0 {
                    vec![
                        mov(RAX, 1),
                        mov(RDI, RAX),
                        push(0xa),
                        mov(RSI, RSP),
                        mov(RDX, 1),
                        Syscall,
                        add(RSP, 8),
                    ]
                } else {
                    match params[0].typ {
                        Type::Int => vec![
                            push(R15),
                            mov(R15, RSP),
                            and(R15, 0b1000),
                            sub(RSP, R15),
                            external("print_int"),
                            mov(RDI, RAX),
                            call("print_int"),
                            add(RSP, R15),
                            pop(R15),
                        ],
                        Type::Bool => vec![
                            push(R15),
                            mov(R15, RSP),
                            and(R15, 0b1000),
                            sub(RSP, R15),
                            external("print_bool"),
                            mov(EDI, EAX),
                            call("print_bool"),
                            add(RSP, R15),
                            pop(R15),
                        ],
                        Type::Char => vec![
                            push(RAX),
                            mov(RDI, RSP),
                            push(R15),
                            mov(R15, RSP),
                            and(R15, 0b1000),
                            sub(RSP, R15),
                            external("print_char"),
                            call("print_char"),
                            add(RSP, R15),
                            pop(R15),
                            add(RSP, 8),
                        ],
                        Type::Unit => vec![
                            mov(RAX, 1),
                            mov(RDI, RAX),
                            push(0xA2928),
                            mov(RSI, RSP),
                            mov(RDX, 3),
                            Syscall,
                            add(RSP, 8),
                        ]
                    }
                });
            asm.push(xor(RAX, RAX));
        },
    }

    asm
}

fn compile_ident(s: &String, cenv: &CEnv) -> AsmProg {
    vec![mov(RAX, mem![RSP + cenv.locate(s)])]
}

fn compile_op2(e1: &TypedExpr, e2: &TypedExpr, cenv: &mut CEnv) -> AsmProg {
    let mut asm = compile_expr(e1, cenv);
    asm.push(push(RAX));
    cenv.increment_stack(1);
    asm.append(&mut compile_expr(e2, cenv));
    asm
}

fn compile_op(op: &TypedOps, cenv: &mut CEnv) -> AsmProg {
    match op {
        TypedOps::Ternary(c, e1, e2) => {
            let mut asm = compile_expr(c, cenv);
            let done = cenv.genlabel("done");
            let cond_neg = cenv.genlabel("cond_neg");
            asm.push(test(RAX, RAX));
            asm.push(je(cond_neg.clone()));
            asm.append(&mut compile_expr(e1, cenv));
            asm.push(jmp(done.clone()));
            asm.push(label(cond_neg));
            asm.append(&mut compile_expr(e2, cenv));
            asm.push(label(done));
            asm
        }
        TypedOps::Not(e) => {
            let mut asm = compile_expr(e, cenv);
            asm.push(xor(RAX, 1));
            asm
        }
        TypedOps::BitNot(e) => {
            let mut asm = compile_expr(e, cenv);
            asm.push(not(RAX));
            asm
        }
        TypedOps::Neg(e) => {
            let mut asm = compile_expr(e, cenv);
            asm.push(neg(RAX));
            asm
        }
        TypedOps::Pos(e) => compile_expr(e, cenv),
        TypedOps::Plus(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![pop(RDI), add(RAX, RDI)]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::Minus(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![pop(RDI), sub(RDI, RAX), mov(RAX, RDI)]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::Mul(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![pop(RDI), imul2(RAX, RDI)]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::Div(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![mov(RCX, RAX), pop(RAX), Cqo, idiv(RCX)]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::Mod(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                mov(RCX, RAX),
                pop(RAX),
                Cqo,
                idiv(RCX),
                mov(RAX, RDX),
            ]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::Eq(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                pop(RDI),
                mov(RCX, RAX),
                xor(RAX, RAX),
                mov(R8, 1),
                cmp(RCX, RDI),
                cmove(RAX, R8),
            ]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::Neq(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                pop(RDI),
                mov(RCX, RAX),
                xor(RAX, RAX),
                mov(R8, 1),
                cmp(RCX, RDI),
                cmovne(RAX, R8),
            ]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::Lt(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                pop(RDI),
                mov(RCX, RAX),
                xor(RAX, RAX),
                mov(R8, 1),
                cmp(RDI, RCX),
                cmovl(RAX, R8),
            ]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::Leq(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                pop(RDI),
                mov(RCX, RAX),
                xor(RAX, RAX),
                mov(R8, 1),
                cmp(RDI, RCX),
                cmovle(RAX, R8),
            ]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::Gt(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                pop(RDI),
                mov(RCX, RAX),
                xor(RAX, RAX),
                mov(R8, 1),
                cmp(RDI, RCX),
                cmovg(RAX, R8),
            ]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::Geq(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![
                pop(RDI),
                mov(RCX, RAX),
                xor(RAX, RAX),
                mov(R8, 1),
                cmp(RDI, RCX),
                cmovge(RAX, R8),
            ]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::And(e1, e2) => {
            let done = cenv.genlabel("done");
            let mut asm = compile_expr(e1, cenv);
            asm.append(&mut vec![test(RAX, RAX), je(done.clone())]);
            asm.append(&mut compile_expr(e2, cenv));
            asm.push(label(done));
            asm
        }
        TypedOps::Or(e1, e2) => {
            let done = cenv.genlabel("done");
            let mut asm = compile_expr(e1, cenv);
            asm.append(&mut vec![test(RAX, RAX), jne(done.clone())]);
            asm.append(&mut compile_expr(e2, cenv));
            asm.push(label(done));
            asm
        }
        TypedOps::BitAnd(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![pop(RDI), and(RAX, RDI)]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::BitOr(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![pop(RDI), or(RAX, RDI)]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::BitXor(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![pop(RDI), xor(RAX, RDI)]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::BitShiftLeft(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![mov(RCX, RAX), pop(RAX), shl(RAX, CL)]);
            cenv.decrement_stack(1);
            asm
        }
        TypedOps::BitShiftRight(e1, e2) => {
            let mut asm = compile_op2(e1, e2, cenv);
            asm.append(&mut vec![mov(RCX, RAX), pop(RAX), sar(RAX, CL)]);
            cenv.decrement_stack(1);
            asm
        }
    }
}

fn compile_datum(datum: &Datum) -> AsmProg {
    match datum {
        Datum::Int(i) => vec![mov(RAX, *i)],
        Datum::Bool(b) => vec![mov(EAX, if *b { 1 } else { 0 })],
        Datum::Char(c) => vec![mov(RAX, *c as i128)],
        Datum::Unit => vec![xor(RAX, RAX)],
    }
}
