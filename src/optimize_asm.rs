use crate::inter_rep::IRInst::{Add, Cmp, Lea, Mov, Ret, Sub, Xor};
use crate::inter_rep::{AsmProg, Imm, Mem, Operand, SimpleOperand};
use crate::{cmovcc, reg, opint};
use std::mem::replace;

macro_rules! take {
    ($e:expr) => {
        replace($e, Ret)
    };
}

pub fn optimize_asm(mut prog: AsmProg) -> AsmProg {
    let mut asm = vec![];
    let mut s = prog.as_mut_slice();
    // let mut index;
    loop {
        /*
        Many of these cases may never come up (for example mov a, a), but they are included anyway

        Currently new optimizable patterns can be created by this, have not optimized this for now
        Ex:
        mov rax, 1
        mov rdi, rdi
        mov rax, 2
        =>
        mov rax, 1
        mov rax, 2

        but a better optimization would be
        mov rax, 2
         */
        match s {
         [Sub(reg!(r1), reg!(r2)), ..] if r1 == r2 => {
             let Sub(r, _) = take!(&mut s[0]) else { unreachable!() };
             asm.push(Xor(r.clone(), r));
             s = &mut s[1..];
         }
            [Mov(reg!(r1), _) | cmovcc!(r1, _), Mov(reg!(r2), _), ..]
            | [Mov(reg!(r1), reg!(r2)) | cmovcc!(r1, reg!(r2)), ..]
            | [Lea(r1,
                   // I will create a macro soon, current macro only works for expr
                   Operand::Mem(Mem {
                       base: Some(SimpleOperand::Reg(r2)),
                       index: None, scale: 1,
                       displacement: Imm::Int(0)
                   })), ..]
            if r1 == r2 => {
             s = &mut s[1..];
         }
         [Sub(reg!(_), opint!(0)) | Add(reg!(_), opint!(0)), ..] => {
             let Sub(r, _) = take!(&mut s[0]) else { unreachable!() };
             asm.push(Cmp(r, opint!(0)));
             s = &mut s[1..];
         }
         [_, ..] => {
             asm.push(take!(&mut s[0]));
             s = &mut s[1..];
         }
         [] => break,
     }
    }
    asm
}

