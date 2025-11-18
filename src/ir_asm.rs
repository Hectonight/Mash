use logos::source;
use crate::inter_rep::{AsmProg, IRInst, Operand, Register};
use crate::inter_rep::Imm::{Int, Label as LabelO};
use crate::inter_rep::IRInst::*;
use crate::inter_rep::Operand::{Imm, Mem, Reg};
use crate::inter_rep::R8::CL;
use crate::inter_rep::Register::{R8, R16, R32, R64};
use crate::reg;

pub fn ir_to_asm(program: AsmProg) -> String {
    let s = program.iter()
        .map(instr_to_asm)
        .collect::<Vec<String>>()
        .join("\n");
    s
}

pub fn instr_to_asm(instr: &IRInst) -> String {

    match instr {
        | Mov(dest @ Reg(R8(_)), source @ Reg(R8(_)))
        | Mov(dest @ Reg(R16(_)), source @ Reg(R16(_)))
        | Mov(dest @ Reg(R32(_)), source @ Reg(R32(_)))
        | Mov(dest @ Reg(R64(_)), source @ Reg(R64(_)))
        | Mov(dest @ Reg(_), source @ Mem(_))
        | Mov(dest @ Reg(_), source @ Imm(_))
        | Mov(dest @ Mem(_), source @ Reg(_))
        => format!("mov {}, {}", dest, source),
        | Mov(dest @ Mem(_), source @ Imm(_))
        => format!("mov qword {}, {}", dest, source),

        | Not(r) => format!("not {}", r),
        | Neg(r) => format!("neg {}", r),
        | Extern(s) => format!("extern {}", s),
        | Label(s) => format!("{}:", s),
        | Global(s) => format!("global {}", s),
        | Section(s) => format!("section {}", s),
        | Syscall => String::from("syscall"),
        | Ret => String::from("ret"),
        | Call(s) => format!("call {}", s),

        | Sal(dest @ Reg(_), source @ Reg(reg!(CL)))
        | Sal(dest @ Reg(_), source @ Imm(_))
        => format!("sal {}, {}", dest, source),
        | Sal(dest @ Mem(_), source @ Reg(reg!(CL)))
        | Sal(dest @ Mem(_), source @ Imm(_))
        => format!("sal qword {}, {}", dest, source),

        | Shl(dest @ Reg(_), source @ Reg(reg!(CL)))
        | Shl(dest @ Reg(_), source @ Imm(_))
        => format!("shl {}, {}", dest, source),
        | Shl(dest @ Mem(_), source @ Reg(reg!(CL)))
        | Shl(dest @ Mem(_), source @ Imm(_))
        => format!("shl qword {}, {}", dest, source),

        | Sar(dest @ Reg(_), source @ Reg(reg!(CL)))
        | Sar(dest @ Reg(_), source @ Imm(_))
        => format!("sar {}, {}", dest, source),
        | Sar(dest @ Mem(_), source @ Reg(reg!(CL)))
        | Sar(dest @ Mem(_), source @ Imm(_))
        => format!("sar qword {}, {}", dest, source),

        | Shr(dest @ Reg(_), source @ Reg(reg!(CL)))
        | Shr(dest @ Reg(_), source @ Imm(_))
        => format!("shr {}, {}", dest, source),
        | Shr(dest @ Mem(_), source @ Reg(reg!(CL)))
        | Shr(dest @ Mem(_), source @ Imm(_))
        => format!("shr qword {}, {}", dest, source),

        | Test(dest @ Reg(R8(_)), source @ Reg(R8(_)))
        | Test(dest @ Reg(R16(_)), source @ Reg(R16(_)))
        | Test(dest @ Reg(R32(_)), source @ Reg(R32(_)))
        | Test(dest @ Reg(R64(_)), source @ Reg(R64(_)))
        | Test(dest @ Reg(_), source @ Imm(_))
        | Test(dest @ Reg(_), source @ Mem(_))
        | Test(dest @ Mem(_), source @ Reg(_))
        => format!("test {}, {}", dest, source),
        | Test(dest @ Mem(_), source @ Imm(_))
        => format!("test qword {}, {}", dest, source),

        | And(dest @ Reg(R8(_)), source @ Reg(R8(_)))
        | And(dest @ Reg(R16(_)), source @ Reg(R16(_)))
        | And(dest @ Reg(R32(_)), source @ Reg(R32(_)))
        | And(dest @ Reg(R64(_)), source @ Reg(R64(_)))
        | And(dest @ Reg(_), source @ Imm(_))
        | And(dest @ Reg(_), source @ Mem(_))
        | And(dest @ Mem(_), source @ Reg(_))
        => format!("and {}, {}", dest, source),
        | And(dest @ Mem(_), source @ Imm(_))
        => format!("and qword {}, {}", dest, source),

        | Or(dest @ Reg(R8(_)), source @ Reg(R8(_)))
        | Or(dest @ Reg(R16(_)), source @ Reg(R16(_)))
        | Or(dest @ Reg(R32(_)), source @ Reg(R32(_)))
        | Or(dest @ Reg(R64(_)), source @ Reg(R64(_)))
        | Or(dest @ Reg(_), source @ Imm(_))
        | Or(dest @ Reg(_), source @ Mem(_))
        | Or(dest @ Mem(_), source @ Reg(_))
        => format!("or {}, {}", dest, source),
        | Or(dest @ Mem(_), source @ Imm(_))
        => format!("or qword {}, {}", dest, source),

        | Xor(dest @ Reg(R8(_)), source @ Reg(R8(_)))
        | Xor(dest @ Reg(R16(_)), source @ Reg(R16(_)))
        | Xor(dest @ Reg(R32(_)), source @ Reg(R32(_)))
        | Xor(dest @ Reg(R64(_)), source @ Reg(R64(_)))
        | Xor(dest @ Reg(_), source @ Imm(_))
        | Xor(dest @ Reg(_), source @ Mem(_))
        | Xor(dest @ Mem(_), source @ Reg(_))
        => format!("xor {}, {}", dest, source),
        | Xor(dest @ Mem(_), source @ Imm(_))
        => format!("xor qword {}, {}", dest, source),

        | Add(dest @ Reg(R8(_)), source @ Reg(R8(_)))
        | Add(dest @ Reg(R16(_)), source @ Reg(R16(_)))
        | Add(dest @ Reg(R32(_)), source @ Reg(R32(_)))
        | Add(dest @ Reg(R64(_)), source @ Reg(R64(_)))
        | Add(dest @ Reg(_), source @ Imm(_))
        | Add(dest @ Reg(_), source @ Mem(_))
        | Add(dest @ Mem(_), source @ Reg(_))
        => format!("add {}, {}", dest, source),
        | Add(dest @ Mem(_), source @ Imm(_))
        => format!("add qword {}, {}", dest, source),

        | Sub(dest @ Reg(R8(_)), source @ Reg(R8(_)))
        | Sub(dest @ Reg(R16(_)), source @ Reg(R16(_)))
        | Sub(dest @ Reg(R32(_)), source @ Reg(R32(_)))
        | Sub(dest @ Reg(R64(_)), source @ Reg(R64(_)))
        | Sub(dest @ Reg(_), source @ Imm(_))
        | Sub(dest @ Reg(_), source @ Mem(_))
        | Sub(dest @ Mem(_), source @ Reg(_))
        => format!("cmp {}, {}", dest, source),
        | Sub(dest @ Mem(_), source @ Imm(_))
        => format!("cmp qword {}, {}", dest, source),

        | Cmp(dest @ Reg(R8(_)), source @ Reg(R8(_)))
        | Cmp(dest @ Reg(R16(_)), source @ Reg(R16(_)))
        | Cmp(dest @ Reg(R32(_)), source @ Reg(R32(_)))
        | Cmp(dest @ Reg(R64(_)), source @ Reg(R64(_)))
        | Cmp(dest @ Reg(_), source @ Imm(_))
        | Cmp(dest @ Reg(_), source @ Mem(_))
        | Cmp(dest @ Mem(_), source @ Reg(_))
        => format!("cmp {}, {}", dest, source),
        | Cmp(dest @ Mem(_), source @ Imm(_))
        => format!("cmp qword {}, {}", dest, source),

        | Pop(dest @ Reg(R16(_)))
        | Pop(dest @ Reg(R64(_)))
        => format!("pop {}", dest),
        | Pop(Mem(dest))
        => format!("pop qword {}", dest),

        | Push(dest @ Reg(R16(_)))
        | Push(dest @ Reg(R64(_)))
        | Push(dest @ Imm(_))
        => format!("push {}", dest),
        | Push(Mem(dest)) => format!("push qword {}", dest),

        | Inc(dest @ Reg(_))
        | Inc(dest @ Mem(_))
        => format!("inc {}", dest),

        | Dec(dest @ Reg(_))
        | Dec(dest @ Mem(_))
        => format!("dec {}", dest),

        | Mul(Reg(dest)) => format!("mul {}", dest),
        | Mul(Mem(dest)) => format!("mul qword {}", dest),

        | Div(Reg(dest)) => format!("div {}", dest),
        | Div(Mem(dest)) => format!("div qword {}", dest),

        | IDiv(Reg(dest)) => format!("idiv {}", dest),
        | IDiv(Mem(dest)) => format!("idiv qword {}", dest),

        | IMul(Reg(dest), None, None)
        => format!("mul {}", dest),
        | IMul(Reg(dest @ R16(_)), Some(source @ Reg(R16(_))), None)
        | IMul(Reg(dest @ R16(_)), Some(source @ Mem(_)), None)
        | IMul(Reg(dest @ R16(_)), Some(source @ Imm(_)), None)
        | IMul(Reg(dest @ R32(_)), Some(source @ Reg(R32(_))), None)
        | IMul(Reg(dest @ R32(_)), Some(source @ Mem(_)), None)
        | IMul(Reg(dest @ R32(_)), Some(source @ Imm(_)), None)
        | IMul(Reg(dest @ R64(_)), Some(source @ Reg(R64(_))), None)
        | IMul(Reg(dest @ R64(_)), Some(source @ Mem(_)), None)
        | IMul(Reg(dest @ R64(_)), Some(source @ Imm(_)), None)
        => format!("imul {}, {}", dest, source),
        | IMul(Mem(dest), None, None)
        => format!("mul qword {}", dest),
        | IMul(Reg(dest @ R16(_)), Some(source @ Reg(R16(_))), Some(i))
        | IMul(Reg(dest @ R16(_)), Some(source @ Mem(_)), Some(i))
        | IMul(Reg(dest @ R32(_)), Some(source @ Reg(R32(_))), Some(i))
        | IMul(Reg(dest @ R32(_)), Some(source @ Mem(_)), Some(i))
        | IMul(Reg(dest @ R64(_)), Some(source @ Reg(R64(_))), Some(i))
        | IMul(Reg(dest @ R64(_)), Some(source @ Mem(_)), Some(i))
        => format!("mul {}, {} {}", dest, source, i),

        | Lea(dest @ R16(_), source @ Imm(_))
        | Lea(dest @ R16(_), source @ Mem(_))
        | Lea(dest @ R32(_), source @ Imm(_))
        | Lea(dest @ R32(_), source @ Mem(_))
        | Lea(dest @ R64(_), source @ Imm(_))
        | Lea(dest @ R64(_), source @ Mem(_))
        => format!("lea {}, {}", dest, source),

        | Jmp(dest @ Imm(_))
        | Jmp(dest @ Mem(_))
        | Jmp(dest @ Reg(R16(_)))
        | Jmp(dest @ Reg(R32(_)))
        | Jmp(dest @ Reg(R64(_)))
        => format!("jmp {}", dest),

        | Je(dest) => format!("je {}", dest),
        | Jne(dest) => format!("jne {}", dest),
        | Jl(dest) => format!("jl {}", dest),
        | Jle(dest) => format!("jle {}", dest),
        | Jg(dest) => format!("jg {}", dest),
        | Jge(dest) => format!("jge {}", dest),
        | Ja(dest) => format!("ja {}", dest),
        | Jae(dest) => format!("jae {}", dest),
        | Jb(dest) => format!("jb {}", dest),
        | Jbe(dest) => format!("jbe {}", dest),
        | Jc(dest) => format!("jc {}", dest),
        | Jz(dest) => format!("jz {}", dest),
        | Jze(dest) => format!("jze {}", dest),
        | Jnl(dest) => format!("jnl {}", dest),
        | Jng(dest) => format!("jng {}", dest),
        | Jnge(dest) => format!("jnge {}", dest),
        | Jnle(dest) => format!("jnle {}", dest),
        | Jna(dest) => format!("jna {}", dest),
        | Jnb(dest) => format!("jnb {}", dest),
        | Jnae(dest) => format!("jnae {}", dest),
        | Jnbe(dest) => format!("jnbe {}", dest),
        | Jo(dest) => format!("jo {}", dest),
        | Jp(dest) => format!("jp {}", dest),
        | Jno(dest) => format!("jno {}", dest),
        | Jnp(dest) => format!("jnp {}", dest),
        | Js(dest) => format!("js {}", dest),
        | Jpo(dest) => format!("jpo {}", dest),
        | Jpe(dest) => format!("jpe {}", dest),
        | Jns(dest) => format!("jns {}", dest),
        | Jnz(dest) => format!("jnz {}", dest),


        | CMove(dest @ R16(_), source @ Reg(R16(_)))
        | CMove(dest @ R16(_), source @ Mem(_))
        | CMove(dest @ R32(_), source @ Reg(R32(_)))
        | CMove(dest @ R32(_), source @ Mem(_))
        | CMove(dest @ R64(_), source @ Reg(R64(_)))
        | CMove(dest @ R64(_), source @ Mem(_))
        => format!("cmove {}, {}", dest, source),

        | CMovne(dest @ R16(_), source @ Reg(R16(_)))
        | CMovne(dest @ R16(_), source @ Mem(_))
        | CMovne(dest @ R32(_), source @ Reg(R32(_)))
        | CMovne(dest @ R32(_), source @ Mem(_))
        | CMovne(dest @ R64(_), source @ Reg(R64(_)))
        | CMovne(dest @ R64(_), source @ Mem(_))
        => format!("cmovne {}, {}", dest, source),

        | CMovl(dest @ R16(_), source @ Reg(R16(_)))
        | CMovl(dest @ R16(_), source @ Mem(_))
        | CMovl(dest @ R32(_), source @ Reg(R32(_)))
        | CMovl(dest @ R32(_), source @ Mem(_))
        | CMovl(dest @ R64(_), source @ Reg(R64(_)))
        | CMovl(dest @ R64(_), source @ Mem(_))
        => format!("cmovl {}, {}", dest, source),

        | CMovle(dest @ R16(_), source @ Reg(R16(_)))
        | CMovle(dest @ R16(_), source @ Mem(_))
        | CMovle(dest @ R32(_), source @ Reg(R32(_)))
        | CMovle(dest @ R32(_), source @ Mem(_))
        | CMovle(dest @ R64(_), source @ Reg(R64(_)))
        | CMovle(dest @ R64(_), source @ Mem(_))
        => format!("cmovle {}, {}", dest, source),

        | CMovg(dest @ R16(_), source @ Reg(R16(_)))
        | CMovg(dest @ R16(_), source @ Mem(_))
        | CMovg(dest @ R32(_), source @ Reg(R32(_)))
        | CMovg(dest @ R32(_), source @ Mem(_))
        | CMovg(dest @ R64(_), source @ Reg(R64(_)))
        | CMovg(dest @ R64(_), source @ Mem(_))
        => format!("cmovg {}, {}", dest, source),

        | CMovge(dest @ R16(_), source @ Reg(R16(_)))
        | CMovge(dest @ R16(_), source @ Mem(_))
        | CMovge(dest @ R32(_), source @ Reg(R32(_)))
        | CMovge(dest @ R32(_), source @ Mem(_))
        | CMovge(dest @ R64(_), source @ Reg(R64(_)))
        | CMovge(dest @ R64(_), source @ Mem(_))
        => format!("cmovge {}, {}", dest, source),

        | CMova(dest @ R16(_), source @ Reg(R16(_)))
        | CMova(dest @ R16(_), source @ Mem(_))
        | CMova(dest @ R32(_), source @ Reg(R32(_)))
        | CMova(dest @ R32(_), source @ Mem(_))
        | CMova(dest @ R64(_), source @ Reg(R64(_)))
        | CMova(dest @ R64(_), source @ Mem(_))
        => format!("cmova {}, {}", dest, source),

        | CMovae(dest @ R16(_), source @ Reg(R16(_)))
        | CMovae(dest @ R16(_), source @ Mem(_))
        | CMovae(dest @ R32(_), source @ Reg(R32(_)))
        | CMovae(dest @ R32(_), source @ Mem(_))
        | CMovae(dest @ R64(_), source @ Reg(R64(_)))
        | CMovae(dest @ R64(_), source @ Mem(_))
        => format!("cmovae {}, {}", dest, source),

        | CMovb(dest @ R16(_), source @ Reg(R16(_)))
        | CMovb(dest @ R16(_), source @ Mem(_))
        | CMovb(dest @ R32(_), source @ Reg(R32(_)))
        | CMovb(dest @ R32(_), source @ Mem(_))
        | CMovb(dest @ R64(_), source @ Reg(R64(_)))
        | CMovb(dest @ R64(_), source @ Mem(_))
        => format!("cmovb {}, {}", dest, source),

        | CMovbe(dest @ R16(_), source @ Reg(R16(_)))
        | CMovbe(dest @ R16(_), source @ Mem(_))
        | CMovbe(dest @ R32(_), source @ Reg(R32(_)))
        | CMovbe(dest @ R32(_), source @ Mem(_))
        | CMovbe(dest @ R64(_), source @ Reg(R64(_)))
        | CMovbe(dest @ R64(_), source @ Mem(_))
        => format!("cmovbe {}, {}", dest, source),

        | CMovc(dest @ R16(_), source @ Reg(R16(_)))
        | CMovc(dest @ R16(_), source @ Mem(_))
        | CMovc(dest @ R32(_), source @ Reg(R32(_)))
        | CMovc(dest @ R32(_), source @ Mem(_))
        | CMovc(dest @ R64(_), source @ Reg(R64(_)))
        | CMovc(dest @ R64(_), source @ Mem(_))
        => format!("cmovc {}, {}", dest, source),

        | CMovz(dest @ R16(_), source @ Reg(R16(_)))
        | CMovz(dest @ R16(_), source @ Mem(_))
        | CMovz(dest @ R32(_), source @ Reg(R32(_)))
        | CMovz(dest @ R32(_), source @ Mem(_))
        | CMovz(dest @ R64(_), source @ Reg(R64(_)))
        | CMovz(dest @ R64(_), source @ Mem(_))
        => format!("cmovz {}, {}", dest, source),

        | CMovze(dest @ R16(_), source @ Reg(R16(_)))
        | CMovze(dest @ R16(_), source @ Mem(_))
        | CMovze(dest @ R32(_), source @ Reg(R32(_)))
        | CMovze(dest @ R32(_), source @ Mem(_))
        | CMovze(dest @ R64(_), source @ Reg(R64(_)))
        | CMovze(dest @ R64(_), source @ Mem(_))
        => format!("cmovze {}, {}", dest, source),

        | CMovnl(dest @ R16(_), source @ Reg(R16(_)))
        | CMovnl(dest @ R16(_), source @ Mem(_))
        | CMovnl(dest @ R32(_), source @ Reg(R32(_)))
        | CMovnl(dest @ R32(_), source @ Mem(_))
        | CMovnl(dest @ R64(_), source @ Reg(R64(_)))
        | CMovnl(dest @ R64(_), source @ Mem(_))
        => format!("cmovnl {}, {}", dest, source),

        | CMovng(dest @ R16(_), source @ Reg(R16(_)))
        | CMovng(dest @ R16(_), source @ Mem(_))
        | CMovng(dest @ R32(_), source @ Reg(R32(_)))
        | CMovng(dest @ R32(_), source @ Mem(_))
        | CMovng(dest @ R64(_), source @ Reg(R64(_)))
        | CMovng(dest @ R64(_), source @ Mem(_))
        => format!("cmovng {}, {}", dest, source),

        | CMovnge(dest @ R16(_), source @ Reg(R16(_)))
        | CMovnge(dest @ R16(_), source @ Mem(_))
        | CMovnge(dest @ R32(_), source @ Reg(R32(_)))
        | CMovnge(dest @ R32(_), source @ Mem(_))
        | CMovnge(dest @ R64(_), source @ Reg(R64(_)))
        | CMovnge(dest @ R64(_), source @ Mem(_))
        => format!("cmovnge {}, {}", dest, source),

        | CMovnle(dest @ R16(_), source @ Reg(R16(_)))
        | CMovnle(dest @ R16(_), source @ Mem(_))
        | CMovnle(dest @ R32(_), source @ Reg(R32(_)))
        | CMovnle(dest @ R32(_), source @ Mem(_))
        | CMovnle(dest @ R64(_), source @ Reg(R64(_)))
        | CMovnle(dest @ R64(_), source @ Mem(_))
        => format!("cmovnle {}, {}", dest, source),

        | CMovna(dest @ R16(_), source @ Reg(R16(_)))
        | CMovna(dest @ R16(_), source @ Mem(_))
        | CMovna(dest @ R32(_), source @ Reg(R32(_)))
        | CMovna(dest @ R32(_), source @ Mem(_))
        | CMovna(dest @ R64(_), source @ Reg(R64(_)))
        | CMovna(dest @ R64(_), source @ Mem(_))
        => format!("cmovna {}, {}", dest, source),

        | CMovnb(dest @ R16(_), source @ Reg(R16(_)))
        | CMovnb(dest @ R16(_), source @ Mem(_))
        | CMovnb(dest @ R32(_), source @ Reg(R32(_)))
        | CMovnb(dest @ R32(_), source @ Mem(_))
        | CMovnb(dest @ R64(_), source @ Reg(R64(_)))
        | CMovnb(dest @ R64(_), source @ Mem(_))
        => format!("cmovnb {}, {}", dest, source),

        | CMovnae(dest @ R16(_), source @ Reg(R16(_)))
        | CMovnae(dest @ R16(_), source @ Mem(_))
        | CMovnae(dest @ R32(_), source @ Reg(R32(_)))
        | CMovnae(dest @ R32(_), source @ Mem(_))
        | CMovnae(dest @ R64(_), source @ Reg(R64(_)))
        | CMovnae(dest @ R64(_), source @ Mem(_))
        => format!("cmovnae {}, {}", dest, source),

        | CMovnbe(dest @ R16(_), source @ Reg(R16(_)))
        | CMovnbe(dest @ R16(_), source @ Mem(_))
        | CMovnbe(dest @ R32(_), source @ Reg(R32(_)))
        | CMovnbe(dest @ R32(_), source @ Mem(_))
        | CMovnbe(dest @ R64(_), source @ Reg(R64(_)))
        | CMovnbe(dest @ R64(_), source @ Mem(_))
        => format!("cmovnbe {}, {}", dest, source),

        | CMovo(dest @ R16(_), source @ Reg(R16(_)))
        | CMovo(dest @ R16(_), source @ Mem(_))
        | CMovo(dest @ R32(_), source @ Reg(R32(_)))
        | CMovo(dest @ R32(_), source @ Mem(_))
        | CMovo(dest @ R64(_), source @ Reg(R64(_)))
        | CMovo(dest @ R64(_), source @ Mem(_))
        => format!("cmovo {}, {}", dest, source),

        | CMovp(dest @ R16(_), source @ Reg(R16(_)))
        | CMovp(dest @ R16(_), source @ Mem(_))
        | CMovp(dest @ R32(_), source @ Reg(R32(_)))
        | CMovp(dest @ R32(_), source @ Mem(_))
        | CMovp(dest @ R64(_), source @ Reg(R64(_)))
        | CMovp(dest @ R64(_), source @ Mem(_))
        => format!("cmovp {}, {}", dest, source),

        | CMovno(dest @ R16(_), source @ Reg(R16(_)))
        | CMovno(dest @ R16(_), source @ Mem(_))
        | CMovno(dest @ R32(_), source @ Reg(R32(_)))
        | CMovno(dest @ R32(_), source @ Mem(_))
        | CMovno(dest @ R64(_), source @ Reg(R64(_)))
        | CMovno(dest @ R64(_), source @ Mem(_))
        => format!("cmovno {}, {}", dest, source),

        | CMovnp(dest @ R16(_), source @ Reg(R16(_)))
        | CMovnp(dest @ R16(_), source @ Mem(_))
        | CMovnp(dest @ R32(_), source @ Reg(R32(_)))
        | CMovnp(dest @ R32(_), source @ Mem(_))
        | CMovnp(dest @ R64(_), source @ Reg(R64(_)))
        | CMovnp(dest @ R64(_), source @ Mem(_))
        => format!("cmovnp {}, {}", dest, source),

        | CMovs(dest @ R16(_), source @ Reg(R16(_)))
        | CMovs(dest @ R16(_), source @ Mem(_))
        | CMovs(dest @ R32(_), source @ Reg(R32(_)))
        | CMovs(dest @ R32(_), source @ Mem(_))
        | CMovs(dest @ R64(_), source @ Reg(R64(_)))
        | CMovs(dest @ R64(_), source @ Mem(_))
        => format!("cmovs {}, {}", dest, source),

        | CMovpo(dest @ R16(_), source @ Reg(R16(_)))
        | CMovpo(dest @ R16(_), source @ Mem(_))
        | CMovpo(dest @ R32(_), source @ Reg(R32(_)))
        | CMovpo(dest @ R32(_), source @ Mem(_))
        | CMovpo(dest @ R64(_), source @ Reg(R64(_)))
        | CMovpo(dest @ R64(_), source @ Mem(_))
        => format!("cmovpo {}, {}", dest, source),

        | CMovpe(dest @ R16(_), source @ Reg(R16(_)))
        | CMovpe(dest @ R16(_), source @ Mem(_))
        | CMovpe(dest @ R32(_), source @ Reg(R32(_)))
        | CMovpe(dest @ R32(_), source @ Mem(_))
        | CMovpe(dest @ R64(_), source @ Reg(R64(_)))
        | CMovpe(dest @ R64(_), source @ Mem(_))
        => format!("cmovpe {}, {}", dest, source),

        | CMovns(dest @ R16(_), source @ Reg(R16(_)))
        | CMovns(dest @ R16(_), source @ Mem(_))
        | CMovns(dest @ R32(_), source @ Reg(R32(_)))
        | CMovns(dest @ R32(_), source @ Mem(_))
        | CMovns(dest @ R64(_), source @ Reg(R64(_)))
        | CMovns(dest @ R64(_), source @ Mem(_))
        => format!("cmovns {}, {}", dest, source),

        | CMovnz(dest @ R16(_), source @ Reg(R16(_)))
        | CMovnz(dest @ R16(_), source @ Mem(_))
        | CMovnz(dest @ R32(_), source @ Reg(R32(_)))
        | CMovnz(dest @ R32(_), source @ Mem(_))
        | CMovnz(dest @ R64(_), source @ Reg(R64(_)))
        | CMovnz(dest @ R64(_), source @ Mem(_))
        => format!("cmovnz {}, {}", dest, source),

        _ => unimplemented!("No match for {:?}", instr)
    }
}