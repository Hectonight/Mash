use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum Register {
    R64(R64),
    R32(R32),
    R16(R16),
    R8(R8),
}

macro_rules! define_registers {
    ($regname:ident { $($variant:ident),* $(,)? }) => {
        #[derive(Debug, Clone, Copy)]
        pub enum $regname { $($variant),* }

        impl std::fmt::Display for $regname {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", format!("{:?}", self).to_lowercase())
            }
        }

        impl From<$regname> for Register {
            fn from(reg: $regname) -> Register {
                Register::$regname(reg)
            }
        }

        impl From<$regname> for Operand {
            fn from(reg: $regname) -> Operand {
                Operand::Reg(Register::from(reg))
            }
        }
    };
}

impl From<Register> for Operand {
    fn from(reg: Register) -> Operand {
        Operand::Reg(reg)
    }
}

#[macro_export]
macro_rules! reg {
    (RAX)  => { ($crate::inter_rep::Register::R64($crate::inter_rep::R64::RAX)) };
    (RBX)  => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::RBX) };
    (RCX)  => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::RCX) };
    (RDX)  => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::RDX) };
    (RSI)  => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::RSI) };
    (RDI)  => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::RDI) };
    (RBP)  => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::RBP) };
    (RSP)  => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::RSP) };
    (R8)   => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::R8) };
    (R9)   => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::R9) };
    (R10)  => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::R10) };
    (R11)  => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::R11) };
    (R12)  => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::R12) };
    (R13)  => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::R13) };
    (R14)  => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::R14) };
    (R15)  => { $crate::inter_rep::Register::R64($crate::inter_rep::R64::R15) };

    (EAX)  => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::EAX) };
    (EBX)  => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::EBX) };
    (ECX)  => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::ECX) };
    (EDX)  => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::EDX) };
    (ESI)  => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::ESI) };
    (EDI)  => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::EDI) };
    (EBP)  => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::EBP) };
    (ESP)  => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::ESP) };
    (R8D)  => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::R8D) };
    (R9D)  => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::R9D) };
    (R10D) => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::R10D) };
    (R11D) => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::R11D) };
    (R12D) => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::R12D) };
    (R13D) => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::R13D) };
    (R14D) => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::R14D) };
    (R15D) => { $crate::inter_rep::Register::R32($crate::inter_rep::R32::R15D) };

    (AX)   => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::AX) };
    (BX)   => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::BX) };
    (CX)   => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::CX) };
    (DX)   => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::DX) };
    (SI)   => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::SI) };
    (DI)   => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::DI) };
    (BP)   => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::BP) };
    (SP)   => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::SP) };
    (R8W)  => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::R8W) };
    (R9W)  => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::R9W) };
    (R10W) => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::R10W) };
    (R11W) => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::R11W) };
    (R12W) => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::R12W) };
    (R13W) => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::R13W) };
    (R14W) => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::R14W) };
    (R15W) => { $crate::inter_rep::Register::R16($crate::inter_rep::R16::R15W) };

    (AL)   => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::AL) };
    (BL)   => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::BL) };
    (CL)   => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::CL) };
    (DL)   => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::DL) };
    (AH)   => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::AH) };
    (BH)   => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::BH) };
    (CH)   => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::CH) };
    (DH)   => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::DH) };
    (SIL)  => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::SIL) };
    (DIL)  => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::DIL) };
    (BPL)  => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::BPL) };
    (SPL)  => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::SPL) };
    (R8B)  => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::R8B) };
    (R9B)  => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::R9B) };
    (R10B) => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::R10B) };
    (R11B) => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::R11B) };
    (R12B) => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::R12B) };
    (R13B) => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::R13B) };
    (R14B) => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::R14B) };
    (R15B) => { $crate::inter_rep::Register::R8($crate::inter_rep::R8::R15B) };
}

define_registers!(R64 { RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP, R8, R9, R10, R11, R12, R13, R14, R15 });
define_registers!(R32 { EAX, EBX, ECX, EDX, ESI, EDI, EBP, ESP, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D });
define_registers!(R16 { AX, BX, CX, DX, SI, DI, BP, SP, R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W });
define_registers!(R8 { AL, BL, CL, DL, AH, BH, CH, DH, SIL, DIL, BPL, SPL, R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B });



impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Register::R64(v) => v.fmt(f),
            Register::R32(v) => v.fmt(f),
            Register::R16(v) => v.fmt(f),
            Register::R8(v) => v.fmt(f),
        }
    }
}


#[derive(Debug)]
pub enum IRInst {
    Global(Label),
    Section(String),
    Extern(Label),
    Label(Label),
    Mov(Operand, Operand),
    Not(Operand),
    Neg(Operand),
    Sal(Operand, Operand),
    Sar(Operand, Operand),
    Shl(Operand, Operand),
    Shr(Operand, Operand),
    Call(Operand),
    Pop(Operand),
    Push(Operand),
    Cmp(Operand, Operand),
    Sub(Operand, Operand),
    Add(Operand, Operand),
    Test(Operand, Operand),
    And(Operand, Operand),
    Or(Operand, Operand),
    Xor(Operand, Operand),
    Mul(Operand),
    IMul(Operand, Option<Operand>, Option<Imm>),
    IDiv(Operand),
    Div(Operand),
    Lea(Register, Operand),
    Inc(Operand),
    Dec(Operand),
    Syscall,
    Ret,
    Jmp(Operand),

    Je(Imm),
    Jne(Imm),
    Jl(Imm),
    Jle(Imm),
    Jg(Imm),
    Jge(Imm),
    Ja(Imm),
    Jae(Imm),
    Jb(Imm),
    Jbe(Imm),
    Jc(Imm),
    Jz(Imm),
    Jze(Imm),
    Jnl(Imm),
    Jng(Imm),
    Jnge(Imm),
    Jnle(Imm),
    Jna(Imm),
    Jnb(Imm),
    Jnae(Imm),
    Jnbe(Imm),
    Jo(Imm),
    Jp(Imm),
    Jno(Imm),
    Jnp(Imm),
    Js(Imm),
    Jpo(Imm),
    Jpe(Imm),
    Jns(Imm),
    Jnz(Imm),

    CMove(Register, Operand),
    CMovne(Register, Operand),
    CMovl(Register, Operand),
    CMovle(Register, Operand),
    CMovg(Register, Operand),
    CMovge(Register, Operand),
    CMova(Register, Operand),
    CMovae(Register, Operand),
    CMovb(Register, Operand),
    CMovbe(Register, Operand),
    CMovc(Register, Operand),
    CMovz(Register, Operand),
    CMovze(Register, Operand),
    CMovnl(Register, Operand),
    CMovng(Register, Operand),
    CMovnge(Register, Operand),
    CMovnle(Register, Operand),
    CMovna(Register, Operand),
    CMovnb(Register, Operand),
    CMovnae(Register, Operand),
    CMovnbe(Register, Operand),
    CMovo(Register, Operand),
    CMovp(Register, Operand),
    CMovno(Register, Operand),
    CMovnp(Register, Operand),
    CMovs(Register, Operand),
    CMovpo(Register, Operand),
    CMovpe(Register, Operand),
    CMovns(Register, Operand),
    CMovnz(Register, Operand)
}


#[derive(Debug, Clone)]
pub struct Mem { base: Option<SimpleOperand>, index: Option<Register>, scale: u8, displacement: Imm }

impl Display for Mem {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            &Mem { base: Some(ref base), index: None, scale: _, displacement: Imm::Int(0) } => write!(f, "[{}]", base),
            &Mem { base: None, index: Some(ref index), scale: 1, displacement: Imm::Int(0) } => write!(f, "[{}]", index),
            &Mem { base: None, index: None, scale: _, ref displacement } => write!(f, "[{}]", displacement),
            &Mem { base: Some(ref base), index: Some(ref index), scale: 1, displacement: Imm::Int(0)} => write!(f, "[{} + {}]", base, index),
            &Mem { base: Some(ref base), index: None, scale: _, ref displacement} => write!(f, "[{} + {}]", base, displacement),
            &Mem { base: None, index: Some(ref index), scale, displacement: Imm::Int(0) } => write!(f, "[{} * {}]", index, scale),
            &Mem { base: None, index: Some(ref index), scale: 1, ref displacement } => write!(f, "[{} + {}]", index, displacement),
            &Mem { base: Some(ref base), index: Some(ref index), ref scale, displacement: Imm::Int(0)} => write!(f, "[{} + {} * {}]", base, index, scale),
            &Mem { base: Some(ref base), index: Some(ref index), scale: 1, ref displacement} => write!(f, "[{} + {} + {}]", base, index, displacement),
            &Mem { base: None, index: Some(ref index), ref scale, ref displacement} => write!(f, "[{} * {} + {}]", index, scale, displacement),
            &Mem { base: Some(ref base), index: Some(ref index), ref scale, ref displacement} => write!(f, "[{} + {} * {} + {}]", base, index, scale, displacement),
        }
    }
}


#[derive(Debug, Clone)]
pub enum Operand {
    Imm(Imm),
    Reg(Register),
    Mem(Mem)
}

#[derive(Debug, Clone)]
pub enum SimpleOperand {
    Imm(Imm),
    Reg(Register),
}


#[derive(Debug, Clone)]
pub enum Imm {
    Int(i128),
    Label(Label)
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Imm(v) => v.fmt(f),
            Operand::Reg(v) => v.fmt(f),
            Operand::Mem(v) => v.fmt(f),
        }
    }
}

impl Display for SimpleOperand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SimpleOperand::Imm(v) => v.fmt(f),
            SimpleOperand::Reg(v) => v.fmt(f),
        }
    }
}

impl Display for Imm {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Imm::Int(v) => v.fmt(f),
            Imm::Label(v) => v.fmt(f),
        }
    }
}

pub type AsmProg = Vec<IRInst>;
pub type Label = String;

impl From<Mem> for Operand {
    fn from(value: Mem) -> Self {
        Operand::Mem(value)
    }
}

macro_rules! impl_int {
    ($t:ty) => {
        impl From<$t> for Imm {
            fn from(value: $t) -> Self {
                Imm::Int(value as i128)
            }
        }

        impl From<$t> for Operand {
            fn from(value: $t) -> Self {
                Operand::Imm(value.into())
            }
        }
    };
}


impl_int!(i8);
impl_int!(i16);
impl_int!(i32);
impl_int!(i64);
impl_int!(i128);
impl_int!(isize);

impl_int!(u8);
impl_int!(u16);
impl_int!(u32);
impl_int!(u64);
impl_int!(usize);

impl From<String> for Imm {
    fn from(value: String) -> Self {
        Imm::Label(value)
    }
}

impl From<String> for Operand {
    fn from(value: String) -> Self {
        Operand::Imm(Imm::from(value))
    }
}

impl From<&str> for Imm {
    fn from(value: &str) -> Self {
        Imm::Label(String::from(value))
    }
}

impl From<&str> for Operand {
    fn from(value: &str) -> Self {
        Operand::Imm(Imm::from(value))
    }
}






