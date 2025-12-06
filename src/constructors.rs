#![allow(dead_code)]
use crate::inter_rep::IRInst::IMul;
use crate::inter_rep::{IRInst, Imm, Label, Operand, Register};

#[macro_export]
macro_rules! make_op_op {
    ($name:ident, $instr_name:ident) => {
        pub fn $name<A: Into<Operand>, B: Into<Operand>>(a: A, b: B) -> IRInst {
            IRInst::$instr_name(a.into(), b.into())
        }
    };
}
macro_rules! make_op {
    ($name:ident, $instr_name:ident) => {
        pub fn $name<A: Into<Operand>>(a: A) -> IRInst {
            IRInst::$instr_name(a.into())
        }
    };
}

macro_rules! make_reg_op {
    ($name:ident, $instr_name:ident) => {
        pub fn $name<A: Into<Register>, B: Into<Operand>>(a: A, b: B) -> IRInst {
            IRInst::$instr_name(a.into(), b.into())
        }
    };
}

macro_rules! make_imm {
    ($name:ident, $instr_name:ident) => {
        pub fn $name<A: Into<Imm>>(a: A) -> IRInst {
            IRInst::$instr_name(a.into())
        }
    };
}

macro_rules! make_label {
    ($name:ident, $instr_name:ident) => {
        pub fn $name<A: Into<Label>>(a: A) -> IRInst {
            IRInst::$instr_name(a.into())
        }
    };
}

pub fn imul<A: Into<Operand>, B: Into<Operand>, C: Into<Imm>>(
    op1: A,
    op2: Option<B>,
    imm: Option<C>,
) -> IRInst {
    let oper1 = op1.into();
    let oper2 = op2.map(Into::into);
    let i = imm.map(Into::into);
    IMul(oper1, oper2, i)
}

pub fn imul3<A: Into<Operand>, B: Into<Operand>, C: Into<Imm>>(op1: A, op2: B, imm: C) -> IRInst {
    let oper1 = op1.into();
    let oper2 = op2.into();
    let i = imm.into();
    IMul(oper1, Some(oper2), Some(i))
}

pub fn imul2<A: Into<Operand>, B: Into<Operand>>(op1: A, op2: B) -> IRInst {
    let oper1 = op1.into();
    let oper2 = op2.into();
    IMul(oper1, Some(oper2), None)
}

pub fn imul1<A: Into<Operand>>(op1: A) -> IRInst {
    let oper1 = op1.into();
    IMul(oper1, None, None)
}

make_op_op!(mov, Mov);
make_op_op!(add, Add);
make_op_op!(sub, Sub);
make_op_op!(cmp, Cmp);
make_op_op!(test, Test);
make_op_op!(and, And);
make_op_op!(or, Or);
make_op_op!(xor, Xor);
make_op_op!(shl, Shl);
make_op_op!(shr, Shr);
make_op_op!(sal, Sal);
make_op_op!(sar, Sar);

make_reg_op!(lea, Lea);
make_reg_op!(cmove, CMove);
make_reg_op!(cmovne, CMovne);
make_reg_op!(cmovl, CMovl);
make_reg_op!(cmovle, CMovle);
make_reg_op!(cmovg, CMovg);
make_reg_op!(cmovge, CMovge);
make_reg_op!(cmova, CMova);
make_reg_op!(cmovae, CMovae);
make_reg_op!(cmovb, CMovb);
make_reg_op!(cmovbe, CMovbe);
make_reg_op!(cmovc, CMovc);
make_reg_op!(cmovz, CMovz);
make_reg_op!(cmovze, CMovze);
make_reg_op!(cmovnl, CMovnl);
make_reg_op!(cmovng, CMovng);
make_reg_op!(cmovnge, CMovnge);
make_reg_op!(cmovnle, CMovnle);
make_reg_op!(cmovna, CMovna);
make_reg_op!(cmovnb, CMovnb);
make_reg_op!(cmovnae, CMovnae);
make_reg_op!(cmovnbe, CMovnbe);
make_reg_op!(cmovo, CMovo);
make_reg_op!(cmovp, CMovp);
make_reg_op!(cmovno, CMovno);
make_reg_op!(cmovnp, CMovnp);
make_reg_op!(cmovs, CMovs);
make_reg_op!(cmovpo, CMovpo);
make_reg_op!(cmovpe, CMovpe);
make_reg_op!(cmovns, CMovns);
make_reg_op!(cmovnz, CMovnz);

make_label!(global, Global);
make_label!(label, Label);
make_label!(section, Section);
make_label!(external, Extern);

make_imm!(je, Je);
make_imm!(jne, Jne);
make_imm!(jl, Jl);
make_imm!(jle, Jle);
make_imm!(jg, Jg);
make_imm!(jge, Jge);
make_imm!(ja, Ja);
make_imm!(jae, Jae);
make_imm!(jb, Jb);
make_imm!(jbe, Jbe);
make_imm!(jc, Jc);
make_imm!(jz, Jz);
make_imm!(jze, Jze);
make_imm!(jnl, Jnl);
make_imm!(jng, Jng);
make_imm!(jnge, Jnge);
make_imm!(jnle, Jnle);
make_imm!(jna, Jna);
make_imm!(jnb, Jnb);
make_imm!(jnae, Jnae);
make_imm!(jnbe, Jnbe);
make_imm!(jo, Jo);
make_imm!(jp, Jp);
make_imm!(jno, Jno);
make_imm!(jnp, Jnp);
make_imm!(js, Js);
make_imm!(jpo, Jpo);
make_imm!(jpe, Jpe);
make_imm!(jns, Jns);
make_imm!(jnz, Jnz);

make_op!(not, Not);
make_op!(neg, Neg);
make_op!(mul, Mul);
make_op!(div, Div);
make_op!(idiv, IDiv);
make_op!(jmp, Jmp);
make_op!(call, Call);
make_op!(pop, Pop);
make_op!(push, Push);
make_op!(inc, Inc);
make_op!(dec, Dec);

#[macro_export]
macro_rules! int {
    ($d:literal) => {
        $crate::types::TypedExpr {
            typ: $crate::types::Type::Int,
            expr: $crate::types::Expr::Datum($crate::types::Datum::Int($d)),
        }
    };
    (_) => {
        $crate::types::TypedExpr {
            typ: $crate::types::Type::Int,
            expr: $crate::types::Expr::Datum($crate::types::Datum::Int(_)),
        }
    };
    ($id:ident) => {
        $crate::types::TypedExpr {
            typ: $crate::types::Type::Int,
            expr: $crate::types::Expr::Datum($crate::types::Datum::Int($id)),
        }
    };
    ($e:expr) => {
        $crate::types::TypedExpr {
            typ: $crate::types::Type::Int,
            expr: $crate::types::Expr::Datum($crate::types::Datum::Int($e)),
        }
    };
}

#[macro_export]
macro_rules! datum {
    (_) => {
        $crate::types::TypedExpr {
            typ: _,
            expr: $crate::types::Expr::Datum(_),
        }
    };
    ($d:ident) => {
        $crate::types::TypedExpr {
            typ: _,
            expr: $crate::types::Expr::Datum($d),
        }
    };
    ($d:expr) => {
        $crate::types::TypedExpr {
            typ: _,
            expr: $crate::types::Expr::Datum($d),
        }
    };
}

#[macro_export]
macro_rules! bool {
    (true) => {
        $crate::types::TypedExpr {
            typ: $crate::types::Type::Bool,
            expr: $crate::types::Expr::Datum($crate::types::Datum::Bool(true)),
        }
    };
    (false) => {
        $crate::types::TypedExpr {
            typ: $crate::types::Type::Bool,
            expr: $crate::types::Expr::Datum($crate::types::Datum::Bool(false)),
        }
    };
    (_) => {
        $crate::types::TypedExpr {
            typ: $crate::types::Type::Bool,
            expr: $crate::types::Expr::Datum($crate::types::Datum::Bool(_)),
        }
    };
    ($id:ident) => {
        $crate::types::TypedExpr {
            typ: $crate::types::Type::Bool,
            expr: $crate::types::Expr::Datum($crate::types::Datum::Bool($id)),
        }
    };
    ($e:expr) => {
        $crate::types::TypedExpr {
            typ: $crate::types::Type::Bool,
            expr: $crate::types::Expr::Datum($crate::types::Datum::Bool($e)),
        }
    };
}

#[macro_export]
macro_rules! ident {
    (_) => {
        $crate::types::TypedExpr {
            typ: _,
            expr: $crate::types::Expr::Identifier(_),
        }
    };
    ($id:ident) => {
        $crate::types::TypedExpr {
            typ: _,
            expr: $crate::types::Expr::Identifier($id),
        }
    };
    ($e:expr) => {
        $crate::types::TypedExpr {
            typ: _,
            expr: $crate::types::Expr::Identifier($e),
        }
    };
}

#[macro_export]
macro_rules! op {
    ($typ:ident, $op:ident $(, $e:expr)+ $(,)?) => {
        $crate::types::TypedExpr {
            typ: $crate::types::Type::$typ,
            expr: $crate::types::Expr::Op(
                $crate::types::TypedOps::$op(
                    $( Box::from($e), )+
                )
            ),
        }
    };

    (($typ:expr), $op:ident $(, $e:expr)+ $(,)?) => {
        op!($typ => $op $(, $e)+)
    };

    ($typ:expr => $op:ident $(, $e:expr)+ $(,)?) => {
        TypedExpr {
            typ: $typ,
            expr: Expr::Op(
                TypedOps::$op(
                    $( Box::from($e), )+
                )
            ),
        }
    };
}

#[macro_export]
macro_rules! match_op {
    ($typ:ident, $op:ident $(, $e:ident)+ $(,)?) => {
        $crate::types::TypedExpr {
            typ: $crate::types::Type::$typ,
            expr: $crate::types::Expr::Op(
                $crate::types::TypedOps::$op(
                    $( $e, )+
                )
            ),
        }
    };

    ($p:pat, $op:ident $(, $e:ident)+ $(,)?) => {
        $crate::types::TypedExpr {
            typ: $p,
            expr: $crate::types::Expr::Op(
                $crate::types::TypedOps::$op(
                    $( $e, )+
                )
            ),
        }
    };
}
