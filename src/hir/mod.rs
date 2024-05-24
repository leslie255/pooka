pub mod builder;

use derive_more::From;

use std::{
    fmt::{self, Debug},
    rc::Rc,
};

use crate::span::Spanned;

pub type FnDeclArgs = Vec<(Pat, Ty)>;

#[derive(Debug, Clone, PartialEq)]
pub enum HirItem {
    TypeDecl(Rc<str>, Ty),
    TypeAlias(Rc<str>, Ty),
    FnDecl {
        name: Rc<str>,
        args: FnDeclArgs,
        ret: Ty,
        body: FnBody,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnBody {
    Block(Block),
    Expr(Expr),
    /// Signature-only functions.
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callee {
    Static(Rc<str>),
    Dynamic(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOper {
    Custom(Rc<str>),
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    Eq,
    Ne,
    BoolAnd,
    BoolOr,
    Gt,
    Lt,
    Ge,
    Le,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MonoOper {
    Custom(Rc<str>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Spanned<Stmt>>,
    pub tail: Option<Box<Spanned<Expr>>>,
}

#[derive(Debug, Clone, PartialEq, From)]
pub enum Pat {
    Binding(Mutness, Rc<str>),
    Tuple(Vec<Pat>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(Rc<str>),
    Call(Callee, Vec<Expr>),
    BinOper(Box<Expr>, BinOper, Box<Expr>),
    MonoOper(MonoOper, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Nothing,
    Expr(Expr),
    Assign(Expr, Expr),
    TypedDecl(Pat, Ty, Expr),
    TypelessDecl(Pat, Expr),
}

#[derive(Clone, PartialEq)]
pub enum Ty {
    Int(Signness, IntSize),
    Float(FloatSize),
    Bool,
    Char,
    Ptr(Mutness, Box<Ty>),
    SlicePtr(Mutness, Box<Ty>),
    Typename(Rc<str>),
    Tuple(Vec<Ty>),
}

impl Ty {
    pub const fn void() -> Self {
        Self::Tuple(Vec::new())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Signness {
    Signed,
    Unsigned,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Mutness {
    Mut,
    NoMut,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntSize {
    Size,
    _64,
    _32,
    _16,
    _8,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FloatSize {
    _64,
    _32,
}

impl Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Int(Signness::Unsigned, IntSize::Size) => write!(f, "usize"),
            Ty::Int(Signness::Unsigned, IntSize::_64) => write!(f, "u64"),
            Ty::Int(Signness::Unsigned, IntSize::_32) => write!(f, "u32"),
            Ty::Int(Signness::Unsigned, IntSize::_16) => write!(f, "u16"),
            Ty::Int(Signness::Unsigned, IntSize::_8) => write!(f, "u8"),
            Ty::Int(Signness::Signed, IntSize::_64) => write!(f, "i64"),
            Ty::Int(Signness::Signed, IntSize::_32) => write!(f, "i32"),
            Ty::Int(Signness::Signed, IntSize::_16) => write!(f, "i16"),
            Ty::Int(Signness::Signed, IntSize::_8) => write!(f, "i8"),
            Ty::Int(Signness::Signed, IntSize::Size) => write!(f, "isize"),
            Ty::Float(FloatSize::_64) => write!(f, "f64"),
            Ty::Float(FloatSize::_32) => write!(f, "f32"),
            Ty::Bool => write!(f, "bool"),
            Ty::Char => write!(f, "char"),
            Ty::Ptr(Mutness::Mut, t) => write!(f, "&mut {t:?}"),
            Ty::Ptr(Mutness::NoMut, t) => write!(f, "&{t:?}"),
            Ty::SlicePtr(Mutness::Mut, t) => write!(f, "&mut [{t:?}]"),
            Ty::SlicePtr(Mutness::NoMut, t) => write!(f, "&[{t:?}]"),
            Ty::Typename(x) => write!(f, "{x:?}"),
            Ty::Tuple(xs) => match &xs[..] {
                [] => write!(f, "()"),
                [xs @ .., y] => {
                    write!(f, "(")?;
                    for x in xs {
                        write!(f, "{x:?}, ")?;
                    }
                    write!(f, "{y:?})")
                }
            },
        }
    }
}
