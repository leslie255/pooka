mod transform;

use std::rc::Rc;

use crate::span::Spanned;

#[derive(Debug, Clone, PartialEq)]
pub enum PirItem {
    TypeDecl(Rc<str>, Ty),
    TypeAlias(Rc<str>, Ty),
    FnDecl(Rc<str>, Block),
}

pub type FnDeclArgs = Vec<(Rc<str>, Ty)>;

#[derive(Debug, Clone, PartialEq)]
pub struct Block {}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Int(Signness, IntSize),
    Float(FloatSize),
    Bool,
    Char,
    Ptr(Spanned<Mutness>, Box<Spanned<Ty>>),
    SlicePtr(Spanned<Mutness>, Box<Spanned<Ty>>),
    Typename(Rc<str>),
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
