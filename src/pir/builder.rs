use crate::ast;
use crate::pir;

use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum PirBuildError {}

#[derive(Debug, Clone, PartialEq)]
pub struct PirBuilderState {}

impl PirBuilderState {
    pub fn new() -> Self {
        Self {}
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PirItem {
    TypeDecl(Rc<str>, Ty),
    TypeAlias(Rc<str>, Ty),
}

pub fn build_item(
    state: &mut PirBuilderState,
    item: &Spanned<ast::AstItem>,
) -> Result<PirItem, Spanned<PirBuildError>> {
    let _ = state;
    match item.inner() {
        ast::AstItem::FnDecl(_) => todo!(),
        ast::AstItem::TypeDecl(typedecl, _) => Ok(PirItem::TypeDecl(
            typedecl.name.as_ref().clone(),
            build_ty(&typedecl.rhs),
        )),
        ast::AstItem::TypeAlias(typealias, _) => Ok(PirItem::TypeAlias(
            typealias.name.as_ref().clone(),
            build_ty(&typealias.rhs),
        )),
    }
}

pub fn build_ty(ty: &Spanned<ast::Ty>) -> Ty {
    match ty.inner() {
        ast::Ty::Typename(ident) => match &ident.as_ref()[..] {
            "i64" => pir::Ty::Int(Signness::Signed, IntSize::_64),
            "i32" => pir::Ty::Int(Signness::Signed, IntSize::_32),
            "i16" => pir::Ty::Int(Signness::Signed, IntSize::_16),
            "i8" => pir::Ty::Int(Signness::Signed, IntSize::_8),
            "u64" => pir::Ty::Int(Signness::Unsigned, IntSize::_64),
            "u32" => pir::Ty::Int(Signness::Unsigned, IntSize::_32),
            "u16" => pir::Ty::Int(Signness::Unsigned, IntSize::_16),
            "u8" => pir::Ty::Int(Signness::Unsigned, IntSize::_8),
            "f64" => pir::Ty::Float(FloatSize::_64),
            "f32" => pir::Ty::Float(FloatSize::_32),
            "char" => pir::Ty::Char,
            "bool" => pir::Ty::Bool,
            _ => pir::Ty::Typename(ident.as_ref().clone()),
        },
        ast::Ty::Ptr {
            amp: _,
            mut_,
            child,
        } => pir::Ty::Ptr(
            match mut_.inner() {
                Some(_) => Mutness::Mut,
                None => Mutness::NoMut,
            },
            Box::new(build_ty(child)),
        ),
        ast::Ty::SlicePtr {
            amp: _,
            mut_,
            child,
        } => pir::Ty::SlicePtr(
            match mut_.inner() {
                Some(_) => Mutness::Mut,
                None => Mutness::NoMut,
            },
            Box::new(build_ty(&child.inner().1)),
        ),
        ast::Ty::Tuple((_, children, _)) => {
            pir::Ty::Tuple(children.iter().map(|child| build_ty(child)).collect())
        }
        #[allow(unused_variables)]
        ast::Ty::Struct { struct_: _, fields } => todo!(),
        #[allow(unused_variables)]
        ast::Ty::Union { union_: _, fields } => todo!(),
        #[allow(unused_variables)]
        ast::Ty::Enum { enum_: _, fields } => todo!(),
    }
}
