use crate::ast;
use crate::pir;
use crate::span::ToSpanned;

use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum PirError {}

#[allow(unused)]
pub fn transform_item(cx: &mut (), item: Spanned<ast::Item>) -> Result<(), PirError> {
    let Spanned(item, span) = item;
    match item {
        ast::Item::FnDecl(_) => todo!(),
        ast::Item::TypeDecl(_, ty) => todo!(),
        ast::Item::TypeAlias(_, _) => todo!(),
    }
}

pub fn transform_ty(ty: &Spanned<ast::Ty>) -> Spanned<pir::Ty> {
    let Spanned(ty, span) = ty;
    match ty {
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
        }
        .to_spanned(span.clone()),
        ast::Ty::Ptr {
            amp: _,
            mut_,
            child,
        } => {
            let child = transform_ty(child);
            let mutness = if mut_.is_some() {
                Mutness::Mut
            } else {
                Mutness::NoMut
            };
            pir::Ty::Ptr(mutness.to_spanned(mut_.span()), Box::new(child)).to_spanned(span.clone())
        }
        #[allow(unused_variables)]
        ast::Ty::SlicePtr {
            amp: _,
            mut_,
            child,
        } => todo!(),
        ast::Ty::Tuple(_) => todo!(),
        #[allow(unused_variables)]
        ast::Ty::Struct { struct_: _, fields } => todo!(),
        #[allow(unused_variables)]
        ast::Ty::Union { union_: _, fields } => todo!(),
        #[allow(unused_variables)]
        ast::Ty::Enum { enum_: _, fields } => todo!(),
    }
}
