use crate::ast;
use crate::ast::AstItem;
use crate::hir;

use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum HirBuildError {}

#[derive(Debug, Clone, PartialEq)]
pub struct HirBuilderState {}

impl HirBuilderState {
    pub fn new() -> Self {
        Self {}
    }
}

pub fn build_item(
    state: &mut HirBuilderState,
    item: &Spanned<AstItem>,
) -> Result<HirItem, Spanned<HirBuildError>> {
    match item.inner() {
        #[allow(unused_variables)]
        AstItem::FnDecl(ast::FnDecl {
            name,
            colon_colon: _,
            args: Spanned((_, args, _), _),
            ret,
            body,
        }) => {
            let args: Vec<_> = args
                .iter()
                .map(|Spanned((pat, _, ty), _)| (build_pat(pat), build_ty(ty)))
                .collect();
            let ret = match ret.inner() {
                Some((_, t)) => build_ty(t),
                None => Ty::void(),
            };
            let body = match body.inner() {
                ast::FnBody::Expr(_, expr, _) => FnBody::Expr(build_expr(state, expr)?),
                ast::FnBody::Block(_) => todo!(),
                ast::FnBody::Semicolon(_) => todo!(),
            };
            Ok(HirItem::FnDecl {
                name: name.as_ref().clone(),
                args,
                ret,
                body,
            })
        }
        AstItem::TypeDecl(typedecl, _) => Ok(HirItem::TypeDecl(
            typedecl.name.as_ref().clone(),
            build_ty(&typedecl.rhs),
        )),
        AstItem::TypeAlias(typealias, _) => Ok(HirItem::TypeAlias(
            typealias.name.as_ref().clone(),
            build_ty(&typealias.rhs),
        )),
    }
}

pub fn build_stmt(
    state: &mut HirBuilderState,
    expr: &Spanned<ast::Stmt>,
) -> Result<Stmt, Spanned<HirBuildError>> {
    match expr.inner() {
        ast::Stmt::Nothing(_) => Ok(Stmt::Nothing),
        ast::Stmt::Expr(expr, _) => build_expr(state, expr).map(Stmt::Expr),
        ast::Stmt::VarDecl(vardecl, _) => match vardecl.inner() {
            ast::VarDecl::WithType {
                lhs,
                colon: _,
                ty,
                eq: _,
                rhs,
            } => Ok(Stmt::TypedDecl(
                build_pat(lhs),
                build_ty(ty),
                build_expr(state, rhs)?,
            )),
            ast::VarDecl::WithoutType {
                lhs,
                colon_eq: _,
                rhs,
            } => Ok(Stmt::TypelessDecl(build_pat(lhs), build_expr(state, rhs)?)),
        },
        ast::Stmt::Block(_) => todo!(),
    }
}

#[allow(unused)]
pub fn build_expr(
    state: &mut HirBuilderState,
    expr: &Spanned<ast::Expr>,
) -> Result<Expr, Spanned<HirBuildError>> {
    match expr.inner() {
        ast::Expr::Literal(_) => todo!(),
        ast::Expr::Ident(s) => Ok(Expr::Ident(s.as_ref().clone())),
        ast::Expr::Tuple(_) => todo!(),
        ast::Expr::OperExpr(_) => todo!(),
        ast::Expr::Call(_, _) => todo!(),
        ast::Expr::Block(_) => todo!(),
    }
}

pub fn build_ty(ty: &Spanned<ast::Ty>) -> Ty {
    match ty.inner() {
        ast::Ty::Typename(ident) => match &ident.as_ref()[..] {
            "isize" => hir::Ty::Int(Signness::Signed, IntSize::Size),
            "i64" => hir::Ty::Int(Signness::Signed, IntSize::_64),
            "i32" => hir::Ty::Int(Signness::Signed, IntSize::_32),
            "i16" => hir::Ty::Int(Signness::Signed, IntSize::_16),
            "i8" => hir::Ty::Int(Signness::Signed, IntSize::_8),
            "usize" => hir::Ty::Int(Signness::Unsigned, IntSize::Size),
            "u64" => hir::Ty::Int(Signness::Unsigned, IntSize::_64),
            "u32" => hir::Ty::Int(Signness::Unsigned, IntSize::_32),
            "u16" => hir::Ty::Int(Signness::Unsigned, IntSize::_16),
            "u8" => hir::Ty::Int(Signness::Unsigned, IntSize::_8),
            "f64" => hir::Ty::Float(FloatSize::_64),
            "f32" => hir::Ty::Float(FloatSize::_32),
            "char" => hir::Ty::Char,
            "bool" => hir::Ty::Bool,
            _ => hir::Ty::Typename(ident.as_ref().clone()),
        },
        ast::Ty::Ptr {
            amp: _,
            mut_,
            child,
        } => hir::Ty::Ptr(
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
        } => hir::Ty::SlicePtr(
            match mut_.inner() {
                Some(_) => Mutness::Mut,
                None => Mutness::NoMut,
            },
            Box::new(build_ty(&child.inner().1)),
        ),
        ast::Ty::Tuple((_, children, _)) => hir::Ty::Tuple(children.iter().map(build_ty).collect()),
        #[allow(unused_variables)]
        ast::Ty::Struct { struct_: _, fields } => todo!(),
        #[allow(unused_variables)]
        ast::Ty::Union { union_: _, fields } => todo!(),
        #[allow(unused_variables)]
        ast::Ty::Enum { enum_: _, fields } => todo!(),
    }
}

pub fn build_pat(pat: &Spanned<ast::Pat>) -> Pat {
    match pat.inner() {
        ast::Pat::Binding(Spanned(Some(_), _), s) => Pat::Binding(Mutness::Mut, s.as_ref().clone()),
        ast::Pat::Binding(Spanned(None, _), s) => Pat::Binding(Mutness::NoMut, s.as_ref().clone()),
        ast::Pat::Tuple((_, xs, _)) => xs.iter().map(build_pat).collect::<Vec<_>>().into(),
    }
}
