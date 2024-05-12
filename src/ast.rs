use crate::{span::Spanned, token::tokens::*};

#[derive(Clone, PartialEq)]
pub struct Punctuated<T, P> {
    pub items: Vec<(Spanned<T>, Spanned<P>)>,
    pub last: Option<Box<Spanned<T>>>,
}

#[derive(Clone, PartialEq)]
pub struct InBraces<T> {
    pub left: Spanned<BraceL>,
    pub inner: Spanned<T>,
    pub right: Spanned<BraceR>,
}

#[derive(Clone, PartialEq)]
pub struct InBrackets<T> {
    pub inner: Spanned<T>
}

#[derive(Clone, PartialEq)]
pub struct InParens<T> {
    pub left: Spanned<ParenL>,
    pub inner: Spanned<T>,
    pub right: Spanned<ParenR>,
}

#[derive(Clone, PartialEq)]
pub enum Pat {
    Binding(Spanned<Mutness>, Spanned<Ident>),
    Tuple(Spanned<InParens<Punctuated<Pat, Token![,]>>>),
}

#[derive(Clone, PartialEq)]
pub struct PatTy {
    pub pat: Spanned<Pat>,
    pub colon: Spanned<Token![:]>,
    pub ty: Spanned<Ty>,
}

#[derive(Clone, PartialEq)]
pub enum Ty {
    Typename(Ident),
    Ptr(Spanned<Mutness>, Box<Spanned<Ty>>),
    SlicePtr(Spanned<Mutness>, Box<Ty>),
    Struct {
        struct_: Spanned<Token![struct]>,
        fields: Spanned<InBraces<Punctuated<PatTy, Token![,]>>>,
    },
    Union {
        union_: Spanned<Token![union]>,
        fields: Spanned<InBraces<Punctuated<PatTy, Token![,]>>>,
    },
    Enum(Spanned<Punctuated<PatTy, Token![,]>>),
}

#[derive(Clone, Copy, PartialEq)]
pub enum Mutness {
    Immutable,
    Mutable,
}

#[derive(Clone, PartialEq)]
pub struct EnumVariant {
    pub name: Spanned<Ident>,
    pub fields: Spanned<InParens<Punctuated<Ty, Token![,]>>>
}

#[derive(Clone, PartialEq)]
pub enum Expr {
    Ident(Ident),
}

#[derive(Clone, PartialEq)]
pub enum Literal {
    StrLiteral(StrLiteral),
    IntLiteral(IntLiteral),
    FloatLiteral(FloatLiteral),
    CharLiteral(CharLiteral),
    BoolLiteral(BoolLiteral),
}
