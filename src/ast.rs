use crate::{span::Spanned, token::tokens::*};

#[derive(Clone, PartialEq, Debug)]
pub struct Punctuated<T, P> {
    pub items: Vec<(Spanned<T>, Spanned<P>)>,
    pub last: Option<Box<Spanned<T>>>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Surrounded<L, T, R> {
    pub left: Spanned<L>,
    pub inner: Spanned<T>,
    pub right: Spanned<R>,
}

pub type InBraces<T> = Surrounded<BraceL, T, BracketR>;
pub type InBrackets<T> = Surrounded<BracketL, T, BracketR>;
pub type InParens<T> = Surrounded<ParenL, T, ParenR>;

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
