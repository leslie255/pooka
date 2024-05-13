use std::fmt::{self, Debug};

use crate::{
    span::Spanned,
    token::{tokens::*, Token},
};

#[derive(Clone, PartialEq)]
pub struct Punctuated<T, P> {
    pub pairs: Vec<(Spanned<T>, Spanned<P>)>,
    pub last: Option<Box<Spanned<T>>>,
}

impl<T, P> Debug for Punctuated<T, P>
where
    T: Debug,
    P: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut list = f.debug_list();
        for (x, p) in &self.pairs {
            list.entry(x);
            list.entry(p);
        }
        self.last.as_ref().inspect(|x| {
            list.entry(x);
        });
        list.finish()
    }
}

pub type InBraces<T> = (Spanned<BraceL>, Spanned<T>, Spanned<BraceR>);
pub type InBrackets<T> = (Spanned<BracketL>, Spanned<T>, Spanned<BracketR>);
pub type InParens<T> = (Spanned<ParenL>, Spanned<T>, Spanned<ParenR>);

pub type PatTy = (Spanned<Pat>, Spanned<Token![:]>, Spanned<Ty>);

pub type Mutness = Option<Token![mut]>;

pub type TuplePat = InParens<Punctuated<Pat, Token![,]>>;

#[derive(Clone, PartialEq, Debug)]
pub enum Pat {
    Binding(Spanned<Mutness>, Spanned<Ident>),
    Tuple(TuplePat),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Ty {
    Typename(Ident),
    Ptr {
        amp: Spanned<Token![&]>,
        mut_: Spanned<Mutness>,
        child: Box<Spanned<Ty>>,
    },
    SlicePtr {
        amp: Spanned<Token![&]>,
        mut_: Spanned<Mutness>,
        child: Box<Spanned<InBrackets<Ty>>>,
    },
    Tuple(InParens<Punctuated<Ty, Token![,]>>),
    Struct {
        struct_: Spanned<Token![struct]>,
        fields: Spanned<InBraces<Punctuated<PatTy, Token![,]>>>,
    },
    Union {
        union_: Spanned<Token![union]>,
        fields: Spanned<InBraces<Punctuated<PatTy, Token![,]>>>,
    },
    Enum {
        enum_: Spanned<Token![enum]>,
        fields: Spanned<InBraces<Punctuated<EnumVariant, Token![,]>>>,
    },
}

#[derive(Clone, PartialEq, Debug)]
pub struct EnumVariant {
    pub name: Spanned<Ident>,
    pub fields: Spanned<Option<InParens<Punctuated<Ty, Token![,]>>>>,
}

#[derive(Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(Ident),
    Tuple(Spanned<Punctuated<Expr, Token![,]>>),
}

#[derive(Clone, PartialEq)]
pub enum Literal {
    StrLiteral(StrLiteral),
    IntLiteral(IntLiteral),
    FloatLiteral(FloatLiteral),
    CharLiteral(CharLiteral),
    BoolLiteral(BoolLiteral),
}
