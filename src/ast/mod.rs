pub mod parse;

use std::{fmt::{self, Debug}, rc::Rc};

use crate::{
    span::Spanned,
    token::{tokens::*, Token},
};

use derive_more::From;

#[derive(Clone, PartialEq)]
pub struct Punctuated<T, P> {
    pub pairs: Vec<(Spanned<T>, Spanned<P>)>,
    pub last: Option<Box<Spanned<T>>>,
}

impl<T, P> Punctuated<T, P> {
    pub fn iter(&self) -> impl Iterator<Item = &Spanned<T>> {
        self.into_iter()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Spanned<T>> {
        self.into_iter()
    }
}

impl<'a, T, P> IntoIterator for &'a Punctuated<T, P> {
    type Item = &'a Spanned<T>;

    type IntoIter = impl Iterator<Item = &'a Spanned<T>> + 'a;

    fn into_iter(self) -> Self::IntoIter {
        self.pairs
            .iter()
            .map(|(x, _)| x)
            .chain(self.last.iter().map(|x| x.as_ref()))
    }
}

impl<'a, T, P> IntoIterator for &'a mut Punctuated<T, P> {
    type Item = &'a mut Spanned<T>;

    type IntoIter = impl Iterator<Item = &'a mut Spanned<T>> + 'a;

    fn into_iter(self) -> Self::IntoIter {
        self.pairs
            .iter_mut()
            .map(|(x, _)| x)
            .chain(self.last.iter_mut().map(|x| x.as_mut()))
    }
}

impl<T, P> IntoIterator for Punctuated<T, P> {
    type Item = Spanned<T>;

    type IntoIter = impl Iterator<Item = Spanned<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.pairs
            .into_iter()
            .map(|(x, _)| x)
            .chain(self.last.into_iter().map(|x| *x))
    }
}

pub type InBraces<T> = (Spanned<BraceL>, Spanned<T>, Spanned<BraceR>);
pub type InBrackets<T> = (Spanned<BracketL>, Spanned<T>, Spanned<BracketR>);
pub type InParens<T> = (Spanned<ParenL>, Spanned<T>, Spanned<ParenR>);

pub type PatTy = (Spanned<Pat>, Spanned<Token![:]>, Spanned<Ty>);

pub type Mutness = Option<Token![mut]>;

pub type TuplePat = InParens<Punctuated<Pat, Token![,]>>;

#[derive(Clone, PartialEq, From)]
pub enum Pat {
    Binding(Spanned<Mutness>, Spanned<Ident>),
    Tuple(TuplePat),
}

#[derive(Clone, PartialEq, Debug, From)]
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

#[derive(Clone, PartialEq, Debug, From)]
pub enum Literal {
    Str(StrLiteral),
    Int(IntLiteral),
    Float(FloatLiteral),
    Char(CharLiteral),
    Bool(BoolLiteral),
}

#[derive(Clone, PartialEq, Debug, From)]
pub enum Oper {
    UnreservedPunct(Rc<str>),
    Period,
    Eq,
    Ast,
    Tilde,
    Amp,
    Verbar,
    Circ,
    GtGt,
    LtLt,
    GtGtEq,
    LtLtEq,
    AmpEq,
    VerbarEq,
    CircEq,
    Excl,
    AmpAmp,
    VerbarVerbar,
    Plus,
    Minus,
    Sol,
    Percnt,
    PlusEq,
    MinusEq,
    AstEq,
    SolEq,
    PercntEq,
    Gt,
    Lt,
    GtEq,
    LtEq,
    EqEq,
    ExclEq,
    Commat,
}

#[derive(Clone, PartialEq, Debug, From)]
pub enum ExprOrOper {
    Expr(Expr),
    Oper(Oper),
}

pub type TupleExpr = InParens<Punctuated<Expr, Token![,]>>;
pub type CallArgs = InParens<Punctuated<Expr, Token![,]>>;

#[derive(Clone, PartialEq, Debug, From)]
pub enum Expr {
    Literal(Literal),
    Ident(Ident),
    Tuple(TupleExpr),
    #[allow(clippy::enum_variant_names)]
    OperExpr(Vec<Spanned<ExprOrOper>>),
    Call(Box<Spanned<Expr>>, Spanned<CallArgs>),
    Block(Block),
}

#[derive(Clone, PartialEq, Debug, From)]
pub enum Stmt {
    Empty(Token![;]),
    Expr(Spanned<Expr>, Spanned<Token![;]>),
    VarDecl(Spanned<VarDecl>, Spanned<Token![;]>),
    Block(Block),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Block {
    pub brace_l: Spanned<BraceL>,
    pub stmts: Vec<Spanned<Stmt>>,
    pub tail: Option<Box<Spanned<Expr>>>,
    pub brace_r: Spanned<BraceR>,
}

#[derive(Clone, PartialEq, From)]
pub enum ColonEq_ {
    OneToken(Token![:=]),
    TwoTokens(Spanned<Token![:]>, Spanned<Token![=]>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum VarDecl {
    WithType {
        lhs: Spanned<Pat>,
        colon: Spanned<Token![:]>,
        ty: Spanned<Ty>,
        eq: Spanned<Token![=]>,
        rhs: Spanned<Expr>,
    },
    WithoutType {
        lhs: Spanned<Pat>,
        colon_eq: Spanned<ColonEq_>,
        rhs: Spanned<Expr>,
    },
}

pub type FnDeclArgs = InParens<Punctuated<PatTy, Token![,]>>;
pub type FnDeclRet = Option<(Spanned<Token![->]>, Spanned<Ty>)>;

#[derive(Clone, PartialEq, From)]
pub enum SemicolonOrBlock {
    Block(Block),
    Semicolon(Token![;]),
}

#[derive(Clone, PartialEq, Debug)]
pub struct FnDecl {
    pub name: Spanned<Ident>,
    pub colon_colon: Spanned<Token![::]>,
    pub args: Spanned<FnDeclArgs>,
    pub ret: Spanned<FnDeclRet>,
    pub body: Spanned<SemicolonOrBlock>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypeDecl {
    pub type_: Spanned<Token![type]>,
    pub name: Spanned<Ident>,
    pub eq: Spanned<Token![=]>,
    pub rhs: Spanned<Ty>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct TypeAlias {
    pub typealias: Spanned<Token![typealias]>,
    pub name: Spanned<Ident>,
    pub eq: Spanned<Token![=]>,
    pub rhs: Spanned<Ty>,
}

#[derive(Clone, PartialEq, Debug, From)]
pub enum Item {
    FnDecl(FnDecl),
    TypeDecl(Spanned<TypeDecl>, Spanned<Token![;]>),
    TypeAlias(Spanned<TypeAlias>, Spanned<Token![;]>),
}

// MARK: AST Formatting

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

impl Debug for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pat::Binding(Spanned(Some(_), _), name) => write!(f, "mut {name:?}"),
            Pat::Binding(Spanned(None, _), name) => write!(f, "{name:?}"),
            Pat::Tuple(fields) => {
                let mut debug_tuple = f.debug_tuple("Tuple");
                for field in fields.1.inner().iter() {
                    debug_tuple.field(field);
                }
                debug_tuple.finish()
            }
        }
    }
}

impl Debug for ColonEq_ {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ColonEq_::OneToken(t) => Debug::fmt(t, f),
            ColonEq_::TwoTokens(t0, t1) => f.debug_list().entry(t0).entry(t1).finish(),
        }
    }
}

impl Debug for SemicolonOrBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SemicolonOrBlock::Block(body) => Debug::fmt(body, f),
            SemicolonOrBlock::Semicolon(semicolon) => Debug::fmt(semicolon, f),
        }
    }
}
