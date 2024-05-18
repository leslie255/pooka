use std::{iter::Peekable, ops::Range, rc::Rc, slice};

use crate::{
    ast::*,
    source_str::SourceIndex,
    span::{span, spanned_into, Span, Spanned, ToSpanned},
    token::{
        tokens::{
            self, BoolLiteral, BraceL, BraceR, CharLiteral, FloatLiteral, Ident, IntLiteral,
            StrLiteral,
        },
        Token, TokenTrait,
    },
};

pub fn parse<T: Parse>(parser_state: &mut ParserState) -> Result<Spanned<T>, Spanned<ParseError>> {
    T::parse(parser_state)
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedEof,
    ExpectToken(Token),
    ExpectColonEq,
    ExpectVarDecl,
    ExpectPat,
    ExpectTy,
    ExpectLiteral,
    ExpectExpr,
    ExpectStmt,
    InvalidPattern,
}

#[derive(Debug, Clone)]
pub struct ParserState<'a> {
    tokens: Peekable<slice::Iter<'a, Spanned<Token>>>,
    path: Rc<str>,
    prev_span: Span,
}

impl<'a> ParserState<'a> {
    /// `tokens` must have at least one item.
    pub fn new(tokens: &'a [Spanned<Token>], path: Rc<str>) -> Self {
        let mut tokens = tokens.iter().peekable();
        let prev_span = tokens
            .peek()
            .expect("Argument `tokens` must have at least one item")
            .span();
        Self {
            tokens,
            path,
            prev_span,
        }
    }
    fn peek(&mut self) -> Option<&Spanned<Token>> {
        let &t = self.tokens.peek()?;
        self.prev_span = t.span();
        Some(t)
    }
    fn next(&mut self) -> Option<&Spanned<Token>> {
        let t = self.tokens.next()?;
        self.prev_span = t.span();
        Some(t)
    }
    #[allow(unreachable_code)]
    fn peek_or_eof_error(&mut self) -> Result<&Spanned<Token>, Spanned<ParseError>> {
        let &t = self.tokens.peek().ok_or_else(|| {
            panic!();
            ParseError::UnexpectedEof.to_spanned(self.prev_span.clone())
        })?;
        self.prev_span = t.span();
        return Ok(t);
    }
    #[allow(unreachable_code)]
    fn next_or_eof_error(&mut self) -> Result<&Spanned<Token>, Spanned<ParseError>> {
        let t = self.tokens.next().ok_or_else(|| {
            panic!();
            ParseError::UnexpectedEof.to_spanned(self.prev_span.clone())
        })?;
        self.prev_span = t.span();
        return Ok(t);
    }
}

pub trait Parse: Sized {
    fn peek(state: &mut ParserState) -> bool;
    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>>;
}

impl<L, R> Parse for (Spanned<L>, Spanned<R>)
where
    L: Parse,
    R: Parse,
{
    fn peek(state: &mut ParserState) -> bool {
        L::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let left = L::parse(state)?;
        let right = R::parse(state)?;
        let start = find_span_start!(left, right);
        let end = find_span_end!(right, left);
        let span = Span::new(Some(state.path.clone()), join_range(start, end));
        Ok((left, right).to_spanned(span))
    }
}

impl<L, C, R> Parse for (Spanned<L>, Spanned<C>, Spanned<R>)
where
    L: Parse,
    C: Parse,
    R: Parse,
{
    fn peek(state: &mut ParserState) -> bool {
        L::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let left = L::parse(state)?;
        let center = C::parse(state)?;
        let right = R::parse(state)?;
        let start: Option<SourceIndex> = find_span_start!(left, center, right);
        let end: Option<SourceIndex> = find_span_end!(right, center, left);
        let span = Span::new(Some(state.path.clone()), join_range(start, end));
        Ok((left, center, right).to_spanned(span))
    }
}

impl<T, P> Parse for Punctuated<T, P>
where
    T: Parse,
    P: Parse,
{
    fn peek(state: &mut ParserState) -> bool {
        T::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let mut self_ = Punctuated::<T, P> {
            pairs: Vec::new(),
            last: None,
        };
        let mut start = None;
        let mut end = start;
        loop {
            if !T::peek(state) {
                break;
            }
            let item = T::parse(state)?;
            item.1.range.clone().inspect(|i| {
                start.get_or_insert(i.start);
                end = Some(i.end);
            });
            if P::peek(state) {
                let punct = P::parse(state)?;
                self_.pairs.push((item, punct));
            } else {
                self_.last = Some(Box::new(item));
                break;
            }
        }
        let span = Span::new(Some(state.path.clone()), join_range(start, end));
        Ok(self_.to_spanned(span))
    }
}

impl<T> Parse for Option<T>
where
    T: Parse,
{
    fn peek(state: &mut ParserState) -> bool {
        T::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        if T::peek(state) {
            T::parse(state).map(|x| x.map(Some))
        } else {
            Ok(None.to_spanned(span!(None, None)))
        }
    }
}

impl Parse for Pat {
    fn peek(state: &mut ParserState) -> bool {
        Mutness::peek(state) || Ident::peek(state) || TuplePat::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        if Mutness::peek(state) {
            let mutness = Mutness::parse(state)?;
            let ident = Ident::parse(state)?;
            let start = find_span_start!(mutness, ident);
            let end = find_span_end!(ident, mutness);
            let span = Span::new(Some(state.path.clone()), join_range(start, end));
            Ok(Self::Binding(mutness, ident).to_spanned(span))
        } else if Ident::peek(state) {
            let ident = Ident::parse(state)?;
            let span = ident.1.clone();
            Ok(Self::Binding(None.to_spanned(Span::default()), ident).to_spanned(span))
        } else if TuplePat::peek(state) {
            let tuple_pat = TuplePat::parse(state)?;
            Ok(tuple_pat.map(|tuple_pat| Self::Tuple(tuple_pat)))
        } else {
            Err(ParseError::ExpectPat.to_spanned(state.prev_span.clone()))
        }
    }
}

impl Parse for Ty {
    fn peek(state: &mut ParserState) -> bool {
        Ident::peek(state)
            || <Token![&]>::peek(state)
            || <InParens<Punctuated<Ty, Token![,]>>>::peek(state)
            || <Token![struct]>::peek(state)
            || <Token![union]>::peek(state)
            || <Token![enum]>::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        if Ident::peek(state) {
            Ok(Ident::parse(state)?.map(Self::Typename))
        } else if <Token![&]>::peek(state) {
            let amp = <Token![&]>::parse(state)?;
            let mut_ = Mutness::parse(state)?;
            if Ty::peek(state) {
                let child = Ty::parse(state)?;
                let start = find_span_start!(amp, mut_, child);
                let end = find_span_end!(child, mut_, amp);
                let span = Span::new(Some(state.path.clone()), join_range(start, end));
                Ok(Self::Ptr {
                    amp,
                    mut_,
                    child: Box::new(child),
                }
                .to_spanned(span))
            } else if <InBrackets<Ty>>::peek(state) {
                let child = <InBrackets<Ty>>::parse(state)?;
                let start = find_span_start!(amp, mut_, child);
                let end = find_span_end!(child, mut_, amp);
                let span = Span::new(Some(state.path.clone()), join_range(start, end));
                Ok(Self::SlicePtr {
                    amp,
                    mut_,
                    child: Box::new(child),
                }
                .to_spanned(span))
            } else {
                Err(ParseError::ExpectTy.to_spanned(state.prev_span.clone()))
            }
        } else if <InParens<Punctuated<Ty, Token![,]>>>::peek(state) {
            Ok(<InParens<Punctuated<Ty, Token![,]>>>::parse(state)?.map(Self::Tuple))
        } else if <Token![struct]>::peek(state) {
            let struct_ = <Token![struct]>::parse(state)?;
            let fields = <InBraces<Punctuated<PatTy, Token![,]>>>::parse(state)?;
            let start = find_span_start!(struct_, fields);
            let end = find_span_end!(fields, struct_);
            let span = Span::new(Some(state.path.clone()), join_range(start, end));
            Ok(Self::Struct { struct_, fields }.to_spanned(span))
        } else if <Token![union]>::peek(state) {
            let union_ = <Token![union]>::parse(state)?;
            let fields = <InBraces<Punctuated<PatTy, Token![,]>>>::parse(state)?;
            let start = find_span_start!(union_, fields);
            let end = find_span_end!(fields, union_);
            let span = Span::new(Some(state.path.clone()), join_range(start, end));
            Ok(Self::Union { union_, fields }.to_spanned(span))
        } else if <Token![enum]>::peek(state) {
            let enum_ = <Token![enum]>::parse(state)?;
            let fields = <InBraces<Punctuated<EnumVariant, Token![,]>>>::parse(state)?;
            let start = find_span_start!(enum_, fields);
            let end = find_span_end!(fields, enum_);
            let span = Span::new(Some(state.path.clone()), join_range(start, end));
            Ok(Self::Enum { enum_, fields }.to_spanned(span))
        } else {
            Err(ParseError::ExpectTy.to_spanned(state.prev_span.clone()))
        }
    }
}

impl Parse for EnumVariant {
    fn peek(state: &mut ParserState) -> bool {
        Ident::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let name = Ident::parse(state)?;
        let fields = <Option<InParens<Punctuated<Ty, Token![,]>>>>::parse(state)?;
        let start = find_span_start!(name, fields);
        let end = find_span_end!(fields, name);
        let span = Span::new(Some(state.path.clone()), join_range(start, end));
        Ok(Self { name, fields }.to_spanned(span))
    }
}

impl Parse for Literal {
    fn peek(state: &mut ParserState) -> bool {
        StrLiteral::peek(state)
            || IntLiteral::peek(state)
            || FloatLiteral::peek(state)
            || CharLiteral::peek(state)
            || BoolLiteral::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        if StrLiteral::peek(state) {
            StrLiteral::parse(state).map(|x| x.map(Self::StrLiteral))
        } else if IntLiteral::peek(state) {
            IntLiteral::parse(state).map(|x| x.map(Self::IntLiteral))
        } else if FloatLiteral::peek(state) {
            FloatLiteral::parse(state).map(|x| x.map(Self::FloatLiteral))
        } else if CharLiteral::peek(state) {
            CharLiteral::parse(state).map(|x| x.map(Self::CharLiteral))
        } else if BoolLiteral::peek(state) {
            BoolLiteral::parse(state).map(|x| x.map(Self::BoolLiteral))
        } else {
            Err(ParseError::ExpectLiteral.to_spanned(state.prev_span.clone()))
        }
    }
}

impl Parse for Expr {
    fn peek(state: &mut ParserState) -> bool {
        Literal::peek(state) || Ident::peek(state) || TupleExpr::peek(state) || Block::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        if Literal::peek(state) {
            Literal::parse(state).map(|x| x.map(Self::Literal))
        } else if Ident::peek(state) {
            Ident::parse(state).map(|x| x.map(Self::Ident))
        } else if TupleExpr::peek(state) {
            TupleExpr::parse(state).map(|x| x.map(Self::Tuple))
        } else if Block::peek(state) {
            Block::parse(state).map(|x| x.map(Self::Block))
        } else {
            Err(ParseError::ExpectExpr.to_spanned(state.prev_span.clone()))
        }
    }
}

impl Parse for Stmt {
    fn peek(state: &mut ParserState) -> bool {
        <Token![;]>::peek(state) || Expr::peek(state) || Block::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        if <Token![;]>::peek(state) {
            <Token![;]>::parse(state).map(|x| x.map(Self::Empty))
        } else if VarDeclOrExpr::peek(state) {
            let var_decl_or_expr = VarDeclOrExpr::parse(state)?;
            let semicolon = <Token![;]>::parse(state)?;
            let start = find_span_start!(var_decl_or_expr, semicolon);
            let end = find_span_end!(var_decl_or_expr, semicolon);
            let span = Span::new(Some(state.path.clone()), join_range(start, end));
            let Spanned(var_decl_or_expr, span_) = var_decl_or_expr;
            let stmt = match var_decl_or_expr {
                VarDeclOrExpr::VarDecl(x) => Stmt::VarDecl(x.to_spanned(span_), semicolon),
                VarDeclOrExpr::Expr(x) => Stmt::Expr(x.to_spanned(span_), semicolon),
            };
            Ok(stmt.to_spanned(span))
        } else if Block::peek(state) {
            Block::parse(state).map(|x| x.map(Self::Block))
        } else {
            Err(ParseError::ExpectStmt.to_spanned(state.prev_span.clone()))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum VarDeclOrExpr {
    VarDecl(VarDecl),
    Expr(Expr),
}

impl From<Expr> for VarDeclOrExpr {
    fn from(v: Expr) -> Self {
        Self::Expr(v)
    }
}

impl From<VarDecl> for VarDeclOrExpr {
    fn from(v: VarDecl) -> Self {
        Self::VarDecl(v)
    }
}

impl Parse for VarDeclOrExpr {
    fn peek(state: &mut ParserState) -> bool {
        Pat::peek(state) || Expr::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let t = state.peek_or_eof_error()?;
        match t.inner() {
            Token::Mut => VarDecl::parse(state).map(|x| x.map(Self::VarDecl)),
            Token::ParenL | Token::Ident(..) => {
                let mut saved_state = state.clone();
                let as_expr = Expr::parse(state);
                let as_pat = Pat::parse(&mut saved_state);
                match (as_expr, as_pat) {
                    (Ok(_), Ok(pat)) if <Token![:]>::peek(state) || <Token![:=]>::peek(state) => {
                        *state = saved_state;
                        VarDecl::parse_after_lhs(pat, state).map(spanned_into)
                    }
                    (Ok(expr), Ok(_)) => Ok(spanned_into(expr)),
                    (Ok(expr), Err(_)) => Ok(spanned_into(expr)),
                    (Err(_), Ok(pat)) => {
                        *state = saved_state;
                        VarDecl::parse_after_lhs(pat, state).map(spanned_into)
                    }
                    (Err(e), Err(_)) => Err(e),
                }
            }
            _ => Expr::parse(state).map(|x| x.map(Self::Expr)),
        }
    }
}

impl ColonEq_ {
    pub fn two_tokens(
        path: Rc<str>,
        colon: Spanned<Token![:]>,
        eq: Spanned<Token![=]>,
    ) -> Spanned<Self> {
        let start = find_span_start!(colon, eq);
        let end = find_span_start!(eq, colon);
        let span = Span::new(Some(path.clone()), join_range(start, end));
        Self::TwoTokens(colon, eq).to_spanned(span)
    }
}

impl VarDecl {
    fn parse_after_lhs(
        lhs: Spanned<Pat>,
        state: &mut ParserState,
    ) -> Result<Spanned<VarDecl>, Spanned<ParseError>> {
        if <Token![:=]>::peek(state) {
            let colon_eq = <Token![:=]>::parse(state)?.map(ColonEq_::OneToken);
            let rhs = Expr::parse(state)?;
            let start = find_span_start!(lhs, colon_eq, rhs);
            let end = find_span_end!(rhs, colon_eq, lhs);
            let span = Span::new(Some(state.path.clone()), join_range(start, end));
            Ok(Self::WithoutType { lhs, colon_eq, rhs }.to_spanned(span))
        } else if <Token![:]>::peek(state) {
            let colon = <Token![:]>::parse(state)?;
            if <Token![=]>::peek(state) {
                let eq = <Token![=]>::parse(state)?;
                let rhs = Expr::parse(state)?;
                let start = find_span_start!(lhs, colon, eq, rhs);
                let end = find_span_end!(rhs, colon, eq, lhs);
                let colon_eq = ColonEq_::two_tokens(state.path.clone(), colon, eq);
                let span = Span::new(Some(state.path.clone()), join_range(start, end));
                Ok(Self::WithoutType { lhs, colon_eq, rhs }.to_spanned(span))
            } else {
                let ty = Ty::parse(state)?;
                let eq = <Token![=]>::parse(state)?;
                let rhs = Expr::parse(state)?;
                let start = find_span_start!(lhs, colon, ty, eq, rhs);
                let end = find_span_end!(rhs, colon, ty, eq, lhs);
                let span = Span::new(Some(state.path.clone()), join_range(start, end));
                Ok(Self::WithType {
                    lhs,
                    colon,
                    ty,
                    eq,
                    rhs,
                }
                .to_spanned(span))
            }
        } else {
            Err(ParseError::ExpectToken(Token![:].to_token()).to_spanned(lhs.1))
        }
    }
}

impl Parse for VarDecl {
    fn peek(state: &mut ParserState) -> bool {
        Pat::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let lhs = Pat::parse(state)?;
        Self::parse_after_lhs(lhs, state)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum StmtOrTail {
    Stmt(Stmt),
    Tail(Expr),
}

impl From<Expr> for StmtOrTail {
    fn from(v: Expr) -> Self {
        Self::Tail(v)
    }
}

impl From<Stmt> for StmtOrTail {
    fn from(v: Stmt) -> Self {
        Self::Stmt(v)
    }
}

impl Parse for StmtOrTail {
    fn peek(state: &mut ParserState) -> bool {
        Expr::peek(state) || Stmt::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        if VarDeclOrExpr::peek(state) {
            let Spanned(var_decl_or_expr, span) = VarDeclOrExpr::parse(state)?;
            match var_decl_or_expr {
                VarDeclOrExpr::VarDecl(var_decl) => {
                    let var_decl = var_decl.to_spanned(span);
                    let semicolon = <Token![;]>::parse(state)?;
                    let start = find_span_start!(var_decl, semicolon);
                    let end = find_span_end!(var_decl, semicolon);
                    let span = Span::new(Some(state.path.clone()), join_range(start, end));
                    let stmt = Stmt::VarDecl(var_decl, semicolon);
                    Ok(Self::Stmt(stmt).to_spanned(span))
                }
                VarDeclOrExpr::Expr(expr) => {
                    let expr = expr.to_spanned(span);
                    if <Token![;]>::peek(state) {
                        let semicolon = <Token![;]>::parse(state)?;
                        let start = find_span_start!(expr, semicolon);
                        let end = find_span_end!(expr, semicolon);
                        let span = Span::new(Some(state.path.clone()), join_range(start, end));
                        Ok(Self::Stmt(Stmt::Expr(expr, semicolon)).to_spanned(span))
                    } else {
                        Ok(expr.map(Self::Tail))
                    }
                }
            }
        } else if Stmt::peek(state) {
            Stmt::parse(state).map(|x| x.map(Self::Stmt))
        } else {
            Err(ParseError::ExpectStmt.to_spanned(state.prev_span.clone()))
        }
    }
}

impl Parse for Block {
    fn peek(state: &mut ParserState) -> bool {
        BraceL::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let brace_l = BraceL::parse(state)?;
        let (mut start, mut end) = match brace_l.1.range.clone() {
            Some(range) => (Some(range.start), Some(range.end)),
            None => (None, None),
        };
        let mut stmts = Vec::<Spanned<Stmt>>::new();
        let mut tail = Option::<Box<Spanned<Expr>>>::None;
        loop {
            if !Stmt::peek(state) {
                break;
            }
            let Spanned(stmt_or_tail, span) = StmtOrTail::parse(state)?;
            match stmt_or_tail {
                StmtOrTail::Stmt(stmt) => {
                    span.range.clone().inspect(|i| {
                        start.get_or_insert(i.start);
                        end = Some(i.end);
                    });
                    stmts.push(stmt.to_spanned(span));
                }
                StmtOrTail::Tail(expr) => {
                    span.range.clone().inspect(|i| {
                        start.get_or_insert(i.start);
                        end = Some(i.end);
                    });
                    tail = Some(Box::new(expr.to_spanned(span)));
                    break;
                }
            }
        }
        let brace_r = BraceR::parse(state)?;
        brace_r.1.range.clone().inspect(|i| {
            start.get_or_insert(i.start);
            end = Some(i.end);
        });
        let span = Span::new(Some(state.path.clone()), join_range(start, end));
        Ok(Self {
            brace_l,
            stmts,
            brace_r,
            tail,
        }
        .to_spanned(span))
    }
}

macro find_span_start {
    ($withspan:expr) => {
        if let Some(range) = $withspan.span().range.clone() {
            Some(range.start)
        } else {
            None
        }
    },
    ($withspan:expr , $($ts:tt)*) => {
        if let Some(range) = $withspan.span().range.clone() {
            Some(range.start)
        } else {
            find_span_start!($($ts)*)
        }
    },
    () => {None},
}

macro find_span_end {
    ($withspan:expr) => {
        if let Some(range) = $withspan.span().range.clone() {
            Some(range.end)
        } else {
            None
        }
    },
    ($withspan:expr , $($ts:tt)*) => {
        if let Some(range) = $withspan.span().range.clone() {
            Some(range.end)
        } else {
            find_span_start!($($ts)*)
        }
    },
    () => {None},
}

fn join_range(start: Option<SourceIndex>, end: Option<SourceIndex>) -> Option<Range<SourceIndex>> {
    match (start, end) {
        (None, None) => None,
        (None, Some(i)) => Some(i..i),
        (Some(i), None) => Some(i..i),
        (Some(start), Some(end)) => Some(start..end),
    }
}

macro impl_parse_for_token {
    ($name:ident) => {
        impl Parse for tokens::$name {
            fn peek(parser: &mut ParserState) -> bool {
                match parser.peek() {
                    Some(t) => match t.inner() {
                        Token::$name => true,
                        _ => false,
                    },
                    None => false,
                }
            }
            fn parse(parser: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
                match parser.peek() {
                    Some(t) => match t.inner() {
                        Token::$name => {
                            let span = t.span();
                            parser.next();
                            Ok(tokens::$name.to_spanned(span))
                        },
                        _ => Err(ParseError::ExpectToken(tokens::$name::default().to_token())
                            .to_spanned(parser.prev_span.clone())),
                    },
                    None => Err(ParseError::UnexpectedEof.to_spanned(parser.prev_span.clone())),
                }
            }
        }
    },
    ($name:ident(_)) => {
        impl Parse for tokens::$name {
            fn peek(parser: &mut ParserState) -> bool {
                match parser.peek() {
                    Some(t) => match &t.inner() {
                        Token::$name(_) => true,
                        _ => false,
                    },
                    None => false,
                }
            }
            fn parse(parser: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
                match parser.peek() {
                    Some(t) => match t.inner() {
                        Token::$name(x) => {
                            let span = t.span();
                            let x = x.clone();
                            parser.next();
                            Ok(tokens::$name(x).to_spanned(span))
                        },
                        _ => Err(ParseError::ExpectToken(tokens::$name::default().to_token())
                            .to_spanned(parser.prev_span.clone())),
                    },
                    None => Err(ParseError::UnexpectedEof.to_spanned(parser.prev_span.clone())),
                }
            }
        }
    },
    ($name:ident, $($ts:tt)*) => {
        impl_parse_for_token!($name);
        impl_parse_for_token!($($ts)*);
    },
    ($name:ident(_), $($ts:tt)*) => {
        impl_parse_for_token!($name(_));
        impl_parse_for_token!($($ts)*);
    },
    () => {},
}

impl_parse_for_token! {
    Ident(_), MacroDir(_), StrLiteral(_), IntLiteral(_), FloatLiteral(_), CharLiteral(_), BoolLiteral(_),
    UnreservedPunct(_), Mut, Struct, Union, Enum, Typealias, Type, If, Else, Loop, While, Return, Break, Continue,
    ParenL, ParenR, BracketL, BracketR, BraceL, BraceR, Comma, Period, Eq, ColonEq, Colon, ColonColon, Semicolon, Ast,
    Tilde, Amp, Verbar, Circ, GtGt, LtLt, GtGtEq, LtLtEq, AmpEq, VerbarEq, CircEq, Excl, AmpAmp, VerbarVerbar, Plus,
    Minus, Sol, Percnt, PlusEq, MinusEq, AstEq, SolEq, PercntEq, Gt, Lt, GtEq, LtEq, EqEq, ExclEq, Arrow, Commat,
}
