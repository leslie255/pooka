use std::{iter::Peekable, ops::Range, rc::Rc, slice};

use crate::{
    ast::*,
    source_str::SourceIndex,
    span::{Span, Spanned, ToSpanned},
    token::{tokens, Token, TokenTrait},
};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedEof,
    ExpectToken(Token),
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
            .span
            .clone();
        Self {
            tokens,
            path,
            prev_span,
        }
    }
    fn peek(&mut self) -> Option<&Spanned<Token>> {
        self.tokens.peek().copied()
    }
    fn next(&mut self) -> Option<&Spanned<Token>> {
        let t = self.tokens.next()?;
        self.prev_span = t.span.clone();
        return Some(t);
    }
}

pub trait Parse: Sized {
    fn peek(state: &mut ParserState) -> bool;
    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>>;
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
            items: Vec::new(),
            last: None,
        };
        let mut start = None;
        let mut end = start;
        loop {
            if !T::peek(state) {
                break;
            }
            let item = T::parse(state)?;
            item.span.range.clone().inspect(|i| end = Some(i.end));
            if start.is_none() {
                item.span.range.clone().inspect(|i| start = Some(i.start));
            }
            if P::peek(state) {
                let punct = P::parse(state)?;
                self_.items.push((item, punct));
            } else {
                self_.last = Some(Box::new(item));
                break;
            }
        }
        let span = Span::new(Some(state.path.clone()), join_range(start, end));
        return Ok(self_.to_spanned(span));
    }
}

impl<L, T, R> Parse for Surrounded<L, T, R>
where
    L: Parse,
    T: Parse,
    R: Parse,
{
    fn peek(state: &mut ParserState) -> bool {
        L::peek(state)
    }

    fn parse(state: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
        let left = L::parse(state)?;
        let inner = T::parse(state)?;
        let right = R::parse(state)?;
        let start: Option<SourceIndex> = find_span_start!(left, inner, right);
        let end: Option<SourceIndex> = find_span_end!(right, inner, left);
        let span = Span::new(Some(state.path.clone()), join_range(start, end));
        return Ok(Self { left, inner, right }.to_spanned(span));
    }
}

macro find_span_start {
    ($withspan:expr) => {
        if let Some(range) = $withspan.span.range.clone() {
            Some(range.start)
        } else {
            None
        }
    },
    ($withspan:expr , $($ts:tt)*) => {
        if let Some(range) = $withspan.span.range.clone() {
            Some(range.start)
        } else {
            find_span_start!($($ts)*)
        }
    },
    () => {None},
}

macro find_span_end {
    ($withspan:expr) => {
        if let Some(range) = $withspan.span.range.clone() {
            Some(range.end)
        } else {
            None
        }
    },
    ($withspan:expr , $($ts:tt)*) => {
        if let Some(range) = $withspan.span.range.clone() {
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
                    Some(t) => match &t.inner {
                        Token::$name => true,
                        _ => false,
                    },
                    None => false,
                }
            }
            fn parse(parser: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
                match parser.peek() {
                    Some(t) => match &t.inner {
                        Token::$name => {
                            let span = t.span.clone();
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
                    Some(t) => match &t.inner {
                        Token::$name(_) => true,
                        _ => false,
                    },
                    None => false,
                }
            }
            fn parse(parser: &mut ParserState) -> Result<Spanned<Self>, Spanned<ParseError>> {
                match parser.peek() {
                    Some(t) => match &t.inner {
                        Token::$name(x) => {
                            let span = t.span.clone();
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
    Ident(_), MacroDir(_), StrLiteral(_), IntLiteral(_), FloatLiteral(_), CharLiteral(_), BoolLiteral(_), UnreservedPunct(_), Mut, Struct, Union, Enum, Typealias, Type, If, Loop, While, Return, Break, Continue, ParenL, ParenR, BracketL, BracketR, BraceL, BraceR, Comma, Period, Eq, ColonEq, Colon, ColonColon, Ast, Tilde, Amp, Verbar, Circ, GtGt, LtLt, GtGtEq, LtLtEq, AmpEq, VerbarEq, CircEq, Excl, AmpAmp, VerbarVerbar, Plus, Minus, Sol, Percnt, PlusEq, MinusEq, AstEq, SolEq, PercntEq, Gt, Lt, GtEq, LtEq, EqEq, ExclEq, Arrow, Commat,
}
