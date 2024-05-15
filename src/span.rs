use std::{
    fmt::{self, Debug},
    ops::{Deref, DerefMut, Range},
    rc::Rc,
};

use crate::source_str::SourceIndex;

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Span {
    pub path: Option<Rc<str>>,
    pub range: Option<Range<SourceIndex>>,
}

impl Span {
    pub const fn new(path: Option<Rc<str>>, range: Option<Range<SourceIndex>>) -> Self {
        Self { path, range }
    }
}

pub macro span {
    (None , None) => {
        Span {
            path: None,
            range: None,
        }
    },
    (None, $range:expr) => {
        Span {
            path: None,
            range: Some($range),
        }
    },
    ($path:expr , None) => {
        Span {
            path: Some($path),
            range: None,
        }
    },
    ($path:expr , $range:expr $(,)?) => {
        Span {
            path: Some($path),
            range: Some($range),
        }
    },
}

impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (&self.range, &self.path) {
            (None, None) => write!(f, "(unknown span)"),
            (None, Some(path)) => write!(f, "(? @ {path:?})"),
            (Some(range), None) => {
                write!(f, "({}..{} @ ?)", range.start.position, range.end.position)
            }
            (Some(range), Some(path)) => write!(
                f,
                "({}..{} @ {path:?})",
                range.start.position, range.end.position
            ),
        }
    }
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        let span = self.span;
        f(self.inner).to_spanned(span)
    }
}

impl<T> Debug for Spanned<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.inner, f)
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub trait ToSpanned: Sized {
    fn to_spanned(self, span: Span) -> Spanned<Self>;
}

impl<T> ToSpanned for T {
    fn to_spanned(self, span: Span) -> Spanned<Self> {
        Spanned { inner: self, span }
    }
}

pub fn spanned_into<T, U>(self_: Spanned<T>) -> Spanned<U> where U:From<T> 
{
    self_.map(Into::into)
}
