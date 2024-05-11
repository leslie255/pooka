use std::{
    ops::{Deref, DerefMut},
    rc::Rc,
};

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub src: Option<Rc<str>>,
    pub start: usize,
    pub end: usize,
}

#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
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
