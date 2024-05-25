use std::fmt::Debug;

use index_vec::{Idx, IndexSlice};

#[repr(transparent)]
pub struct FmtEnumerated<I: Debug + Idx, T: Debug>(IndexSlice<I, [T]>);
impl<I: Debug + Idx, T: Debug> FmtEnumerated<I, T> {
    pub const fn new(slice: &IndexSlice<I, [T]>) -> &Self {
        unsafe { std::mem::transmute(slice) }
    }
}
impl<I: Debug + Idx, T: Debug> Debug for FmtEnumerated<I, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.0.iter_enumerated()).finish()
    }
}

pub trait FmtEnumeratedTrait<I: Debug + Idx, T: Debug> {
    /// Debug format an `IndexSlice` as key-value pairs.
    fn fmt_enumerated(&self) -> &FmtEnumerated<I, T>;
}
impl<I: Debug + Idx, T: Debug> FmtEnumeratedTrait<I, T> for IndexSlice<I, [T]> {
    fn fmt_enumerated(&self) -> &FmtEnumerated<I, T> {
        FmtEnumerated::new(self)
    }
}

pub trait IndexVecUncheckedGet<I: Idx, T> {
    unsafe fn get_unchecked(&self, idx: I) -> &T;
    unsafe fn get_unchecked_mut(&mut self, idx: I) -> &mut T;
}
impl<I: Idx, T> IndexVecUncheckedGet<I, T> for IndexSlice<I, [T]> {
    unsafe fn get_unchecked(&self, idx: I) -> &T {
        self.raw.get_unchecked(idx.index())
    }

    unsafe fn get_unchecked_mut(&mut self, idx: I) -> &mut T {
        self.raw.get_unchecked_mut(idx.index())
    }
}

pub trait OptionExt<T> {
    fn try_map<U, E>(self, f: impl FnOnce(T) -> Result<U, E>) -> Result<Option<U>, E>;
}

impl<T> OptionExt<T> for Option<T> {
    fn try_map<U, E>(self, f: impl FnOnce(T) -> Result<U, E>) -> Result<Option<U>, E> {
        match self {
            Some(x) => match f(x) {
                Ok(x) => Ok(Some(x)),
                Err(e) => Err(e),
            },
            None => Ok(None),
        }
    }
}
