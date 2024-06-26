use std::{
    fmt::{Debug, Display},
    ops::Range,
    slice,
};

/// An immutable, leaked string type that allows O(1) indexing using `SourceIndex`.
/// `SourceIndex` can only be obtained by `SourceCharIndices.next`.
/// Doesn't implement `Index` because it requires lifetime of `SourceIndex` to match
/// `SourceStr`.
///
/// # Examples
///
/// ```
/// let string = SourceStr::from("你好，世界\n🌮\nПривет, мир\n");
///
/// let index0 = string.char_indices().skip(8).next().unwrap().0;
/// let index1 = string.char_indices().skip(13).next().unwrap().0;
///
/// assert_eq!(string.slice(index0..index1), "Привет");
/// ```
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct SourceStr<'s> {
    raw: &'s str,
}

impl<'s> SourceStr<'s> {
    /// Returns an iterator of the character and their indices
    ///
    /// # Examples
    ///
    /// ```
    /// let string = SourceStr::from("你好，世界\n🌮\nПривет, мир\n");
    /// let mut iter = string.char_indices();
    ///
    /// let (index0, char) = iter.next().unwrap();
    /// assert_eq!(char, '你');
    ///
    /// let (index1, char) = iter.next().unwrap();
    /// assert_eq!(char, '好');
    ///
    /// let nihao = string.slice(index0..index1);
    /// assert_eq!(nihao, "你好");
    /// ```
    pub fn char_indices(self) -> SourceCharIndices<'s> {
        SourceCharIndices {
            iter: self.as_bytes().iter(),
            i: 0,
            raw_index: 0,
        }
    }

    /// Slice the string into a `&str`
    ///
    /// # Panics
    /// Panics if index out of range
    ///
    /// ```
    /// let string = SourceStr::from("你好，世界\n🌮\nПривет, мир\n");
    ///
    /// let index0 = string.char_indices().skip(8).next().unwrap().0;
    /// let index1 = string.char_indices().skip(13).next().unwrap().0;
    ///
    /// assert_eq!(string.slice(index0..index1), "Привет");
    /// ```
    #[inline]
    pub fn slice(self, index: Range<SourceIndex>) -> &'s str {
        let bytes = self
            .as_bytes()
            .get(index.start.raw..index.end.raw + index.end.len);
        let bytes = match bytes {
            Some(bytes) => bytes,
            None => {
                println!("`SourceStr` index out of range when slicing");
                panic!();
            }
        };
        unsafe { std::str::from_utf8_unchecked(bytes) }
    }
}

impl<'s> From<&'s str> for SourceStr<'s> {
    fn from(s: &'s str) -> Self {
        Self { raw: s }
    }
}

impl Debug for SourceStr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.raw, f)
    }
}

impl Display for SourceStr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.raw, f)
    }
}

impl<'s> SourceStr<'s> {
    #[must_use]
    #[inline]
    pub fn as_str(self) -> &'s str {
        self.raw
    }

    #[must_use]
    #[inline]
    pub fn as_bytes(self) -> &'s [u8] {
        self.raw.as_bytes()
    }
}

/// An index pointing to a raw position in the source string
/// # Safety
/// Index produced from one source can never be used in another source string
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct SourceIndex {
    raw: usize,
    /// length of the character (in bytes)
    len: usize,
    /// The index of the character in the string
    pub position: usize,
}

impl SourceIndex {
    #[allow(dead_code)]
    #[inline]
    pub unsafe fn raw_index(self) -> usize {
        self.raw
    }
}

const CONT_MASK: u8 = 0b0011_1111;

#[inline]
const fn utf8_first_byte(byte: u8, width: u32) -> u32 {
    (byte & (0x7F >> width)) as u32
}

/// Returns the value of `ch` updated with continuation byte `byte`.
#[inline]
const fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
    (ch << 6) | (byte & CONT_MASK) as u32
}

/// Modified from `core::str::next_code_point`
/// Returns the length of the character, and the next code point in UTF-8
unsafe fn next_code_point_indexed<'a, I: Iterator<Item = &'a u8>>(
    bytes: &mut I,
) -> Option<(usize, u32)> {
    // Decode UTF-8
    let x = *bytes.next()?;
    if x < 128 {
        return Some((1, x as u32));
    }

    // Multibyte case follows
    // Decode from a byte combination out of: [[[x y] z] w]
    // NOTE: Performance is sensitive to the exact formulation here
    let init = utf8_first_byte(x, 2);
    // SAFETY: `bytes` produces an UTF-8-like string,
    // so the iterator must produce a value here.
    let y = *bytes.next().unwrap_unchecked();
    let mut increments = 2usize;
    let mut ch = utf8_acc_cont_byte(init, y);
    if x >= 0xE0 {
        // [[x y z] w] case
        // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid
        // SAFETY: `bytes` produces an UTF-8-like string,
        // so the iterator must produce a value here.
        let z = *bytes.next().unwrap_unchecked();
        increments = 3;
        let y_z = utf8_acc_cont_byte((y & CONT_MASK) as u32, z);
        ch = init << 12 | y_z;
        if x >= 0xF0 {
            // [x y z w] case
            // use only the lower 3 bits of `init`
            // SAFETY: `bytes` produces an UTF-8-like string,
            // so the iterator must produce a value here.
            let w = *bytes.next().unwrap_unchecked();
            increments = 4;
            ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
        }
    }

    Some((increments, ch))
}

#[derive(Clone, Debug)]
pub struct SourceCharIndices<'s> {
    iter: slice::Iter<'s, u8>,
    i: usize,
    raw_index: usize,
}

impl Iterator for SourceCharIndices<'_> {
    type Item = (SourceIndex, char);

    fn next(&mut self) -> Option<(SourceIndex, char)> {
        unsafe {
            let (len, ch) = next_code_point_indexed(&mut self.iter)?;
            let raw_index = self.raw_index;
            self.raw_index += len;
            let position = self.i;
            self.i += 1;
            let ch = char::from_u32_unchecked(ch);

            Some((
                SourceIndex {
                    raw: raw_index,
                    len,
                    position,
                },
                ch,
            ))
        }
    }
}
