#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate core as std;

#[cfg(not(feature = "std"))]
extern crate alloc;

use std::{
    borrow::Borrow,
    cmp::{self, Ordering},
    fmt, hash, iter,
    ops::Deref,
};

#[cfg(not(feature = "std"))]
use alloc::{
    string::{String, ToString},
    sync::Arc,
};

#[cfg(feature = "std")]
use std::sync::Arc;

/// A `SmolStr` is a string type that has the following properties:
///
/// * `size_of::<SmolStr>() == size_of::<String>()`
///
/// And all the properies of [SmolStrN::<22>].
pub type SmolStr = SmolStrN<INLINE_CAP>;

impl SmolStr {
    #[deprecated = "Use `new_inline` instead"]
    pub const fn new_inline_from_ascii(len: usize, bytes: &[u8]) -> SmolStr {
        let _len_is_short = [(); INLINE_CAP + 1][len];

        const ZEROS: &[u8] = &[0; INLINE_CAP];

        let mut buf = [0; INLINE_CAP];
        macro_rules! s {
            ($($idx:literal),*) => ( $(s!(set $idx);)* );
            (set $idx:literal) => ({
                let src: &[u8] = [ZEROS, bytes][($idx < len) as usize];
                let byte = src[$idx];
                let _is_ascii = [(); 128][byte as usize];
                buf[$idx] = byte
            });
        }
        s!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21);
        Self(Repr::Inline {
            len: len as u8,
            buf,
        })
    }
}

/// A `SmolStrN` is a string type that has the following properties:
///
/// * `Clone` is `O(1)`
/// * Strings are stack-allocated if they are:
///     * Up to N bytes long
///     * Longer than N bytes, but substrings of `WS` (see below). Such strings consist
///     solely of consecutive newlines, followed by consecutive spaces
/// * If a string does not satisfy the aforementioned conditions, it is heap-allocated
///
/// Unlike `String`, however, `SmolStrN` is immutable. The primary use case for
/// `SmolStrN` is a good enough default storage for tokens of typical programming
/// languages. Strings consisting of a series of newlines, followed by a series of
/// whitespace are a typical pattern in computer programs because of indentation.
/// Note that a specialized interner might be a better solution for some use cases.
#[derive(Clone)]
pub struct SmolStrN<const N: usize>(Repr<N>);

impl<const N: usize> SmolStrN<N> {
    /// Constructs inline variant of `SmolStrN`.
    ///
    /// Panics if `text.len() > N`.
    #[inline]
    pub const fn new_inline(text: &str) -> Self {
        let mut buf = [0; N];
        let mut i = 0;
        while i < text.len() {
            buf[i] = text.as_bytes()[i];
            i += 1
        }
        Self(Repr::Inline {
            len: text.len() as u8,
            buf,
        })
    }

    pub fn new<T>(text: T) -> Self
    where
        T: AsRef<str>,
    {
        Self(Repr::new(text))
    }

    #[inline(always)]
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    #[inline(always)]
    pub fn to_string(&self) -> String {
        self.as_str().to_string()
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[inline(always)]
    pub fn is_heap_allocated(&self) -> bool {
        match self.0 {
            Repr::Heap(..) => true,
            _ => false,
        }
    }

    fn from_char_iter<I: iter::Iterator<Item = char>>(mut iter: I) -> Self {
        let (min_size, _) = iter.size_hint();
        if min_size > N {
            let heap: String = iter.collect();
            return Self(Repr::Heap(heap.into_boxed_str().into()));
        }
        let mut len = 0;
        let mut buf = [0u8; N];
        while let Some(ch) = iter.next() {
            let size = ch.len_utf8();
            if size + len > N {
                let (min_remaining, _) = iter.size_hint();
                let mut heap = String::with_capacity(size + len + min_remaining);
                heap.push_str(std::str::from_utf8(&buf[..len]).unwrap());
                heap.push(ch);
                heap.extend(iter);
                return Self(Repr::Heap(heap.into_boxed_str().into()));
            }
            ch.encode_utf8(&mut buf[len..]);
            len += size;
        }
        Self(Repr::Inline {
            len: len as u8,
            buf,
        })
    }
}

impl<const N: usize> Default for SmolStrN<N> {
    fn default() -> Self {
        Self::new("")
    }
}

impl<const N: usize> Deref for SmolStrN<N> {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl<const N: usize, const M: usize> PartialEq<SmolStrN<N>> for SmolStrN<M> {
    fn eq(&self, other: &SmolStrN<N>) -> bool {
        self.as_str() == other.as_str()
    }
}

impl<const N: usize> Eq for SmolStrN<N> {}

impl<const N: usize> PartialEq<str> for SmolStrN<N> {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl<const N: usize> PartialEq<SmolStrN<N>> for str {
    fn eq(&self, other: &SmolStrN<N>) -> bool {
        other == self
    }
}

impl<'a, const N: usize> PartialEq<&'a str> for SmolStrN<N> {
    fn eq(&self, other: &&'a str) -> bool {
        self == *other
    }
}

impl<'a, const N: usize> PartialEq<SmolStrN<N>> for &'a str {
    fn eq(&self, other: &SmolStrN<N>) -> bool {
        *self == other
    }
}

impl<const N: usize> PartialEq<String> for SmolStrN<N> {
    fn eq(&self, other: &String) -> bool {
        self.as_str() == other
    }
}

impl<const N: usize> PartialEq<SmolStrN<N>> for String {
    fn eq(&self, other: &SmolStrN<N>) -> bool {
        other == self
    }
}

impl<'a, const N: usize> PartialEq<&'a String> for SmolStrN<N> {
    fn eq(&self, other: &&'a String) -> bool {
        self == *other
    }
}

impl<'a, const N: usize> PartialEq<SmolStrN<N>> for &'a String {
    fn eq(&self, other: &SmolStrN<N>) -> bool {
        *self == other
    }
}

impl<const N: usize> Ord for SmolStrN<N> {
    fn cmp(&self, other: &SmolStrN<N>) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl<const N: usize> PartialOrd for SmolStrN<N> {
    fn partial_cmp(&self, other: &SmolStrN<N>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<const N: usize> hash::Hash for SmolStrN<N> {
    fn hash<H: hash::Hasher>(&self, hasher: &mut H) {
        self.as_str().hash(hasher)
    }
}

impl<const N: usize> fmt::Debug for SmolStrN<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

impl<const N: usize> fmt::Display for SmolStrN<N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}

impl<const N: usize> iter::FromIterator<char> for SmolStrN<N> {
    fn from_iter<I: iter::IntoIterator<Item = char>>(iter: I) -> Self {
        let iter = iter.into_iter();
        Self::from_char_iter(iter)
    }
}

fn build_from_str_iter<T, const N: usize>(mut iter: impl Iterator<Item = T>) -> SmolStrN<N>
where
    T: AsRef<str>,
    String: iter::Extend<T>,
{
    let mut len = 0;
    let mut buf = [0u8; N];
    while let Some(slice) = iter.next() {
        let slice = slice.as_ref();
        let size = slice.len();
        if size + len > N {
            let mut heap = String::with_capacity(size + len);
            heap.push_str(std::str::from_utf8(&buf[..len]).unwrap());
            heap.push_str(&slice);
            heap.extend(iter);
            return SmolStrN(Repr::Heap(heap.into_boxed_str().into()));
        }
        (&mut buf[len..][..size]).copy_from_slice(slice.as_bytes());
        len += size;
    }
    SmolStrN(Repr::Inline {
        len: len as u8,
        buf,
    })
}

impl<const N: usize> iter::FromIterator<String> for SmolStrN<N> {
    fn from_iter<I: iter::IntoIterator<Item = String>>(iter: I) -> Self {
        build_from_str_iter(iter.into_iter())
    }
}

impl<'a, const N: usize> iter::FromIterator<&'a String> for SmolStrN<N> {
    fn from_iter<I: iter::IntoIterator<Item = &'a String>>(iter: I) -> Self {
        Self::from_iter(iter.into_iter().map(|x| x.as_str()))
    }
}

impl<'a, const N: usize> iter::FromIterator<&'a str> for SmolStrN<N> {
    fn from_iter<I: iter::IntoIterator<Item = &'a str>>(iter: I) -> Self {
        build_from_str_iter(iter.into_iter())
    }
}

impl<T, const N: usize> From<T> for SmolStrN<N>
where
    T: Into<String> + AsRef<str>,
{
    fn from(text: T) -> Self {
        Self::new(text)
    }
}

impl<const N: usize> From<SmolStrN<N>> for String {
    fn from(text: SmolStrN<N>) -> Self {
        text.as_str().into()
    }
}

impl<const N: usize> Borrow<str> for SmolStrN<N> {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

#[cfg(feature = "arbitrary")]
impl<'a, const N: usize> arbitrary::Arbitrary<'a> for SmolStrN<N> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> Result<Self, arbitrary::Error> {
        let s = <&str>::arbitrary(u)?;
        Ok(Self::new(s))
    }
}

const INLINE_CAP: usize = 22;
const N_NEWLINES: usize = 32;
const N_SPACES: usize = 128;
const WS: &str =
    "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n                                                                                                                                ";

#[derive(Clone, Debug)]
enum Repr<const N: usize> {
    Heap(Arc<str>),
    Inline { len: u8, buf: [u8; N] },
    Substring { newlines: usize, spaces: usize },
}

impl<const N: usize> Repr<N> {
    fn new<T>(text: T) -> Self
    where
        T: AsRef<str>,
    {
        {
            let text = text.as_ref();

            let len = text.len();
            if len <= N {
                let mut buf = [0; N];
                buf[..len].copy_from_slice(text.as_bytes());
                return Repr::Inline {
                    len: len as u8,
                    buf,
                };
            }

            if len <= N_NEWLINES + N_SPACES {
                let bytes = text.as_bytes();
                let possible_newline_count = cmp::min(len, N_NEWLINES);
                let newlines = bytes[..possible_newline_count]
                    .iter()
                    .take_while(|&&b| b == b'\n')
                    .count();
                let possible_space_count = len - newlines;
                if possible_space_count <= N_SPACES && bytes[newlines..].iter().all(|&b| b == b' ')
                {
                    let spaces = possible_space_count;
                    return Repr::Substring { newlines, spaces };
                }
            }
        }

        Repr::Heap(text.as_ref().into())
    }

    #[inline(always)]
    fn len(&self) -> usize {
        match self {
            Repr::Heap(data) => data.len(),
            Repr::Inline { len, .. } => *len as usize,
            Repr::Substring { newlines, spaces } => *newlines + *spaces,
        }
    }

    #[inline(always)]
    fn is_empty(&self) -> bool {
        match self {
            Repr::Heap(data) => data.is_empty(),
            Repr::Inline { len, .. } => *len == 0,
            // A substring isn't created for an empty string.
            Repr::Substring { .. } => false,
        }
    }

    #[inline]
    fn as_str(&self) -> &str {
        match self {
            Repr::Heap(data) => &*data,
            Repr::Inline { len, buf } => {
                let len = *len as usize;
                let buf = &buf[..len];
                unsafe { ::std::str::from_utf8_unchecked(buf) }
            }
            Repr::Substring { newlines, spaces } => {
                let newlines = *newlines;
                let spaces = *spaces;
                assert!(newlines <= N_NEWLINES && spaces <= N_SPACES);
                &WS[N_NEWLINES - newlines..N_NEWLINES + spaces]
            }
        }
    }
}

#[cfg(feature = "serde")]
mod serde {
    use super::SmolStrN;
    use ::serde::de::{Deserializer, Error, Unexpected, Visitor};
    use std::fmt;

    // https://github.com/serde-rs/serde/blob/629802f2abfd1a54a6072992888fea7ca5bc209f/serde/src/private/de.rs#L56-L125
    fn smol_str<'de: 'a, 'a, D, const N: usize>(deserializer: D) -> Result<SmolStrN<N>, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct SmolStrNVisitor<const N: usize>;

        impl<'a, const N: usize> Visitor<'a> for SmolStrNVisitor<N> {
            type Value = SmolStrN<N>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                Ok(Self::Value::from(v))
            }

            fn visit_borrowed_str<E>(self, v: &'a str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                Ok(Self::Value::from(v))
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: Error,
            {
                Ok(Self::Value::from(v))
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: Error,
            {
                match std::str::from_utf8(v) {
                    Ok(s) => Ok(Self::Value::from(s)),
                    Err(_) => Err(Error::invalid_value(Unexpected::Bytes(v), &self)),
                }
            }

            fn visit_borrowed_bytes<E>(self, v: &'a [u8]) -> Result<Self::Value, E>
            where
                E: Error,
            {
                match std::str::from_utf8(v) {
                    Ok(s) => Ok(Self::Value::from(s)),
                    Err(_) => Err(Error::invalid_value(Unexpected::Bytes(v), &self)),
                }
            }

            fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
            where
                E: Error,
            {
                match String::from_utf8(v) {
                    Ok(s) => Ok(Self::Value::from(s)),
                    Err(e) => Err(Error::invalid_value(
                        Unexpected::Bytes(&e.into_bytes()),
                        &self,
                    )),
                }
            }
        }

        deserializer.deserialize_str(SmolStrNVisitor)
    }

    impl<const N: usize> serde::Serialize for SmolStrN<N> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            self.as_str().serialize(serializer)
        }
    }

    impl<'de, const N: usize> serde::Deserialize<'de> for SmolStrN<N> {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            smol_str(deserializer)
        }
    }
}
