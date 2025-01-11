pub use my_ecs::util::*;
use std::{borrow, borrow::Cow, cmp, fmt, hash, ops::Deref};

pub trait AsOr<A: ?Sized, B: ?Sized> {
    fn as_or(&self) -> Or<&A, &B>;
}

impl<A: ?Sized, B: ?Sized> AsOr<A, B> for Or<&A, &B> {
    fn as_or(&self) -> Or<&A, &B> {
        *self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Rect<T> {
    pub width: T,
    pub height: T,
}

#[derive(Clone)]
pub struct StaticStr(Or<&'static str, Box<str>>);

impl StaticStr {
    pub fn new<T>(s: T) -> Self
    where
        T: Into<Cow<'static, str>>,
    {
        let s: Cow<'static, str> = s.into();
        match s {
            Cow::Borrowed(s) => Self(Or::A(s)),
            Cow::Owned(s) => Self(Or::B(s.into())),
        }
    }
}

impl fmt::Debug for StaticStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Or::A(s) => s.fmt(f),
            Or::B(s) => s.fmt(f),
        }
    }
}

impl fmt::Display for StaticStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Or::A(s) => s.fmt(f),
            Or::B(s) => s.fmt(f),
        }
    }
}

impl Deref for StaticStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match &self.0 {
            Or::A(s) => s,
            Or::B(s) => s,
        }
    }
}

impl Default for StaticStr {
    fn default() -> Self {
        Self::new("")
    }
}

impl From<&'static str> for StaticStr {
    fn from(value: &'static str) -> Self {
        Self(Or::A(value))
    }
}

impl From<Box<str>> for StaticStr {
    fn from(value: Box<str>) -> Self {
        Self(Or::B(value))
    }
}

impl From<String> for StaticStr {
    fn from(value: String) -> Self {
        Self(Or::B(value.into()))
    }
}

impl borrow::Borrow<str> for StaticStr {
    fn borrow(&self) -> &str {
        self
    }
}

impl hash::Hash for StaticStr {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        let s: &str = self;
        s.hash(state);
    }
}

impl PartialEq for StaticStr {
    fn eq(&self, other: &Self) -> bool {
        let this: &str = self;
        let other: &str = other;
        this == other
    }
}

impl Eq for StaticStr {}

impl PartialOrd for StaticStr {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for StaticStr {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        let this: &str = self;
        let other: &str = other;
        this.cmp(other)
    }
}
