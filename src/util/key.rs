use crate::util::string::{RcStr, ToStr};
use std::{
    borrow::Cow,
    hash::{Hash, Hasher},
    thread_local,
};

/// Resource key.
#[derive(Debug, Eq, Clone)]
pub struct ResKey {
    /// Unique ID.
    /// TODO: upper bits will be used for automatically generated sub-keys
    /// such as scene's pipelines.
    pub(crate) id: u64,

    /// Optional label. Use default() if you don't want label.
    pub(crate) label: RcStr,
}

impl ResKey {
    #[inline]
    pub const fn new(id: u64, label: RcStr) -> Self {
        Self { id, label }
    }

    #[inline]
    pub fn with_id(mut self, id: impl Into<u64>) -> Self {
        self.id = id.into();
        self
    }

    #[inline]
    pub fn with_label(mut self, label: impl Into<RcStr>) -> Self {
        self.label = label.into();
        self
    }
}

impl From<&ResKey> for ResKey {
    #[inline]
    fn from(value: &ResKey) -> Self {
        value.clone()
    }
}

impl ToStr for ResKey {
    fn to_str(&self) -> Cow<str> {
        Cow::Borrowed(&self.label)
    }
}

impl Hash for ResKey {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for ResKey {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T: Into<u64>> From<T> for ResKey {
    #[inline]
    fn from(value: T) -> Self {
        Self {
            id: value.into(),
            ..Default::default()
        }
    }
}

impl Default for ResKey {
    #[inline]
    fn default() -> Self {
        DUMMY_RC_STR.with(|label| Self::new(0_u64, label.clone()))
    }
}

thread_local! {
    pub(crate) static DUMMY_RC_STR: RcStr = "".into();
}
