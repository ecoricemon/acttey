use crate::util::string::{RcStr, ToStr};
use std::{
    borrow::Cow,
    hash::{Hash, Hasher},
    thread_local,
};

/// Id for disginguishing objects such as colors, meshes, or graphics pipelines.
#[derive(Debug, Eq, Clone)]
pub struct ObjectKey {
    /// Unique ID.
    /// TODO: upper bits will be used for automatically generated sub-keys
    /// such as scene's pipelines.
    pub(crate) id: u64,

    /// Optional label. Use default() if you don't want label.
    pub(crate) label: RcStr,
}

impl ObjectKey {
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

impl From<&ObjectKey> for ObjectKey {
    #[inline]
    fn from(value: &ObjectKey) -> Self {
        value.clone()
    }
}

impl ToStr for ObjectKey {
    fn to_str(&self) -> Cow<str> {
        Cow::Borrowed(&self.label)
    }
}

impl Hash for ObjectKey {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for ObjectKey {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T: Into<u64>> From<T> for ObjectKey {
    #[inline]
    fn from(value: T) -> Self {
        Self {
            id: value.into(),
            ..Default::default()
        }
    }
}

impl Default for ObjectKey {
    #[inline]
    fn default() -> Self {
        DUMMY_RC_STR.with(|label| Self::new(0_u64, label.clone()))
    }
}

thread_local! {
    pub(crate) static DUMMY_RC_STR: RcStr = RcStr::new("");
}
