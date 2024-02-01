use crate::util::{RcStr, ToStr};
use std::{
    borrow::Cow,
    rc::Rc,
    hash::{Hash, Hasher},
    thread_local,
};

/// Resource key.
#[derive(Debug, Eq, Clone)]
pub struct ResKey {
    /// Unique ID.
    /// TODO: upper bits will be used for sub-keys such as scene's pipelines.
    pub(crate) id: u64,

    /// Optional label. Use default() if you don't want label.
    pub(crate) label: RcStr,
}

impl ResKey {
    #[inline]
    pub fn new(id: impl Into<u64>, label: impl Into<RcStr>) -> Self {
        Self {
            id: id.into(),
            label: label.into(),
        }
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
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for ResKey {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T: Into<u64>> From<T> for ResKey {
    fn from(value: T) -> Self {
        Self {
            id: value.into(),
            ..Default::default()
        }
    }
}

impl Default for ResKey {
    fn default() -> Self {
        DUMMY_RC_STR.with(|label| Self::new(0_u64, label))
    }
}

thread_local! {
    static DUMMY_RC_STR: RcStr = "".into();
}
