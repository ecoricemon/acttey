use crate::util::string::ToStr;
use std::{
    borrow::Cow,
    hash::{Hash, Hasher},
    sync::{Arc, OnceLock},
};

/// Id for distinguishing objects such as colors, meshes, or graphics pipelines.
#[derive(Debug, Eq, Clone)]
pub struct ObjectKey {
    /// Unique ID.
    /// TODO: upper bits will be used for automatically generated sub-keys
    /// such as scene's pipelines.
    id: u32,

    /// Optional label. Use default() if you don't want label.
    label: Arc<str>,
}

impl ObjectKey {
    pub const fn new(id: u32, label: Arc<str>) -> Self {
        Self { id, label }
    }

    pub fn dummy() -> Self {
        static DUMMY: OnceLock<ObjectKey> = OnceLock::new();
        DUMMY
            .get_or_init(|| Self::new(u32::MAX, "dummy".into()))
            .clone()
    }

    pub const fn is_dummy(&self) -> bool {
        self.id == u32::MAX
    }

    pub const fn id(&self) -> u32 {
        self.id
    }

    pub const fn label(&self) -> &Arc<str> {
        &self.label
    }
}

impl From<&ObjectKey> for ObjectKey {
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
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for ObjectKey {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T: Into<u32>> From<T> for ObjectKey {
    fn from(value: T) -> Self {
        Self {
            id: value.into(),
            ..Default::default()
        }
    }
}

impl Default for ObjectKey {
    fn default() -> Self {
        Self::dummy()
    }
}
