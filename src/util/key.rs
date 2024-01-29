use crate::util::ToStr;
use std::{
    borrow::Cow,
    rc::Rc,
    hash::{Hash, Hasher},
    thread_local,
};

/// Resource key.
#[derive(Debug, Eq, Clone)]
pub struct ResKey {
    id: u32,
    label: Rc<str>,
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

impl<T: Into<u32>> From<T> for ResKey {
    fn from(value: T) -> Self {
        Self {
            id: value.into(),
            ..Default::default()
        }
    }
}

impl Default for ResKey {
    fn default() -> Self {
        DUMMY_RC_STR.with(|s| {
            Self {
                id: 0,
                label: Rc::clone(s),
            }
        })
    }
}

thread_local! {
    static DUMMY_RC_STR: Rc<str> = Rc::from("dummy");
}
