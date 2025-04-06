use crate::{
    ActteyError, AppHasher,
    ds::vec::GenVec,
    util::{AsOr, StaticStr, macros::debug_format},
};
use my_ecs::prelude::{Or, ResourceId, With};
use std::collections::HashMap;

pub trait HasLabel {
    fn label(&self) -> &str;
}

#[derive(Debug)]
pub(crate) struct LabelledGenVec<T> {
    values: GenVec<T, AppHasher>,
    lmap: HashMap<StaticStr, With<usize, u64>, AppHasher>,
}

impl<T> LabelledGenVec<T> {
    pub(crate) fn new() -> Self {
        Self {
            values: GenVec::new(),
            lmap: HashMap::default(),
        }
    }
}

impl<T> LabelledGenVec<T>
where
    T: HasLabel,
{
    pub(crate) fn add(&mut self, value: T) -> Result<With<usize, u64>, ActteyError> {
        // Is the label unique?
        if self.lmap.contains_key(value.label()) {
            let reason = debug_format!("{} conflicts", value.label());
            return Err(ActteyError::Conflict(reason, ()));
        }

        // Stores value.
        let label = StaticStr::new(value.label().to_owned());
        let ii = self.values.add(value);

        // Stores label-value mapping.
        self.lmap.insert(label, ii);

        Ok(ii)
    }

    pub(crate) fn remove(&mut self, key: Or<With<usize, u64>, &str>) -> Option<T> {
        let ii = match key {
            Or::A(ii) => ii,
            Or::B(label) => *self.lmap.get(label)?,
        };

        // Removes value.
        let old = self.values.remove(ii);

        // Removes label-value mapping.
        if let Some(old) = &old {
            self.lmap.remove(old.label());
        }

        old
    }

    pub(crate) fn get(&self, key: Or<With<usize, u64>, &str>) -> Option<&T> {
        let ii = match key {
            Or::A(ii) => ii,
            Or::B(label) => *self.lmap.get(label)?,
        };

        self.values.get(ii)
    }

    pub(crate) fn get_mut(&mut self, key: Or<With<usize, u64>, &str>) -> Option<&mut T> {
        let ii = match key {
            Or::A(ii) => ii,
            Or::B(label) => *self.lmap.get(label)?,
        };

        self.values.get_mut(ii)
    }
}

impl<T> Default for LabelledGenVec<T> {
    fn default() -> Self {
        Self::new()
    }
}

macro_rules! impl_as_or_with_str {
    ($a:ty) => {
        impl AsOr<$a, str> for $a {
            fn as_or(&self) -> Or<&$a, &str> {
                Or::A(self)
            }
        }

        impl AsOr<$a, str> for &$a {
            fn as_or(&self) -> Or<&$a, &str> {
                Or::A(self)
            }
        }

        impl AsOr<$a, str> for str {
            fn as_or(&self) -> Or<&$a, &str> {
                Or::B(self)
            }
        }

        impl AsOr<$a, str> for &str {
            fn as_or(&self) -> Or<&$a, &str> {
                Or::B(self)
            }
        }

        impl AsOr<$a, str> for Box<str> {
            fn as_or(&self) -> Or<&$a, &str> {
                Or::B(self)
            }
        }

        impl AsOr<$a, str> for std::rc::Rc<str> {
            fn as_or(&self) -> Or<&$a, &str> {
                Or::B(self)
            }
        }

        impl AsOr<$a, str> for std::sync::Arc<str> {
            fn as_or(&self) -> Or<&$a, &str> {
                Or::B(self)
            }
        }

        impl<'a> AsOr<$a, str> for std::borrow::Cow<'a, str> {
            fn as_or(&self) -> Or<&$a, &str> {
                Or::B(self)
            }
        }

        impl AsOr<$a, str> for StaticStr {
            fn as_or(&self) -> Or<&$a, &str> {
                Or::B(self)
            }
        }

        impl AsOr<$a, str> for String {
            fn as_or(&self) -> Or<&$a, &str> {
                Or::B(self)
            }
        }
    };
}
impl_as_or_with_str!(ResourceId);
