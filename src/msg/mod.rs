pub mod manager;

pub mod prelude {
    pub use super::{Command, Event, EventType};
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Event type that users can register.
pub enum EventType {
    /// JS "scale" event.
    Scale = 0,
    /// JS "mousemove" event.
    MouseMove,
    /// JS "click" event.
    Click,
    /// JS "resize" event.
    Resize,
}

/// Number of variants in the [`EventType`].
pub(crate) const EVENT_TYPE_NUM: usize = 4;

/// This helps us not to make mistake when we made a change to the `EventType`.
const _: () = {
    // Checks out if there's a brand new variant in the `EventType`.
    match EventType::Scale {
        EventType::Scale => { /* Is this the first varitant? See assert below */ }
        EventType::MouseMove => {}
        EventType::Click => {}
        EventType::Resize => { /* Is this the last variant? See assert below */ }
    }

    // Variants in the `EventType` is used as indices.
    // So that the first one should be 0.
    assert!(EventType::Scale as isize == 0);

    // The number of variants must be equal to the last one + 1.
    assert!(EventType::Resize as isize + 1 == EVENT_TYPE_NUM as isize);
};

impl From<EventType> for &'static str {
    fn from(value: EventType) -> Self {
        match value {
            EventType::Scale => "scale",
            EventType::Resize => "resize",
            EventType::MouseMove => "mousemove",
            EventType::Click => "click",
        }
    }
}

pub trait Message: 'static {
    /// Required.
    fn is_event() -> bool;

    /// Provided.
    fn is_command() -> bool {
        !Self::is_event()
    }
}

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct Event<T>(T);

impl<T: 'static> Message for Event<T> {
    fn is_event() -> bool {
        true
    }
}

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct Command<T>(pub T);

impl<T> Command<T> {
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T: 'static> Message for Command<T> {
    fn is_event() -> bool {
        false
    }
}
