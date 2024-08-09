pub mod prelude {
    pub use super::EventType;
}

/// Number of variants in the [`EventType`].
//
// NOTE: We're waiting for `std::mem::variant_count()` to be stable.
pub(crate) const EVENT_TYPE_NUM: usize = 4;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// Event type that users can register.
pub enum EventType {
    Scale = 0,
    MouseMove,
    Click,
    Resize,
}

const EVENT_TYPE_STR: [&str; EVENT_TYPE_NUM] = ["scale", "mousemove", "click", "resize"];

const IDX_TO_EVENT: [EventType; EVENT_TYPE_NUM] = [
    EventType::Scale,
    EventType::MouseMove,
    EventType::Click,
    EventType::Resize,
];

/// This helps us not to make mistakes when we made a change to the [`EventType`].
const _: () = {
    let mut cnt = 0;

    macro_rules! validate {
        ($var:ident, $s:expr, $i:expr) => {
            assert!(const_str::equal!(EventType::$var.as_str(), $s));
            assert!(EventType::$var as isize == IDX_TO_EVENT[$i] as isize);
            cnt += 1;
        };
    }

    validate!(Scale, "scale", 0);
    validate!(MouseMove, "mousemove", 1);
    validate!(Click, "click", 2);
    validate!(Resize, "resize", 3);

    // All variants are checked out?
    assert!(cnt == EVENT_TYPE_NUM);
};

impl EventType {
    pub const fn as_str(&self) -> &'static str {
        EVENT_TYPE_STR[*self as isize as usize]
    }

    pub fn from_str(s: &str) -> Option<Self> {
        (0..EVENT_TYPE_NUM)
            .filter(|&i| EVENT_TYPE_STR[i] == s)
            .map(|i| IDX_TO_EVENT[i])
            .next()
    }
}

impl ToString for EventType {
    fn to_string(&self) -> String {
        self.as_str().to_owned()
    }
}
