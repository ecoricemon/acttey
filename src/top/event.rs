use crate::worker::msg;
use std::collections::{
    vec_deque::{Drain, Iter},
    VecDeque,
};

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
pub(super) const EVENT_TYPE_NUM: usize = 4;

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

pub enum Event {
    Resized(msg::MsgEventCanvasResize),
    MouseMove(msg::MsgEventMouseMove),
    Click(msg::MsgEventClick),
}

pub struct EventManager {
    resize: VecDeque<msg::MsgEventCanvasResize>,
    mouse_move: VecDeque<msg::MsgEventMouseMove>,
    click: VecDeque<msg::MsgEventClick>,
}

impl EventManager {
    pub fn new() -> Self {
        Self {
            resize: VecDeque::with_capacity(1),
            mouse_move: VecDeque::with_capacity(16),
            click: VecDeque::with_capacity(8),
        }
    }

    #[inline]
    pub(crate) fn push(&mut self, event: Event) {
        match event {
            Event::Resized(msg) => self.resize.push_back(msg),
            Event::MouseMove(msg) => self.mouse_move.push_back(msg),
            Event::Click(msg) => self.click.push_back(msg),
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.resize.clear();
        self.mouse_move.clear();
        self.click.clear();
    }

    #[inline]
    pub fn iter_resize(&self) -> Iter<msg::MsgEventCanvasResize> {
        self.resize.iter()
    }

    #[inline]
    pub fn drain_resize(&mut self) -> Drain<msg::MsgEventCanvasResize> {
        self.resize.drain(..)
    }

    #[inline]
    pub fn iter_mouse_move_event(&self) -> Iter<msg::MsgEventMouseMove> {
        self.mouse_move.iter()
    }

    #[inline]
    pub fn drain_mouse_move_event(&mut self) -> Drain<msg::MsgEventMouseMove> {
        self.mouse_move.drain(..)
    }

    #[inline]
    pub fn iter_click_event(&self) -> Iter<msg::MsgEventClick> {
        self.click.iter()
    }

    #[inline]
    pub fn drain_click_event(&mut self) -> Drain<msg::MsgEventClick> {
        self.click.drain(..)
    }
}

impl Default for EventManager {
    fn default() -> Self {
        Self::new()
    }
}
