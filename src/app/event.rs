use ahash::AHashMap;
use std::{
    collections::{
        vec_deque::{Drain, Iter},
        VecDeque,
    },
};
use wasm_bindgen::prelude::*;

pub enum Command {
    RemoveSystem(u32),
}

pub enum Event {
    Resized(u32),
    Mouse(u32, web_sys::MouseEvent),
    Keyboard(u32, web_sys::KeyboardEvent),
    Command(Command),
}

impl From<(u32, web_sys::MouseEvent)> for Event {
    #[inline(always)]
    fn from(value: (u32, web_sys::MouseEvent)) -> Self {
        Self::Mouse(value.0, value.1)
    }
}

impl From<(u32, web_sys::KeyboardEvent)> for Event {
    #[inline(always)]
    fn from(value: (u32, web_sys::KeyboardEvent)) -> Self {
        Self::Keyboard(value.0, value.1)
    }
}

pub(crate) enum EventListener {
    NoArg(Closure<dyn Fn()>),
    Mouse(Closure<dyn Fn(web_sys::MouseEvent)>),
    Keyboard(Closure<dyn Fn(web_sys::KeyboardEvent)>),
}

impl From<Closure<dyn Fn()>> for EventListener {
    fn from(value: Closure<dyn Fn()>) -> Self {
        EventListener::NoArg(value)
    }
}

impl From<Closure<dyn Fn(web_sys::MouseEvent)>> for EventListener {
    fn from(value: Closure<dyn Fn(web_sys::MouseEvent)>) -> Self {
        EventListener::Mouse(value)
    }
}

impl From<Closure<dyn Fn(web_sys::KeyboardEvent)>> for EventListener {
    fn from(value: Closure<dyn Fn(web_sys::KeyboardEvent)>) -> Self {
        EventListener::Keyboard(value)
    }
}

pub struct EventManager {
    listeners: AHashMap<String, EventListener>,
    resized: VecDeque<u32>,
    mouse: VecDeque<(u32, web_sys::MouseEvent)>,
    keyboard: VecDeque<(u32, web_sys::KeyboardEvent)>,
    command: VecDeque<Command>,
}

impl EventManager {
    pub fn new() -> Self {
        Self {
            listeners: AHashMap::new(),
            resized: VecDeque::with_capacity(1),
            mouse: VecDeque::with_capacity(16),
            keyboard: VecDeque::with_capacity(8),
            command: VecDeque::new(),
        }
    }

    pub fn contains_event_listener(&self, selectors: &str, type_: &str) -> bool {
        self.listeners
            .contains_key(&Self::listeners_key(selectors, type_))
    }

    #[inline(always)]
    fn listeners_key(selectors: &str, type_: &str) -> String {
        format!("{} {}", selectors, type_)
    }

    pub(crate) fn add_event_listener(
        &mut self,
        selectors: &str,
        type_: &str,
        listener: EventListener,
    ) -> Option<EventListener> {
        self.listeners
            .insert(Self::listeners_key(selectors, type_), listener)
    }

    pub(crate) fn remove_event_listener(
        &mut self,
        selectors: &str,
        type_: &str,
    ) -> Option<EventListener> {
        self.listeners
            .remove(&Self::listeners_key(selectors, type_))
    }

    #[inline]
    pub(crate) fn push(&mut self, event: Event) {
        match event {
            Event::Resized(handle) => {
                self.resized.clear();
                self.resized.push_back(handle);
            }
            Event::Mouse(handle, event) => self.mouse.push_back((handle, event)),
            Event::Keyboard(handle, event) => self.keyboard.push_back((handle, event)),
            Event::Command(command) => self.command.push_back(command),
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.resized.clear();
        self.mouse.clear();
        self.keyboard.clear();
        // User should process command, so that not clear in here.
    }

    #[inline]
    pub fn is_command_empty(&self) -> bool {
        self.command.is_empty()
    }

    #[inline]
    pub fn iter_resized(&self) -> Iter<u32> {
        self.resized.iter()
    }

    #[inline]
    pub fn drain_resized(&mut self) -> Drain<u32> {
        self.resized.drain(..)
    }

    #[inline]
    pub fn iter_mouse_events(&self) -> Iter<(u32, web_sys::MouseEvent)> {
        self.mouse.iter()
    }

    #[inline]
    pub fn drain_mouse_events(&mut self) -> Drain<(u32, web_sys::MouseEvent)> {
        self.mouse.drain(..)
    }

    #[inline]
    pub fn iter_keyboard_events(&self) -> Iter<(u32, web_sys::KeyboardEvent)> {
        self.keyboard.iter()
    }

    #[inline]
    pub fn drain_keyboard_events(&mut self) -> Drain<(u32, web_sys::KeyboardEvent)> {
        self.keyboard.drain(..)
    }

    #[inline]
    pub fn iter_commands(&self) -> Iter<Command> {
        self.command.iter()
    }

    #[inline]
    pub fn drain_commands(&mut self) -> Drain<Command> {
        self.command.drain(..)
    }
}

impl Default for EventManager {
    fn default() -> Self {
        Self::new()
    }
}
