use super::{Event, Message};
use crate::{common::AppHasher, ty, worker::msg};
use std::{
    any::{Any, TypeId},
    collections::{HashMap, VecDeque},
    fmt::Debug,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

#[derive(Debug)]
pub struct MessageQueue<M>(VecDeque<M>);

impl<M> Deref for MessageQueue<M> {
    type Target = VecDeque<M>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<M> DerefMut for MessageQueue<M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub(crate) struct MessageManager {
    /// Event queues.  
    /// Each queue is a sole [`Resource`].
    //
    // TypeId of event -> `EventQueue`.
    ev_queues: HashMap<TypeId, Box<dyn Any>, AppHasher>,

    /// Command queues.  
    /// Each queue is a sole [`Resource`].
    //
    // TypeId of command -> `EventQueue`.
    cmd_queues: HashMap<TypeId, Box<dyn Any>, AppHasher>,

    /// clear() methods of event queues.
    ev_clear: HashMap<TypeId, Box<dyn FnMut()>, AppHasher>,

    /// is_empty() methods of command queues.
    cmd_is_empty: HashMap<TypeId, Box<dyn Fn() -> bool>, AppHasher>,
}

impl Debug for MessageManager {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MessageManager")
            .field("ev_queues", &self.ev_queues)
            .field("cmd_queues", &self.cmd_queues)
            .finish_non_exhaustive()
    }
}

impl MessageManager {
    pub(crate) fn new() -> Self {
        Self {
            ev_queues: HashMap::default(),
            cmd_queues: HashMap::default(),
            ev_clear: HashMap::default(),
            cmd_is_empty: HashMap::default(),
        }
    }

    pub(crate) fn iter_command_types(&self) -> impl Iterator<Item = &TypeId> {
        self.cmd_queues.iter().map(|(k, _v)| k)
    }

    pub(crate) fn has_command(&self, ty: &TypeId) -> bool {
        if let Some(is_empty) = self.cmd_is_empty.get(ty) {
            !is_empty()
        } else {
            false
        }
    }

    pub(crate) fn register_event<M: Message>(&mut self) {
        debug_assert!(M::is_event());

        // Registers event queue.
        let queue = MessageQueue(VecDeque::<M>::new());
        self.ev_queues.insert(ty!(M), Box::new(queue));

        // Registers clear() method too.
        let mut ptr = unsafe { self.queue_ptr::<M>().unwrap_unchecked() };
        let clear = move || {
            // Safety: Validity of the ptr is guaranteed by this structure.
            unsafe { ptr.as_mut().clear() }
        };
        self.ev_clear.insert(ty!(M), Box::new(clear));
    }

    pub(crate) fn register_command<M: Message>(&mut self) {
        debug_assert!(M::is_command());

        // Registers command queue.
        let queue = MessageQueue(VecDeque::<M>::new());
        self.cmd_queues.insert(ty!(M), Box::new(queue));

        // Registers is_empty() method too.
        let ptr = unsafe { self.queue_ptr::<M>().unwrap_unchecked() };
        let is_empty = move || {
            // Safety: Validity of the ptr is guaranteed by this structure.
            unsafe { ptr.as_ref().is_empty() }
        };
        self.cmd_is_empty.insert(ty!(M), Box::new(is_empty));
    }

    pub(crate) fn queue_ptr<M: Message>(&mut self) -> Option<NonNull<MessageQueue<M>>> {
        fn inner<M: Message>(
            queues: &mut HashMap<TypeId, Box<dyn Any>, AppHasher>,
        ) -> Option<NonNull<MessageQueue<M>>> {
            queues.get_mut(&ty!(M)).map(|queue| {
                let queue = queue.downcast_mut::<MessageQueue<M>>().unwrap();
                let ptr = queue as *mut _;
                // Safety: Infallible.
                unsafe { NonNull::new_unchecked(ptr) }
            })
        }

        let res = inner::<M>(&mut self.ev_queues);
        if res.is_some() {
            return res;
        }
        inner::<M>(&mut self.cmd_queues)
    }

    pub(crate) fn event_queue_ptrs<'a>(
        &'a mut self,
    ) -> impl Iterator<Item = (TypeId, NonNull<u8>)> + 'a {
        self.ev_queues.iter_mut().map(|(k, v)| {
            // Safety: Infallible.
            let ptr = unsafe { NonNull::new_unchecked(v.as_mut() as *mut _ as *mut u8) };
            (*k, ptr)
        })
    }

    /// Helper function.
    ///
    /// # Safety
    ///
    /// Undefined behavior if the queue is referenced somewhere.
    pub(crate) unsafe fn push_resize(&mut self, msg: msg::MsgEventCanvasResize) {
        // Safety: We registered the event in the constructor.
        let queue = unsafe {
            self.queue_ptr::<Event<msg::MsgEventCanvasResize>>()
                .unwrap()
                .as_mut()
        };
        queue.push_back(Event(msg));
    }

    /// Helper function.
    ///
    /// # Safety
    ///
    /// Undefined behavior if the queue is referenced somewhere.
    pub(crate) unsafe fn push_mouse_move(&mut self, msg: msg::MsgEventMouseMove) {
        // Safety: We registered the event in the constructor.
        let queue = unsafe {
            self.queue_ptr::<Event<msg::MsgEventMouseMove>>()
                .unwrap()
                .as_mut()
        };
        queue.push_back(Event(msg));
    }

    /// Helper function.
    ///
    /// # Safety
    ///
    /// Undefined behavior if the queue is referenced somewhere.
    pub(crate) unsafe fn push_click(&mut self, msg: msg::MsgEventClick) {
        // Safety: We registered the event in the constructor.
        let queue = unsafe {
            self.queue_ptr::<Event<msg::MsgEventClick>>()
                .unwrap()
                .as_mut()
        };
        queue.push_back(Event(msg))
    }

    /// # Safety
    ///
    /// Undefined behavior if any queue is referenced outside.
    pub(crate) unsafe fn clear(&mut self) {
        for clear in self.ev_clear.values_mut() {
            clear();
        }
    }
}

impl Default for MessageManager {
    fn default() -> Self {
        Self::new()
    }
}
