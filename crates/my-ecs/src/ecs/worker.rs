use super::{sched::ctrl::SubContext, sys::system::SystemId};
use crate::ds::ManagedConstPtr;
use std::{any::Any, fmt};

pub mod prelude {
    pub use super::Work;
}

/// A trait for worker.
///
/// Worker should be able to `park` itself or `unpark`ed by a function call.
/// When the worker is unparked, worker should get ready to handle messages
/// using the passed [`SubContext`]. See an example code below.
///
/// ```ignore
/// fn unpark(&mut self, cx: ManagedConstPtr<SubContext>) -> bool {
///     // Sends `cx` to the associated worker.
///     self.tx.send(cx).is_ok();
/// }
///
/// fn worker_function(&self) {
///     // Parking during idle.
///     while let Ok(cx) = rx.recv() {
///         // `SubContext` provides message handling function for you.
///         SubContext::execute(cx);
///     }
/// }
/// ```
pub trait Work {
    /// Wakes the associated worker then call [`SubContext::execute`] on the
    /// worker, and then returns true if all is ok.
    fn unpark(&mut self, cx: ManagedConstPtr<SubContext>) -> bool;

    /// Blocks then returns true.
    ///
    /// It's highly recommended not to use long-term spin-lock because it may
    /// last undefinitely.
    fn park(&mut self) -> bool {
        true
    }

    /// Returns worker name.
    fn name(&self) -> &str;
}

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub(crate) struct WorkerId {
    id: u32,
    group_index: u16,
    worker_index: u16,
}

impl WorkerId {
    const DUMMY: Self = Self {
        id: u32::MAX,
        group_index: u16::MAX,
        worker_index: u16::MAX,
    };

    pub(crate) const fn new(id: u32, group_index: u16, worker_index: u16) -> Self {
        Self {
            id,
            group_index,
            worker_index,
        }
    }

    pub(crate) const fn dummy() -> Self {
        Self::DUMMY
    }

    pub(crate) const fn group_index(&self) -> u16 {
        self.group_index
    }

    pub(crate) const fn worker_index(&self) -> u16 {
        self.worker_index
    }
}

pub(crate) enum Message {
    Handle(WorkerId),
    /// When a worker finishes its task, it will send this message to the main thread.
    //
    // Channel is based on mpsc. So it's needed to include identification of sender.
    Fin(WorkerId, SystemId),

    Aborted(WorkerId, SystemId),

    /// If a worker panics, the worker must notify it.
    Panic(PanicMessage),
}

impl fmt::Debug for Message {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Handle(wid) => write!(f, "Message::Handle({wid:?})"),
            Self::Fin(wid, sid) => write!(f, "Message::Fin({wid:?}, {sid:?})"),
            Self::Aborted(wid, sid) => write!(f, "Message::Aborted({wid:?}, {sid:?})"),
            Self::Panic(msg) => write!(f, "Message::Panic({msg:?})"),
        }
    }
}

pub(crate) struct PanicMessage {
    pub(crate) wid: WorkerId,
    pub(crate) sid: SystemId,
    pub(crate) payload: Box<dyn Any + Send>,
    pub(crate) unrecoverable: bool,
}

impl fmt::Debug for PanicMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PanicMessage")
            .field("wid", &self.wid)
            .field("sid", &self.sid)
            .field("unrecoverable", &self.unrecoverable)
            .finish_non_exhaustive()
    }
}
