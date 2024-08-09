use super::sys::{
    request::RequestBuffer,
    system::{Invoke, SystemId},
};
use crate::ds::prelude::*;
use std::{
    fmt,
    marker::PhantomPinned,
    ops::{AddAssign, Deref},
    pin::Pin,
    sync::mpsc::{self, Receiver, RecvError, SendError, Sender, TryRecvError},
    thread::Thread,
};

pub trait BuildWorker {
    type Output: Work;

    fn new(name: String) -> Self;
    fn spawn(self) -> Result<Self::Output, String>;
}

pub trait Work {
    /// If succeeded to wake the worker up, returns true.
    fn unpark(&mut self, job: ManagedConstPtr<Job>) -> bool;

    /// If succeeded to make the worker sleep, returns true.
    fn park(&mut self) -> bool {
        true
    }

    /// Returns worker name.
    fn get_name(&self) -> &str;
}

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
#[repr(transparent)]
pub(crate) struct WorkerIndex(usize);

impl WorkerIndex {
    pub(crate) const fn new(index: usize) -> Self {
        Self(index)
    }

    pub(crate) const fn into_inner(self) -> usize {
        self.0
    }
}

impl AddAssign<usize> for WorkerIndex {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

impl fmt::Display for WorkerIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub(crate) enum ChMsg {
    /// Main thread sends function pointer and its argument pointer to worker.
    /// Worker that received this message must call the function with the argument.
    Task(
        ManagedMutPtr<dyn Invoke>,
        ManagedMutPtr<RequestBuffer>,
        SystemId,
    ),

    /// Main thread sends this message to notify end of the job.
    End,

    /// When a worker finishes its task, it will send this message to the main thread.
    //
    // Channel is based on mpsc. So it's needed to include identification of sender.
    Fin(WorkerIndex, SystemId),
}

impl fmt::Debug for ChMsg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Task(task_ptr, buf_ptr, sid) => {
                write!(f, "ChMsg::Task({:?}, {:?}, {:?})", task_ptr, buf_ptr, sid)
            }
            Self::End => write!(f, "ChMsg::End"),
            Self::Fin(widx, rkey) => write!(f, "ChMsg::Fin({widx:?}, {rkey:?})"),
        }
    }
}

#[derive(Debug)]
pub(crate) struct MainChannel {
    /// For cloning only.
    tx: Sender<ChMsg>,

    /// Receiving channel.
    rx: Receiver<ChMsg>,
}

impl MainChannel {
    pub(crate) fn new() -> Self {
        let (tx, rx) = mpsc::channel();
        Self { tx, rx }
    }

    pub(crate) fn clone_tx(&self) -> Sender<ChMsg> {
        self.tx.clone()
    }

    /// Tries to receive a channel message, but doesn't block.
    pub(crate) fn try_recv(&self) -> Result<ChMsg, TryRecvError> {
        self.rx.try_recv()
    }
}

impl Default for MainChannel {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub(crate) struct SubChannel {
    /// Main channel can send messages through this transmit terminal.
    tx: Sender<ChMsg>,

    /// Worker will receive the pointer to this job.
    job: Pin<Box<Job>>,
}

impl SubChannel {
    pub(crate) fn new(tx: Sender<ChMsg>, opp: Thread, widx: WorkerIndex) -> Self {
        let (ch, tx) = WorkerChannel::new(tx, opp);
        Self {
            tx,
            job: Box::pin(Job {
                ch,
                widx,
                _pin: PhantomPinned,
            }),
        }
    }

    pub(crate) fn job_ptr(&self) -> *const Job {
        self.job.as_ref().get_ref() as *const _
    }
}

impl Deref for SubChannel {
    type Target = Sender<ChMsg>;

    fn deref(&self) -> &Self::Target {
        &self.tx
    }
}

#[derive(Debug)]
pub struct Job {
    ch: WorkerChannel,
    widx: WorkerIndex,
    _pin: PhantomPinned,
}

impl Job {
    pub fn process(&self) {
        while let Ok(msg) = self.ch.recv() {
            match msg {
                // Worker received a task.
                ChMsg::Task(mut task, mut buf, sid) => {
                    task.invoke(&mut buf);

                    // We need to drop `ManagedMutPtr` before we send Fin message.
                    // In debug mode, `ManagedMutPtr` is checked whether it's unique or not.
                    // If main thread is unparked before we drop those pointers,
                    // main thread could create another `ManagedMutPtr`
                    // that has the exact same address because it's pretended freed.
                    // As a result, it causes panic.
                    drop(task);
                    drop(buf);

                    self.ch.send(ChMsg::Fin(self.widx, sid)).unwrap();
                    self.ch.unpark_opposite();
                }

                // Worker is notified the job is done.
                ChMsg::End => return,

                // The direction of this message is Main -> Worker.
                ChMsg::Fin(..) => unreachable!("message going towards main"),
            }
        }
        unreachable!("worker failed to receive message");
    }
}

#[derive(Debug)]
pub(crate) struct WorkerChannel {
    tx: Sender<ChMsg>,
    rx: Receiver<ChMsg>,
    opp: Thread,
}

impl WorkerChannel {
    pub(crate) fn new(tx: Sender<ChMsg>, opp: Thread) -> (Self, Sender<ChMsg>) {
        let (tx_other, rx_this) = mpsc::channel();
        let ch = Self {
            tx,
            rx: rx_this,
            opp,
        };
        (ch, tx_other)
    }

    pub(crate) fn recv(&self) -> Result<ChMsg, RecvError> {
        self.rx.recv()
    }

    pub(crate) fn send(&self, msg: ChMsg) -> Result<(), SendError<ChMsg>> {
        self.tx.send(msg)
    }

    pub(crate) fn unpark_opposite(&self) {
        self.opp.unpark();
    }
}
