use super::{
    request::{RequestBuffer, RequestKey},
    system::Invoke,
};
use crate::ds::ptr::ManagedConstPtr;
use std::{
    fmt::{Debug, Display},
    marker::PhantomPinned,
    ops::{AddAssign, Deref},
    pin::Pin,
    ptr::NonNull,
    sync::mpsc::{self, Receiver, RecvError, SendError, Sender, TryRecvError},
    thread::Thread,
};

pub trait BuildWorker {
    fn new(name: String) -> Self;
    fn spawn(self) -> impl Work;
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
pub struct WorkerIndex(usize);

impl WorkerIndex {
    pub const fn new(index: usize) -> Self {
        Self(index)
    }

    pub const fn into_inner(self) -> usize {
        self.0
    }
}

impl AddAssign<usize> for WorkerIndex {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

impl Display for WorkerIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub enum ChMsg {
    /// Main thread sends function pointer and its argument pointer to worker.
    /// Worker that received this message must call the function with the argument.
    Task(NonNull<dyn Invoke>, NonNull<RequestBuffer>),

    /// Main thread sends this message to notify end of the job.
    End,

    /// When a worker finishes its task, it will send this message to the main thread.
    //
    // Channel is based on mpsc. So it's needed to include identification of sender.
    Fin(WorkerIndex, RequestKey),
}

impl Debug for ChMsg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Task(task_ptr, buf_ptr) => {
                write!(f, "ChMsg::Task({task_ptr:?}, {buf_ptr:?})")
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
                ChMsg::Task(mut task_ptr, mut buf_ptr) => {
                    let task = unsafe { task_ptr.as_mut() };
                    let buf = unsafe { buf_ptr.as_mut() };
                    task.invoke(buf);
                    self.ch.send(ChMsg::Fin(self.widx, task.rkey())).unwrap();
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
