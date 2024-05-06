pub mod msg;

use msg::ChMsg;
use std::{
    fmt::{Debug, Display},
    marker::PhantomPinned,
    num::NonZeroU32,
    ops::AddAssign,
    pin::Pin,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::{self, Receiver, RecvError, SendError, Sender, TryRecvError},
        Arc,
    },
    thread::{self, Thread},
};
use wasm_bindgen::prelude::*;

/// Binds JS.
#[wasm_bindgen(module = "/src/worker/workerGen.js")]
extern "C" {
    /// Spawn new worker in JS side in order to make bundler know about dependency.
    #[wasm_bindgen(js_name = "createWorker")]
    fn create_worker(name: &str) -> web_sys::Worker;
}

/// Binds JS.
/// This makes wasm-bindgen bring `worker.js` to the `pkg` directory.
/// So that bundler can bundle it together.
#[wasm_bindgen(module = "/src/worker/worker.js")]
extern "C" {
    /// Nothing to do.
    #[wasm_bindgen]
    fn attachWorker();
}

#[derive(Debug)]
pub struct MainWorker {
    /// JS worker handle.
    handle_js: web_sys::Worker,

    /// Worker name. You can see this name in browser's dev tool.
    _name: String,

    /// Callback for message from main worker.
    /// Not used for now.
    callback: Closure<dyn FnMut(web_sys::Event)>,
}

impl MainWorker {
    /// Spawns main worker from the window context.
    /// Main worker has id 0.
    pub fn spawn(name: &str) -> Result<Self, JsValue> {
        // Creates a new worker.
        let handle_js = create_worker(name);

        // Sets our dummy callback.
        let callback = Closure::new(|_| {});
        handle_js.set_onmessage(Some(callback.as_ref().unchecked_ref()));

        // Initializes the worker.
        let msg = js_sys::Array::new_with_length(3);
        let id = 0;
        msg.set(0, id.into());
        msg.set(1, wasm_bindgen::module());
        msg.set(2, wasm_bindgen::memory()); // TODO: optional
        handle_js.post_message(&msg)?;

        Ok(Self {
            handle_js,
            _name: name.to_owned(),
            callback,
        })
    }

    #[inline]
    pub fn handle_js(&self) -> &web_sys::Worker {
        &self.handle_js
    }

    /// Registers `callback`.
    pub fn register_callback(&mut self, callback: Closure<dyn FnMut(web_sys::Event)>) {
        self.handle_js
            .set_onmessage(Some(callback.as_ref().unchecked_ref()));
        self.callback = callback;
    }

    #[inline]
    pub fn post_message(&self, msg: &JsValue) -> Result<(), JsValue> {
        self.handle_js.post_message(msg)
    }
}

/// Sub worker data.
/// Main worker owns this sub worker data, so that data isn't visible to actual sub worker.
/// Instead, main worker send the sub worker a channel so that they can communicate through the channel.
pub struct Worker {
    /// JS worker handle.
    handle_js: web_sys::Worker,

    /// Rust worker handle.
    handle_rust: Thread,

    /// Worker name. You can see this name in browser's dev tool.
    name: String,

    /// Worker id given by [`Self::spawn()`] caller.
    /// It's not the same value with the [`Self::handle_rust`]'s.
    id: WorkerId,

    /// Callback for message from worker.
    callback: Closure<dyn FnMut(web_sys::Event)>,

    /// Send channel that belongs to main worker.
    /// We can send channel messages to the actual sub worker through this channel.
    tx: Sender<ChMsg>,

    /// Channel belong to sub worker.
    /// Pointer of [`Channel`] will be sent through JS postMessage() in [`Self::open`].
    /// Therefore, it must not be moved.
    ch: Pin<Box<Channel>>,

    /// Spawning worker takes time, but we need to know when it becomes ready in order to get its Rust handle safely.
    ready: Arc<AtomicBool>,
}

impl Worker {
    /// Spawns main worker from the window context.
    /// Main worker has id 0.
    pub fn spawn(name: &str, id: NonZeroU32, tx_common: Sender<ChMsg>) -> Result<Self, JsValue> {
        let id = WorkerId::new(id.get());

        // Creates a channel.
        // Don't be confused, Input `tx`: sub -> main, Output `tx`: main -> sub.
        let (ch, tx) = Channel::new(tx_common, thread::current());
        let ch = Box::pin(ch);

        // Creates a new worker.
        let handle_js = create_worker(name);

        // Sets current thread hanlde to the main's.
        // This handle will be updated in a minute.
        let handle_rust = thread::current();

        // Listens to worker's ready notification.
        let ready = Arc::new(AtomicBool::new(false));
        let ready_clone = Arc::clone(&ready);
        let callback = Closure::new(move |_| {
            ready_clone.store(true, Ordering::Relaxed);
        });
        handle_js.set_onmessage(Some(callback.as_ref().unchecked_ref()));

        // Initializes the worker.
        let msg = js_sys::Array::new_with_length(3);
        msg.set(0, id.into_u32().into());
        msg.set(1, wasm_bindgen::module());
        msg.set(2, wasm_bindgen::memory()); // TODO: optional
        handle_js.post_message(&msg)?;

        Ok(Self {
            handle_js,
            handle_rust,
            name: name.to_owned(),
            id,
            callback,
            tx,
            ch,
            ready,
        })
    }

    #[inline]
    pub fn handle_js(&self) -> &web_sys::Worker {
        &self.handle_js
    }

    #[inline]
    pub fn handle_rust(&self) -> &Thread {
        &self.handle_rust
    }

    #[inline]
    pub(crate) fn set_handle_rust(&mut self, handle: Thread) {
        self.handle_rust = handle;
        crate::log!(
            "[D] worker({}) has Rust thread handle: {:?}",
            self.id,
            self.handle_rust.id()
        );
    }

    #[inline]
    pub fn is_ready(&self) -> bool {
        self.ready.load(Ordering::Relaxed)
    }

    /// Registers `callback`.
    pub fn register_callback(&mut self, callback: Closure<dyn FnMut(web_sys::Event)>) {
        self.handle_js
            .set_onmessage(Some(callback.as_ref().unchecked_ref()));
        self.callback = callback;
    }

    /// Wakes this worker up and enters channel message loop.
    /// Don't forget to call [`Self::close`].
    //
    // NOTE: RAII doen't look good because schedule() becomes to need additional buffer.
    pub fn open(&self) {
        let ch_ptr = self.ch.as_ref().get_ref() as *const Channel;
        self.handle_js.post_message(&JsValue::from(ch_ptr)).unwrap();
    }

    /// Escapes from the channel message loop by sending [`ChMsg::End`].
    #[inline]
    pub fn close(&self) -> Result<(), SendError<ChMsg>> {
        self.tx.send(ChMsg::End)
    }

    /// Sends a channel message to the sub worker.
    #[inline]
    pub fn send(&self, msg: ChMsg) -> Result<(), SendError<ChMsg>> {
        self.tx.send(msg)
    }
}

impl Drop for Worker {
    /// Terminates web worker *immediately*.
    fn drop(&mut self) {
        self.handle_js.terminate();
        crate::log!("worker({}) was terminated", &self.name);
    }
}

impl Debug for Worker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Worker").field("name", &self.name).finish()
    }
}

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub struct WorkerId(u32);

impl WorkerId {
    pub const fn new(id: u32) -> Self {
        Self(id)
    }

    pub const fn into_u32(self) -> u32 {
        self.0
    }
}

impl AddAssign<u32> for WorkerId {
    #[inline]
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

impl Display for WorkerId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub struct MainChannel {
    /// For cloning only.
    tx: Sender<ChMsg>,

    /// Receiving channel.
    rx: Receiver<ChMsg>,
}

impl MainChannel {
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel();
        Self { tx, rx }
    }

    #[inline]
    pub fn clone_tx(&self) -> Sender<ChMsg> {
        self.tx.clone()
    }

    /// Blocks until receiving a channel message.
    #[inline]
    pub fn recv(&self) -> Result<ChMsg, RecvError> {
        self.rx.recv()
    }

    /// Tries to receive a channel message, but doesn't block.
    #[inline]
    pub fn try_recv(&self) -> Result<ChMsg, TryRecvError> {
        self.rx.try_recv()
    }
}

impl Default for MainChannel {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct Channel {
    tx: Sender<ChMsg>,
    rx: Receiver<ChMsg>,
    opp: Thread,
    _pin: PhantomPinned,
}

impl Channel {
    pub fn new(tx: Sender<ChMsg>, opp: Thread) -> (Self, Sender<ChMsg>) {
        let (tx_other, rx_this) = mpsc::channel();
        let ch = Self {
            tx,
            rx: rx_this,
            opp,
            _pin: PhantomPinned,
        };
        (ch, tx_other)
    }

    #[inline]
    pub fn recv(&self) -> Result<ChMsg, RecvError> {
        self.rx.recv()
    }

    #[inline]
    pub fn try_recv(&self) -> Result<ChMsg, TryRecvError> {
        self.rx.try_recv()
    }

    #[inline]
    pub fn send(&self, msg: ChMsg) -> Result<(), SendError<ChMsg>> {
        self.tx.send(msg)
    }

    #[inline]
    pub fn unpark_opposite(&self) {
        self.opp.unpark();
    }
}
