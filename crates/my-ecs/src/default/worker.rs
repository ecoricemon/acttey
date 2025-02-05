//! Provides [`Work`](crate::ecs::worker::Work) implementations and worker pool.
//!
//! If build target is native, the module exposes worker type which based on
//! [`std::thread::Thread`]. While the build target is web, the module exposes
//! web worker instead.

/// Common interface for worker pool implementations.
pub trait AsWorkerPool<W>: From<Vec<W>> + Into<Vec<W>> {
    /// Creates an empty worker pool.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// let pool = WorkerPool::new();
    /// assert!(pool.is_empty());
    /// ```
    fn new() -> Self;

    /// Creates worker pool with workers as many as number of available logical
    /// cpus.
    ///
    /// Number of logical cpus depends on platform which this crate runs on.
    /// This method guarantees the returned worker pool to have at least one
    /// worker in it even if it failed to get the number of logical cpus.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// let pool = WorkerPool::with_all_cpus();
    /// assert!(!pool.is_empty());
    /// ```
    fn with_all_cpus() -> Self {
        #[cfg(not(target_arch = "wasm32"))]
        let len = {
            std::thread::available_parallelism()
                .unwrap_or(unsafe { std::num::NonZeroUsize::new_unchecked(1) })
                .get()
        };

        #[cfg(target_arch = "wasm32")]
        let len = crate::util::web::available_parallelism();

        Self::with_len(len)
    }

    /// Creates worker pool with `len` workers.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// let pool = WorkerPool::with_len(1);
    /// assert_eq!(pool.len() , 1);
    /// ```
    fn with_len(len: usize) -> Self;

    /// Returns number of workers in the worker pool.
    fn len(&self) -> usize;

    /// Returns true if the worker pool doesn't contain any workers in it.
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Appends a worker in the worker pool.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// let mut pool = WorkerPool::new();
    /// assert!(pool.is_empty());
    ///
    /// let worker = WorkerBuilder::new("name").spawn().unwrap();
    /// pool.append(worker);
    /// assert_eq!(pool.len(), 1);
    /// ```
    fn append(&mut self, worker: W);
}

#[cfg(not(target_arch = "wasm32"))]
pub use non_web::*;

#[cfg(target_arch = "wasm32")]
pub use web::*;

#[cfg(not(target_arch = "wasm32"))]
mod non_web {
    use super::*;
    use crate::{ds::ManagedConstPtr, ecs::prelude::*, util};
    use std::{
        fmt,
        sync::mpsc::{self, Sender},
        thread::{Builder, JoinHandle},
    };

    /// A data type holding [`Worker`]s.
    #[derive(Debug)]
    #[repr(transparent)]
    pub struct WorkerPool {
        workers: Vec<Worker>,
    }

    impl AsWorkerPool<Worker> for WorkerPool {
        fn new() -> Self {
            Self {
                workers: Vec::new(),
            }
        }

        fn with_len(len: usize) -> Self {
            let mut this = Self::new();

            let mut name = "worker0".to_owned();
            for _ in 0..len {
                let worker = WorkerBuilder::new(&name).spawn().unwrap();
                this.append(worker);
                util::str::increase_rnumber(&mut name);
            }

            this
        }

        fn len(&self) -> usize {
            self.workers.len()
        }

        fn append(&mut self, worker: Worker) {
            self.workers.push(worker);
        }
    }

    impl Default for WorkerPool {
        fn default() -> Self {
            Self::new()
        }
    }

    impl From<Vec<Worker>> for WorkerPool {
        fn from(value: Vec<Worker>) -> Self {
            Self { workers: value }
        }
    }

    impl From<WorkerPool> for Vec<Worker> {
        fn from(value: WorkerPool) -> Self {
            value.workers
        }
    }

    /// [`Worker`] builder.
    ///
    /// You can spawn [`Worker`] from this builder.
    #[derive(Debug)]
    pub struct WorkerBuilder<'a> {
        inner: Builder,
        name: &'a str,
    }

    impl<'a> WorkerBuilder<'a> {
        /// Creates a new [`WorkerBuilder`].
        pub fn new(name: &'a str) -> Self {
            Self {
                inner: Builder::new().name(name.to_owned()),
                name,
            }
        }

        /// Sets worker's stack size in bytes.
        pub fn stack_size(self, size: usize) -> Self {
            Self {
                inner: self.inner.stack_size(size),
                name: self.name,
            }
        }

        /// Spawns a new [`Worker`] from the builder.
        pub fn spawn(self) -> Result<Worker, std::io::Error> {
            Worker::spawn(self)
        }
    }

    /// Worker handle.
    ///
    /// When [`Worker`] is dropped, it waits for its associated worker to
    /// finish.
    pub struct Worker {
        name: Box<str>,
        tx: Sender<Option<ManagedConstPtr<SubContext>>>,
        join_handle: Option<JoinHandle<()>>,
    }

    impl Worker {
        fn spawn(builder: WorkerBuilder) -> Result<Self, std::io::Error> {
            let (tx, rx) = mpsc::channel::<Option<ManagedConstPtr<SubContext>>>();
            let join_handle = builder.inner.spawn(move || {
                while let Some(cx) = rx.recv().unwrap() {
                    SubContext::execute(cx);
                }
            })?;
            Ok(Self {
                name: builder.name.into(),
                tx,
                join_handle: Some(join_handle),
            })
        }
    }

    impl fmt::Debug for Worker {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("Worker")
                .field("name", &self.name)
                .finish_non_exhaustive()
        }
    }

    impl Drop for Worker {
        fn drop(&mut self) {
            // `rx` could be broken if worker panics.
            let _ = self.tx.send(None);
            // Safety: `join_handle` must have been filled.
            let join_handle = unsafe { self.join_handle.take().unwrap_unchecked() };
            let _ = join_handle.join();
        }
    }

    impl Work for Worker {
        fn unpark(&mut self, cx: ManagedConstPtr<SubContext>) -> bool {
            let res = self.tx.send(Some(cx));
            res.is_ok()
        }

        fn park(&mut self) -> bool {
            true
        }

        fn name(&self) -> &str {
            &self.name
        }
    }
}

#[cfg(target_arch = "wasm32")]
mod web {
    use super::*;
    use crate::{
        ds::{ManagedConstPtr, NonNullExt},
        ecs::prelude::*,
        util::{macros::impl_from_for_enum, prelude::*},
    };
    use std::{
        cell::RefCell,
        collections::VecDeque,
        fmt,
        future::Future,
        mem,
        mem::ManuallyDrop,
        ops::{Deref, DerefMut},
        pin::Pin,
        rc::Rc,
        sync::{
            atomic::{AtomicBool, Ordering},
            Arc, OnceLock,
        },
    };
    use wasm_bindgen::prelude::*;

    /// A data type holding [`Worker`]s.
    #[derive(Debug)]
    #[repr(transparent)]
    pub struct WorkerPool {
        workers: Vec<Worker>,
    }

    impl WorkerPool {
        fn clear(&mut self) {
            self.workers.clear();
        }
    }

    impl AsWorkerPool<Worker> for WorkerPool {
        fn new() -> Self {
            Self {
                workers: Vec::new(),
            }
        }

        fn with_len(len: usize) -> Self {
            let mut this = Self::new();

            let mut name = "worker0".to_owned();
            for _ in 0..len {
                let worker = WorkerBuilder::new(&name).spawn().unwrap();
                this.append(worker);
                str_util::increase_rnumber(&mut name);
            }

            this
        }

        fn len(&self) -> usize {
            self.workers.len()
        }

        fn append(&mut self, worker: Worker) {
            self.workers.push(worker);
        }
    }

    impl Default for WorkerPool {
        fn default() -> Self {
            Self::new()
        }
    }

    impl From<Vec<Worker>> for WorkerPool {
        fn from(value: Vec<Worker>) -> Self {
            Self { workers: value }
        }
    }

    impl From<WorkerPool> for Vec<Worker> {
        fn from(value: WorkerPool) -> Self {
            value.workers
        }
    }

    /// [`MainWorker`] builder.
    ///
    /// You can spawn [`MainWorker`] from this builder.
    #[derive(Debug)]
    #[repr(transparent)]
    pub struct MainWorkerBuilder<'a> {
        inner: WorkerBuilder<'a>,
    }

    impl<'a> MainWorkerBuilder<'a> {
        /// Creates a [`WorkerBuilder`] with default name 'main-worker'.
        pub fn new() -> Self {
            let inner = WorkerBuilder::new("main-worker").with_listen("mainOnMessage");
            Self { inner }
        }

        /// Creates a [`WorkerBuilder`] with the given name.
        pub fn with_name(self, name: &'a str) -> Self {
            Self {
                inner: self.inner.with_name(name),
            }
        }

        /// Creates a [`WorkerBuilder`] with the given name of initialization
        /// function.
        ///
        /// Default initialization function is 'mainOnMessage'.
        pub fn with_init(self, init: &'a str) -> Self {
            Self {
                inner: self.inner.with_init(init),
            }
        }

        /// Spawns a [`MainWorker`].
        pub fn spawn(self) -> Result<MainWorker, JsValue> {
            MainWorker::spawn(self)
        }
    }

    impl<'a> Default for MainWorkerBuilder<'a> {
        fn default() -> Self {
            Self::new()
        }
    }

    /// Main worker handle.
    ///
    /// Main worker is a web worker that is parent of other sub workers. Main
    /// worker is responsible for communication with window context, spawning
    /// sub workers, creating ecs instance, and running ecs. You can think of
    /// main worker as main thread where 'main' function runs on native
    /// environment.
    ///
    /// # Common worker hierarchy
    ///
    /// window - main worker - sub workers
    ///
    /// # Why we need main worker
    ///
    /// Ecs instance blocks sometimes to wait for messages from sub workers. But
    /// browsers doesn't allow us to block on window context. So we need an
    /// extra web worker.
    #[derive(Debug)]
    #[repr(transparent)]
    pub struct MainWorker {
        inner: Worker,
    }

    impl MainWorker {
        fn spawn(builder: MainWorkerBuilder) -> Result<Self, JsValue> {
            Ok(Self {
                inner: builder.inner.spawn()?,
            })
        }

        /// Spawns sub workers as many as the given number on the main worker.
        ///
        /// Sub workers are behind the main worker so that you cannot
        /// communicate with them directly.
        ///
        /// # Examples
        ///
        /// ```
        /// use my_ecs::prelude::*;
        ///
        /// let main = MainWorkerBuilder::new().spawn().unwrap();
        /// let num_cpus = web_util::available_parallelism();
        /// main.spawn_children(num_cpus);
        /// ```
        pub fn spawn_children(&self, num: usize) {
            self.delegate(
                |arg| {
                    let num: f64 = arg.unchecked_into_f64();
                    let num = num as usize;
                    JS_MAIN_CX.with_borrow_mut(|cx| {
                        for _ in 0..num {
                            str_util::increase_rnumber(&mut cx.child_name);
                            let worker = WorkerBuilder::new(&cx.child_name).spawn().unwrap();
                            cx.pool.append(worker);
                        }
                    });
                },
                num.into(),
            );
        }

        /// Sends the main worker a function that initializes ecs instance.
        ///
        /// The main worker will execute the function and store the returned ecs
        /// instance once it's ready.
        ///
        /// # Examples
        ///
        /// ```
        /// use my_ecs::prelude::*;
        ///
        /// let main = MainWorkerBuilder::new().spawn().unwrap();
        /// main.init_app(|pool| {
        ///     let num_workers = pool.len();
        ///     Ecs::default(pool, [num_workers])
        /// });
        /// ```
        pub fn init_app<F, R>(&self, f: F)
        where
            F: FnOnce(WorkerPool) -> R + 'static,
            R: Into<LeakedEcsApp>,
        {
            let f = move |pool: WorkerPool| -> LeakedEcsApp { f(pool).into() };
            let f: DynFnOnce<WorkerPool, LeakedEcsApp> = ManuallyDrop::new(Box::new(f));
            helper(self, f);

            // === Internal helper functions ===

            fn helper(this: &MainWorker, f: DynFnOnce<WorkerPool, LeakedEcsApp>) {
                let arg = DynFnOnceCodec::encode_into_array(f);
                this.delegate(
                    |arg| {
                        JS_MAIN_CX.with_borrow_mut(|cx| {
                            let arg: js_sys::Uint32Array = arg.unchecked_into();
                            // Safety: `arg` is `f`.
                            unsafe {
                                let f = DynFnOnceCodec::decode_from_array(&arg)
                                    .cast::<WorkerPool, LeakedEcsApp>();
                                cx.schedule_fn(f);
                            };
                            cx.consume_if_ready();
                        });
                    },
                    arg.into(),
                );
            }
        }

        /// Sends the main worker a function that accesses ecs instance.
        ///
        /// But if main worker doesn't have ecs instance, the function will be
        /// dropped without execution. Don't forget to call
        /// [`MainWorker::init_app`] beforehand.
        ///
        /// # Examples
        ///
        /// ```
        /// use my_ecs::prelude::*;
        ///
        /// let main = MainWorkerBuilder::new().spawn().unwrap();
        ///
        /// main.init_app(|pool| {
        ///     let num_workers = pool.len();
        ///     Ecs::default(pool, [num_workers])
        /// });
        ///
        /// main.with_app(|app| { /* ... */ });
        /// ```
        pub fn with_app<F>(&self, f: F)
        where
            F: FnOnce(EcsExt<'static>) + 'static,
        {
            let f: DynFnOnce<EcsExt<'static>, ()> = ManuallyDrop::new(Box::new(f));
            helper(self, f);

            // === Internal helper functions ===

            fn helper(this: &MainWorker, f: DynFnOnce<EcsExt<'static>, ()>) {
                let arg = DynFnOnceCodec::encode_into_array(f);
                this.delegate(
                    |arg| {
                        JS_MAIN_CX.with_borrow_mut(|cx| {
                            let arg: js_sys::Uint32Array = arg.unchecked_into();
                            // Safety: `arg` is `f`.
                            unsafe {
                                let f = DynFnOnceCodec::decode_from_array(&arg)
                                    .cast::<EcsExt<'static>, ()>();
                                cx.schedule_fn(f);
                            };
                            cx.consume_if_ready();
                        });
                    },
                    arg.into(),
                );
            }
        }

        /// Executes the given future on the main worker using JS runtime.
        ///
        /// This method doesn't block, but the main worker stacks other
        /// requested functions rather than executing them until the given
        /// future is completed.
        ///
        /// Web APIs which return `Promise` should be called carefully. They
        /// eagerly put tasks to JS runtime queue, and the tasks cannot make
        /// more progress while Rust wasm holds CPU. That means that Rust wasm
        /// should stop its processing to complete JS `Promise`. In other words,
        /// Rust wasm cannot wait to be woken up by `Promise`.
        ///
        /// # Examples
        ///
        /// ```
        /// use my_ecs::prelude::*;
        /// use web_sys::{WorkerGlobalScope, Response};
        /// use wasm_bindgen_futures::JsFuture;
        ///
        /// let main = MainWorkerBuilder::new().spawn().unwrap();
        ///
        /// main.init_app(|pool| {
        ///     let num_workers = pool.len();
        ///     Ecs::default(pool, [num_workers])
        /// });
        ///
        /// main.with_app_await(|app| async {
        ///     let global: WorkerGlobalScope = js_sys::global().unchecked_into();
        ///     let promise = global.fetch_with_str("<some-url>");
        ///     let resp = JsFuture::from(promise).await.unwrap();
        /// });
        /// ```
        pub fn with_app_await<F, Fut>(&self, f: F)
        where
            F: FnOnce(EcsExt<'static>) -> Fut + 'static,
            Fut: Future<Output = ()> + 'static,
        {
            type Arg = EcsExt<'static>;
            type R = Pin<Box<dyn Future<Output = ()>>>;

            let f = |ecs: Arg| async move {
                f(ecs).await;

                // Resumes the main worker, so that it can consume buffered
                // functions.
                JS_MAIN_CX.with_borrow_mut(|cx| {
                    cx.resume();
                    cx.consume_if_ready();
                });
            };
            let f: Box<dyn FnOnce(Arg) -> R> = Box::new(move |ecs| Box::pin(f(ecs)));
            let f: DynFnOnce<Arg, R> = ManuallyDrop::new(f);

            helper(self, f);

            // === Internal helper functions ===

            fn helper(this: &MainWorker, f: DynFnOnce<Arg, R>) {
                let arg = DynFnOnceCodec::encode_into_array(f);
                this.delegate(
                    |arg| {
                        JS_MAIN_CX.with_borrow_mut(|cx| {
                            let arg: js_sys::Uint32Array = arg.unchecked_into();
                            // Safety: `arg` is `f`.
                            unsafe {
                                let f = DynFnOnceCodec::decode_from_array(&arg).cast::<Arg, R>();
                                cx.schedule_fn(f);
                            };
                            cx.consume_if_ready();
                        });
                    },
                    arg.into(),
                );
            }
        }

        /// Sends the main worker a function to call it on the main worker
        /// context.
        ///
        /// The function will be called once the main worker is ready.
        pub fn delegate(&self, f: fn(arg: JsValue), arg: JsValue) {
            MessageFn { f }.post_to(&self.handle(), arg);
        }
    }

    impl Drop for MainWorker {
        fn drop(&mut self) {
            JS_MAIN_CX.with_borrow_mut(|cx| {
                cx.pool.clear();
            });
        }
    }

    impl Deref for MainWorker {
        type Target = Worker;

        fn deref(&self) -> &Self::Target {
            &self.inner
        }
    }

    impl DerefMut for MainWorker {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.inner
        }
    }

    /// Listener of messages from the main worker.
    ///
    /// This function is exposed to JS, and it looks like,
    ///
    /// ```js
    /// // index.js
    /// const main_worker = new Worker('worker.js');
    /// main_worker.onmessage = (msg) => {
    ///     // this function
    /// };
    ///
    /// // worker.js
    /// postMessage('to window');
    /// ```
    #[wasm_bindgen(js_name = mainOnMessage)]
    pub fn main_on_message(msg: JsValue) {
        if let Some(arr) = msg.dyn_ref::<js_sys::Array>() {
            let header = arr.get(0);
            match MessageHeader::from_js_value(header).0 {
                MessageHeader::FN_INNER => {
                    let f = MessageFn::read_body(arr).f;
                    let arg = MessageFn::read_argument(arr);
                    f(arg);
                }
                _ => {
                    crate::log!("[W] unknown message on main worker");
                }
            }
        } else {
            web_util::worker_post_message(&msg).unwrap();
        }
    }

    thread_local! {
        static JS_MAIN_CX: RefCell<MainWorkerContext> = RefCell::new(
            MainWorkerContext::new()
        );

        static CONSUME_IF_READY: RefCell<Closure<dyn FnMut()>> = RefCell::new(
            Closure::new(|| {
                JS_MAIN_CX.with_borrow_mut(|cx| cx.consume_if_ready());
            })
        );
    }

    enum FnOnMain {
        InitEcs(DynFnOnceExt<WorkerPool, LeakedEcsApp>),
        WithEcs(DynFnOnceExt<EcsExt<'static>, ()>),
        WithEcsAwait(DynFnOnceExt<EcsExt<'static>, Pin<Box<dyn Future<Output = ()>>>>),
    }

    impl_from_for_enum!(
        "outer" = FnOnMain; "var" = InitEcs;
        "inner" = DynFnOnceExt<WorkerPool, LeakedEcsApp>
    );
    impl_from_for_enum!(
        "outer" = FnOnMain; "var" = WithEcs;
        "inner" = DynFnOnceExt<EcsExt<'static>, ()>
    );
    impl_from_for_enum!(
        "outer" = FnOnMain; "var" = WithEcsAwait;
        "inner" = DynFnOnceExt<EcsExt<'static>, Pin<Box<dyn Future<Output = ()>>>>
    );

    struct MainWorkerContext {
        /// Worker pool.
        pool: WorkerPool,
        ecs: Option<LeakedEcsApp>,
        /// Child worker name that will be given to the next spawned child worker.
        child_name: String,
        pending: VecDeque<FnOnMain>,
        pause: Arc<AtomicBool>,
    }

    impl MainWorkerContext {
        fn new() -> Self {
            Self {
                pool: WorkerPool::new(),
                ecs: None,
                child_name: "sub-worker0".to_owned(),
                pending: VecDeque::new(),
                pause: Arc::new(AtomicBool::new(false)),
            }
        }

        fn schedule_fn<F>(&mut self, f: F)
        where
            F: Into<FnOnMain>,
        {
            self.pending.push_back(f.into());
        }

        fn consume_if_ready(&mut self) {
            // If child workers are not ready yet, we need to give CPU to JS
            // runtime.
            if !self.is_ready() {
                CONSUME_IF_READY.with_borrow(|ready_run| {
                    const WAIT_MS: i32 = 10;
                    let cb = ready_run.as_ref().unchecked_ref();
                    let global = web_util::worker_global();
                    global
                        .set_timeout_with_callback_and_timeout_and_arguments_0(cb, WAIT_MS)
                        .unwrap();
                });
                return;
            }

            if self.is_paused() {
                return;
            }

            while let Some(f) = self.pending.pop_front() {
                match f {
                    FnOnMain::InitEcs(f) => {
                        let pool = mem::take(&mut self.pool);
                        let ecs = f.call(pool);
                        self.ecs = Some(ecs);
                    }
                    FnOnMain::WithEcs(f) => {
                        if let Some(ecs) = self.ecs.as_ref() {
                            // Safety: We're accessing valid ecs once at a time.
                            unsafe { f.call(ecs.get()) };
                        }
                    }
                    FnOnMain::WithEcsAwait(f) => {
                        if let Some(ecs) = self.ecs.as_ref() {
                            // Safety: We're accessing valid ecs once at a time.
                            let fut = unsafe { f.call(ecs.get()) };
                            wasm_bindgen_futures::spawn_local(fut);

                            // Stops consuming further before being resumed.
                            self.pause();
                            break;
                        }
                    }
                }
            }
        }

        fn is_ready(&self) -> bool {
            let children = &self.pool.workers;
            children.iter().all(|child| child.is_ready())
        }

        fn is_paused(&self) -> bool {
            self.pause.load(Ordering::Relaxed)
        }

        fn pause(&self) {
            self.pause.store(true, Ordering::Relaxed);
        }

        fn resume(&self) {
            self.pause.store(false, Ordering::Relaxed);
        }
    }

    impl fmt::Debug for MainWorkerContext {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("MainWorkerContext")
                .field("pool", &self.pool)
                .finish_non_exhaustive()
        }
    }

    impl Default for MainWorkerContext {
        fn default() -> Self {
            Self::new()
        }
    }

    /// [`Worker`] builder.
    ///
    /// You can spawn [`Worker`] from this builder.
    #[derive(Debug)]
    pub struct WorkerBuilder<'a> {
        name: &'a str,
        script: Option<&'a str>,
        listen: &'a str,
        init: &'a str,
    }

    impl<'a> WorkerBuilder<'a> {
        /// Default message listener of the worker.
        /// See [`worker_on_message`].
        const DEFAULT_LISTEN: &'static str = "workerOnMessage";
        const DEFAULT_INIT: &'static str = "workerInit";

        /// Creates a new [`WorkerBuilder`].
        pub const fn new(name: &'a str) -> Self {
            Self {
                name,
                script: None,
                init: Self::DEFAULT_INIT,
                listen: Self::DEFAULT_LISTEN,
            }
        }

        /// Creates a new [`WorkerBuilder`] with the given name.
        pub const fn with_name(mut self, name: &'a str) -> Self {
            self.name = name;
            self
        }

        /// Creates a new [`WorkerBuilder`] with the given script.
        pub const fn with_script(mut self, script: &'a str) -> Self {
            self.script = Some(script);
            self
        }

        /// Creates a new [`WorkerBuilder`] with the given name of
        /// initialization function.
        pub const fn with_init(mut self, init: &'a str) -> Self {
            self.init = init;
            self
        }

        /// Creates a new [`WorkerBuilder`] with the given `onmessage` listener.
        pub const fn with_listen(mut self, listen: &'a str) -> Self {
            self.listen = listen;
            self
        }

        /// Spawns a new [`Worker`] from the builder.
        pub fn spawn(self) -> Result<Worker, JsValue> {
            Worker::spawn(self)
        }
    }

    /// Worker handle.
    pub struct Worker {
        /// JS worker handle.
        handle: web_sys::Worker,

        /// Worker name. You can see this name in browser's dev tool.
        name: Box<str>,

        /// Callback for worker's first response, which is a notification of
        /// the worker's readiness.
        ///
        /// The callback will be replaced with [`Worker::on_message`] once it
        /// called.
        _on_ready: Closure<dyn FnMut()>,

        /// Callback for worker response.
        #[allow(clippy::type_complexity)]
        on_message: Rc<RefCell<Closure<dyn FnMut(web_sys::MessageEvent)>>>,

        /// Determines the worker is spawned and ready to listen to message.
        ready: Arc<AtomicBool>,
    }

    impl fmt::Debug for Worker {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("Worker")
                .field("name", &self.name)
                .field("ready", &self.ready.as_ref())
                .finish_non_exhaustive()
        }
    }

    impl Worker {
        fn spawn(builder: WorkerBuilder) -> Result<Self, JsValue> {
            // Creates a new worker.
            let handle = create_worker(builder.name, builder.script)?;
            let ready = Arc::new(AtomicBool::new(false));
            let on_message = Rc::new(RefCell::new(Closure::new(|_| {})));
            let c_handle = handle.clone();
            let c_ready = Arc::clone(&ready);
            let c_on_message = Rc::clone(&on_message);

            // Listens to worker's ready notification.
            let on_ready = Closure::new(move || {
                let on_message = c_on_message.borrow();
                c_handle.set_onmessage(Some(on_message.as_ref().unchecked_ref()));
                c_ready.store(true, Ordering::Release);
            });
            handle.set_onmessage(Some(on_ready.as_ref().unchecked_ref()));

            // Sets 'WBG_INIT' if it wasn't set yet.
            let wasm_init = WBG_INIT.get_or_init(|| DEFAULT_WBG_INIT.to_owned());

            // TODO: For now, we assume that wasm use shared memory.
            // Initializes the worker.
            use js_sys::{Object, Reflect};
            let msg = Object::new();
            Reflect::set(&msg, &"module".into(), &wasm_bindgen::module())?;
            Reflect::set(&msg, &"memory".into(), &wasm_bindgen::memory())?;
            Reflect::set(&msg, &"url".into(), &IMPORT_META_URL.with(JsValue::clone))?;
            Reflect::set(&msg, &"wasmInit".into(), &wasm_init.into())?;
            Reflect::set(&msg, &"init".into(), &builder.init.into())?;
            Reflect::set(&msg, &"listen".into(), &builder.listen.into())?;
            handle.post_message(&msg)?;

            Ok(Self {
                handle,
                name: builder.name.into(),
                _on_ready: on_ready,
                on_message,
                ready,
            })
        }

        /// Returns JS worker handle.
        pub fn handle(&self) -> web_sys::Worker {
            self.handle.clone()
        }

        /// Returns true if the worker has been fully initialized and ready to
        /// process messages.
        pub fn is_ready(&self) -> bool {
            self.ready.load(Ordering::Relaxed)
        }

        pub fn set_on_message<F>(&self, mut cb: F)
        where
            F: FnMut(JsValue) + 'static,
        {
            let cb: Closure<dyn FnMut(web_sys::MessageEvent)> =
                Closure::new(move |ev: web_sys::MessageEvent| cb(ev.data()));

            if self.is_ready() {
                self.handle.set_onmessage(Some(cb.as_ref().unchecked_ref()));
            }
            *self.on_message.borrow_mut() = cb;
        }

        /// Sends the worker a message.
        pub fn post_message(&self, msg: &JsValue) -> Result<(), JsValue> {
            self.handle.post_message(msg)
        }
    }

    impl Work for Worker {
        fn unpark(&mut self, cx: ManagedConstPtr<SubContext>) -> bool {
            let ptr = cx.as_ptr();

            #[cfg(feature = "check")]
            drop(cx);

            let res = self.handle.post_message(&JsValue::from(ptr));
            res.is_ok()
        }

        fn park(&mut self) -> bool {
            true
        }

        fn name(&self) -> &str {
            &self.name
        }
    }

    impl Drop for Worker {
        /// Terminates web worker *immediately*.
        fn drop(&mut self) {
            self.handle.terminate();
        }
    }

    /// Clients can modify init function of wasm glue JS file before they call [`Worker::spawn`].
    /// If you don't set this value, [`DEFAULT_WBG_INIT`] will be set as default value.
    ///
    /// # Example
    ///
    /// ```rust
    /// use my_ecs::default::prelude::*;
    ///
    /// // Some bundlers may minify export name to '_' or 'default'.
    /// crate::WBG_INIT.set("_".to_owned()).unwrap();
    /// Worker::spawn("worker", 0).unwrap();
    /// ```
    pub static WBG_INIT: OnceLock<String> = OnceLock::new();

    /// wasm-bindgen will generate "__wbg_init" as default export function.
    /// But, I expect that it will be exported as 'default'.
    pub const DEFAULT_WBG_INIT: &str = "default";

    pub const DEFAULT_WORKER_SCRIPT: &str = include_str!("worker.js");

    // Some bundlers could warn about circular dependency caused by worker
    // due to the cycle that looks like
    // "Rust wasm - (bind) -> worker.js -> (import) -> wasm".
    //
    // But, if the worker JS file is substituted with a created object,
    // we can avoid the warning.
    //
    // However, in that case, we need to set bundler to cooperate with the
    // created worker object.
    // For instance, in Vite(v5.4.2), you may need following settings.
    //
    // build: {
    //   rollupOptions: {
    //       // We need to split wasm glue module into a separate chunk
    //       // 1. Not to include window context data.
    //       //    * wasm glue module will be imported in worker context.
    //       //    * In worker context, we can't access something like document.
    //       // 2. To preserve indirectly used exports.
    //       //    * Rollup doesn't know that we're going to access some exported
    //       //      objects, workerOnMessage for instance, in wasm code.
    //       //    * So we need to make Rollup not to drop those objects.
    //       //
    //       // First of all, we need to make a new entry point for wasm.
    //       input: {
    //         wasm: 'pkg/wasm-index.js', // path to wasm glue file.
    //         ...
    //       },
    //       ...
    //
    //       // Then, put the following.
    //       // * https://rollupjs.org/configuration-options/#preserveentrysignatures
    //       //   Although Rollup says default is already 'exports-only',
    //       //   but I guess Vite 5.4.2 changes it to `false`.
    //       preserveEntrySignatures: 'exports-only',
    //     },
    //   ...
    // }
    fn create_worker(name: &str, script: Option<&str>) -> Result<web_sys::Worker, JsValue> {
        let opt = web_sys::WorkerOptions::new();
        opt.set_name(name);
        opt.set_type(web_sys::WorkerType::Module);
        web_sys::Worker::new_with_options(&script_url(script), &opt)
    }

    #[wasm_bindgen]
    extern "C" {
        /// URL of wasm glue JS file.
        //
        // We need this URL of wasm glue JS file in order to import it dynamically in workers.
        // So that workers can share the same wasm module and memory.
        // But note that bundler may evaluate "import.meta.url" statically during bundling,
        // which is not what we want, we need to evaluate it at runtime.
        // Therefore, you need to configure your bundler not to do it.
        // (e.g. Webpack does it basically, but Vite(v5.1.6) doesn't do it)
        #[wasm_bindgen(thread_local_v2, js_namespace = ["import", "meta"], js_name = url)]
        static IMPORT_META_URL: JsValue;
    }

    fn script_url(script: Option<&str>) -> String {
        let script = script.unwrap_or(DEFAULT_WORKER_SCRIPT);
        let blob_parts = js_sys::Array::new_with_length(1);
        blob_parts.set(0, JsValue::from_str(script));

        let options = web_sys::BlobPropertyBag::new();
        options.set_type("application/javascript");

        let blob = web_sys::Blob::new_with_str_sequence_and_options(&blob_parts, &options).unwrap();
        web_sys::Url::create_object_url_with_blob(&blob).unwrap()
    }

    #[wasm_bindgen(js_name = workerInit)]
    pub fn worker_init() {
        // Nothing to do as default behavior.
    }

    /// # Safety
    ///
    /// Undefined behavior if the pointer is not valid or aliased.
    #[wasm_bindgen(js_name = workerOnMessage)]
    pub unsafe fn worker_on_message(cx: *mut SubContext) {
        let cx = ManagedConstPtr::new(NonNullExt::new_unchecked(cx));
        SubContext::execute(cx);
    }

    /// Message event header.
    ///
    /// Inner value will be transmuted into f64 and vice versa. You must
    /// guarantee that f64 representation is not Nan (or Inf). If so,
    /// someone(maybe JS) can change its bit expression into something else with
    /// just preserving the meaning, Nan or Inf or something like that.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct MessageHeader(pub u64);

    impl MessageHeader {
        const MAGIC: u64 = 0x0EC5 << (16 * 3);

        const BASE: u64 = Self::MAGIC;

        const FN_INNER: u64 = Self::BASE + 1;
        const FN: Self = Self(Self::FN_INNER);

        pub fn into_js_value(self) -> JsValue {
            let value = f64::from_bits(self.0);
            debug_assert!(value.is_finite()); // Nan or Inf are now allowed.
            JsValue::from_f64(value)
        }

        pub fn from_js_value(value: JsValue) -> Self {
            Self(value.unchecked_into_f64().to_bits())
        }
    }

    const F64_SIZE: usize = mem::size_of::<f64>();
    const F64_ROUND_UP: usize = F64_SIZE - 1;
    const HEADER_LEN: usize = (mem::size_of::<MessageHeader>() + F64_ROUND_UP) / F64_SIZE;
    const U32_SIZE: usize = mem::size_of::<u32>();
    const U32_ROUND_UP: usize = U32_SIZE - 1;

    /// Instant message for sending a function.
    #[derive(Debug, Clone, Copy)]
    #[repr(C)]
    struct MessageFn {
        /// Function pointer.
        //
        // NOTE: We cannot turn an address into f64 directly.
        // Address could be 'inf' or 'nan' in f64 representation.
        // See https://en.wikipedia.org/wiki/IEEE_754-1985
        // Those numbers will keep the meaning of 'inf' or 'nan' though,
        // they can be freely different number in perspective of bit.
        // So we put this value in Uint32Array.
        //
        // According to googling, OSes use 48-bit address space on 64 bit machines.
        // Even if so, it could be dangerous to convert addresses to f64
        // when OSes fill preceding bits to 1.
        f: fn(arg: JsValue),
    }

    impl MessageFn {
        const HEADER: MessageHeader = MessageHeader::FN;

        /// Returns minimum length of [`js_sys::Array`] for this message.
        const fn message_array_len() -> u32 {
            // 3, message looks like [header, [body array], arg].
            const BODY_LEN: u32 = 1;
            const ARG_LEN: u32 = 1;
            HEADER_LEN as u32 + BODY_LEN + ARG_LEN
        }

        fn post_to(self, worker: &web_sys::Worker, arg: JsValue) {
            let buf = js_sys::Array::new_with_length(Self::message_array_len());
            Self::write_header(&buf);
            self.write_body(&buf);
            Self::write_argument(&buf, arg);
            worker.post_message(&buf).unwrap();
        }

        fn write_header(buf: &js_sys::Array) {
            buf.set(0, Self::HEADER.into_js_value());
        }

        fn write_body(&self, buf: &js_sys::Array) {
            let body = FnCodec::encode_into_array(self.f);
            buf.set(1, body.into());
        }

        fn write_argument(buf: &js_sys::Array, arg: JsValue) {
            buf.set(2, arg);
        }

        fn read_body(buf: &js_sys::Array) -> Self {
            debug_assert!(buf.length() >= Self::message_array_len());
            assert_eq!(Self::HEADER, MessageHeader::from_js_value(buf.get(0)));

            let body: js_sys::Uint32Array = buf.get(1).unchecked_into();

            // Safety: We checked the message header.
            unsafe {
                let f = FnCodec::decode_from_array(&body);
                Self {
                    f: mem::transmute::<fn(), fn(JsValue)>(f),
                }
            }
        }

        fn read_argument(buf: &js_sys::Array) -> JsValue {
            buf.get(2)
        }
    }

    #[repr(C)]
    union FnCodec {
        src: fn(),
        dst: [u32; Self::len()],
    }

    impl FnCodec {
        const fn len() -> usize {
            // 2 (on 64 bit) or 1 (on 32 bit)
            (mem::size_of::<fn()>() + U32_ROUND_UP) / U32_SIZE
        }

        #[inline]
        fn encode_into_array<Arg, R>(f: fn(Arg) -> R) -> js_sys::Uint32Array {
            let arr = js_sys::Uint32Array::new_with_length(Self::len() as u32);
            arr.copy_from(&Self::encode(f));
            arr
        }

        #[inline]
        const fn encode<Arg, R>(f: fn(Arg) -> R) -> [u32; Self::len()] {
            // Safety: Function pointer can safely become u32 array.
            unsafe {
                Self {
                    src: mem::transmute::<fn(Arg) -> R, fn()>(f),
                }
                .dst
            }
        }

        /// # Safety
        ///
        /// Undefined behavior if the given data is not a valid function pointer.
        //
        // Why we don't return `fn(Arg) -> R` like encode_into_array().
        // - Imagine we're returning `fn(Arg) -> R`.
        //   If caller designated `&i32` to `Arg` and `()` to `R`,
        //   then return type will be `fn(&'x i32)`, where 'x is defined by the caller.
        //   Then, caller can call `fn(&'x i32)` directly, but cannot do other things
        //   like pushing the function pointer into `Vec<for<'a> fn(&'a i32)>`.
        //   Because the Vec requires `for<'a>`, which is quite generic,
        //   but caller can't convert 'x, which is less generic, into `for<'a>`.
        // - Therefore, generic is not sufficient.
        #[inline]
        unsafe fn decode_from_array(arr: &js_sys::Uint32Array) -> fn() {
            let mut buf: [u32; Self::len()] = [0; Self::len()];
            arr.copy_to(&mut buf);
            Self::decode(buf)
        }

        /// # Safety
        ///
        /// Undefined behavior if the given data is not a valid function pointer.
        #[inline]
        const unsafe fn decode(encoded: [u32; Self::len()]) -> fn() {
            Self { dst: encoded }.src
        }
    }

    #[repr(C)]
    union DynFnOnceCodec {
        src: DynFnOnce<(), ()>,
        dst: [u32; Self::len()],
    }

    impl DynFnOnceCodec {
        const fn len() -> usize {
            (mem::size_of::<DynFnOnce<(), ()>>() + U32_ROUND_UP) / U32_SIZE
        }

        #[inline]
        fn encode_into_array<Arg, R>(f: DynFnOnce<Arg, R>) -> js_sys::Uint32Array {
            let arr = js_sys::Uint32Array::new_with_length(Self::len() as u32);
            arr.copy_from(&Self::encode(f));
            arr
        }

        #[inline]
        const fn encode<Arg, R>(f: DynFnOnce<Arg, R>) -> [u32; Self::len()] {
            // Safety: Dynamic function pointer can safely become u32 array.
            unsafe {
                Self {
                    src: mem::transmute::<DynFnOnce<Arg, R>, DynFnOnce<(), ()>>(f),
                }
                .dst
            }
        }

        /// # Safety
        ///
        /// Undefined behavior if the given data is not a valid [`DynFnOnce`].
        /// Also, return value must be cast as the original type.
        #[inline]
        unsafe fn decode_from_array(arr: &js_sys::Uint32Array) -> DynFnOnceExt<(), ()> {
            let mut buf: [u32; Self::len()] = [0; Self::len()];
            arr.copy_to(&mut buf);
            Self::decode(buf)
        }

        /// # Safety
        ///
        /// Undefined behavior if the given data is not a valid [`DynFnOnce`].
        /// Also, return value must be cast as the original type.
        #[inline]
        const unsafe fn decode(encoded: [u32; Self::len()]) -> DynFnOnceExt<(), ()> {
            let f = Self { dst: encoded }.src;
            DynFnOnceExt(f)
        }
    }

    type DynFnOnce<Arg, R> = ManuallyDrop<Box<dyn FnOnce(Arg) -> R>>;

    #[repr(transparent)]
    struct DynFnOnceExt<Arg, R>(DynFnOnce<Arg, R>);

    impl<Arg, R> DynFnOnceExt<Arg, R> {
        /// # Safety
        ///
        /// Undefined behavior if the type is not correct.
        unsafe fn cast<ToArg, ToR>(self) -> DynFnOnceExt<ToArg, ToR> {
            unsafe { mem::transmute::<DynFnOnceExt<Arg, R>, DynFnOnceExt<ToArg, ToR>>(self) }
        }

        fn call(self, arg: Arg) -> R {
            unsafe {
                let inner: DynFnOnce<Arg, R> = mem::transmute(self);
                let f = ManuallyDrop::into_inner(inner);
                f(arg)
            }
        }
    }

    impl<Arg, R> Drop for DynFnOnceExt<Arg, R> {
        fn drop(&mut self) {
            let inner = &mut self.0;
            unsafe { ManuallyDrop::drop(inner) };
        }
    }
}
