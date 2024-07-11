#[cfg(not(target_arch = "wasm32"))]
pub use non_wasm::*;

#[cfg(target_arch = "wasm32")]
pub use wasm::*;

#[cfg(not(target_arch = "wasm32"))]
mod non_wasm {
    use crate::{ds::prelude::*, ecs::prelude::*};
    use std::{
        sync::mpsc::{self, Sender},
        thread::{Builder, JoinHandle},
    };

    #[derive(Debug)]
    pub struct WorkerBuilder {
        inner: Builder,
        name: String,
    }

    impl WorkerBuilder {
        pub fn stack_size(self, size: usize) -> Self {
            Self {
                inner: self.inner.stack_size(size),
                name: self.name,
            }
        }

        /// Unlike [`Self::spwan`], this returns concrete type.
        pub fn spawn_concrete(self) -> Result<Worker, std::io::Error> {
            Worker::spawn(self)
        }
    }

    impl BuildWorker for WorkerBuilder {
        fn new(name: String) -> Self {
            Self {
                inner: Builder::new().name(name.clone()),
                name,
            }
        }

        fn spawn(self) -> impl Work {
            Worker::spawn(self).unwrap()
        }
    }

    pub struct Worker {
        name: String,
        tx: Sender<Option<ManagedConstPtr<Job>>>,
        join_handle: Option<JoinHandle<()>>,
    }

    impl Worker {
        pub(super) fn spawn(builder: WorkerBuilder) -> Result<Self, std::io::Error> {
            let (tx, rx) = mpsc::channel::<Option<ManagedConstPtr<Job>>>();
            let join_handle = builder.inner.spawn(move || {
                while let Some(job) = rx.recv().unwrap() {
                    job.process();
                }
            })?;
            Ok(Self {
                name: builder.name,
                tx,
                join_handle: Some(join_handle),
            })
        }
    }

    impl Drop for Worker {
        fn drop(&mut self) {
            self.tx.send(None).unwrap();
            let join_handle = self.join_handle.take().unwrap();
            let _ = join_handle.join();
        }
    }

    impl Work for Worker {
        fn unpark(&mut self, job: ManagedConstPtr<Job>) -> bool {
            let res = self.tx.send(Some(job));
            res.is_ok()
        }

        fn get_name(&self) -> &str {
            &self.name
        }
    }
}

#[cfg(target_arch = "wasm32")]
mod wasm {
    use crate::{ds::prelude::*, ecs::prelude::*};
    use once_cell::sync::OnceCell;
    use std::{
        fmt::Debug,
        sync::{
            atomic::{AtomicBool, Ordering},
            Arc,
        },
    };
    use wasm_bindgen::prelude::*;

    #[derive(Debug)]
    pub struct WorkerBuilder<'a> {
        name: String,
        script: Option<&'a str>,
    }

    impl<'a> WorkerBuilder<'a> {
        pub fn script(self, script: &'a str) -> Self {
            Self {
                name: self.name,
                script: Some(script),
            }
        }

        /// Unlike [`Self::spwan`], this returns concrete type.
        pub fn spawn_concrete(self) -> Result<Worker, JsValue> {
            Worker::spawn(self)
        }
    }

    impl<'a> BuildWorker for WorkerBuilder<'a> {
        fn new(name: String) -> Self {
            Self { name, script: None }
        }

        fn spawn(self) -> impl Work {
            Worker::spawn(self).unwrap()
        }
    }

    pub struct Worker {
        /// JS worker handle.
        handle: web_sys::Worker,

        /// Worker name. You can see this name in browser's dev tool.
        name: String,

        /// Callback for message from worker.
        callback: Closure<dyn FnMut(web_sys::MessageEvent)>,

        /// Spawning worker takes time, but we need to know when it becomes ready in order to get its Rust handle safely.
        ready: Arc<AtomicBool>,
    }

    impl Debug for Worker {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("Worker")
                .field("name", &self.name)
                .field("ready", &self.ready.as_ref())
                .finish_non_exhaustive()
        }
    }

    impl Worker {
        pub(super) fn spawn(builder: WorkerBuilder) -> Result<Self, JsValue> {
            // Creates a new worker.
            let handle = create_worker(&builder.name, builder.script)?;

            // Listens to worker's ready notification.
            let ready = Arc::new(AtomicBool::new(false));
            let ready_clone = Arc::clone(&ready);
            let callback = Closure::new(move |_| {
                ready_clone.store(true, Ordering::Relaxed);
            });
            handle.set_onmessage(Some(callback.as_ref().unchecked_ref()));

            // Sets 'WBG_INIT' if it wasn't set yet.
            let init_method = WBG_INIT.get_or_init(|| DEFAULT_WBG_INIT.to_owned());

            // TODO: For now, we assume that wasm use shared memory.
            // Initializes the worker.
            use js_sys::{Object, Reflect};
            let msg = Object::new();
            Reflect::set(&msg, &"module".into(), &wasm_bindgen::module())?;
            Reflect::set(&msg, &"memory".into(), &wasm_bindgen::memory())?;
            Reflect::set(&msg, &"import_url".into(), &IMPORT_META_URL.as_str().into())?;
            Reflect::set(&msg, &"init_method".into(), &init_method.into())?;
            handle.post_message(&msg)?;

            Ok(Self {
                handle,
                name: builder.name,
                callback,
                ready,
            })
        }

        pub fn handle(&self) -> web_sys::Worker {
            self.handle.clone()
        }

        pub fn is_ready(&self) -> bool {
            self.ready.load(Ordering::Relaxed)
        }

        /// Registers `callback`.
        pub fn register_callback(&mut self, callback: Closure<dyn FnMut(web_sys::MessageEvent)>) {
            self.handle
                .set_onmessage(Some(callback.as_ref().unchecked_ref()));
            self.callback = callback;
        }

        pub fn post_message(&self, msg: &JsValue) -> Result<(), JsValue> {
            self.handle.post_message(msg)
        }
    }

    impl Work for Worker {
        fn unpark(&mut self, job: ManagedConstPtr<Job>) -> bool {
            let ptr = job.into_inner().as_ptr();
            let res = self.handle.post_message(&JsValue::from(ptr));
            res.is_ok()
        }

        fn get_name(&self) -> &str {
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
    pub static WBG_INIT: OnceCell<String> = OnceCell::new();

    pub const DEFAULT_WBG_INIT: &str = "__wbg_init";

    pub const DEFAULT_WORKER_SCRIPT: &str = include_str!("worker.js");

    // Some bundlers could warn about circular dependency caused by worker
    // such as "Rust wasm - (bind) -> worker.js -> (import) -> wasm".
    // We can avoid it by removing JS file although it requires other types of settings to bundler.
    // For instance, In Vite(v5.1.6) conf, you may need 
    // - 'build.rollupOptions.output.manualChunks: [..]' for spliting wasm glue JS file.
    fn create_worker(name: &str, script: Option<&str>) -> Result<web_sys::Worker, JsValue> {
        web_sys::Worker::new_with_options(
            &script_url(script),
            web_sys::WorkerOptions::new()
                .name(name)
                .type_(web_sys::WorkerType::Module),
        )
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
        #[wasm_bindgen(js_namespace = ["import", "meta"], js_name = url)]
        static IMPORT_META_URL: String;
    }

    fn script_url(script: Option<&str>) -> String {
        let script = script.unwrap_or(DEFAULT_WORKER_SCRIPT);
        let blob_parts = js_sys::Array::new_with_length(1);
        blob_parts.set(0, JsValue::from_str(script));

        let mut options = web_sys::BlobPropertyBag::new();
        options.type_("application/javascript");

        let blob = web_sys::Blob::new_with_str_sequence_and_options(&blob_parts, &options).unwrap();
        web_sys::Url::create_object_url_with_blob(&blob).unwrap()
    }

    /// # Safety
    ///
    /// Undefined behavior if the pointer is not valid or aliased.
    #[wasm_bindgen(js_name = workerOnMessage)]
    pub unsafe fn worker_onmessage(job: *const Job) {
        // Safety: The crate guarantees that
        // - `job` is valid.
        // - `job` is not aliased, which means this function is where the `job` is only used at a time.
        let job = unsafe { job.as_ref().unwrap() };
        job.process();
    }
}
