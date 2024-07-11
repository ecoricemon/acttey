pub mod msg;

pub mod prelude {
    // Re-export WBG_INIT because only clients know that.
    pub use my_ecs::default::prelude::WBG_INIT;
}

use my_ecs::{default::prelude::*, ds::prelude::*, ecs::prelude::*};
use std::{
    fmt::Debug,
    ops::{Deref, DerefMut},
};
use wasm_bindgen::prelude::*;

#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct MainWorker(Worker);

impl MainWorker {
    /// Spawns main worker from the window context.
    pub fn spawn(name: String) -> Result<Self, JsValue> {
        WorkerBuilder::new(name)
            .script(include_str!("main-worker.js"))
            .spawn_concrete()
            .map(|worker| MainWorker(worker))
    }
}

impl Deref for MainWorker {
    type Target = Worker;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for MainWorker {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct SubWorker(Worker);

impl SubWorker {
    /// Spawns main worker from the window context.
    pub fn spawn(name: String) -> Result<Self, JsValue> {
        WorkerBuilder::new(name)
            .spawn_concrete()
            .map(|worker| SubWorker(worker))
    }
}

impl Deref for SubWorker {
    type Target = Worker;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for SubWorker {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Work for SubWorker {
    fn unpark(&mut self, job: ManagedConstPtr<Job>) -> bool {
        self.0.unpark(job)
    }

    fn park(&mut self) -> bool {
        self.0.park()
    }

    fn get_name(&self) -> &str {
        self.0.get_name()
    }
}
