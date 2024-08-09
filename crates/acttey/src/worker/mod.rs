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
pub(crate) struct MainWorker {
    inner: Worker,
}

impl MainWorker {
    /// Spawns main worker.
    pub(crate) fn spawn(name: String, listener: &str) -> Result<Self, JsValue> {
        WorkerBuilder::new(name)
            .listener(listener)
            .spawn_concrete()
            .map(|worker| MainWorker { inner: worker })
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

#[derive(Debug)]
#[repr(transparent)]
pub struct SubWorker {
    inner: Worker,
}

impl SubWorker {
    /// Spawns sub worker.
    pub fn spawn(name: String) -> Result<Self, JsValue> {
        WorkerBuilder::new(name)
            .spawn_concrete()
            .map(|worker| SubWorker { inner: worker })
    }
}

impl Deref for SubWorker {
    type Target = Worker;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for SubWorker {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl Work for SubWorker {
    fn unpark(&mut self, job: ManagedConstPtr<Job>) -> bool {
        self.inner.unpark(job)
    }

    fn park(&mut self) -> bool {
        self.inner.park()
    }

    fn get_name(&self) -> &str {
        self.inner.get_name()
    }
}
