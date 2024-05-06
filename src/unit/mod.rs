pub mod prelude {
    pub use super::Tick;
}

/// Monotonically increasing counter which [`Scheduler::schedule`](crate::ecs::schedule::Scheduler::schedule) increases every time when it's called.
/// `schedule()` is called once every frame, so that `Tick` increases on a frame basis.
/// For exampple, if actual fps is 60, then `Tick` will increase by 60 in a sec.
pub type Tick = u32;
