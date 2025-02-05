use super::{
    ent::{component::Component, entity::EntityId},
    entry::Ecs,
    post::EntMoveStorage,
    sched::comm::CommandSender,
    DynResult,
};
use crate::{ds::ReadyFuture, util::macros::impl_from_for_enum};
use std::{fmt, ptr::NonNull, sync::MutexGuard};

pub mod prelude {
    pub use super::Command;
}

/// Command to an ECS instance.
///
/// Command is one way to modify ECS instance directly such as adding or
/// removing systems. In the command method, an ECS handle is given and you can
/// make change to the ECS insance using the handle.
///
/// # Example
///
/// ```
/// use my_ecs::prelude::*;
///
/// struct MyCommand;
///
/// impl Command for MyCommand {
///     fn command(&mut self, mut ecs: Ecs) -> DynResult<()> {
///         ecs.add_system(|| { /* ... */}).take()?;
///         Ok(())
///     }
/// }
/// ```
pub trait Command: Send + 'static {
    /// A method accessing the whole ECS instance.
    ///
    /// After calling this method, the command will be dropped.
    #[allow(unused_variables)]
    fn command(&mut self, ecs: Ecs<'_>) -> DynResult<()>;

    /// Cancellation method which is called when the command cannot be
    /// executed for some reason.
    ///
    /// After calling this method, the command will be dropped.
    fn cancel(&mut self) {}
}

impl fmt::Debug for dyn Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "dyn Command")
    }
}

impl<F> Command for Option<F>
where
    F: FnOnce(Ecs) -> DynResult<()> + Send + 'static,
{
    fn command(&mut self, ecs: Ecs<'_>) -> DynResult<()> {
        if let Some(f) = self.take() {
            f(ecs)
        } else {
            Err("command has been taken".into())
        }
    }
}

impl<F> Command for F
where
    F: FnMut(Ecs) -> DynResult<()> + Send + 'static,
{
    fn command(&mut self, ecs: Ecs<'_>) -> DynResult<()> {
        self(ecs)
    }
}

impl<F> Command for DynResult<F>
where
    F: FnOnce(Ecs) -> DynResult<()> + Send + 'static,
{
    fn command(&mut self, ecs: Ecs<'_>) -> DynResult<()> {
        let empty = Err("command has been taken".into());
        let this = std::mem::replace(self, empty);

        match this {
            Ok(f) => f(ecs),
            Err(e) => Err(e),
        }
    }
}

/// Empty command.
///
/// This implementation helps clients to use '?' operator in their command
/// functions.
impl Command for DynResult<()> {
    fn command(&mut self, _ecs: Ecs<'_>) -> DynResult<()> {
        let empty = Err("command has been taken".into());
        std::mem::replace(self, empty)
    }
}

/// Empty command.
///
/// This implementation allows clients make commands returning just `()`, called
/// unit.
impl Command for () {
    fn command(&mut self, _ecs: Ecs<'_>) -> DynResult<()> {
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) enum CommandObject {
    /// Trait object command.
    Boxed(Box<dyn Command>),

    /// Ready future containing command.
    Future(ReadyFuture),

    Raw(RawCommand),
}

impl_from_for_enum!("outer" = CommandObject; "var" = Boxed; "inner" = Box<dyn Command>);
impl_from_for_enum!("outer" = CommandObject; "var" = Future; "inner" = ReadyFuture);
impl_from_for_enum!("outer" = CommandObject; "var" = Raw; "inner" = RawCommand);

impl CommandObject {
    pub(crate) fn command(self, ecs: Ecs<'_>) -> DynResult<()> {
        match self {
            Self::Boxed(mut boxed) => boxed.command(ecs),
            Self::Future(future) => {
                // Safety: consume() requires correct `Arg` and `CR` types.
                // - `Arg` type is `Ecs<'_>`.
                // - `CR` type is `DynResult<()>`.
                // We matched the types with `consume_ready_future`.
                unsafe { future.consume::<Ecs<'_>, DynResult<()>>(ecs) }
            }
            Self::Raw(raw) => raw.command(ecs),
        }
    }

    pub(crate) fn cancel(self) {
        match self {
            Self::Boxed(mut boxed) => boxed.cancel(),
            Self::Future(_future) => {}
            Self::Raw(raw) => raw.cancel(),
        }
    }
}

/// Like other commands, RawCommand is also executed only once.
#[derive(Debug)]
pub(crate) struct RawCommand {
    data: NonNull<u8>,
    command: unsafe fn(NonNull<u8>, Ecs<'_>) -> DynResult<()>,
    cancel: unsafe fn(NonNull<u8>),
}

unsafe impl Send for RawCommand {}

impl RawCommand {
    pub(crate) unsafe fn new<C: Command>(cmd: &C) -> Self {
        let data = unsafe { NonNull::new_unchecked((cmd as *const _ as *const u8).cast_mut()) };

        unsafe fn command<C: Command>(data: NonNull<u8>, ecs: Ecs<'_>) -> DynResult<()> {
            let data = unsafe { data.cast::<C>().as_mut() };
            data.command(ecs)
        }

        unsafe fn cancel<C: Command>(data: NonNull<u8>) {
            let data = unsafe { data.cast::<C>().as_mut() };
            data.cancel();
        }

        Self {
            data,
            command: command::<C>,
            cancel: cancel::<C>,
        }
    }

    fn command(self, ecs: Ecs<'_>) -> DynResult<()> {
        // Safety: Calling `self.command` is safe because it's guaranteed
        // by owner called new().
        unsafe { (self.command)(self.data, ecs) }
    }

    fn cancel(self) {
        // Safety: Calling `self.cancel` is safe because it's guaranteed by
        // owner called new().
        unsafe { (self.cancel)(self.data) };
    }
}

/// A command builder to attach or detach some components to or from an entity.
///
/// By attaching or detathcing components, entity move from one entity container
/// to another arises because the entity doesn't belong to the previous entity
/// container. If destination entity container doesn't exist at the time, new
/// entity container is generated first then the entity moves into it.
///
/// There are two options about how to handle built command.
/// * Call [`EntityMoveCommandBuilder::finish`]
///   - Just build a command then give it to the caller.
/// * Drop the builder without calling the `finish` method.
///   - If something has changed, build a command then send it to main worker in
///     order to handle it.
pub struct EntityMoveCommandBuilder<'a> {
    tx_cmd: &'a CommandSender,
    guard: MutexGuard<'a, EntMoveStorage>,
    eid: EntityId,
    len: usize,
}

impl<'a> EntityMoveCommandBuilder<'a> {
    pub(crate) const fn new(
        tx_cmd: &'a CommandSender,
        guard: MutexGuard<'a, EntMoveStorage>,
        eid: EntityId,
    ) -> Self {
        Self {
            tx_cmd,
            guard,
            eid,
            len: 0,
        }
    }

    /// Finishes to build an entity move command.
    pub fn finish(mut self) -> impl Command + 'static {
        assert!(self.len > 0);

        let cmd = self._finish();

        // Drops mutex guard without scheduling command.
        self.len = 0;
        drop(self);

        cmd
    }

    /// Adds component attachment instruction to a command builder.
    ///
    /// The command builder will generate a command with the instruction when
    /// [`EntityMoveCommandBuilder::finish`] is called.
    pub fn attach<C: Component>(mut self, component: C) -> Self {
        self.guard.insert_addition(self.eid, component);
        self.len += 1;
        self
    }

    /// Adds component detatchment instruction to a command builder.
    ///
    /// The command builder will generate a command with the instruction when
    /// [`EntityMoveCommandBuilder::finish`] is called.
    pub fn detach<C: Component>(mut self) -> Self {
        self.guard.insert_removal(self.eid, C::key());
        self.len += 1;
        self
    }

    fn _finish(&mut self) -> EntityMoveCommand {
        self.guard.set_command_length(self.len);
        EntityMoveCommand
    }
}

impl Drop for EntityMoveCommandBuilder<'_> {
    fn drop(&mut self) {
        if self.len > 0 {
            let cmd = self._finish();
            // Boxing a ZST command doesn't allocate memory for it.
            self.tx_cmd
                .send_or_cancel(CommandObject::Boxed(Box::new(cmd)));
        }
    }
}

struct EntityMoveCommand;

impl Command for EntityMoveCommand {
    fn command(&mut self, ecs: Ecs<'_>) -> DynResult<()> {
        let post = unsafe { ecs.post_ptr().as_ref() };
        let mut guard = post.lock_entity_move_storage();
        guard.consume(ecs);
        Ok(())
    }
}
