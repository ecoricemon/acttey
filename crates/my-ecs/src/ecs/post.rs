use crate::{
    ds::{AnyVec, UnsafeFuture},
    ecs::{
        cmd::{Command, EntityMoveCommandBuilder},
        ent::{
            component::{Component, ComponentKey},
            entity::{EntityId, EntityKeyRef},
            storage::{EntityContainer, EntityReg},
        },
        entry::{Ecs, EcsEntry},
        lock::RequestLockFuture,
        resource::Resource,
        sched::{
            comm::{CommandSender, ParkingSender},
            ctrl::{MainWaker, SubContext, UnsafeWaker, SUB_CONTEXT},
            task::{AsyncTask, Task},
        },
        sys::request::Request,
        worker::Message,
        DynResult,
    },
    util::macros::debug_format,
};
use std::{
    cmp,
    collections::{hash_map::Entry, HashMap},
    future::Future,
    ptr::NonNull,
    sync::{
        atomic::{AtomicU32, Ordering},
        Arc, Mutex, MutexGuard,
    },
};

pub mod prelude {
    pub use super::{Commander, Post};
}

/// A [`Resource`] to send command or future.
///
/// This resource provides clients funtionalities to send commands or futures
/// in their systems. This resource also provides interior mutability, so that
/// clients can request the resource with [`ResRead`](crate::prelude::ResRead).
//
// By registering this resource in each ECS instance, it is possible to have
// multiple ECS instances in one worker. Global function & static variable
// approach, on the other hand, is not good option for that because it cannot
// determine which ECS instance is the destination of sending easily.
pub struct Post {
    tx_cmd: CommandSender,
    tx_msg: ParkingSender<Message>,
    tx_dedi: ParkingSender<Task>,
    fut_cnt: Arc<AtomicU32>,
    ent_move: Mutex<EntMoveStorage>,
}

impl Post {
    pub(super) fn new(
        tx_cmd: CommandSender,
        tx_msg: ParkingSender<Message>,
        tx_dedi: ParkingSender<Task>,
        fut_cnt: Arc<AtomicU32>,
    ) -> Self {
        Self {
            tx_cmd,
            tx_msg,
            tx_dedi,
            fut_cnt,
            ent_move: Mutex::new(EntMoveStorage::new()),
        }
    }

    /// Sends the given command to ECS scheduler.
    ///
    /// Commands are executed on the main worker, but the command is not
    /// executed at the time of sending. ECS scheduler runs all buffered
    /// commands at the end of current cycle and before the next cycle.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::sync::mpsc;
    ///
    /// let (tx, rx) = mpsc::channel();
    ///
    /// // A system sending a command.
    /// let tx_a = tx.clone();
    /// let sys_a = move |rr: ResRead<Post>| {
    ///     tx_a.send("sys_a").unwrap();
    ///     let tx_aa = tx_a.clone();
    ///     rr.send_command(move |ecs| {
    ///         tx_aa.send("cmd_a").unwrap();
    ///         Ok(())
    ///     });
    /// };
    ///
    /// // A system sending a command.
    /// let tx_b = tx.clone();
    /// let sys_b = move |rr: ResRead<Post>| {
    ///     tx_b.send("sys_b").unwrap();
    ///     let tx_bb = tx_b.clone();
    ///     rr.send_command(move |ecs| {
    ///         tx_bb.send("cmd_b").unwrap();
    ///         Ok(())
    ///     });
    /// };
    ///
    /// Ecs::default(WorkerPool::with_len(2), [2])
    ///     .add_systems((sys_a, sys_b))
    ///     .step();
    ///
    /// // No data dependency between `sys_a` and `sys_b`, so they can be run
    /// // simultaneously.
    /// assert!(matches!(rx.try_recv(), Ok("sys_a") | Ok("sys_b")));
    /// assert!(matches!(rx.try_recv(), Ok("sys_a") | Ok("sys_b")));
    /// assert!(matches!(rx.try_recv(), Ok("cmd_a") | Ok("cmd_b")));
    /// assert!(matches!(rx.try_recv(), Ok("cmd_a") | Ok("cmd_b")));
    /// ```
    pub fn send_command<F>(&self, f: F)
    where
        F: FnOnce(Ecs) -> DynResult<()> + Send + 'static,
    {
        let wrapped = Some(f);
        let boxed: Box<dyn Command> = Box::new(wrapped);
        self.tx_cmd.send_or_cancel(boxed.into());
    }

    /// Sends the given future to ECS scheduler.
    ///
    /// The future is guaranteed to be polled more than or just once in the
    /// current cycle.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::{time::Duration, sync::{Arc, Mutex}};
    ///
    /// let state = Arc::new(Mutex::new(0));
    ///
    /// let c_state = Arc::clone(&state);
    /// let foo = async move {
    ///     *c_state.lock().unwrap() = 1;
    ///     async_io::Timer::after(Duration::from_millis(10)).await;
    ///     *c_state.lock().unwrap() = 2;
    /// };
    ///
    /// Ecs::default(WorkerPool::new(), [])
    ///     .add_once_system(move |rr: ResRead<Post>| {
    ///         rr.send_future(foo);
    ///     })
    ///     .run(|_| {});
    ///
    /// assert_eq!(*state.lock().unwrap(), 2);
    /// ```
    pub fn send_future<F, R>(&self, future: F)
    where
        F: Future<Output = R> + Send + 'static,
        R: Command,
    {
        let ptr = SUB_CONTEXT.get();

        if ptr.is_dangling() {
            on_main(self, future);
        } else {
            on_sub(future, *ptr);
        }

        // === Internal helper functions ===

        fn on_main<F, R>(this: &Post, future: F)
        where
            F: Future<Output = R> + Send + 'static,
            R: Command,
        {
            // Allocates memory for the future.
            let waker = MainWaker::new(this.tx_dedi.clone());
            let handle = UnsafeFuture::new(future, waker, consume_ready_future::<R>);

            // Increases future count.
            this.fut_cnt.fetch_add(1, Ordering::Relaxed);

            // Pushes the future handle onto dedicated queue.
            let task = Task::Async(AsyncTask(handle));
            this.tx_dedi.send(task).unwrap();
        }

        fn on_sub<F, R>(future: F, cx: NonNull<SubContext>)
        where
            F: Future<Output = R> + Send + 'static,
            R: Command,
        {
            // Allocates memory for the future.
            let waker = UnsafeWaker::new(cx.as_ptr());
            let handle = UnsafeFuture::new(future, waker, consume_ready_future::<R>);

            // Pushes the future handle onto local future queue.
            // Safety: Current worker has valid sub context pointer.
            let comm = unsafe { cx.as_ref().get_comm() };
            comm.push_future_task(handle);

            // Increases future count.
            // Main worker will check this whenever it needs.
            comm.signal().add_future_count(1);

            // If current worker's local queue is not empty, current worker cannot
            // do the future task promptly, so wakes another worker to steal it.
            if !comm.is_local_empty() {
                comm.signal().sub().notify_one();
            }
        }
    }

    pub fn request_lock<'buf, Req>(&self) -> RequestLockFuture<'buf, Req>
    where
        Req: Request,
    {
        RequestLockFuture::new(self.tx_cmd.clone(), self.tx_msg.clone())
    }

    pub fn change_entity(&self, eid: EntityId) -> EntityMoveCommandBuilder<'_> {
        let guard = self.lock_entity_move_storage();
        EntityMoveCommandBuilder::new(&self.tx_cmd, guard, eid)
    }

    pub(crate) fn as_commander(&self) -> Commander<'_> {
        Commander(self)
    }

    pub(crate) fn lock_entity_move_storage(&self) -> MutexGuard<'_, EntMoveStorage> {
        self.ent_move.lock().unwrap()
    }

    pub(crate) fn shrink_to_fit(&self) {
        let mut guard = self.ent_move.lock().unwrap();
        guard.shrink_to_fit();
    }
}

impl Resource for Post {}

pub struct Commander<'a>(&'a Post);

impl Commander<'_> {
    pub fn change_entity(&self, eid: EntityId) -> EntityMoveCommandBuilder<'_> {
        self.0.change_entity(eid)
    }
}

pub(crate) fn consume_ready_future<T: Command>(mut res: T, ecs: Ecs<'_>) -> DynResult<()> {
    res.command(ecs)
}

/// A temporary storage containing entity move commands caused by attaching or
/// detaching components to currently existing entities.
#[derive(Debug)]
pub(crate) struct EntMoveStorage {
    /// Operations on entities whether to add or remove components.
    ops: Vec<Operation>,

    /// Lengths of commands.
    ///
    /// A command is composed of a set of operations. Length of it is number
    /// of operations of the set.
    /// In other words, `lens.iter().sum() == ops.len()`.
    lens: Vec<usize>,

    /// Component values to be added.
    adds: HashMap<ComponentKey, AnyVec>,

    /// A buffer for holding component keys temporarily.
    ckey_buf: Vec<ComponentKey>,
}

impl EntMoveStorage {
    pub(crate) fn new() -> Self {
        Self {
            ops: Vec::new(),
            lens: Vec::new(),
            adds: HashMap::default(),
            ckey_buf: Vec::new(),
        }
    }

    pub(crate) fn insert_addition<C: Component>(&mut self, eid: EntityId, comp: C) {
        self.ops.push(Operation {
            from: eid,
            target: C::key(),
            dir: Direction::Add,
        });
        match self.adds.entry(C::key()) {
            Entry::Occupied(mut entry) => {
                let v = entry.get_mut();
                // Safety: Vector holds `AddingComponent`.
                unsafe { v.push(comp) };
            }
            Entry::Vacant(entry) => {
                let mut v = AnyVec::new(C::type_info());
                // Safety: Vector holds `AddingComponent`.
                unsafe { v.push(comp) };
                entry.insert(v);
            }
        }
    }

    pub(crate) fn insert_removal(&mut self, eid: EntityId, ckey: ComponentKey) {
        self.ops.push(Operation {
            from: eid,
            target: ckey,
            dir: Direction::Remove,
        });
    }

    pub(crate) fn set_command_length(&mut self, len: usize) {
        debug_assert!(len > 0);
        self.lens.push(len);
    }

    pub(crate) fn consume(&mut self, mut ecs: Ecs<'_>) {
        while let Some(len) = self.lens.pop() {
            self.handle(len, &mut ecs);
        }
    }

    /// Moves an entity from one entity container to another.
    ///
    /// If failed to find source entity, does nothing.
    ///
    /// # Safety
    ///
    /// - `adds` must contain proper component values in it.
    fn handle(&mut self, len: usize, ecs: &mut Ecs<'_>) {
        // Safety: We got an operation, which means length must exist.
        let src_eid = unsafe { self.ops.last().unwrap_unchecked().from };

        // Gets src entity container.
        let ei = src_eid.container_index();
        let src_ekey = EntityKeyRef::Index(&ei);
        let Some(mut src_cont) = ecs.entity_container_ptr(src_ekey) else {
            return;
        };
        // Safety: We got the pointer from Ecs right before.
        let src_cont_mut = unsafe { src_cont.as_mut() };

        // Gets dst entity container.
        self.set_dst_ckeys(len, src_cont_mut);
        let mut dst_cont = self.find_dst_cont(src_cont_mut, ecs);
        debug_assert_ne!(src_cont, dst_cont);

        // Safety: We got the pointer from Ecs right before.
        let dst_cont_mut = unsafe { dst_cont.as_mut() };

        // Moves an entity from src to dst.
        if let Some(src_vi) = src_cont_mut.to_value_index(src_eid.row_index()) {
            self.move_entity(src_vi, src_cont_mut, dst_cont_mut);
        }

        self.ops.truncate(self.ops.len() - len);
    }

    /// # Panics
    ///
    /// Panics if
    /// - Inserted operations try to add components that already belong to
    ///   the source entity container.
    /// - Inserted operations try to remove components that doesn't belong
    ///   to the source entity container.
    fn set_dst_ckeys(&mut self, len: usize, src_cont: &EntityContainer) {
        let contains_src =
            |ckey: &ComponentKey| src_cont.get_tag().get_component_keys().contains(ckey);

        self.ckey_buf.clear();
        self.ckey_buf
            .extend(src_cont.get_tag().get_component_keys().iter());

        let op_iter = self.ops.iter().rev().take(len);

        for Operation {
            from: _from,
            target,
            dir,
        } in op_iter
        {
            match dir {
                Direction::Add => {
                    let reason = debug_format!(
                        "adding component({:?}) to entity({:?}) failed. it already belongs to the entity",
                        target,
                        _from
                    );
                    assert!(!contains_src(target), "{}", reason);
                    self.ckey_buf.push(*target);
                }
                Direction::Remove => {
                    let reason = debug_format!(
                        "removing component({:?}) from entity({:?}) failed. it doesn't belong to the entity",
                        target,
                        _from
                    );
                    assert!(contains_src(target), "{}", reason);

                    let reason =
                        debug_format!("removing the same component more than once is not allowed");
                    let (i, _) = self
                        .ckey_buf
                        .iter()
                        .enumerate()
                        .find(|(_, ckey)| *ckey == target)
                        .expect(&reason);
                    self.ckey_buf.swap_remove(i);
                }
            }
        }

        self.ckey_buf.sort_unstable();
    }

    fn find_dst_cont(
        &self,
        src_cont: &EntityContainer,
        ecs: &mut Ecs<'_>,
    ) -> NonNull<EntityContainer> {
        let dst_ckeys = &self.ckey_buf;

        let dst_ekey = EntityKeyRef::Ckeys(dst_ckeys);
        if let Some(dst_cont) = ecs.entity_container_ptr(dst_ekey) {
            dst_cont
        } else {
            let mut desc = EntityReg::new(None, src_cont.create_twin());

            for dst_ckey in dst_ckeys.iter() {
                if src_cont.contains_column(dst_ckey) {
                    // Safety: Infallible.
                    let tinfo = unsafe {
                        let ci = src_cont.get_column_index(dst_ckey).unwrap_unchecked();
                        *src_cont.get_column_info(ci).unwrap_unchecked()
                    };
                    desc.add_component(tinfo);
                } else {
                    // Safety: Infallible.
                    debug_assert!(self.adds.contains_key(dst_ckey));
                    let v = unsafe { self.adds.get(dst_ckey).unwrap_unchecked() };
                    desc.add_component(*v.type_info())
                }
            }

            let res = ecs.register_entity(desc);
            debug_assert!(res.is_ok());

            let dst_ekey = EntityKeyRef::Ckeys(dst_ckeys);
            ecs.entity_container_ptr(dst_ekey).unwrap()
        }
    }

    #[rustfmt::skip]
    fn move_entity(
        &mut self,
        src_vi: usize,
        src_cont: &mut EntityContainer,
        dst_cont: &mut EntityContainer,
    ) {
        // TODO: Test required.
        //
        // Safety
        // 1. We call begin_xxx -> add/remove_xxx -> end_xxx to add/remove
        //    entity to/from an entity container.
        // 2. We get component key from entity container. Therefore
        //    unwrapping column index gotten using the key is safe.

        unsafe {
            let src_ckeys = Arc::clone(src_cont.get_tag().get_component_keys());
            let dst_ckeys = Arc::clone(dst_cont.get_tag().get_component_keys());

            src_cont.begin_remove_row_by_value_index(src_vi);
            dst_cont.begin_add_row();

            let (mut src_ci, src_len) = (0, src_ckeys.len());
            let (mut dst_ci, dst_len) = (0, dst_ckeys.len());

            while src_ci < src_len && dst_ci < dst_len {
                match src_ckeys[src_ci].cmp(&dst_ckeys[dst_ci]) {
                    cmp::Ordering::Equal => { // src = dst : src -> dst
                        src_to_dst(src_cont, dst_cont, src_ci, src_vi, dst_ci);
                        src_ci += 1;
                        dst_ci += 1;
                    }
                    cmp::Ordering::Greater => { // src > dst : buf -> dst
                        buf_to_dst(&mut self.adds, dst_cont, &dst_ckeys[dst_ci], dst_ci);
                        dst_ci += 1;
                    }
                    cmp::Ordering::Less => { // src < dst : src -> drop
                        src_cont.drop_value_by_value_index(src_ci, src_vi);
                        src_ci += 1;
                    }
                }
            }
            while src_ci < src_len { // src -> drop
                src_cont.drop_value_by_value_index(src_ci, src_vi);
                src_ci += 1;
            }
            while dst_ci < dst_len { // buf -> dst
                buf_to_dst(&mut self.adds, dst_cont, &dst_ckeys[dst_ci], dst_ci);
                dst_ci += 1;
            }

            src_cont.end_remove_row_by_value_index(src_vi);
            dst_cont.end_add_row();
        }

        #[inline]
        unsafe fn src_to_dst(
            src_cont: &mut EntityContainer,
            dst_cont: &mut EntityContainer,
            src_ci: usize,
            src_vi: usize,
            dst_ci: usize,
        ) {
            unsafe {
                let src_ptr = src_cont
                    .value_ptr_by_value_index(src_ci, src_vi)
                    .unwrap_unchecked();
                dst_cont.add_value(dst_ci, src_ptr);
                src_cont.forget_value_by_value_index(src_ci, src_vi);
            }
        }

        #[inline]
        unsafe fn buf_to_dst(
            bufs: &mut HashMap<ComponentKey, AnyVec>,
            dst_cont: &mut EntityContainer,
            dst_ckey: &ComponentKey,
            dst_ci: usize,
        ) {
            let buf = unsafe {
                let buf = bufs
                    .get_mut(dst_ckey)
                    .unwrap_unchecked();
                let buf_ptr = buf
                    .get_raw_unchecked(buf.len() - 1); // Infallible
                dst_cont.add_value(dst_ci, buf_ptr);
                buf
            };
            buf.pop_forget();
        }
    }

    fn shrink_to_fit(&mut self) {
        self.ops.shrink_to_fit();
        for v in self.adds.values_mut() {
            v.shrink_to_fit();
        }
        // No need for `self.adds`. We never remove items from it.
    }
}

#[derive(Debug, Clone, Copy)]
struct Operation {
    /// Entity that the operation will be executed on.
    from: EntityId,

    /// The operation's target component.
    target: ComponentKey,

    /// Whether be added to the entity or removed from the entity.
    dir: Direction,
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    Add,
    Remove,
}

#[cfg(test)]
mod tests {
    #[test]
    #[should_panic]
    #[rustfmt::skip]
    fn test_add_existing_component_panic() {
        use crate as my_ecs;
        use my_ecs::prelude::*;

        #[derive(Entity)]
        struct Ea { ca: Ca }
        #[derive(Component)] struct Ca;

        Ecs::default(WorkerPool::with_len(1), [1])
            .register_entity_of::<Ea>()
            .add_system(SystemDesc::new().with_once(|rr: ResRead<Post>, ew: EntWrite<Ea>| {
                let eid = ew.take_recur().add(Ea { ca: Ca });
                // `Ea` contains `Ca`, so clients cannot add `Ca` again.
                rr.change_entity(eid).attach(Ca);
            }))
            .step();
    }

    #[test]
    #[should_panic]
    #[rustfmt::skip]
    fn test_add_duplicated_components_panic() {
        use crate as my_ecs;
        use my_ecs::prelude::*;

        #[derive(Entity)]
        struct Ea { ca: Ca }
        #[derive(Component)] struct Ca;
        #[derive(Component)] struct Cb;

        Ecs::default(WorkerPool::with_len(1), [1])
            .register_entity_of::<Ea>()
            .add_system(SystemDesc::new().with_once(|rr: ResRead<Post>, ew: EntWrite<Ea>| {
                let eid = ew.take_recur().add(Ea { ca: Ca });
                // Duplicated components are not allowed.
                rr.change_entity(eid).attach(Cb).attach(Cb);
            }))
            .step();
    }

    #[test]
    #[should_panic]
    #[rustfmt::skip]
    fn test_remove_unknown_component_panic() {
        use crate as my_ecs;
        use my_ecs::prelude::*;

        #[derive(Entity)]
        struct Ea { ca: Ca }
        #[derive(Component)] struct Ca;
        #[derive(Component)] struct Cb;

        Ecs::default(WorkerPool::with_len(1), [1])
            .register_entity_of::<Ea>()
            .add_system(SystemDesc::new().with_once(|rr: ResRead<Post>, ew: EntWrite<Ea>| {
                let eid = ew.take_recur().add(Ea { ca: Ca });
                // `Ea` doesn't contain `Cb`, so clients cannot remove `Cb`.
                rr.change_entity(eid).detach::<Cb>();
            }))
            .step();
    }

    #[test]
    #[should_panic]
    #[rustfmt::skip]
    fn test_remove_the_same_component_many_panic() {
        use crate as my_ecs;
        use my_ecs::prelude::*;

        #[derive(Entity)]
        struct Ea { ca: Ca, cb: Cb }
        #[derive(Component)] struct Ca;
        #[derive(Component)] struct Cb;

        Ecs::default(WorkerPool::with_len(1), [1])
            .register_entity_of::<Ea>()
            .add_system(SystemDesc::new().with_once(|rr: ResRead<Post>, ew: EntWrite<Ea>| {
                let eid = ew.take_recur().add(Ea { ca: Ca, cb: Cb });
                // Removing the same component multiple times is not allowed.
                rr.change_entity(eid).detach::<Cb>().detach::<Cb>();
            }))
            .step();
    }
}
