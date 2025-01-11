use crate::{
    debug_format,
    ds::vec::AnyVec,
    ecs::{
        ent::{
            component::{Component, ComponentKey},
            entity::{EntityId, EntityKeyRef},
            storage::{EntityContainer, EntityReg},
        },
        entry::{Ecs, EcsEntry},
        sched::ctrl::SUB_CONTEXT,
    },
};
use std::{
    cmp,
    collections::{hash_map::Entry, HashMap},
    ptr::NonNull,
    sync::{Arc, Mutex, MutexGuard},
};

/// Returns shared reference to [`Shared`] struct.
///
/// This function can be called on sub worker context only. If you're on main
/// worker context, then get the reference through [`Ecs`].
pub(crate) fn get_shared<'a>() -> &'a Shared {
    let ptr = SUB_CONTEXT.get();
    assert!(!ptr.is_dangling());

    // Safety: Current worker has valid sub context pointer.
    unsafe { ptr.as_ref().get_shared() }
}

/// Globally shared data.
///
/// To be shared across workers, this struct is intended to be wrapped in
/// [`Arc`].
///
/// This struct contains data that can be accessed from main or sub workers.
/// Main worker can access this through [`Ecs`] while sub workers can do it
/// through [`get_shared`].
#[derive(Debug)]
pub(crate) struct Shared {
    ent_move: Mutex<EntMoveStorage>,
}

impl Shared {
    pub(crate) fn new() -> Self {
        Self {
            ent_move: Mutex::new(EntMoveStorage::new()),
        }
    }

    /// Locks mutex of the [`EntMoveStorage`] and returns mutex guard of it.
    pub(crate) fn lock_entity_move_storage(&self) -> MutexGuard<'_, EntMoveStorage> {
        self.ent_move.lock().unwrap()
    }

    pub(crate) fn shrink_to_fit(&self) {
        self.ent_move.lock().unwrap().shrink_to_fit();
    }
}

/// A temporary storage containing entity move commands caused by attaching or
/// detaching components to currently existing entities.
#[derive(Debug)]
pub(crate) struct EntMoveStorage {
    /// Operations on entities whether to add or remove components.
    ops: Vec<Operation>,

    /// Lenghts of commands.
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
        // Safety: We got the poiner from Ecs right before.
        let src_cont_mut = unsafe { src_cont.as_mut() };

        // Gets dst entity container.
        self.set_dst_ckeys(len, src_cont_mut);
        let mut dst_cont = self.find_dst_cont(src_cont_mut, ecs);
        debug_assert_ne!(src_cont, dst_cont);

        // Safety: We got the poiner from Ecs right before.
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
                    let errmsg = debug_format!(
                        "adding component({:?}) to entity({:?}) failed. it already belongs to the entity",
                        target,
                        _from
                    );
                    assert!(!contains_src(target), "{errmsg}");
                    self.ckey_buf.push(*target);
                }
                Direction::Remove => {
                    let errmsg = debug_format!(
                        "removing component({:?}) from entity({:?}) failed. it doesn't belong to the entity",
                        target,
                        _from
                    );
                    assert!(contains_src(target), "{errmsg}");

                    let errmsg =
                        debug_format!("removing the same component more than once is not allowed");
                    let (i, _) = self
                        .ckey_buf
                        .iter()
                        .enumerate()
                        .find(|(_, ckey)| *ckey == target)
                        .expect(&errmsg);
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
            let src_ptr = src_cont
                .value_ptr_by_value_index(src_ci, src_vi)
                .unwrap_unchecked();
            dst_cont.add_value(dst_ci, src_ptr);
            src_cont.forget_value_by_value_index(src_ci, src_vi);
        }

        #[inline]
        unsafe fn buf_to_dst(
            bufs: &mut HashMap<ComponentKey, AnyVec>,
            dst_cont: &mut EntityContainer,
            dst_ckey: &ComponentKey,
            dst_ci: usize,
        ) {
            let buf = bufs
                .get_mut(dst_ckey)
                .unwrap_unchecked();
            let buf_ptr = buf
                .get_raw_unchecked(buf.len() - 1); // Infallible
            dst_cont.add_value(dst_ci, buf_ptr);
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

    /// Whether be added to the enitty or removed from the entity.
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
    fn test_add_existing_component_panic() {
        use crate as my_ecs;
        use my_ecs::prelude::*;

        #[derive(Entity)]
        struct Ea {
            ca: Ca,
        }
        #[derive(Component)]
        struct Ca;

        let mut ecs = Ecs::default(WorkerPool::with_len(1), [1]);
        ecs.register_entity_of::<Ea>().unwrap();
        ecs.add_system(SystemDesc::new().with_once(|ew: EntWrite<Ea>| {
            let eid = ew.take_recur().unwrap().add(Ea { ca: Ca });
            // `Ea` contains `Ca`, so clients cannot add `Ca` again.
            cmd::entity(eid).attach(Ca);
        }))
        .unwrap();
        ecs.run().schedule_all();
    }

    #[test]
    #[should_panic]
    fn test_add_duplicated_components_panic() {
        use crate as my_ecs;
        use my_ecs::prelude::*;

        #[derive(Entity)]
        struct Ea {
            ca: Ca,
        }
        #[derive(Component)]
        struct Ca;
        #[derive(Component)]
        struct Cb;

        let mut ecs = Ecs::default(WorkerPool::with_len(1), [1]);
        ecs.register_entity_of::<Ea>().unwrap();
        ecs.add_system(SystemDesc::new().with_once(|ew: EntWrite<Ea>| {
            let eid = ew.take_recur().unwrap().add(Ea { ca: Ca });
            // Duplicated components are not allowed.
            cmd::entity(eid).attach(Cb).attach(Cb);
        }))
        .unwrap();
        ecs.run().schedule_all();
    }

    #[test]
    #[should_panic]
    fn test_remove_unknonw_component_panic() {
        use crate as my_ecs;
        use my_ecs::prelude::*;

        #[derive(Entity)]
        struct Ea {
            ca: Ca,
        }
        #[derive(Component)]
        struct Ca;
        #[derive(Component)]
        struct Cb;

        let mut ecs = Ecs::default(WorkerPool::with_len(1), [1]);
        ecs.register_entity_of::<Ea>().unwrap();
        ecs.add_system(SystemDesc::new().with_once(|ew: EntWrite<Ea>| {
            let eid = ew.take_recur().unwrap().add(Ea { ca: Ca });
            // `Ea` doesn't contain `Cb`, so clients cannot remove `Cb`.
            cmd::entity(eid).detach::<Cb>();
        }))
        .unwrap();
        ecs.run().schedule_all();
    }

    #[test]
    #[should_panic]
    fn test_remove_the_same_component_many_panic() {
        use crate as my_ecs;
        use my_ecs::prelude::*;

        #[derive(Entity)]
        struct Ea {
            ca: Ca,
            cb: Cb,
        }
        #[derive(Component)]
        struct Ca;
        #[derive(Component)]
        struct Cb;

        let mut ecs = Ecs::default(WorkerPool::with_len(1), [1]);
        ecs.register_entity_of::<Ea>().unwrap();
        ecs.add_system(SystemDesc::new().with_once(|ew: EntWrite<Ea>| {
            let eid = ew.take_recur().unwrap().add(Ea { ca: Ca, cb: Cb });
            // Removing the same component multiple times is not allowed.
            cmd::entity(eid).detach::<Cb>().detach::<Cb>();
        }))
        .unwrap();
        ecs.run().schedule_all();
    }
}
