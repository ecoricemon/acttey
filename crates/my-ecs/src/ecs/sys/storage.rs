use super::{
    request::RequestKey,
    system::{SystemGroup, SystemId},
};
use crate::ecs::{
    resource::{ResourceKey, ResourceStorage},
    sys::{
        filter::{FilterInfo, FilterKey, StoreFilterInfo},
        query::{
            EntQueryInfo, EntQueryKey, QueryInfo, QueryKey, ResQueryInfo, ResQueryKey,
            StoreEntQueryInfo, StoreQueryInfo, StoreResQueryInfo,
        },
        request::{RequestInfo, StoreRequestInfo},
        system::{InsertPos, NonZeroTick, SystemData, SystemKey},
    },
    EcsResult,
};
use crate::util::prelude::*;
use std::{
    array,
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::BuildHasher,
    mem,
    ops::{Deref, DerefMut},
    ptr,
    rc::Rc,
};

/// * `S` - Hasher.
/// * `N` - Number of [`SystemGroup`], which operates in a different configurable way from each other.
#[derive(Debug)]
pub(crate) struct SystemStorage<S, const N: usize> {
    pub(crate) sys: Multi<WorkingSystemGroup<S>, N>,

    /// A storage including request, query and filter information together.
    //
    // When a system is registered, it's corresponding request and
    // related other information is registered here, and it can be shared from other systems.
    // When it comes to unregister, each system data will unregister itself from
    // this stroage when it's dropped.
    rinfo: Rc<RefCell<RequestInfoStorage<S>>>,

    /// Dedicated systems, which are not allowed to be run from other workers.
    /// So they must be run on main worker.
    /// A system that requests any dedicated resources becomes a dedicated system.
    pub(crate) dedi_sys: HashSet<SystemId, S>,

    /// [`ResourceKey`] -> sorted [`SystemId`].
    res_to_sys: HashMap<ResourceKey, Vec<SystemId>, S>,
}

impl<S, const N: usize> SystemStorage<S, N>
where
    S: BuildHasher + Default + 'static,
{
    pub(crate) fn new() -> Self {
        // For now, group index `gi` below is limited up to u16::MAX - 1 by `SystemId`.
        // Here, we check N in terms of bounds at compile time.
        let _: () = const { assert!(N < SystemId::max_group_index() as usize) };

        let rinfo = Rc::new(RefCell::new(RequestInfoStorage::new()));
        let groups = array::from_fn(|gi| {
            let rinfo = Rc::clone(&rinfo);
            WorkingSystemGroup::new(rinfo, gi as u16)
        });

        Self {
            sys: Multi::new(groups),
            rinfo,
            dedi_sys: HashSet::default(),
            res_to_sys: HashMap::default(),
        }
    }

    pub(crate) fn get_system_group_mut(&mut self, gi: usize) -> &mut WorkingSystemGroup<S> {
        self.sys.switch_to(gi)
    }

    pub(crate) fn request_info_storage(&mut self) -> Rc<RefCell<RequestInfoStorage<S>>> {
        Rc::clone(&self.rinfo)
    }

    pub(crate) fn register_system(
        &mut self,
        gi: usize,
        skey: SystemKey,
        sdata: SystemData,
        volatile: bool,
        res: &ResourceStorage<S>,
    ) -> EcsResult<()> {
        let sid = sdata.id();
        let res_read = Rc::clone(&sdata.get_request_info().res_read().1);
        let res_write = Rc::clone(&sdata.get_request_info().res_write().1);

        self.sys
            .switch_to(gi)
            .register_system(skey, sdata, volatile);

        // Determines whether or not the system is dedicated.
        let rkey_iter = res_read.rkeys().iter().chain(res_write.rkeys());
        if rkey_iter.clone().any(|rkey| res.is_dedicated2(rkey)) {
            self.dedi_sys.insert(sid);
        }

        // Resource -> system
        for rkey in rkey_iter {
            self.res_to_sys
                .entry(*rkey)
                .and_modify(|sids| {
                    sids.push(sid);
                    sids.sort_unstable();
                })
                .or_insert(vec![sid]);
        }

        Ok(())
    }

    /// Activates the system. If the system is already active, nothing takes place.
    pub(crate) fn activate_system(
        &mut self,
        target: &SystemId,
        at: InsertPos,
        live: NonZeroTick,
    ) -> EcsResult<()> {
        self.sys.activate_system(target, at, live)
    }
}

#[derive(Debug)]
pub(crate) struct WorkingSystemGroup<S> {
    sys: SystemGroup<S>,

    /// Type erased operation function pointer.
    /// The type is defined when you call [`Self::set_operation`].
    //
    // This struct doesn't know about the exact op parameters.
    // They will be defined from external.
    // We could use generic parameter for them, but it propagate to other structs.
    // We don't have to notify the type outside this struct.
    // Raw pointer helps us remove unwanted generic parameter.
    op: *const (),

    /// Function pointer for validation of [`Self::op`].
    //
    // Function pointer of generic function is not deterministic.
    // See https://github.com/rust-lang/rust/issues/54685
    // They can be optimized away, so it would behave diffrently as you expected.
    //
    // e.g.
    // fn foo<T>() { /* noop */ }
    // let foo_i32 = foo::<i32> as *const ();
    // let foo_i64 = foo::<i64> as *const ();
    // // assert_ne!(foo_i32, foo_i64); // Will panic in *release* mode.
    //
    // Therefore, we're going to validate the type in debug mode only.
    #[cfg(debug_assertions)]
    op_test: *const (),
}

impl<S> WorkingSystemGroup<S>
where
    S: BuildHasher + Default,
{
    fn new<Stor>(stor: Rc<RefCell<Stor>>, gi: u16) -> Self
    where
        Stor: StoreRequestInfo + 'static,
    {
        Self {
            sys: SystemGroup::new(stor, gi),
            op: ptr::null(),
            #[cfg(debug_assertions)]
            op_test: ptr::null(),
        }
    }

    pub(crate) fn set_operation<Arg>(&mut self, op: fn(&mut SystemGroup<S>, Arg)) {
        #[cfg(debug_assertions)]
        {
            self.op_test = Self::operate_unchecked::<Arg> as *const ();
        }

        self.op = op as *const ();
    }

    /// # Safety
    ///
    /// Undefined behavior if argument type is different from the type used at
    /// [`Self::set_operation`].
    pub(crate) unsafe fn operate_unchecked<Arg>(&mut self, arg: Arg) {
        // Validates generic parameter `Arg`.
        #[cfg(debug_assertions)]
        {
            assert_eq!(self.op_test, Self::operate_unchecked::<Arg> as *const ());
        }

        let op: fn(&mut SystemGroup<S>, Arg) = mem::transmute(self.op);
        op(&mut self.sys, arg);
    }
}

impl<S> Deref for WorkingSystemGroup<S> {
    type Target = SystemGroup<S>;

    fn deref(&self) -> &Self::Target {
        &self.sys
    }
}

impl<S> DerefMut for WorkingSystemGroup<S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.sys
    }
}

/// Storage containig request and other info.
#[derive(Debug)]
pub(crate) struct RequestInfoStorage<S> {
    /// [`RequestKey`] -> [`RequestInfo`].
    rinfo: HashMap<RequestKey, Rc<RequestInfo>, S>,

    /// [`QueryKey`] -> [`QueryInfo`].
    qinfo: HashMap<QueryKey, Rc<QueryInfo>, S>,

    /// [`ResQueryKey`] -> [`ResQueryInfo`].
    rqinfo: HashMap<ResQueryKey, Rc<ResQueryInfo>, S>,

    /// [`EntQueryKey`] -> [`EntQueryInfo`].
    eqinfo: HashMap<EntQueryKey, Rc<EntQueryInfo>, S>,

    /// [`FilterKey`] -> [`FilterInfo`].
    finfo: HashMap<FilterKey, Rc<FilterInfo>, S>,
}

impl<S> RequestInfoStorage<S>
where
    S: Default,
{
    fn new() -> Self {
        Self {
            rinfo: HashMap::default(),
            qinfo: HashMap::default(),
            rqinfo: HashMap::default(),
            eqinfo: HashMap::default(),
            finfo: HashMap::default(),
        }
    }
}

impl<S> RequestInfoStorage<S>
where
    S: BuildHasher,
{
    // for future use.
    #[allow(dead_code)]
    pub(crate) fn get_request_info(&self, key: &RequestKey) -> Option<Rc<RequestInfo>> {
        StoreRequestInfo::get(self, key)
    }

    // for future use.
    #[allow(dead_code)]
    pub(crate) fn get_query_info(&self, key: &QueryKey) -> Option<Rc<QueryInfo>> {
        StoreQueryInfo::get(self, key)
    }

    // for future use.
    #[allow(dead_code)]
    pub(crate) fn get_resource_query_info(&self, key: &ResQueryKey) -> Option<Rc<ResQueryInfo>> {
        StoreResQueryInfo::get(self, key)
    }

    // for future use.
    #[allow(dead_code)]
    pub(crate) fn get_entity_query_info(&self, key: &EntQueryKey) -> Option<Rc<EntQueryInfo>> {
        StoreEntQueryInfo::get(self, key)
    }

    // for future use.
    #[allow(dead_code)]
    pub(crate) fn get_filter_info(&self, key: &FilterKey) -> Option<Rc<FilterInfo>> {
        StoreFilterInfo::get(self, key)
    }

    fn remove(&mut self, key: &RequestKey) {
        // Removes request info if it's not referenced from external anymore.
        if matches!(self.rinfo.get(key), Some(x) if Rc::strong_count(x) == 1) {
            // Safety: We checked it in matches.
            let rinfo = unsafe { self.rinfo.remove(key).unwrap_unchecked() };

            // `RequestInfo` contains other info, so copy keys and drop rinfo first
            // in order to keep remove code simple.
            let read_key = rinfo.read().0;
            let write_key = rinfo.write().0;
            let res_read_key = rinfo.res_read().0;
            let res_write_key = rinfo.res_write().0;
            let ent_write_key = rinfo.ent_write().0;
            drop(rinfo);

            // Removes read & write query and filter info.
            remove_qinfo_finfo(self, &read_key);
            remove_qinfo_finfo(self, &write_key);

            // Removes read & write resource info.
            remove_rqinfo(self, &res_read_key);
            remove_rqinfo(self, &res_write_key);

            // Removes write entity info.
            remove_eqinfo(self, &ent_write_key);
        }

        // Removes query and filter info if it's not referenced from external anymore.
        // This function must be called inside `remove()`.
        fn remove_qinfo_finfo<S>(this: &mut RequestInfoStorage<S>, key: &QueryKey)
        where
            S: BuildHasher,
        {
            // `self.qinfo` = 1.
            const QINFO_EMPTY_STRONG_CNT: usize = 1;

            if matches! (
                this.qinfo.get(key),
                Some(x) if Rc::strong_count(x) == QINFO_EMPTY_STRONG_CNT
            ) {
                // `QueryInfo` contains `FilterInfo` in it.
                // We need to remove `FilterInfo` first.
                // Safety: We checked it in matches.
                let qinfo = unsafe { this.qinfo.remove(key).unwrap_unchecked() };

                // Removes filter info it's not referenced from external anymore.
                for (fkey, finfo) in qinfo.filters() {
                    // `finfo` + `self.finfo` = 2.
                    const FINFO_EMPTY_STRONG_CNT: usize = 2;

                    if Rc::strong_count(finfo) == FINFO_EMPTY_STRONG_CNT {
                        this.finfo.remove(fkey);
                    }
                }
            }
        }

        // Removes resource query info if it's not referenced from external anymore.
        // This function must be called inside `remove()`.
        fn remove_rqinfo<S>(this: &mut RequestInfoStorage<S>, key: &ResQueryKey)
        where
            S: BuildHasher,
        {
            // `self.rqinfo` = 1.
            const EMPTY_STRONG_CNT: usize = 1;

            if matches! (
                this.rqinfo.get(key),
                Some(x) if Rc::strong_count(x) == EMPTY_STRONG_CNT
            ) {
                this.rqinfo.remove(key);
            }
        }

        // Removes entity query info if it's not referenced from external anymore.
        // This function must be called inside `remove()`.
        fn remove_eqinfo<S>(this: &mut RequestInfoStorage<S>, key: &EntQueryKey)
        where
            S: BuildHasher,
        {
            // `self.eqinfo` = 1.
            const EMPTY_STRONG_CNT: usize = 1;

            if matches! (
                this.eqinfo.get(key),
                Some(x) if Rc::strong_count(x) == EMPTY_STRONG_CNT
            ) {
                this.eqinfo.remove(key);
            }
        }
    }
}

impl<S> Default for RequestInfoStorage<S>
where
    S: Default,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<S> StoreRequestInfo for RequestInfoStorage<S>
where
    S: BuildHasher,
{
    fn get(&self, key: &RequestKey) -> Option<Rc<RequestInfo>> {
        self.rinfo.get(key).map(Rc::clone)
    }

    fn insert(&mut self, key: RequestKey, info: &Rc<RequestInfo>) {
        self.rinfo.insert(key, Rc::clone(info));
    }

    // Top level cleaner.
    fn remove(&mut self, key: &RequestKey) {
        self.remove(key)
    }
}

impl<S> StoreQueryInfo for RequestInfoStorage<S>
where
    S: BuildHasher,
{
    fn get(&self, key: &QueryKey) -> Option<Rc<QueryInfo>> {
        self.qinfo.get(key).map(Rc::clone)
    }

    fn insert(&mut self, key: QueryKey, info: &Rc<QueryInfo>) {
        self.qinfo.insert(key, Rc::clone(info));
    }
}

impl<S> StoreResQueryInfo for RequestInfoStorage<S>
where
    S: BuildHasher,
{
    fn get(&self, key: &ResQueryKey) -> Option<Rc<ResQueryInfo>> {
        self.rqinfo.get(key).map(Rc::clone)
    }

    fn insert(&mut self, key: ResQueryKey, info: &Rc<ResQueryInfo>) {
        self.rqinfo.insert(key, Rc::clone(info));
    }
}

impl<S> StoreEntQueryInfo for RequestInfoStorage<S>
where
    S: BuildHasher,
{
    fn get(&self, key: &EntQueryKey) -> Option<Rc<EntQueryInfo>> {
        self.eqinfo.get(key).map(Rc::clone)
    }

    fn insert(&mut self, key: EntQueryKey, info: &Rc<EntQueryInfo>) {
        self.eqinfo.insert(key, Rc::clone(info));
    }
}

impl<S> StoreFilterInfo for RequestInfoStorage<S>
where
    S: BuildHasher,
{
    fn get(&self, key: &FilterKey) -> Option<Rc<FilterInfo>> {
        self.finfo.get(key).map(Rc::clone)
    }

    fn insert(&mut self, key: FilterKey, info: &Rc<FilterInfo>) {
        self.finfo.insert(key, Rc::clone(info));
    }
}
