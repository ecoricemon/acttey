use super::{
    filter::{FilterInfo, FilterKey, StoreFilterInfo},
    query::{
        EntQueryInfo, EntQueryKey, EntQueryMut, EntWrite, Query, QueryInfo, QueryKey, QueryMut,
        Read, ResQuery, ResQueryInfo, ResQueryKey, ResQueryMut, ResRead, ResWrite,
        StoreEntQueryInfo, StoreQueryInfo, StoreResQueryInfo, Write,
    },
    request::{Request, RequestBuffer, RequestInfo, RequestKey, Response, StoreRequestInfo},
};
use crate::ds::{
    list::{ListPos, SetList},
    vec::SimpleVecPool,
};
use std::{
    any::{self, TypeId},
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::BuildHasher,
    marker::PhantomData,
    num::NonZeroU32,
    ops::AddAssign,
    ptr::NonNull,
    sync::Arc,
};

#[derive(Debug)]
pub struct SystemManager<S> {
    /// [`SystemKey`] -> [`SystemId`].
    map: HashMap<SystemKey, Vec<SystemId>, S>,

    /// System id that will be given to new registered system.
    cur_id: SystemId,

    /// Currently activated systems.
    active: SetList<SystemId, SystemData, S>,

    /// When you register a new system, the system is stored here without activating it.
    /// To activate it, call [`Self::activate_system`].
    inactive: HashMap<SystemId, SystemData, S>,

    /// Volatile systems will be removed permanently instead of moving to inactive list.
    /// For instance, setup system and FnOnce system are volatile.
    volatile: HashSet<SystemId, S>,

    /// Active system's lifetime.
    lifetime: SystemLifetime<S>,

    /// System position to be run.
    cur_pos: ListPos,
}

impl<S> SystemManager<S>
where
    S: BuildHasher + Default,
{
    pub fn new<Stor>(info_stor: &mut Stor) -> Self
    where
        Stor: StoreRequestInfo,
    {
        // `SetList` requires default head node, just makes empty system and puts it in.
        let mut head_node = ().into_data(info_stor);
        head_node.id = SystemId::new(0);

        Self {
            map: HashMap::default(),
            cur_id: SystemId::new(1),
            active: SetList::new(head_node),
            inactive: HashMap::default(),
            volatile: HashSet::default(),
            lifetime: SystemLifetime::new(),
            cur_pos: ListPos::end(),
        }
    }
}

impl<S> SystemManager<S>
where
    S: BuildHasher,
{
    pub fn contains(&self, sid: &SystemId) -> bool {
        self.contains_active(sid) || self.contains_inactive(sid)
    }

    pub fn contains_active(&self, sid: &SystemId) -> bool {
        self.active.contains_key(sid)
    }

    pub fn contains_inactive(&self, sid: &SystemId) -> bool {
        self.inactive.contains_key(sid)
    }

    pub fn get_system_ids(&self, skey: &SystemKey) -> Option<&Vec<SystemId>> {
        self.map.get(skey)
    }

    /// Retrieves *active* system data corresponding to the given system key.
    /// Note that this method doen't look into inactive systems.
    pub fn get_system_data(&self, sid: &SystemId) -> Option<&SystemData> {
        self.active.get(sid)
    }

    /// Retrieves *active* system data corresponding to the given system key.
    /// Note that this method doen't look into inactive systems.
    pub fn get_system_data_mut(&mut self, sid: &SystemId) -> Option<&mut SystemData> {
        self.active.get_mut(sid)
    }

    /// Returns system id that will be assigned to the next registered system.
    pub fn next_system_id(&self) -> SystemId {
        self.cur_id
    }

    pub fn activate_system_num(&self) -> usize {
        self.active.len_occupied()
    }

    pub fn register_system(
        &mut self,
        skey: SystemKey,
        mut sdata: SystemData,
        volatile: bool,
    ) -> SystemId {
        self.map
            .entry(skey)
            .and_modify(|sids| sids.push(self.cur_id))
            .or_insert(vec![self.cur_id]);

        sdata.id = self.next_system_id();
        self.inactive.insert(self.cur_id, sdata);
        if volatile {
            let must_true = self.volatile.insert(self.cur_id);
            debug_assert!(must_true);
        }

        let sid = self.cur_id;
        self.cur_id += 1;
        sid
    }

    /// Unregisters system if and only if the system is inactive.
    pub fn unregister_system(&mut self, sid: &SystemId) -> Option<SystemData> {
        // Blocks to unregister active system.
        assert!(!self.contains_active(sid));

        self.inactive.remove(sid)
    }

    /// Returns true if the system was successfully activated.
    #[must_use]
    pub fn activate_system(
        &mut self,
        target: &SystemId,
        after: &SystemId,
        live: NonZeroU32,
    ) -> bool {
        self._active_system(target, live, Some(after), false)
    }

    pub fn activate_system_as_first(&mut self, target: &SystemId, live: NonZeroU32) -> bool {
        self._active_system(target, live, None, false)
    }

    pub fn activate_system_as_last(&mut self, target: &SystemId, live: NonZeroU32) -> bool {
        self._active_system(target, live, None, true)
    }

    /// * `after` - A system that will be followed by `target`.
    /// * `back` - This will be ignored if `after` is not None.
    /// If this is true, `target` will be inserted at the end of active list.
    /// Otherwise, it will be inserted at the front of the list.
    fn _active_system(
        &mut self,
        target: &SystemId,
        live: NonZeroU32,
        after: Option<&SystemId>,
        back: bool,
    ) -> bool {
        enum InsertPos {
            Middle,
            Back,
            Front,
        }

        // Determines insert position.
        let insert_pos = if let Some(after) = after {
            if !self.active.contains_key(after) {
                return false;
            }
            InsertPos::Middle
        } else if back {
            InsertPos::Back
        } else {
            InsertPos::Front
        };

        if let Some((target, sdata)) = self.inactive.remove_entry(target) {
            // Inserts system data into active list.
            let is_activated = match insert_pos {
                // Safety: If `Middle`, after must be Some.
                InsertPos::Middle => unsafe {
                    self.active.insert(target, sdata, after.unwrap_unchecked())
                },
                InsertPos::Back => self.active.push_back(target, sdata),
                InsertPos::Front => self.active.push_front(target, sdata),
            };
            // Inserts system id into lifetime manager.
            if is_activated {
                self.lifetime.register(target, live);
                return true;
            }
        }
        false
    }

    pub(crate) fn tick(&mut self) {
        if let Some(expired_sids) = self.lifetime.tick() {
            while let Some(sid) = expired_sids.pop() {
                let sdata = self.active.remove(&sid).unwrap();
                if !self.volatile.remove(&sdata.id) {
                    self.inactive.insert(sid, sdata);
                }
            }
        }
    }

    /// Resets both system position and run history.
    pub(crate) fn iter_begin(&mut self) {
        self.cur_pos = self.active.get_first_position();
    }

    /// Returns system to be run this time.
    pub(crate) fn iter_value_at_current(&mut self) -> Option<&mut SystemData> {
        // Safety: We control the position in this structure.
        unsafe { self.iter_value_at(self.cur_pos) }
    }

    /// Returns system at the given position.
    ///
    /// # Safety
    ///
    /// Undefined behavior if the given position is invalid.
    pub(crate) unsafe fn iter_value_at(&mut self, pos: ListPos) -> Option<&mut SystemData> {
        self.active
            .get_next_unchecked_mut(pos)
            .map(|(_, sdata)| sdata)
    }

    /// Moves system position on to the next.
    /// In addition to it, records the current system onto the run history.
    pub(crate) fn iter_next(&mut self) {
        if let Some((next, _sdata)) = unsafe { self.active.get_next_unchecked_mut(self.cur_pos) } {
            self.cur_pos = next;
        }
    }

    /// Returns current system position.
    pub(crate) fn iter_current(&self) -> ListPos {
        self.cur_pos
    }

    /// Retrieves the next system data.
    pub(crate) fn peek_next(&self, sid: &SystemId) -> Option<&SystemData> {
        self.active.get_next(sid)
    }
}

#[derive(Debug)]
struct SystemLifetime<S> {
    /// Monotonically increasing counter.
    tick: Tick,

    /// [`Tick`] -> [`Self::pool`] index.
    lives: HashMap<Tick, usize, S>,

    /// Vector contains [`SystemKey`]s to be dead at a specific tick.
    pool: SimpleVecPool<SystemId>,
}

impl<S> SystemLifetime<S>
where
    S: Default,
{
    fn new() -> Self {
        Self {
            tick: 0,
            lives: HashMap::default(),
            pool: SimpleVecPool::new(),
        }
    }
}

impl<S> SystemLifetime<S>
where
    S: BuildHasher,
{
    fn register(&mut self, sid: SystemId, live: NonZeroU32) {
        let end = self.tick.saturating_add(live.get());
        let index = if let Some(index) = self.lives.get(&end) {
            *index
        } else {
            let index = self.pool.request();
            self.lives.insert(end, index);
            index
        };
        let vec = self.pool.get(index);
        vec.push(sid);
    }

    fn tick(&mut self) -> Option<&mut Vec<SystemId>> {
        self.tick += 1;
        self.lives
            .remove(&self.tick)
            .map(|index| self.pool.get_release(index))
    }
}

pub trait System: 'static + Sized {
    type Req: Request;

    fn run(&mut self, resp: Response<Self::Req>);

    fn key() -> SystemKey {
        SystemKey::new(TypeId::of::<Self>())
    }

    fn into_data<S>(self, info_stor: &mut S) -> SystemData
    where
        S: StoreRequestInfo,
    {
        SystemData {
            id: SystemId::dummy(), // id will be overwritten.
            name: any::type_name::<Self>(),
            invoke: Box::new(self),
            req: Box::new((
                <Self::Req as Request>::key(),
                <Self::Req as Request>::get_info(info_stor),
            )),
        }
    }
}

/// [`TypeId`] for [`System`].
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
#[repr(transparent)]
pub struct SystemKey(TypeId);

impl SystemKey {
    pub const fn new(ty: TypeId) -> Self {
        Self(ty)
    }

    pub const fn as_type(&self) -> &TypeId {
        &self.0
    }
}

/// Unique system identification determined by [`SystemManager`].
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
#[repr(transparent)]
pub struct SystemId(u32);

impl SystemId {
    pub const fn dummy() -> Self {
        Self(u32::MAX)
    }

    pub const fn is_dummy(&self) -> bool {
        self.0 == u32::MAX
    }

    pub const fn new(id: u32) -> Self {
        Self(id)
    }

    pub const fn into_u32(self) -> u32 {
        self.0
    }
}

impl AddAssign<u32> for SystemId {
    fn add_assign(&mut self, rhs: u32) {
        self.0 += rhs;
    }
}

impl Display for SystemId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct SystemData {
    /// The other identification for the systems.
    pub(crate) id: SystemId,

    /// System name determined by [`std::any::type_name`].
    name: &'static str,

    /// Entry point to the [`System::run`].
    invoke: Box<dyn Invoke>,

    /// A [`System`] has a deterministic number of [`RequestInfo`]s (just one for now).
    /// But each of them is quite heavy.
    /// Moreover, we need to move the whole [`SystemData`] during scheduling according to its idle/active state.
    /// To reduce the burden caused by move, we had better put this infrequent access data on the heap, not on the stack.
    req: Box<(RequestKey, Arc<RequestInfo>)>,
}

impl Debug for SystemData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SystemData")
            .field("id", &self.id)
            .field("name", &self.name)
            .field("invoke", &"Box<dyn Invoke>")
            .field("req", &self.req)
            .finish()
    }
}

impl SystemData {
    pub fn id(&self) -> SystemId {
        self.id
    }

    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn invokable(&self) -> &dyn Invoke {
        self.invoke.as_ref()
    }

    pub fn invokable_mut(&mut self) -> &mut dyn Invoke {
        self.invoke.as_mut()
    }

    pub fn req(&self) -> &(RequestKey, Arc<RequestInfo>) {
        &self.req
    }

    pub fn task_ptr(&mut self) -> NonNull<dyn Invoke> {
        let ptr = self.invoke.as_mut() as *mut _;
        // Safety: Infallible.
        unsafe { NonNull::new_unchecked(ptr) }
    }
}

/// Empty system.
impl System for () {
    type Req = ();
    fn run(&mut self, _resp: Response<Self::Req>) {}
}

/// Object safe trait for the [`System`].
pub trait Invoke {
    fn invoke(&mut self, buf: &mut RequestBuffer);
    fn skey(&self) -> SystemKey;
    fn rkey(&self) -> RequestKey;
}

impl<S: System> Invoke for S {
    fn invoke(&mut self, buf: &mut RequestBuffer) {
        let resp = Response::new(buf);
        self.run(resp);
    }

    fn skey(&self) -> SystemKey {
        S::key()
    }

    fn rkey(&self) -> RequestKey {
        <S as System>::Req::key()
    }
}

#[derive(Debug)]
pub struct SharedInfo<S> {
    /// Shared [`RequestKey`] -> [`RequestInfo`] map.
    rinfo: HashMap<RequestKey, Arc<RequestInfo>, S>,

    /// Shared [`QueryKey`] -> [`QueryInfo`] map.
    qinfo: HashMap<QueryKey, Arc<QueryInfo>, S>,

    /// Shared [`ResQueryKey`] -> [`ResQueryInfo`] map.
    rqinfo: HashMap<ResQueryKey, Arc<ResQueryInfo>, S>,

    /// Shared [`EntQueryKey`] -> [`EntQueryInfo`] map.
    eqinfo: HashMap<EntQueryKey, Arc<EntQueryInfo>, S>,

    /// Shared [`FilterKey`] -> [`FilterInfo`] map.
    finfo: HashMap<FilterKey, Arc<FilterInfo>, S>,
}

impl<S> SharedInfo<S>
where
    S: Default,
{
    pub fn new() -> Self {
        Self {
            rinfo: HashMap::default(),
            qinfo: HashMap::default(),
            rqinfo: HashMap::default(),
            eqinfo: HashMap::default(),
            finfo: HashMap::default(),
        }
    }
}

impl<S> SharedInfo<S>
where
    S: BuildHasher,
{
    pub fn get_request_info(&self, key: &RequestKey) -> Option<&Arc<RequestInfo>> {
        StoreRequestInfo::get(self, key)
    }

    pub fn get_query_info(&self, key: &QueryKey) -> Option<&Arc<QueryInfo>> {
        StoreQueryInfo::get(self, key)
    }

    pub fn get_resource_query_info(&self, key: &ResQueryKey) -> Option<&Arc<ResQueryInfo>> {
        StoreResQueryInfo::get(self, key)
    }

    pub fn get_filter_info(&self, key: &FilterKey) -> Option<&Arc<FilterInfo>> {
        StoreFilterInfo::get(self, key)
    }
}

impl<S> Default for SharedInfo<S>
where
    S: Default,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<S> StoreRequestInfo for SharedInfo<S>
where
    S: BuildHasher,
{
    fn get(&self, key: &RequestKey) -> Option<&Arc<RequestInfo>> {
        self.rinfo.get(key)
    }

    fn insert(&mut self, key: RequestKey, info: Arc<RequestInfo>) {
        self.rinfo.insert(key, info);
    }

    fn remove(&mut self, key: &RequestKey) -> Option<Arc<RequestInfo>> {
        self.rinfo.remove(key)
    }
}

impl<S> StoreQueryInfo for SharedInfo<S>
where
    S: BuildHasher,
{
    fn get(&self, key: &QueryKey) -> Option<&Arc<QueryInfo>> {
        self.qinfo.get(key)
    }

    fn insert(&mut self, key: QueryKey, info: Arc<QueryInfo>) {
        self.qinfo.insert(key, info);
    }

    fn remove(&mut self, key: &QueryKey) -> Option<Arc<QueryInfo>> {
        self.qinfo.remove(key)
    }
}

impl<S> StoreResQueryInfo for SharedInfo<S>
where
    S: BuildHasher,
{
    fn get(&self, key: &ResQueryKey) -> Option<&Arc<ResQueryInfo>> {
        self.rqinfo.get(key)
    }

    fn insert(&mut self, key: ResQueryKey, info: Arc<ResQueryInfo>) {
        self.rqinfo.insert(key, info);
    }

    fn remove(&mut self, key: &ResQueryKey) -> Option<Arc<ResQueryInfo>> {
        self.rqinfo.remove(key)
    }
}

impl<S> StoreEntQueryInfo for SharedInfo<S>
where
    S: BuildHasher,
{
    fn get(&self, key: &EntQueryKey) -> Option<&Arc<EntQueryInfo>> {
        self.eqinfo.get(key)
    }

    fn insert(&mut self, key: EntQueryKey, info: Arc<EntQueryInfo>) {
        self.eqinfo.insert(key, info);
    }

    fn remove(&mut self, key: &EntQueryKey) -> Option<Arc<EntQueryInfo>> {
        self.eqinfo.remove(key)
    }
}

impl<S> StoreFilterInfo for SharedInfo<S>
where
    S: BuildHasher,
{
    fn get(&self, key: &FilterKey) -> Option<&Arc<FilterInfo>> {
        self.finfo.get(key)
    }

    fn insert(&mut self, key: FilterKey, info: Arc<FilterInfo>) {
        self.finfo.insert(key, info);
    }

    fn remove(&mut self, key: &FilterKey) -> Option<Arc<FilterInfo>> {
        self.finfo.remove(key)
    }
}

/// This structure helps a function implements [`System`].
/// Because we can't use blanket impl for the `System`, due to confliction to other impls,
/// We put this helper between function and `System`.
/// That means a function can become `FnSystem` which implements `System`.
pub struct FnSystem<Req, F> {
    run: F,
    _marker: PhantomData<Req>,
}

pub struct FnOnceSystem<Req, F> {
    pub run: F,
    pub _marker: PhantomData<Req>,
}

impl<Req: Debug, F> Debug for FnOnceSystem<Req, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FnOnceSystem")
            .field("_marker", &self._marker)
            .finish_non_exhaustive()
    }
}

/// Placeholder for the [`FnSystem`]'s generic parameter.
/// This helps us avoid impl confliction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PlaceHolder;

#[rustfmt::skip]
mod impl_for_fn_system {
    use super::*;

    macro_rules! _impl {
        (
            $req_with_placeholder:ty,
            $req_with_tuple:ty
            $(,r=$r:ident)?
            $(,w=$w:ident)?
            $(,rr=$rr:ident)?
            $(,rw=$rw:ident)?
            $(,ew=$ew:ident)?
        ) => {
            impl<F $(,$r)? $(,$w)? $(,$rr)? $(,$rw)? $(,$ew)?> From<F>
                for FnSystem<$req_with_placeholder, F>
            where
                F: FnMut(
                    $(Read<$r>,)?
                    $(Write<$w>,)?
                    $(ResRead<$rr>,)?
                    $(ResWrite<$rw>,)?
                    $(EntWrite<$ew>,)?
                ),
                $($r: Query,)?
                $($w: QueryMut,)?
                $($rr: ResQuery,)?
                $($rw: ResQueryMut,)?
                $($ew: EntQueryMut,)?
            {
                fn from(value: F) -> Self {
                    Self {
                        run: value,
                        _marker: PhantomData,
                    }
                }
            }

            impl<F $(,$r)? $(,$w)? $(,$rr)? $(,$rw)? $(,$ew)?> From<F>
                for StructOrFnSystem<(), $req_with_placeholder, F>
            where
                F: FnMut(
                    $(Read<$r>,)?
                    $(Write<$w>,)?
                    $(ResRead<$rr>,)?
                    $(ResWrite<$rw>,)?
                    $(EntWrite<$ew>,)?
                ),
                $($r: Query,)?
                $($w: QueryMut,)?
                $($rr: ResQuery,)?
                $($rw: ResQueryMut,)?
                $($ew: EntQueryMut,)?
            {
                fn from(value: F) -> Self {
                    Self::Fn(value.into())
                }
            }

            impl<F $(,$r)? $(,$w)? $(,$rr)? $(,$rw)? $(,$ew)?> System
                for FnSystem<$req_with_placeholder, F>
            where
                F: FnMut(
                    $(Read<$r>,)?
                    $(Write<$w>,)?
                    $(ResRead<$rr>,)?
                    $(ResWrite<$rw>,)?
                    $(EntWrite<$ew>,)?
                ) + 'static,
                $($r: Query,)?
                $($w: QueryMut,)?
                $($rr: ResQuery,)?
                $($rw: ResQueryMut,)?
                $($ew: EntQueryMut,)?
            {
                type Req = $req_with_tuple;

                fn run(&mut self, _resp: Response<Self::Req>) {
                    (self.run)(
                        $(Read::<$r>(_resp.read),)?
                        $(Write::<$w>(_resp.write),)?
                        $(ResRead::<$rr>(_resp.res_read),)?
                        $(ResWrite::<$rw>(_resp.res_write),)?
                        $(EntWrite::<$ew>(_resp.ent_write),)?
                    )
                }
            }

            impl<F $(,$r)? $(,$w)? $(,$rr)? $(,$rw)? $(,$ew)?> 
                AsFnSystemKey<$req_with_placeholder> for F
            where
                F: FnMut(
                    $(Read<$r>,)?
                    $(Write<$w>,)?
                    $(ResRead<$rr>,)?
                    $(ResWrite<$rw>,)?
                    $(EntWrite<$ew>,)?
                ) + 'static,
                $($r: Query,)?
                $($w: QueryMut,)?
                $($rr: ResQuery,)?
                $($rw: ResQueryMut,)?
                $($ew: EntQueryMut,)?
            {
                fn key(self) -> SystemKey {
                    let sys: FnSystem<_, F> = self.into();
                    sys.skey()
                }
            }

            // Impl conversion of `FnOnce` -> `FnOnceSystem`.
            // See `EcsManager::register_once_system()`.
            impl<F $(,$r)? $(,$w)? $(,$rr)? $(,$rw)? $(,$ew)?> From<F>
                for FnOnceSystem<$req_with_placeholder, F>
            where
                F: FnOnce(
                    $(Read<$r>,)?
                    $(Write<$w>,)?
                    $(ResRead<$rr>,)?
                    $(ResWrite<$rw>,)?
                    $(EntWrite<$ew>,)?
                ),
                $($r: Query,)?
                $($w: QueryMut,)?
                $($rr: ResQuery,)?
                $($rw: ResQueryMut,)?
                $($ew: EntQueryMut,)?
            {
                fn from(value: F) -> Self {
                    Self {
                        run: value,
                        _marker: PhantomData,
                    }
                }
            }

            // Impl conversion of `FnOnceSystem` -> `Box<dyn FnMut(Args)>`.
            // See `EcsManager::register_once_system()`.
            impl<F $(,$r)? $(,$w)? $(,$rr)? $(,$rw)? $(,$ew)?>
                From<FnOnceSystem<$req_with_placeholder, F>> for Box<dyn FnMut((
                    $(Read<$r>,)?
                    $(Write<$w>,)?
                    $(ResRead<$rr>,)?
                    $(ResWrite<$rw>,)?
                    $(EntWrite<$ew>,)?
                ))>
            where
                F: FnOnce(
                    $(Read<$r>,)?
                    $(Write<$w>,)?
                    $(ResRead<$rr>,)?
                    $(ResWrite<$rw>,)?
                    $(EntWrite<$ew>,)?
                ) + 'static,
                $($r: Query,)?
                $($w: QueryMut,)?
                $($rr: ResQuery,)?
                $($rw: ResQueryMut,)?
                $($ew: EntQueryMut,)?
            {
                fn from(value: FnOnceSystem<$req_with_placeholder, F>) -> Self {
                    let mut f = Some(value.run);
                    Box::new(move |args: (
                        $(Read<$r>,)?
                        $(Write<$w>,)?
                        $(ResRead<$rr>,)?
                        $(ResWrite<$rw>,)?
                        $(EntWrite<$ew>,)?
                    )| {
                        #[allow(non_snake_case)]
                        if let Some(f) = f.take() {
                            let ($($r,)? $($w,)? $($rr,)? $($rw,)? $($ew,)?) = args;
                            f($($r,)? $($w,)? $($rr,)? $($rw,)? $($ew,)?)
                        } else {
                            panic!("unable to call {} twice because it implements FnOnce only", std::any::type_name::<F>());
                        }
                    })
                }
            }

            // Impl `System` for `Box<dyn FnMut(Args)>`.
            // See EcsManager::register_once_system().
            impl<$($r,)? $($w,)? $($rr,)? $($rw,)? $($ew,)?> System for 
            Box<dyn FnMut((
                $(Read<$r>,)?
                $(Write<$w>,)?
                $(ResRead<$rr>,)?
                $(ResWrite<$rw>,)?
                $(EntWrite<$ew>,)?
            ))>
            where
                $($r: Query,)?
                $($w: QueryMut,)?
                $($rr: ResQuery,)?
                $($rw: ResQueryMut,)?
                $($ew: EntQueryMut,)?
            {
                type Req = $req_with_tuple;

                fn run(&mut self, _resp: Response<Self::Req>) {
                    (self)((
                        $(Read::<$r>(_resp.read),)?
                        $(Write::<$w>(_resp.write),)?
                        $(ResRead::<$rr>(_resp.res_read),)?
                        $(ResWrite::<$rw>(_resp.res_write),)?
                        $(EntWrite::<$ew>(_resp.ent_write),)?
                    ))
                }
            }
        };
    }

    #[allow(non_camel_case_types)]
    type o = PlaceHolder; // easy to see where it is.

    _impl!((o , o , o , o , o ), ((), (), (), (), ()));
    _impl!((R,  o , o , o , o ), (R,  (), (), (), ()), r=R);
    _impl!((o , W,  o , o , o ), ((), W,  (), (), ()), w=W);
    _impl!((R,  W,  o , o , o ), (R,  W,  (), (), ()), r=R, w=W);
    _impl!((o , o , RR, o , o ), ((), (), RR, (), ()), rr=RR);
    _impl!((R,  o , RR, o , o ), (R,  (), RR, (), ()), r=R, rr=RR);
    _impl!((o , W,  RR, o , o ), ((), W,  RR, (), ()), w=W, rr=RR);
    _impl!((R,  W,  RR, o , o ), (R,  W,  RR, (), ()), r=R, w=W, rr=RR);
    _impl!((o , o , o , RW, o ), ((), (), (), RW, ()), rw=RW);
    _impl!((R,  o , o , RW, o ), (R,  (), (), RW, ()), r=R, rw=RW);
    _impl!((o , W,  o , RW, o ), ((), W,  (), RW, ()), w=W, rw=RW);
    _impl!((R,  W,  o , RW, o ), (R,  W,  (), RW, ()), r=R, w=W, rw=RW);
    _impl!((o , o , RR, RW, o ), ((), (), RR, RW, ()), rr=RR, rw=RW);
    _impl!((R,  o , RR, RW, o ), (R,  (), RR, RW, ()), r=R, rr=RR, rw=RW);
    _impl!((o , W,  RR, RW, o ), ((), W,  RR, RW, ()), w=W, rr=RR, rw=RW);
    _impl!((R,  W,  RR, RW, o ), (R,  W,  RR, RW, ()), r=R, w=W, rr=RR, rw=RW);

    _impl!((o , o , o , o , EW), ((), (), (), (), EW), ew=EW);
    _impl!((R,  o , o , o , EW), (R,  (), (), (), EW), r=R, ew=EW);
    _impl!((o , W,  o , o , EW), ((), W,  (), (), EW), w=W, ew=EW);
    _impl!((R,  W,  o , o , EW), (R,  W,  (), (), EW), r=R, w=W, ew=EW);
    _impl!((o , o , RR, o , EW), ((), (), RR, (), EW), rr=RR, ew=EW);
    _impl!((R,  o , RR, o , EW), (R,  (), RR, (), EW), r=R, rr=RR, ew=EW);
    _impl!((o , W,  RR, o , EW), ((), W,  RR, (), EW), w=W, rr=RR, ew=EW);
    _impl!((R,  W,  RR, o , EW), (R,  W,  RR, (), EW), r=R, w=W, rr=RR, ew=EW);
    _impl!((o , o , o , RW, EW), ((), (), (), RW, EW), rw=RW, ew=EW);
    _impl!((R,  o , o , RW, EW), (R,  (), (), RW, EW), r=R, rw=RW, ew=EW);
    _impl!((o , W,  o , RW, EW), ((), W,  (), RW, EW), w=W, rw=RW, ew=EW);
    _impl!((R,  W,  o , RW, EW), (R,  W,  (), RW, EW), r=R, w=W, rw=RW, ew=EW);
    _impl!((o , o , RR, RW, EW), ((), (), RR, RW, EW), rr=RR, rw=RW, ew=EW);
    _impl!((R,  o , RR, RW, EW), (R,  (), RR, RW, EW), r=R, rr=RR, rw=RW, ew=EW);
    _impl!((o , W,  RR, RW, EW), ((), W,  RR, RW, EW), w=W, rr=RR, rw=RW, ew=EW);
    _impl!((R,  W,  RR, RW, EW), (R,  W,  RR, RW, EW), r=R, w=W, rr=RR, rw=RW, ew=EW);
}

/// Dummy implementation of the [`System`].
/// This is used for [`StructOrFnSystem`] because it needs default generics for [`FnSystem`].
impl System for FnSystem<(), fn()> {
    type Req = ();
    fn run(&mut self, _resp: Response<Self::Req>) {
        unreachable!();
    }
}

pub enum StructOrFnSystem<S, Req, F> {
    Struct(S),
    Fn(FnSystem<Req, F>),
}

impl<S: System> From<S> for StructOrFnSystem<S, (), fn()> {
    fn from(value: S) -> Self {
        Self::Struct(value)
    }
}

impl<Req, F> From<FnSystem<Req, F>> for StructOrFnSystem<(), Req, F> {
    fn from(value: FnSystem<Req, F>) -> Self {
        Self::Fn(value)
    }
}

/// Functions that implements [`FnSystem`] can generate [`SystemKey`] from it.
/// However, that requires call to [`From::from`] to become `FnSystem` from a function.
/// This trait makes you avoid to write boilerplate code like above.
/// You can get a `SystemKey` from a function directly using this trait.
pub trait AsFnSystemKey<M> {
    fn key(self) -> SystemKey;
}

/// Monotonically increasing counter.
/// When the scheduling occurs, this counter increases by 1.
/// For exampple, if actual fps is 60, then `Tick` will increase by 60 in a sec.
pub type Tick = u32;
pub type NonZeroTick = NonZeroU32;
