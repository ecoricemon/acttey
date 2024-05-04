use super::{
    filter::{FilterInfo, FilterKey, StoreFilterInfo},
    query::{
        Query, QueryInfo, QueryKey, QueryMut, Read, ResQuery, ResQueryMut, ResRead, ResWrite,
        StoreQueryInfo, Write,
    },
    request::{Request, RequestBuffer, RequestInfo, RequestKey, Response, StoreRequestInfo},
};
use crate::{
    ds::{
        set_list::{ListPos, SetList},
        vec::vec_pool::SimpleVecPool,
    },
    worker::{msg::ChMsg, Channel, WorkerId},
};
use std::{
    any::{self, TypeId},
    collections::HashMap,
    fmt::{Debug, Display},
    marker::PhantomData,
    num::NonZeroU32,
    ops::AddAssign,
    ptr::NonNull,
    sync::{
        mpsc::{RecvError, SendError},
        Arc,
    },
};

#[derive(Debug)]
pub struct SystemPack {
    /// [`SystemKey`] -> [`SystemId`].
    map: HashMap<SystemKey, SystemId, ahash::RandomState>,

    /// System id that will be given to new registered system.
    cur_id: SystemId,

    /// When you register a new system, the system is stored here without activating it.
    /// To activate it, call [`Self::activate_system`].
    inactive: HashMap<SystemKey, SystemData, ahash::RandomState>,

    /// Currently activated systems.
    active: SetList<SystemKey, SystemData>,

    /// Active system's lifetime.
    lifetime: SystemLifetime,

    /// A system position to be run next time, which is an index of [`Self::active`].
    run_pos: usize,
}

impl SystemPack {
    pub fn new<S>(info_stor: &mut S) -> Self
    where
        S: StoreRequestInfo,
    {
        // `SetList` requires default head node, just makes empty system and puts it in.
        let mut head_node = ().into_data(info_stor);
        head_node.id = SystemId::new(0);

        Self {
            map: HashMap::default(),
            cur_id: SystemId::new(1),
            inactive: HashMap::default(),
            active: SetList::new(head_node),
            lifetime: SystemLifetime::new(),
            run_pos: 0,
        }
    }

    #[inline]
    pub fn contains(&self, skey: &SystemKey) -> bool {
        self.contains_active(skey) || self.contains_inactive(skey)
    }

    #[inline]
    pub fn contains_active(&self, skey: &SystemKey) -> bool {
        self.active.contains_key(skey)
    }

    #[inline]
    pub fn contains_inactive(&self, skey: &SystemKey) -> bool {
        self.inactive.contains_key(skey)
    }

    /// Retrieves *active* system data corresponding to the given system key.
    /// Note that this method doen't look into inactive systems.
    #[inline]
    pub fn get_system_data(&self, skey: &SystemKey) -> Option<&SystemData> {
        self.active.get(skey)
    }

    /// Retrieves *active* system data corresponding to the given system key.
    /// Note that this method doen't look into inactive systems.
    #[inline]
    pub fn get_system_data_mut(&mut self, skey: &SystemKey) -> Option<&mut SystemData> {
        self.active.get_mut(skey)
    }

    /// If the system is not registerd, sets its [`SystemId`] and registers it.
    /// Then, returns the system id.
    /// If you want to replace it, unregister the old one first.
    pub fn register_system(&mut self, skey: SystemKey, mut sdata: SystemData) -> Option<SystemId> {
        if self.contains(&skey) {
            None
        } else {
            let res = Some(self.cur_id);
            sdata.id = self.cur_id;
            self.cur_id += 1;
            self.inactive.insert(skey, sdata);
            res
        }
    }

    /// Unregisters system if and only if the system is inactive.
    pub fn unregister_system(&mut self, skey: &SystemKey) -> Option<SystemData> {
        // Restricts operations during running for now.
        // Do we need a deferred operation?
        assert_eq!(0, self.run_pos);

        self.inactive.remove(skey)
    }

    /// Returns true if the system was successfully activated.
    pub fn activate_system(
        &mut self,
        target: &SystemKey,
        after: &SystemKey,
        live: NonZeroU32,
    ) -> bool {
        // Restricts operations during running for now.
        // Do we need a deferred operation?
        assert_eq!(0, self.run_pos);

        if self.active.contains_key(after) {
            if let Some((target, sdata)) = self.inactive.remove_entry(target) {
                if self.active.insert(target, sdata, after) {
                    self.lifetime.register(target, live);
                    return true;
                }
            }
        }
        false
    }

    pub fn activate_system_as_first(&mut self, target: &SystemKey, live: NonZeroU32) -> bool {
        // Restricts operations during running for now.
        // Do we need a deferred operation?
        assert_eq!(0, self.run_pos);

        if let Some((target, sdata)) = self.inactive.remove_entry(target) {
            if self.active.push_front(target, sdata) {
                self.lifetime.register(target, live);
                return true;
            }
        }
        false
    }

    pub fn activate_system_as_last(&mut self, target: &SystemKey, live: NonZeroU32) -> bool {
        // Restricts operations during running for now.
        // Do we need a deferred operation?
        assert_eq!(0, self.run_pos);

        if let Some((target, sdata)) = self.inactive.remove_entry(target) {
            if self.active.push_back(target, sdata) {
                self.lifetime.register(target, live);
                return true;
            }
        }
        false
    }

    pub(crate) fn tick(&mut self) {
        if let Some(dead_systems) = self.lifetime.tick() {
            while let Some(skey) = dead_systems.pop() {
                let sdata = self.active.remove(&skey).unwrap();
                self.inactive.insert(skey, sdata);
            }
        }
    }

    #[inline]
    pub(crate) unsafe fn iter_next(&self, cur: ListPos) -> Option<(ListPos, &SystemData)> {
        self.active.get_next_unchecked(cur)
    }

    #[inline]
    pub(crate) unsafe fn iter_next_mut(
        &mut self,
        cur: ListPos,
    ) -> Option<(ListPos, &mut SystemData)> {
        self.active.get_next_unchecked_mut(cur)
    }

    #[inline]
    pub(crate) fn iter_begin(&self) -> ListPos {
        self.active.get_first_position()
    }
}

#[derive(Debug)]
struct SystemLifetime {
    /// Monotonically increasing counter.
    tick: u32,

    /// `tick` -> `pool` index.
    lives: HashMap<u32, usize, ahash::RandomState>,

    /// Vector contains [`SystemKey`]s to be dead at a specific tick.
    pool: SimpleVecPool<SystemKey>,
}

impl SystemLifetime {
    fn new() -> Self {
        Self {
            tick: 0,
            lives: HashMap::default(),
            pool: SimpleVecPool::new(),
        }
    }

    fn register(&mut self, skey: SystemKey, live: NonZeroU32) {
        let end = self.tick.saturating_add(live.get());
        let index = if let Some(index) = self.lives.get(&end) {
            *index
        } else {
            let index = self.pool.request();
            self.lives.insert(end, index);
            index
        };
        let vec = self.pool.get(index);
        vec.push(skey);
    }

    fn tick(&mut self) -> Option<&mut Vec<SystemKey>> {
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
            invokable: Box::new(self),
            req: Box::new((
                <Self::Req as Request>::key(),
                <Self::Req as Request>::info(info_stor),
            )),
        }
    }
}

/// [`TypeId`] for [`System`].
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub struct SystemKey(TypeId);

impl SystemKey {
    pub const fn new(ty: TypeId) -> Self {
        Self(ty)
    }

    pub const fn as_type(&self) -> &TypeId {
        &self.0
    }
}

/// Unique system identification.
/// This is determined by [`SystemPack`](super::schedule::SystemPack).
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
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
    #[inline]
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
    invokable: Box<dyn Invokable>,

    /// A [`System`] has a deterministic number of [`RequestInfo`]s (just one for now).
    /// But each of them is quite heavy.
    /// Moreover, we need to move the whole [`SystemData`] during scheduling according to its idle/active state.
    /// To reduce the burden caused by move, we had better put this infrequent access data on the heap, not on the stack.
    req: Box<(RequestKey, Arc<RequestInfo>)>,
}

impl SystemData {
    #[inline]
    pub fn id(&self) -> SystemId {
        self.id
    }

    #[inline]
    pub fn name(&self) -> &'static str {
        self.name
    }

    #[inline]
    pub fn invokable(&self) -> &dyn Invokable {
        self.invokable.as_ref()
    }

    #[inline]
    pub fn invokable_mut(&mut self) -> &mut dyn Invokable {
        self.invokable.as_mut()
    }

    #[inline]
    pub fn req(&self) -> &(RequestKey, Arc<RequestInfo>) {
        &self.req
    }

    #[inline]
    pub fn task_ptr(&mut self) -> NonNull<dyn Invokable> {
        let ptr = self.invokable.as_mut() as *mut _;
        // Safety: Infallible.
        unsafe { NonNull::new_unchecked(ptr) }
    }
}

impl Debug for SystemData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SystemData")
            .field("id", &self.id)
            .field("name", &self.name)
            .field("invokable", &"Box<dyn Invokable>")
            .field("req", &self.req)
            .finish()
    }
}

/// Empty system.
impl System for () {
    type Req = ();
    fn run(&mut self, _resp: Response<Self::Req>) {}
}

/// Object safe trait for the [`System`].
pub trait Invokable {
    fn invoke(&mut self, buf: &mut RequestBuffer);
    fn skey(&self) -> SystemKey;
    fn rkey(&self) -> RequestKey;
}

impl<S: System> Invokable for S {
    #[inline]
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

/// Client accessing main worker from sub worker context in order to retrieve data for requests.
pub struct Client<'a> {
    ch: &'a Channel,
    entry: fn(&mut Client),
    wid: WorkerId,
}

impl<'a> Client<'a> {
    pub const fn new(ch: &'a Channel, entry: fn(&mut Client), wid: WorkerId) -> Self {
        Self { ch, entry, wid }
    }

    #[inline]
    pub(crate) fn wid(&self) -> WorkerId {
        self.wid
    }

    #[inline]
    pub(crate) fn send(&self, msg: ChMsg) -> Result<(), SendError<ChMsg>> {
        let res = self.ch.send(msg);
        self.ch.unpark_opposite();
        res
    }

    #[inline]
    pub(crate) fn recv(&self) -> Result<ChMsg, RecvError> {
        self.ch.recv()
    }
}

#[derive(Debug)]
pub struct SharedInfo {
    rinfo: HashMap<RequestKey, Arc<RequestInfo>, ahash::RandomState>,
    qinfo: HashMap<QueryKey, Arc<QueryInfo>, ahash::RandomState>,
    finfo: HashMap<FilterKey, Arc<FilterInfo>, ahash::RandomState>,
}

impl SharedInfo {
    pub fn new() -> Self {
        Self {
            rinfo: HashMap::default(),
            qinfo: HashMap::default(),
            finfo: HashMap::default(),
        }
    }

    #[inline]
    pub fn get_request_info(&self, key: &RequestKey) -> Option<&Arc<RequestInfo>> {
        StoreRequestInfo::get(self, key)
    }

    #[inline]
    pub fn get_query_info(&self, key: &QueryKey) -> Option<&Arc<QueryInfo>> {
        StoreQueryInfo::get(self, key)
    }

    #[inline]
    pub fn get_filter_info(&self, key: &FilterKey) -> Option<&Arc<FilterInfo>> {
        StoreFilterInfo::get(self, key)
    }
}

impl Default for SharedInfo {
    fn default() -> Self {
        Self::new()
    }
}

impl StoreRequestInfo for SharedInfo {
    #[inline]
    fn get(&self, key: &RequestKey) -> Option<&Arc<RequestInfo>> {
        self.rinfo.get(key)
    }

    #[inline]
    fn insert(&mut self, key: RequestKey, info: Arc<RequestInfo>) {
        self.rinfo.insert(key, info);
    }

    fn remove(&mut self, key: &RequestKey) -> Option<Arc<RequestInfo>> {
        self.rinfo.remove(key)
    }
}

impl StoreQueryInfo for SharedInfo {
    #[inline]
    fn get(&self, key: &QueryKey) -> Option<&Arc<QueryInfo>> {
        self.qinfo.get(key)
    }

    #[inline]
    fn insert(&mut self, key: QueryKey, info: Arc<QueryInfo>) {
        self.qinfo.insert(key, info);
    }

    #[inline]
    fn remove(&mut self, key: &QueryKey) -> Option<Arc<QueryInfo>> {
        self.qinfo.remove(key)
    }
}

impl StoreFilterInfo for SharedInfo {
    #[inline]
    fn get(&self, key: &FilterKey) -> Option<&Arc<FilterInfo>> {
        self.finfo.get(key)
    }

    #[inline]
    fn insert(&mut self, key: FilterKey, info: Arc<FilterInfo>) {
        self.finfo.insert(key, info);
    }

    #[inline]
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

/// Placeholder for the [`FnSystem`]'s generic parameter.
/// This helps us avoid impl confliction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ph;

#[rustfmt::skip]
mod impl_for_fn_system {
    use super::*;

    macro_rules! _impl {
        (
            $req_with_placeholder:ty,
            $req_with_tuple:ty
            $(, r=$r:ident)?
            $(, w=$w:ident)?
            $(, rr=$rr:ident)?
            $(, rw=$rw:ident)?
        ) => {
            impl<F $(, $r)? $(, $w)? $(, $rr)? $(, $rw)?> From<F>
                for FnSystem<$req_with_placeholder, F>
            where
                F: FnMut(
                    $(Read<$r>,)?
                    $(Write<$w>,)?
                    $(ResRead<$rr>,)?
                    $(ResWrite<$rw>,)?
                ),
                $($r: Query,)?
                $($w: QueryMut,)?
                $($rr: ResQuery,)?
                $($rw: ResQueryMut,)?
            {
                #[inline]
                fn from(value: F) -> Self {
                    Self {
                        run: value,
                        _marker: PhantomData,
                    }
                }
            }

            impl<F $(, $r)? $(, $w)? $(, $rr)? $(, $rw)?> From<F>
                for StructOrFnSystem<(), $req_with_placeholder, F>
            where
                F: FnMut(
                    $(Read<$r>,)?
                    $(Write<$w>,)?
                    $(ResRead<$rr>,)?
                    $(ResWrite<$rw>,)?
                ),
                $($r: Query,)?
                $($w: QueryMut,)?
                $($rr: ResQuery,)?
                $($rw: ResQueryMut,)?
            {
                #[inline]
                fn from(value: F) -> Self {
                    Self::Fn(value.into())
                }
            }

            impl<F $(, $r)? $(, $w)? $(, $rr)? $(, $rw)?> System
                for FnSystem<$req_with_placeholder, F>
            where
                F: FnMut(
                    $(Read<$r>,)?
                    $(Write<$w>,)?
                    $(ResRead<$rr>,)?
                    $(ResWrite<$rw>,)?
                ) + 'static,
                $($r: Query,)?
                $($w: QueryMut,)?
                $($rr: ResQuery,)?
                $($rw: ResQueryMut,)?
            {
                type Req = $req_with_tuple;

                #[inline]
                fn run(&mut self, resp: Response<Self::Req>) {
                    (self.run)(
                        $(Read::<$r>(resp.read),)?
                        $(Write::<$w>(resp.write),)?
                        $(ResRead::<$rr>(resp.res_read),)?
                        $(ResWrite::<$rw>(resp.res_write),)?
                    )
                }
            }

            impl<F $(, $r)? $(, $w)? $(, $rr)? $(, $rw)?> AsFnSystemKey<$req_with_placeholder>
                for F
            where
                F: FnMut(
                    $(Read<$r>,)?
                    $(Write<$w>,)?
                    $(ResRead<$rr>,)?
                    $(ResWrite<$rw>,)?
                ) + 'static,
                $($r: Query,)?
                $($w: QueryMut,)?
                $($rr: ResQuery,)?
                $($rw: ResQueryMut,)?
            {
                #[inline]
                fn key(self) -> SystemKey {
                    let sys: FnSystem<_, _> = self.into();
                    sys.skey()
                }
            }
        };
    }

    // NOTE: read-only is disabled for now.

    // _impl!((Ph, Ph, Ph, Ph), ((), (), (), ()));
    _impl!((Ph, Ph, Ph, RW), ((), (), (), RW), rw=RW);
    // _impl!((Ph, Ph, RR, Ph), ((), (), RR, ()), rr=RR);
    _impl!((Ph, Ph, RR, RW), ((), (), RR, RW), rr=RR, rw=RW);

    _impl!((Ph, W, Ph, Ph),  ((), W, (), ()),  w=W);
    _impl!((Ph, W, Ph, RW),  ((), W, (), RW),  w=W, rw=RW);
    _impl!((Ph, W, RR, Ph),  ((), W, RR, ()),  w=W, rr=RR);
    _impl!((Ph, W, RR, RW),  ((), W, RR, RW),  w=W, rr=RR, rw=RW);

    // _impl!((R, Ph, Ph, Ph),  (R, (), (), ()),  r=R);
    _impl!((R, Ph, Ph, RW),  (R, (), (), RW),  r=R, rw=RW);
    // _impl!((R, Ph, RR, Ph),  (R, (), RR, ()),  r=R, rr=RR);
    _impl!((R, Ph, RR, RW),  (R, (), RR, RW),  r=R, rr=RR, rw=RW);

    _impl!((R, W, Ph, Ph),   (R, W, (), ()),   r=R, w=W);
    _impl!((R, W, Ph, RW),   (R, W, (), RW),   r=R, w=W, rw=RW);
    _impl!((R, W, RR, Ph),   (R, W, RR, ()),   r=R, w=W, rr=RR);
    _impl!((R, W, RR, RW),   (R, W, RR, RW),   r=R, w=W, rr=RR, rw=RW);
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
    #[inline]
    fn from(value: S) -> Self {
        Self::Struct(value)
    }
}

impl<Req, F> From<FnSystem<Req, F>> for StructOrFnSystem<(), Req, F> {
    #[inline]
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
