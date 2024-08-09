use super::{
    query::{
        EntQueryMut, EntWrite, Query, QueryMut, Read, ResQuery, ResQueryMut, ResRead, ResWrite,
        Write,
    },
    request::{
        PrivateRequest, Request, RequestBuffer, RequestInfo, RequestKey, Response, StoreRequestInfo,
    },
};
use crate::ds::prelude::*;
use crate::ecs::{EcsError, EcsResult};
use std::{
    any,
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
    hash::BuildHasher,
    marker::PhantomData,
    num::NonZeroU32,
    rc::{Rc, Weak},
};

/// A series of system that will be invoked by a shcheduler.
#[derive(Debug)]
pub struct SystemGroup<S> {
    /// [`SystemKey`] -> [`SystemId`].
    map: HashMap<SystemKey, Vec<SystemId>, S>,

    /// System id that will be given to new registered system.
    cur_id: SystemId,

    /// When you register a new system, the system is stored here without activating it.
    /// To activate it, call [`Self::activate_system`].
    inactive: HashMap<SystemId, SystemData, S>,

    /// Currently activated systems.
    active: SystemCycle<S>,

    /// Volatile systems will be removed permanently instead of moving to inactive list.
    /// For instance, setup system and FnOnce system are volatile.
    volatile: HashSet<SystemId, S>,

    /// Active system's lifetime.
    lifetime: SystemLifetime<S>,
}

impl<S> SystemGroup<S>
where
    S: BuildHasher + Default,
{
    pub(crate) fn new<Stor>(stor: Rc<RefCell<Stor>>, gi: u16) -> Self
    where
        Stor: StoreRequestInfo + 'static,
    {
        let dummy = ().into_data(stor, SystemId::new(gi, 0));

        Self {
            map: HashMap::default(),
            cur_id: SystemId::new(gi, 1),
            inactive: HashMap::default(),
            active: SystemCycle::new(dummy),
            volatile: HashSet::default(),
            lifetime: SystemLifetime::new(),
        }
    }

    pub fn activate_system_num(&self) -> usize {
        self.active.len()
    }

    pub(crate) fn cycle(&mut self) -> &mut SystemCycle<S> {
        &mut self.active
    }

    pub(crate) fn is_active(&self, sid: &SystemId) -> bool {
        self.active.contains(sid)
    }

    pub(crate) fn next_system_id(&self) -> SystemId {
        self.cur_id
    }

    pub(crate) fn register_system(&mut self, skey: SystemKey, sdata: SystemData, volatile: bool) {
        // It can be possible to set system id here,
        // but then, system data may contain invalid system id for a second.
        // That's not what we want, so clients must set the correct id
        // through calling to next_system_id().
        let sid = sdata.id();
        assert_eq!(self.cur_id, sid);

        // Increases system id for next ones.
        self.cur_id.add_item_index(1);

        // Creates mapping between key and id.
        self.map
            .entry(skey)
            .and_modify(|sids| sids.push(sid))
            .or_insert(vec![sid]);

        // Registers the system to inactive list.
        self.inactive.insert(sid, sdata);

        // We need to record whether or not the system is volatile.
        if volatile {
            let must_true = self.volatile.insert(sid);
            debug_assert!(must_true);
        }
    }

    /// Unregisters system if and only if the system is inactive.
    //
    // For future use.
    #[allow(dead_code)]
    pub(crate) fn unregister_system(&mut self, sid: &SystemId) -> Option<SystemData> {
        // Blocks to unregister active system.
        assert!(!self.is_active(sid));

        self.inactive.remove(sid)
    }

    /// Activates the system. If the system is already active, nothing takes place.
    pub(crate) fn activate_system(
        &mut self,
        target: &SystemId,
        at: InsertPos,
        live: NonZeroU32,
    ) -> EcsResult<()> {
        // Validates.
        if let InsertPos::After(after) = at {
            if !self.is_active(after) {
                // Unknown `after` system.
                return Err(EcsError::UnknownSystem);
            }
        }
        if self.is_active(target) {
            return Ok(());
        }

        if let Some((target, sdata)) = self.inactive.remove_entry(target) {
            // Inserts system id into lifetime manager.
            let must_true = self.active.insert(target, sdata, at);
            debug_assert!(must_true);
            self.lifetime.register(target, live);
            Ok(())
        } else {
            // Unknown `target system.
            Err(EcsError::UnknownSystem)
        }
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
}

/// Currently activated systems.
#[derive(Debug)]
#[repr(transparent)]
pub struct SystemCycle<S>(SetList<SystemId, SystemData, S>);

impl<S> SystemCycle<S>
where
    S: BuildHasher + Default,
{
    // `SetList` requires default head node, just makes empty system and puts it in.
    pub(crate) fn new(dummy: SystemData) -> Self {
        Self(SetList::new(dummy))
    }

    pub(crate) fn len(&self) -> usize {
        self.0.len_occupied()
    }

    pub(crate) fn contains(&self, sid: &SystemId) -> bool {
        self.0.contains_key(sid)
    }

    pub(crate) fn insert(&mut self, sid: SystemId, sdata: SystemData, at: InsertPos) -> bool {
        match at {
            InsertPos::After(after) => self.0.insert(sid, sdata, after),
            InsertPos::Back => self.0.push_back(sid, sdata),
            InsertPos::Front => self.0.push_front(sid, sdata),
        }
    }

    pub(crate) fn remove(&mut self, sid: &SystemId) -> Option<SystemData> {
        self.0.remove(sid)
    }

    /// Resets both system position and run history.
    pub(crate) fn iter_begin(&mut self) -> SystemCycleIter<S> {
        SystemCycleIter::new(&mut self.0)
    }

    /// Retrieves the next system data.
    pub(crate) fn peek_next(&self, sid: &SystemId) -> Option<&SystemData> {
        self.0.get_next(sid)
    }
}

#[derive(Debug)]
pub struct SystemCycleIter<'a, S: BuildHasher> {
    /// Currently activated systems.
    systems: &'a mut SetList<SystemId, SystemData, S>,

    /// System position to be run.
    cur_pos: ListPos,
}

impl<'a, S> SystemCycleIter<'a, S>
where
    S: BuildHasher,
{
    pub(crate) fn new(systems: &'a mut SetList<SystemId, SystemData, S>) -> Self {
        let cur_pos = systems.get_first_position();
        Self { systems, cur_pos }
    }

    /// Moves system position on to the next.
    pub(crate) fn next(&mut self) {
        if let Some((next, _sdata)) = self.systems.iter_next(self.cur_pos) {
            self.cur_pos = next;
        }
    }

    /// Returns current system position.
    pub(crate) fn position(&self) -> ListPos {
        self.cur_pos
    }

    /// Returns system to be run this time.
    pub(crate) fn get(&mut self) -> Option<&mut SystemData> {
        self.get_at(self.cur_pos)
    }

    /// Returns system at the given position.
    pub(crate) fn get_at(&mut self, pos: ListPos) -> Option<&mut SystemData> {
        self.systems.iter_next_mut(pos).map(|(_, sdata)| sdata)
    }
}

#[cfg(debug_assertions)]
impl<'a, S> Drop for SystemCycleIter<'a, S>
where
    S: BuildHasher,
{
    fn drop(&mut self) {
        // System cycle iterator must be consumed completely.
        assert!(self.position().is_end());
    }
}

pub enum InsertPos<'a> {
    After(&'a SystemId),
    Back,
    Front,
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
    S: BuildHasher + Default,
{
    fn new() -> Self {
        Self {
            tick: 0,
            lives: HashMap::default(),
            pool: SimpleVecPool::new(),
        }
    }

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
    type Request: Request;

    /// Required.
    fn run(&mut self, resp: Response<Self::Request>);

    /// Provided.
    /// Use the name as a debugging information only.
    fn name() -> &'static str {
        any::type_name::<Self>()
    }

    /// Provided.
    fn key() -> SystemKey {
        SystemKey::of::<Self>()
    }
}

pub(crate) trait PrivateSystem: System {
    type PrivateRequest: PrivateRequest;

    /// Creates system data and stores system information to the storage from the given type.
    fn into_data<S>(self, stor: Rc<RefCell<S>>, sid: SystemId) -> SystemData
    where
        S: StoreRequestInfo + 'static,
    {
        let rinfo = <Self::PrivateRequest as PrivateRequest>::get_info(&mut *stor.borrow_mut());

        let unreg = Box::new(move |rkey: &RequestKey| {
            stor.borrow_mut().remove(rkey);
        });

        SystemData {
            id: sid,
            invoke: Box::new(self),
            info: Rc::new(SystemInfo::new(
                Self::name(),
                <Self::Request as Request>::key(),
                rinfo,
                unreg,
            )),
        }
    }
}

impl<T: System> PrivateSystem for T {
    type PrivateRequest = T::Request;
}

/// [`TypeId`] of a [`System`].
pub type SystemKey = ATypeId<SystemKey_>;
pub struct SystemKey_;

/// Globally unique system identification determined by [`SystemGroup`].
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct SystemId {
    /// Group index.
    gi: u16,

    /// Item index.
    ii: u16,
}

impl SystemId {
    const DUMMY: Self = Self {
        gi: u16::MAX,
        ii: u16::MAX,
    };
    const MAX: u16 = u16::MAX - 1;

    pub const fn dummy() -> Self {
        Self::DUMMY
    }

    pub fn is_dummy(&self) -> bool {
        self == &Self::dummy()
    }

    pub const fn new(gi: u16, ii: u16) -> Self {
        assert!(gi != Self::dummy().gi && ii != Self::dummy().ii);

        Self { gi, ii }
    }

    pub const fn group_index(&self) -> u16 {
        self.gi
    }

    pub const fn max_group_index() -> u16 {
        Self::MAX
    }

    pub const fn item_index(&self) -> u16 {
        self.ii
    }

    pub const fn max_item_index() -> u16 {
        Self::MAX
    }

    pub fn add_item_index(&mut self, by: u16) {
        assert!(self.ii < Self::max_item_index());
        self.ii += by;
    }
}

impl fmt::Display for SystemId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.gi, self.ii)
    }
}

pub(crate) struct SystemData {
    /// The other identification for the systems.
    id: SystemId,

    /// System data and its entry point.
    invoke: Box<dyn Invoke>,

    /// Infrequently accssed information such as system's name or request.
    //
    // * Why Rc
    //
    // - In order to share `SystemInfo` with others.
    // But, we don't share onwership, others can have weak references only.
    // It reduces complexity in terms of self-unregistration from info storage.
    //
    // - In order to reduce size of the struct.
    // We move whole `SystemData` when we activate or inactivate systems.
    info: Rc<SystemInfo>,
}

impl fmt::Debug for SystemData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SystemData")
            .field("id", &self.id)
            .field("info", &self.info)
            .finish_non_exhaustive()
    }
}

impl SystemData {
    pub(crate) fn id(&self) -> SystemId {
        self.id
    }

    // For future use.
    #[allow(dead_code)]
    pub(crate) fn invokable(&self) -> &dyn Invoke {
        self.invoke.as_ref()
    }

    pub(crate) fn invokable_mut(&mut self) -> &mut dyn Invoke {
        self.invoke.as_mut()
    }

    pub(crate) fn info(&self) -> Weak<SystemInfo> {
        Rc::downgrade(&self.info)
    }

    pub(crate) fn get_info(&self) -> &SystemInfo {
        &self.info
    }

    pub(crate) fn get_request_info(&self) -> &Rc<RequestInfo> {
        // Safety: It's always Some. See comments of the field.
        unsafe { self.info.rinfo.as_ref().unwrap_unchecked() }
    }

    pub(crate) fn task_ptr(&mut self) -> NonNullExt<dyn Invoke> {
        let ptr = self.invoke.as_mut() as *mut _;

        // Safety: Infallible.
        unsafe { NonNullExt::new_unchecked(ptr).with_name(self.get_info().name()) }
    }
}

pub(crate) struct SystemInfo {
    /// System name basically determined by [`std::any::type_name`].
    name: &'static str,

    /// [`System`] is related to a [`RequestInfo`].
    /// This field is the key for the `RequestInfo`.
    rkey: RequestKey,

    /// [`System`] is related to a [`RequestInfo`].
    //
    // It's always Some except in drop().
    // In drop(), inner type will be dropped first.
    rinfo: Option<Rc<RequestInfo>>,

    /// Self-unregister function.
    //
    // Request information that this struct holds is held in a central storage
    // as well. We unregister the information from the storage
    // when we're going to not use it anymore.
    #[allow(clippy::type_complexity)]
    unreg: Option<Box<dyn FnOnce(&RequestKey)>>,
}

impl SystemInfo {
    const fn new(
        name: &'static str,
        rkey: RequestKey,
        rinfo: Rc<RequestInfo>,
        unreg: Box<dyn FnOnce(&RequestKey)>,
    ) -> Self {
        Self {
            name,
            rkey,
            rinfo: Some(rinfo),
            unreg: Some(unreg),
        }
    }

    pub(crate) fn name(&self) -> &'static str {
        self.name
    }

    pub(crate) fn request_key(&self) -> RequestKey {
        self.rkey
    }

    pub(crate) fn get_request_info(&self) -> &RequestInfo {
        // Safety: `self.rinfo` is Some except in drop().
        unsafe { self.rinfo.as_ref().unwrap_unchecked() }
    }
}

impl fmt::Debug for SystemInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SystemInfo")
            .field("name", &self.name)
            .finish_non_exhaustive()
    }
}

impl Drop for SystemInfo {
    fn drop(&mut self) {
        // Self-unregister.
        drop(self.rinfo.take());
        let unreg = self.unreg.take().unwrap();
        unreg(&self.rkey);
    }
}

/// Empty system.
impl System for () {
    type Request = ();
    fn run(&mut self, _resp: Response<Self::Request>) {}
}

/// Object safe trait for the [`System`].
pub trait Invoke {
    fn invoke(&mut self, buf: &mut RequestBuffer);
    fn skey(&self) -> SystemKey;
    fn rkey(&self) -> RequestKey;
}

impl<S: System> Invoke for S {
    fn invoke(&mut self, buf: &mut RequestBuffer) {
        self.run(Response::new(buf));
    }

    fn skey(&self) -> SystemKey {
        S::key()
    }

    fn rkey(&self) -> RequestKey {
        <S as System>::Request::key()
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

impl<Req: fmt::Debug, F> fmt::Debug for FnOnceSystem<Req, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FnOnceSystem")
            .field("_marker", &self._marker)
            .finish_non_exhaustive()
    }
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
                type Request = $req_with_tuple;

                fn run(&mut self, _resp: Response<Self::Request>) {
                    (self.run)(
                        $(Read::<$r>(_resp.read),)?
                        $(Write::<$w>(_resp.write),)?
                        $(ResRead::<$rr>(_resp.res_read),)?
                        $(ResWrite::<$rw>(_resp.res_write),)?
                        $(EntWrite::<$ew>(_resp.ent_write),)?
                    )
                }

                fn name() -> &'static str {
                    std::any::type_name::<F>()
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
                type Request = $req_with_tuple;

                fn run(&mut self, _resp: Response<Self::Request>) {
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
    type o = Ph; // easy to see where it is.

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
    type Request = ();
    fn run(&mut self, _resp: Response<Self::Request>) {
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
