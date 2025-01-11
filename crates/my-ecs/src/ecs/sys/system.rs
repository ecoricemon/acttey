use super::{
    query::{
        EntQueryMut, EntWrite, Query, QueryMut, Read, ResQuery, ResQueryMut, ResRead, ResWrite,
        Write,
    },
    request::{
        Request, RequestInfo, RequestKey, Response, StoreRequestInfo, SystemBuffer, RINFO_STOR,
    },
};
use crate::{debug_format, ds::prelude::*, ecs::EcsError, util::Or};
use std::{
    any::{self, Any},
    borrow,
    collections::{HashMap, HashSet},
    fmt,
    hash::{self, BuildHasher},
    marker::PhantomData,
    mem,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    sync::Arc,
};

use SystemState::*;

// Clients can define their systems with some data. And we're going to send
// those systems to other workers, so it's good to add `Send` bound to the trait
// for safety.
#[allow(private_interfaces, private_bounds)]
pub trait System: Send + 'static {
    type Request: Request;

    fn run(&mut self, resp: Response<'_, Self::Request>);

    #[doc(hidden)]
    #[allow(unused_variables)]
    fn run_private(&mut self, sid: SystemId, buf: ManagedMutPtr<SystemBuffer>) {}

    #[allow(unused_variables)]
    fn on_transition(&mut self, from: SystemState, to: SystemState) {}

    /// This name may change in the future.
    /// Use this name as debugging information only.
    fn name() -> &'static str {
        any::type_name::<Self>()
    }

    #[doc(hidden)]
    fn key() -> SystemKey {
        SystemKey::of::<Self>()
    }

    #[doc(hidden)]
    fn into_data(self) -> SystemData
    where
        Self: Sized + 'static,
    {
        let boxed = Box::new(self);
        // Safety: Infallible.
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(boxed)) };
        Self::_create_data(ptr, SystemFlags::OWNED_SET)
    }

    #[doc(hidden)]
    unsafe fn create_data(ptr: NonNull<dyn Invoke + Send>) -> SystemData
    where
        Self: Sized + 'static,
    {
        Self::_create_data(ptr, SystemFlags::OWNED_RESET)
    }

    #[doc(hidden)]
    fn _create_data(invoker: NonNull<dyn Invoke + Send>, flags: SystemFlags) -> SystemData {
        let mut stor = RINFO_STOR.lock().unwrap();
        let rinfo = Arc::clone(Self::Request::get_info_from(&mut *stor));
        drop(stor);

        SystemData {
            id: SystemId::dummy(),
            flags,
            invoker,
            info: Arc::new(SystemInfo::new(
                Self::name(),
                Self::key(),
                Self::Request::key(),
                rinfo,
            )),
        }
    }
}

/// A system group that will be invoked together in a cycle by scheduler.
///
/// Systems can be in one of states such as [`Active`], [`Inactive`], [`Dead`],
/// and [`Poisoned`].
///
/// Possible system transitions are as follows,
///
/// | From         | To           | Methods                                  |
/// | ---          | ---          | ---                                      |
/// |              | [`Inactive`] | [`register`]                             |
/// | [`Inactive`] | [`Active`]   | [`activate`]                             |
/// | [`Inactive`] | [`Dead`]     | [`unregister`]                           |
/// | [`Inactive`] | [`Poisoned`] | [`poison`]                               |
/// | [`Active`]   | [`Inactive`] | [`inactivate`], [`tick`] w\ non-volatile |
/// | [`Active`]   | [`Dead`]     | [`tick`] w\ volatile                     |
/// | [`Active`]   | [`Poisoned`] | [`poison`]                               |
/// | [`Dead`]     |              | [`drain_dead`]                           |
/// | [`Poisoned`] |              | [`drain_poisoned`]                       |
///
/// [`register`]: Self::register
/// [`activate`]: Self::activate
/// [`inactivate`]: Self::inactivate
/// [`poison`]: Self::poison
/// [`unregister`]: Self::unregister
/// [`tick`]: Self::tick
/// [`drain_dead`]: Self::drain_dead
/// [`drain_poisoned`]: Self::drain_poisoned
#[derive(Debug)]
pub(crate) struct SystemGroup<S> {
    /// System id that will be given to new registered system.
    cur_id: SystemId,

    /// Active state systems.
    active: SystemCycle<S>,

    /// Inactive state systems.
    inactive: HashSet<SystemData, S>,

    /// Dead state systems.
    dead: Vec<SystemData>,

    /// Poisoned state systems.
    poisoned: Vec<PoisonedSystem>,

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
    pub(crate) fn new(gi: u16) -> Self {
        let dummy = ().into_data();

        Self {
            cur_id: SystemId::new(gi, 1),
            active: SystemCycle::new(dummy),
            inactive: HashSet::default(),
            dead: Vec::new(),
            poisoned: Vec::new(),
            volatile: HashSet::default(),
            lifetime: SystemLifetime::new(),
        }
    }

    /// Clears all stored systems.
    ///
    /// Systems will be removed through state transitions.
    /// - [`Active`] -> [`Inactive`] -> [`Dead`] -> Removed
    /// - [`Poisoned`] -> Removed
    ///
    /// Which means their [`on_transition`] will be called in order.
    ///
    /// [`on_transition`]: System::on_transition
    pub(crate) fn clear(&mut self) {
        // Active -> Inactive or Dead
        let mut sids = Vec::with_capacity(self.len_active() + self.len_inactive());
        for sdata in self.active.values() {
            sids.push(sdata.id());
        }
        while let Some(sid) = sids.pop() {
            self.inactivate(&sid).unwrap();
        }

        // Inactive -> Dead
        for sdata in self.inactive.iter() {
            sids.push(sdata.id());
        }
        while let Some(sid) = sids.pop() {
            self.unregister(&sid).unwrap();
        }

        // Clears Dead & Poisoned.
        self.drain_dead();
        self.drain_poisoned();
    }

    pub(crate) fn len_active(&self) -> usize {
        self.active.len()
    }

    pub(crate) fn len_inactive(&self) -> usize {
        self.inactive.len()
    }

    // TODO: rename
    pub(crate) fn get_active_mut(&mut self) -> &mut SystemCycle<S> {
        &mut self.active
    }

    pub(crate) fn is_active(&self, sid: &SystemId) -> bool {
        self.active.contains(sid)
    }

    /// Determines whether or not a system for the given id is in [`Active`] or
    /// [`Inactive`] states.
    pub(crate) fn contains(&self, sid: &SystemId) -> bool {
        self.contains_active(sid) || self.contains_inactive(sid)
    }

    /// Determines whether or not a system for the given id is in [`Active`]
    /// state.
    pub(crate) fn contains_active(&self, sid: &SystemId) -> bool {
        self.active.contains(sid)
    }

    /// Determines whether or not a system for the given id is in [`Inactive`]
    /// state.
    pub(crate) fn contains_inactive(&self, sid: &SystemId) -> bool {
        self.inactive.contains(sid)
    }

    /// Looks for a system for the given id in [`Active`] systems.
    pub(crate) fn get_active(&self, sid: &SystemId) -> Option<&SystemData> {
        self.active.get(sid)
    }

    /// Returns system id that the next registered system will have.
    pub(crate) fn next_system_id(&self) -> SystemId {
        self.cur_id
    }

    /// Registers a system.
    ///
    /// If there's no error during registration, the system state becomes
    /// [`Inactive`].
    ///
    /// Cases below are considered error.
    /// - System id is not correct. System id must be the same as the one
    ///   [`Self::next_system_id`] returns.
    /// - Found a system that has the same system id in [`Active`] or `Inactive`
    ///   systems.
    //
    // It can be possible to set system id here, but then, system data may
    // contain invalid system id for a second. That's not what we want, so you
    // must set the correct id by calling [`Self::next_system_id`] beforehand.
    pub(crate) fn register(
        &mut self,
        sdata: SystemData,
        volatile: bool,
    ) -> Result<(), EcsError<SystemData>> {
        // Validates.
        if sdata.id() != self.cur_id {
            let reason = debug_format!("invalid system id");
            return Err(EcsError::Unknown(reason, sdata));
        }
        if self.contains(&sdata.id()) {
            let reason = debug_format!("duplicated system id");
            return Err(EcsError::Unknown(reason, sdata));
        }

        // Increases system id for the next one.
        let sid = sdata.id();
        self.cur_id.add_system_index(1);

        // Inserts the system into inactive list.
        let is_new = self.inactive.insert(sdata);
        debug_assert!(is_new);

        // We need to record whether the system is volatile or not.
        if volatile {
            let is_new = self.volatile.insert(sid);
            debug_assert!(is_new);
        }
        Ok(())
    }

    /// Activates a system for the given id.
    ///
    /// Cases below are considered ok.
    /// - If the system was already in [`Active`] state.
    /// - No error
    ///
    /// Then the system state transitions from [`Inactive`] to `Active`.
    ///
    /// Whereas cases below are considered error.
    /// - If the system was not in `Inactive` state.
    /// - If insert position pointed to a particular system, and the system was
    ///   not in `Active` states.
    pub(crate) fn activate(
        &mut self,
        target: &SystemId,
        at: InsertPos,
        live: Tick,
    ) -> Result<(), EcsError> {
        // Validates.
        if let InsertPos::After(after) = at {
            if !self.is_active(&after) {
                // Unknown `after` system.
                let reason =
                    debug_format!("cannot activate a system due to invalid insert position");
                return Err(EcsError::UnknownSystem(reason, ()));
            }
        }
        if self.is_active(target) {
            return Ok(());
        }

        if let Some(mut sdata) = self.inactive.take(target) {
            debug_assert_eq!(sdata.id(), *target);

            sdata.as_invoker_mut().on_transition(Inactive, Active);
            let must_true = self.active.insert(sdata, at);
            debug_assert!(must_true);

            // Inserts system id into lifetime manager.
            self.lifetime.register(*target, live);
            Ok(())
        } else {
            // Unknown `target system.
            let reason = debug_format!("tried to activate a not inactive system");
            Err(EcsError::UnknownSystem(reason, ()))
        }
    }

    /// Unregisters a system for the given id.
    ///
    /// Cases below are considered ok.
    /// - If the system was already in [`Dead`] state.
    /// - No error
    ///
    /// Then the system state transitions from [`Inactive`] to `Dead`.
    ///
    /// But if the system was not in `Inactive` state, returns error.
    pub(crate) fn unregister(&mut self, sid: &SystemId) -> Result<(), EcsError> {
        if let Some(mut sdata) = self.inactive.take(sid) {
            sdata.as_invoker_mut().on_transition(Inactive, Dead);
            self.dead.push(sdata);
            Ok(())
        } else {
            let reason = debug_format!("tried to unregister a not inactive system");
            Err(EcsError::UnknownSystem(reason, ()))
        }
    }

    /// Moves a system for the given id to [`Poisoned`] state.
    ///
    /// If the system was in one of [`Active`] or [`Inactive`] states, the
    /// system state transitions to `Poisoned` state. Otherwise, just returns
    /// given payload back.
    pub(crate) fn poison(
        &mut self,
        sid: &SystemId,
        payload: Box<dyn Any + Send>,
    ) -> Result<(), EcsError<Box<dyn Any + Send>>> {
        let sdata = if let Some(mut sdata) = self.active.remove(sid) {
            sdata.as_invoker_mut().on_transition(Active, Poisoned);
            sdata
        } else if let Some(mut sdata) = self.inactive.take(sid) {
            sdata.as_invoker_mut().on_transition(Inactive, Poisoned);
            sdata
        } else {
            let reason = debug_format!("tried to poison a not (in)active system");
            return Err(EcsError::UnknownSystem(reason, payload));
        };
        let poisoned = PoisonedSystem::from_system_data(sdata, payload);
        self.poisoned.push(poisoned);
        Ok(())
    }

    /// Inactivates a system for the given id.
    ///
    /// If inactivation is successful, system state changes depends on system
    /// volatility as shown below.
    /// - If the system is volatile, it changes to [`Dead`] state.
    /// - Otherwise, it changes to [`Inactive`] state.
    ///
    /// Cases below are considered ok.
    /// - If the system was already in `Inactive` state.
    /// - No error
    ///
    /// Then the system state transitions from `Active` to `Inactive`.
    ///
    /// But if the system was not in [`Active`] state, returns error.
    pub(crate) fn inactivate(&mut self, sid: &SystemId) -> Result<(), EcsError> {
        let Self {
            active,
            inactive,
            dead,
            volatile,
            ..
        } = self;

        Self::_inactivate(sid, active, inactive, dead, volatile)
    }

    fn _inactivate(
        sid: &SystemId,
        active: &mut SystemCycle<S>,
        inactive: &mut HashSet<SystemData, S>,
        dead: &mut Vec<SystemData>,
        volatile: &mut HashSet<SystemId, S>,
    ) -> Result<(), EcsError> {
        if inactive.contains(sid) {
            Ok(())
        } else if let Some(mut sdata) = active.remove(sid) {
            if volatile.contains(sid) {
                sdata.as_invoker_mut().on_transition(Active, Dead);
                dead.push(sdata);
            } else {
                sdata.as_invoker_mut().on_transition(Active, Inactive);
                inactive.insert(sdata);
            }
            Ok(())
        } else {
            let reason = debug_format!("tried to inactivate a not (in)active system");
            Err(EcsError::UnknownSystem(reason, ()))
        }
    }

    /// Removes the whole systems in [`Dead`] state.
    pub(crate) fn drain_dead(&mut self) -> std::vec::Drain<'_, SystemData> {
        for sdata in self.dead.iter() {
            self.volatile.remove(&sdata.id());
        }
        self.dead.drain(..)
    }

    /// Removes the whole systems in [`Poisoned`] state.
    pub(crate) fn drain_poisoned(&mut self) -> std::vec::Drain<'_, PoisonedSystem> {
        for poisoned in self.poisoned.iter() {
            self.volatile.remove(&poisoned.id());
        }
        self.poisoned.drain(..)
    }

    pub(crate) fn tick(&mut self) {
        let Self {
            active,
            inactive,
            dead,
            volatile,
            lifetime,
            ..
        } = self;

        if let Some(expired_sids) = lifetime.tick() {
            while let Some(sid) = expired_sids.pop() {
                let _ = Self::_inactivate(&sid, active, inactive, dead, volatile);
            }
        }
    }
}

#[derive(Debug)]
pub enum SystemState {
    /// A system state meaning ready to be run.
    Active,

    /// A system state meaning not ready to be run.
    Inactive,

    /// A system state meaning dead. Dead systems cannot be reborn.
    Dead,

    /// A system state meaning panicked. Poisoned systems cannot be reborn.
    Poisoned,
}

/// Currently activated systems.
#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct SystemCycle<S>(SetList<SystemId, SystemData, S>);

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

    pub(crate) fn insert(&mut self, sdata: SystemData, at: InsertPos) -> bool {
        match at {
            InsertPos::After(after) => self.0.insert(sdata.id(), sdata, &after),
            InsertPos::Back => self.0.push_back(sdata.id(), sdata),
            InsertPos::Front => self.0.push_front(sdata.id(), sdata),
        }
    }

    pub(crate) fn remove(&mut self, sid: &SystemId) -> Option<SystemData> {
        self.0.remove(sid)
    }

    pub(crate) fn iter_begin(&mut self) -> SystemCycleIter<'_, S> {
        SystemCycleIter::new(&mut self.0)
    }
}

impl<S> Deref for SystemCycle<S> {
    type Target = SetList<SystemId, SystemData, S>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<S> DerefMut for SystemCycle<S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct SystemCycleIter<'a, S> {
    raw: RawSystemCycleIter<S>,

    _marker: PhantomData<&'a mut ()>,
}

impl<S> SystemCycleIter<'_, S> {
    pub(crate) fn into_raw(self) -> RawSystemCycleIter<S> {
        self.raw
    }

    pub(crate) unsafe fn from_raw(raw: RawSystemCycleIter<S>) -> Self {
        Self {
            raw,
            _marker: PhantomData,
        }
    }

    /// Returns current system position.
    pub(crate) fn position(&self) -> ListPos {
        self.raw.position()
    }
}

impl<'a, S> SystemCycleIter<'a, S>
where
    S: BuildHasher,
{
    pub(crate) fn new(systems: &'a mut SetList<SystemId, SystemData, S>) -> Self {
        Self {
            raw: RawSystemCycleIter::new(systems),
            _marker: PhantomData,
        }
    }

    /// Returns system to be run this time.
    pub(crate) fn get(&mut self) -> Option<&mut SystemData> {
        // Safety: We're actually borrowing `raw.systems`.
        unsafe { self.raw.get() }
    }

    /// Returns system at the given position.
    pub(crate) fn get_at(&mut self, pos: ListPos) -> Option<&mut SystemData> {
        // Safety: We're actually borrowing `raw.systems`.
        unsafe { self.raw.get_at(pos) }
    }
}

#[derive(Debug)]
pub(crate) struct RawSystemCycleIter<S> {
    /// Currently activated systems.
    systems: NonNull<SetList<SystemId, SystemData, S>>,

    /// System position to be run.
    cur_pos: ListPos,
}

impl<S> RawSystemCycleIter<S> {
    /// Returns current system position.
    pub(crate) fn position(&self) -> ListPos {
        self.cur_pos
    }
}

impl<S> RawSystemCycleIter<S>
where
    S: BuildHasher,
{
    pub(crate) fn new(systems: &mut SetList<SystemId, SystemData, S>) -> Self {
        let cur_pos = systems.get_first_position();
        // Safety: Infallible.
        let systems = unsafe { NonNull::new_unchecked(systems as *mut _) };

        Self { systems, cur_pos }
    }

    /// Moves system position on to the next.
    pub(crate) unsafe fn next(&mut self) {
        if let Some((next, _sdata)) = self.systems.as_ref().iter_next(self.cur_pos) {
            self.cur_pos = next;
        }
    }

    /// Returns system to be run this time.
    pub(crate) unsafe fn get(&mut self) -> Option<&mut SystemData> {
        self.get_at(self.cur_pos)
    }

    /// Returns system at the given position.
    pub(crate) unsafe fn get_at(&mut self, pos: ListPos) -> Option<&mut SystemData> {
        self.systems
            .as_mut()
            .iter_next_mut(pos)
            .map(|(_, sdata)| sdata)
    }
}

impl<S> Clone for RawSystemCycleIter<S> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<S> Copy for RawSystemCycleIter<S> {}

#[derive(Debug, Clone, Copy)]
pub enum InsertPos {
    Back,
    Front,
    After(SystemId),
}

#[derive(Debug)]
struct SystemLifetime<S> {
    /// Monotonically increasing counter.
    tick: Tick,

    /// [`Tick`] -> [`Self::pool`] index.
    lives: HashMap<Tick, usize, S>,

    /// Vector contains system ids to be dead at a specific time.
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

    fn register(&mut self, sid: SystemId, live: Tick) {
        debug_assert!(live > 0);

        let end = self.tick.saturating_add(live);
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

/// Empty system.
impl System for () {
    type Request = ();
    fn run(&mut self, _resp: Response<Self::Request>) {}
}

/// Unique identifier for a type implementing [`System`].
pub(crate) type SystemKey = ATypeId<SystemKey_>;
pub(crate) struct SystemKey_;

/// Globally unique system identification determined by [`SystemGroup`].
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct SystemId {
    group_index: u16,
    system_index: u16,
}

impl SystemId {
    const DUMMY: Self = Self {
        group_index: u16::MAX,
        system_index: u16::MAX,
    };
    const MAX: u16 = u16::MAX - 1;

    pub(crate) const fn dummy() -> Self {
        Self::DUMMY
    }

    pub(crate) fn is_dummy(&self) -> bool {
        self == &Self::dummy()
    }

    pub(crate) const fn new(group_index: u16, system_index: u16) -> Self {
        Self {
            group_index,
            system_index,
        }
    }

    pub(crate) const fn group_index(&self) -> u16 {
        self.group_index
    }

    pub(crate) const fn max_group_index() -> u16 {
        Self::MAX
    }

    pub(crate) const fn max_system_index() -> u16 {
        Self::MAX
    }

    pub(crate) fn add_system_index(&mut self, by: u16) {
        assert!(self.system_index < Self::max_system_index());
        self.system_index += by;
    }
}

impl Default for SystemId {
    fn default() -> Self {
        Self::dummy()
    }
}

impl fmt::Display for SystemId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.group_index, self.system_index)
    }
}

/// # Safety
///
/// This struct is commonly managed by scheduler which guarantees valid access
/// in terms of [`Send`] and [`Sync`] even if it contains raw pointers in it.
/// But if clients use this struct on their own purposes, they must keep that in
/// mind.
pub(crate) struct SystemData {
    /// Unique id for a system.
    id: SystemId,

    flags: SystemFlags,

    /// System data and its entry point.
    invoker: NonNull<(dyn Invoke + Send)>,

    /// Infrequently accssed information such as system's name or request.
    //
    // * Why Arc
    // - In order to share `SystemInfo` with others.
    // - In order to reduce size of the struct. We move whole `SystemData` when
    // we activate or inactivate systems.
    info: Arc<SystemInfo>,
}

// Safety:
// - Scheduler controls that a system data is accessed by one worker at a time.
// - Scheduler provides synchronization over workers accessing the system data.
// - Where the scheduler doesn't work, clients must access it carefully.
// - Which means system data is looked like Rust primitive types from its users.
unsafe impl Send for SystemData {}
unsafe impl Sync for SystemData {}

impl SystemData {
    pub(crate) fn try_into_any(self) -> Option<Box<dyn Any + Send>> {
        if self.flags.is_owned() {
            // Safety: Checked.
            let boxed = unsafe { Box::from_raw(self.invoker.as_ptr()) };

            // We don't call drop.
            mem::forget(self);

            Some(boxed.into_any())
        } else {
            None
        }
    }
}

impl Drop for SystemData {
    fn drop(&mut self) {
        // If this data owns `invoker`, deallocates it.
        if self.flags.is_owned() {
            // Safety: Checked.
            unsafe { drop(Box::from_raw(self.invoker.as_ptr())) };
        }
    }
}

impl fmt::Debug for SystemData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SystemData")
            .field("id", &self.id)
            .field("flags", &self.flags)
            .field("info", &self.info)
            .finish_non_exhaustive()
    }
}

impl hash::Hash for SystemData {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for SystemData {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for SystemData {}

impl borrow::Borrow<SystemId> for SystemData {
    fn borrow(&self) -> &SystemId {
        &self.id
    }
}

impl SystemData {
    pub(crate) const fn id(&self) -> SystemId {
        self.id
    }

    pub(crate) const fn flags(&self) -> SystemFlags {
        self.flags
    }

    pub(crate) fn set_id(&mut self, sid: SystemId) {
        self.id = sid;
    }

    pub(crate) fn union_flags(&mut self, sflags: SystemFlags) {
        self.flags |= sflags;
    }

    pub(crate) fn info(&self) -> Arc<SystemInfo> {
        Arc::clone(&self.info)
    }

    pub(crate) fn get_request_info(&self) -> &Arc<RequestInfo> {
        // Safety: It's always Some. See comments of the field.
        unsafe { self.info.rinfo.as_ref().unwrap_unchecked() }
    }

    pub(crate) fn as_invoker_mut(&mut self) -> &mut (dyn Invoke + Send) {
        // Safety: Pointer to invoker is valid to access because
        // - If `sdata` owns it, we can safely access it.
        // - If `sdata` doesn't own it, real owner guarantees that.
        unsafe { self.invoker.as_mut() }
    }

    pub(crate) unsafe fn task_ptr(&mut self) -> ManagedMutPtr<dyn Invoke + Send> {
        ManagedMutPtr::new(NonNullExt::new_unchecked(self.invoker.as_ptr()))
    }
}

#[derive(Debug)]
pub struct PoisonedSystem {
    id: SystemId,
    name: &'static str,
    data: Option<Box<dyn Any + Send>>,
    err_payload: Box<dyn Any + Send>,
}

impl PoisonedSystem {
    const fn new(
        id: SystemId,
        name: &'static str,
        data: Option<Box<dyn Any + Send>>,
        err_payload: Box<dyn Any + Send>,
    ) -> Self {
        Self {
            id,
            name,
            data,
            err_payload,
        }
    }

    fn from_system_data(sdata: SystemData, err_payload: Box<dyn Any + Send>) -> Self {
        let id = sdata.id;
        let name = sdata.info().name;
        let data = sdata.try_into_any();
        Self::new(id, name, data, err_payload)
    }

    pub fn id(&self) -> SystemId {
        self.id
    }

    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn take_data(&mut self) -> Option<Box<dyn Any + Send>> {
        self.data.take()
    }

    pub fn into_error_payload(self) -> Box<dyn Any + Send> {
        self.err_payload
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct SystemFlags(u32);

bitflags::bitflags! {
    impl SystemFlags: u32 {
        const DEDI_SET = 1;
        const DEDI_RESET = 1 << 1;

        const PRIVATE_SET = 1 << 2;
        const PRIVATE_RESET = 1 << 3;

        const OWNED_SET = 1 << 4;
        const OWNED_RESET = 1 << 5;
    }
}

impl SystemFlags {
    pub(crate) const fn is_dedi(&self) -> bool {
        debug_assert!(!self.is_dedi_empty());

        self.intersects(Self::DEDI_SET)
    }

    pub(crate) const fn is_dedi_empty(&self) -> bool {
        !self.intersects(Self::DEDI_SET.union(Self::DEDI_RESET))
    }

    pub(crate) const fn is_private(&self) -> bool {
        debug_assert!(!self.is_private_empty());

        self.intersects(Self::PRIVATE_SET)
    }

    pub(crate) const fn is_private_empty(&self) -> bool {
        !self.intersects(Self::PRIVATE_SET.union(Self::PRIVATE_RESET))
    }

    pub(crate) const fn is_owned(&self) -> bool {
        debug_assert!(!self.is_owned_empty());

        self.intersects(Self::OWNED_SET)
    }

    pub(crate) const fn is_owned_empty(&self) -> bool {
        !self.intersects(Self::OWNED_SET.union(Self::OWNED_RESET))
    }
}

pub(crate) struct SystemInfo {
    /// System name basically determined by [`std::any::type_name`].
    name: &'static str,

    _skey: SystemKey,

    /// [`System`] is related to a [`RequestInfo`].
    /// This field is the key for the `RequestInfo`.
    rkey: RequestKey,

    /// [`System`] is related to a [`RequestInfo`].
    //
    // It's always Some except in drop().
    // In drop(), inner type will be dropped first.
    rinfo: Option<Arc<RequestInfo>>,
}

impl SystemInfo {
    const fn new(
        name: &'static str,
        skey: SystemKey,
        rkey: RequestKey,
        rinfo: Arc<RequestInfo>,
    ) -> Self {
        Self {
            name,
            _skey: skey,
            rkey,
            rinfo: Some(rinfo),
        }
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
        let mut stor = RINFO_STOR.lock().unwrap();
        stor.remove(&self.rkey);
    }
}

pub struct SystemDesc<Sys> {
    /// System itself. Clients cannot put `SystemData` in, which is only allowed
    /// to the crate.
    pub(crate) sys: Or<Sys, SystemData>,

    /// Whether the system is private system or not. Private system is a kind of
    /// systems which is used internally.
    pub(crate) private: bool,

    /// Group index of the system.
    pub group_index: u16,

    /// Whether the system is volatile or not. A volatile system will be
    /// discarded from memory after get executed as much as its lifetime.
    /// Unlike volatile system, non-volatile system will move to inactivate
    /// state instead of being discarded.
    pub volatile: bool,

    /// Lifetime and insert position in an active system cycle.
    /// - Lifetime(live): Determines how long the system should be executed.
    ///   Whenever client schedules ecs, lifetime of executed system decreases
    ///   by 1 conceptually. Zero lifetime is considered inactivation.
    /// - Insert position: Active systems get executed in an order. Client can
    ///   designate where the system locates. [`InsertPos::Front`] means the
    ///   first position in the order, while [`InsertPos::Back`] means the last
    ///   position in the order. Of course, client can put the system in the
    ///   middle of the order by [`InsertPos::After`].
    pub activation: (Tick, InsertPos),
}

impl<Sys> fmt::Debug for SystemDesc<Sys> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SystemDesc")
            .field("private", &self.private)
            .field("group_index", &self.group_index)
            .field("volatile", &self.volatile)
            .field("activation", &self.activation)
            .finish_non_exhaustive()
    }
}

impl<Sys> SystemDesc<Sys>
where
    Sys: System,
{
    pub fn with_system<T, OutSys>(self, sys: T) -> SystemDesc<OutSys>
    where
        T: Into<SystemBond<OutSys>>,
        OutSys: System,
    {
        let sys: SystemBond<OutSys> = sys.into();

        SystemDesc::<OutSys> {
            sys: Or::A(sys.into_inner()),
            private: self.private,
            group_index: self.group_index,
            volatile: self.volatile,
            activation: self.activation,
        }
    }

    pub fn with_once<T, Req, F>(self, sys: T) -> SystemDesc<FnOnceSystem<Req, F>>
    where
        T: Into<FnOnceSystem<Req, F>>,
        FnOnceSystem<Req, F>: System,
    {
        let activation = if self.activation.0 > 0 {
            (1, self.activation.1)
        } else {
            self.activation
        };

        SystemDesc::<FnOnceSystem<Req, F>> {
            sys: Or::A(sys.into()),
            private: self.private,
            group_index: self.group_index,
            volatile: self.volatile,
            activation,
        }
    }

    pub fn with_group_index(self, index: u16) -> Self {
        Self {
            group_index: index,
            ..self
        }
    }

    pub fn with_volatile(self, volatile: bool) -> Self {
        Self { volatile, ..self }
    }

    pub fn with_activation(self, live: Tick, insert_at: InsertPos) -> Self {
        Self {
            activation: (live, insert_at),
            ..self
        }
    }

    // Clients are only able to put in systems, not system data.
    pub fn take_system(self) -> Sys {
        match self.sys {
            Or::A(sys) => sys,
            Or::B(_sdata) => panic!(),
        }
    }

    pub(crate) fn with_data(self, sdata: SystemData) -> SystemDesc<()> {
        SystemDesc::<()> {
            sys: Or::B(sdata),
            private: self.private,
            group_index: self.group_index,
            volatile: self.volatile,
            activation: self.activation,
        }
    }

    pub(crate) fn with_private(self, private: bool) -> Self {
        Self { private, ..self }
    }
}

impl SystemDesc<()> {
    pub const fn new() -> Self {
        Self {
            sys: Or::A(()),
            private: false,
            group_index: 0,
            volatile: true,
            activation: (Tick::MAX, InsertPos::Back),
        }
    }
}

impl Default for SystemDesc<()> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: System> From<T> for SystemDesc<T> {
    fn from(value: T) -> Self {
        SystemDesc::new().with_system(value)
    }
}

/// Object safe trait for the [`System`].
pub(crate) trait Invoke {
    fn invoke(&mut self, buf: &mut SystemBuffer);
    fn invoke_private(&mut self, sid: SystemId, buf: ManagedMutPtr<SystemBuffer>);
    fn on_transition(&mut self, from: SystemState, to: SystemState);
    fn into_any(self: Box<Self>) -> Box<dyn Any + Send>;
}

impl<S: System> Invoke for S {
    fn invoke(&mut self, buf: &mut SystemBuffer) {
        self.run(Response::new(buf));
    }

    fn invoke_private(&mut self, sid: SystemId, buf: ManagedMutPtr<SystemBuffer>) {
        self.run_private(sid, buf);
    }

    fn on_transition(&mut self, from: SystemState, to: SystemState) {
        self.on_transition(from, to);
    }

    fn into_any(self: Box<Self>) -> Box<dyn Any + Send> {
        self
    }
}

/// This struct helps a function implements [`System`].
/// Because we can't use blanket impl for the `System`, due to confliction to other impls,
/// We put this helper between function and `System`.
/// That means a function can become `FnSystem` which implements `System`.
///
/// * Req - Arbitrary length of arguments of F.
/// * F - function.
#[repr(transparent)]
pub struct FnSystem<Req, F> {
    run: F,
    _marker: PhantomData<Req>,
}

unsafe impl<Req, F: Send> Send for FnSystem<Req, F> {}

impl<Req, F> fmt::Debug for FnSystem<Req, F>
where
    Self: System,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Self::name())
    }
}

/// * Req - Arbitrary length of arguments of F.
/// * F - Sendable function.
#[repr(transparent)]
pub struct FnOnceSystem<Req, F> {
    run: Option<F>,
    _marker: PhantomData<Req>,
}

unsafe impl<Req, F: Send> Send for FnOnceSystem<Req, F> {}

impl<Req, F> fmt::Debug for FnOnceSystem<Req, F>
where
    Self: System,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Self::name())
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
            // FnMut(..) -> FnSystem<..>
            impl<F $(,$r)? $(,$w)? $(,$rr)? $(,$rw)? $(,$ew)?>
                From<F> for
                FnSystem<$req_with_placeholder, F>
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

            // FnOnce(..) -> FnOnceSystem<..>
            impl<F $(,$r)? $(,$w)? $(,$rr)? $(,$rw)? $(,$ew)?>
                From<F> for
                FnOnceSystem<$req_with_placeholder, F>
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
                        run: Some(value),
                        _marker: PhantomData,
                    }
                }
            }

            // System for FnSystem<..>
            impl<F $(,$r)? $(,$w)? $(,$rr)? $(,$rw)? $(,$ew)?>
                System for
                FnSystem<$req_with_placeholder, F>
            where
                F: FnMut(
                    $(Read<$r>,)?
                    $(Write<$w>,)?
                    $(ResRead<$rr>,)?
                    $(ResWrite<$rw>,)?
                    $(EntWrite<$ew>,)?
                ) + Send + 'static,
                $($r: Query,)?
                $($w: QueryMut,)?
                $($rr: ResQuery,)?
                $($rw: ResQueryMut,)?
                $($ew: EntQueryMut,)?
            {
                type Request = $req_with_tuple;

                fn run(&mut self, _resp: Response<Self::Request>) {
                    (self.run)(
                        $(Read::<'_, $r>(_resp.read),)?
                        $(Write::<'_, $w>(_resp.write),)?
                        $(ResRead::<'_, $rr>(_resp.res_read),)?
                        $(ResWrite::<'_, $rw>(_resp.res_write),)?
                        $(EntWrite::<$ew>(_resp.ent_write),)?
                    )
                }

                fn name() -> &'static str {
                    std::any::type_name::<F>()
                }
            }

            // System for FnOnceSystem<..>
            impl<F $(,$r)? $(,$w)? $(,$rr)? $(,$rw)? $(,$ew)?>
                System for
                FnOnceSystem<$req_with_placeholder, F>
            where
                F: FnOnce(
                    $(Read<$r>,)?
                    $(Write<$w>,)?
                    $(ResRead<$rr>,)?
                    $(ResWrite<$rw>,)?
                    $(EntWrite<$ew>,)?
                ) + Send + 'static,
                $($r: Query,)?
                $($w: QueryMut,)?
                $($rr: ResQuery,)?
                $($rw: ResQueryMut,)?
                $($ew: EntQueryMut,)?
            {
                type Request = $req_with_tuple;

                fn run(&mut self, _resp: Response<Self::Request>) {
                    if let Some(run) = self.run.take() {
                        (run)(
                            $(Read::<'_, $r>(_resp.read),)?
                            $(Write::<'_, $w>(_resp.write),)?
                            $(ResRead::<'_, $rr>(_resp.res_read),)?
                            $(ResWrite::<'_, $rw>(_resp.res_write),)?
                            $(EntWrite::<$ew>(_resp.ent_write),)?
                        )
                    } else {
                        panic!(
                            "unable to call `{}` twice, which implements FnOnce",
                            std::any::type_name::<F>()
                        );
                    }
                }

                fn name() -> &'static str {
                    std::any::type_name::<F>()
                }
            }

            // FnMut<..> -> SystemBond<..>
            impl<F $(,$r)? $(,$w)? $(,$rr)? $(,$rw)? $(,$ew)?>
                From<F> for
                SystemBond<FnSystem<$req_with_placeholder, F>>
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
                    Self(value.into())
                }
            }

            // FnMut<..> -> SystemDesc<..>
            impl<F $(,$r)? $(,$w)? $(,$rr)? $(,$rw)? $(,$ew)?>
                From<F> for
                SystemDesc<FnSystem<$req_with_placeholder, F>>
            where
                F: FnMut(
                    $(Read<$r>,)?
                    $(Write<$w>,)?
                    $(ResRead<$rr>,)?
                    $(ResWrite<$rw>,)?
                    $(EntWrite<$ew>,)?
                ) + Send + 'static,
                $($r: Query,)?
                $($w: QueryMut,)?
                $($rr: ResQuery,)?
                $($rw: ResQueryMut,)?
                $($ew: EntQueryMut,)?
            {
                fn from(value: F) -> Self {
                    SystemDesc::new().with_system(value)
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

/// A system wrapper to help functions to accept various types of systems.
/// This type doesn't do anything rather than becoming inner system.
//
// When it come to use cases,
// imagine that we'd like to accept 'struct' or 'closure' systems
// in a function like this,
// - fn register<S: System>(s: s) {}
//
// However, to implement `System` for every possible 'closure',
// it must stop by `FnSystem` for blanket impl.
// That's why we need a type to bond 'struct' and 'closure' together.
// Conversions of 'struct' and 'closure' are as follows.
// - FnMut -> FnSystem: System (for blanket impl) -> SystemBond
// - Struct: System -------------------------------> SystemBond
//
// So, we can modify our function like this,
// - fn register<S: Into<SystemBond>>(s: S) {}
#[derive(Debug)]
#[repr(transparent)]
pub struct SystemBond<S>(S);

impl<S> SystemBond<S> {
    pub(crate) fn into_inner(self) -> S {
        self.0
    }
}

impl<S: System> From<S> for SystemBond<S> {
    fn from(value: S) -> Self {
        Self(value)
    }
}

/// Monotonically increasing counter. When scheduling occurs, this counter
/// increases by 1.
pub type Tick = u64;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_systemgroup_unregister_volatile() {
        let mut sgroup = SystemGroup::<std::hash::RandomState>::new(0);

        // Registers an inactive & volatile system.
        // Active: 0, Inactive: 1, Volatile: 1
        let mut sdata: SystemData = ().into_data();
        let sid = sgroup.next_system_id();
        sdata.id = sid;
        sgroup.register(sdata, true).unwrap();
        validate_len(&sgroup, 0, 1, 1);

        // Unregisters the system.
        // Active: 0, Inactive: 0, Volatile: 0
        sgroup.unregister(&sid).unwrap();
        sgroup.drain_dead();
        validate_len(&sgroup, 0, 0, 0);
    }

    #[test]
    fn test_systemgroup_mixed_operations() {
        let mut sgroup = SystemGroup::<std::hash::RandomState>::new(0);

        // Registers an inactive & non-volitile system.
        // Active: 0, Inactive: 1, Volatile: 0
        let mut sdata: SystemData = ().into_data();
        let sid_a = sgroup.next_system_id();
        sdata.id = sid_a;
        sgroup.register(sdata, false).unwrap();
        validate_len(&sgroup, 0, 1, 0);

        // Activates the system.
        // Active: 1, Inactive: 0, Volatile: 0
        sgroup.activate(&sid_a, InsertPos::Back, 1).unwrap();
        validate_len(&sgroup, 1, 0, 0);

        // Registers an inactive & volatile system.
        // Active: 1, Inactive: 1, Volatile: 1
        let mut sdata: SystemData = ().into_data();
        let sid_b = sgroup.next_system_id();
        sdata.id = sid_b;
        sgroup.register(sdata, true).unwrap();
        validate_len(&sgroup, 1, 1, 1);

        // Activates the system.
        // Active: 2, Inactive: 0, Volatile: 1
        sgroup.activate(&sid_b, InsertPos::Back, 2).unwrap();
        validate_len(&sgroup, 2, 0, 1);

        // Calls tick. Non-volatile system will be inactivated.
        // Active: 1, Inactive: 1, Volatile: 1
        sgroup.tick();
        sgroup.drain_dead();
        validate_len(&sgroup, 1, 1, 1);

        // Calls tick. Volatile system will be discarded.
        // Active: 0, Inactive: 1, Volatile: 0
        sgroup.tick();
        sgroup.drain_dead();
        validate_len(&sgroup, 0, 1, 0);

        // Unregisters the remained system.
        // Active: 0, Inactive: 0, Volatile: 0
        sgroup.unregister(&sid_a).unwrap();
        sgroup.drain_dead();
        validate_len(&sgroup, 0, 0, 0);
    }

    fn validate_len<S>(sgroup: &SystemGroup<S>, act: usize, inact: usize, vol: usize)
    where
        S: BuildHasher + Default,
    {
        assert_eq!(sgroup.active.len(), act);
        assert_eq!(sgroup.inactive.len(), inact);
        assert_eq!(sgroup.volatile.len(), vol);
    }
}
