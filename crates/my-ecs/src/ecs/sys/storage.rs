use super::system::{PoisonedSystem, SystemGroup, SystemId};
use crate::ecs::{
    sys::system::{InsertPos, SystemData, Tick},
    EcsError,
};
use crate::util::prelude::*;
use std::{array, hash::BuildHasher};

/// * `S` - Hasher.
/// * `N` - Number of [`SystemGroup`], which operates in a different configurable way from each other.
#[derive(Debug)]
pub(crate) struct SystemStorage<S, const N: usize> {
    pub(crate) sgroups: Multi<SystemGroup<S>, N>,
}

impl<S, const N: usize> SystemStorage<S, N>
where
    S: BuildHasher + Default + 'static,
{
    pub(crate) fn new() -> Self {
        // For now, group index `gi` below is limited up to u16::MAX - 1 by `SystemId`.
        // Here, we check N in terms of bounds at compile time.
        let _: () = const { assert!(N < SystemId::max_group_index() as usize) };

        let sgroups = array::from_fn(|gi| SystemGroup::new(gi as u16));

        Self {
            sgroups: Multi::new(sgroups),
        }
    }

    pub(crate) fn get_group_mut(&mut self, gi: usize) -> &mut SystemGroup<S> {
        self.sgroups.switch_to(gi)
    }

    pub(crate) fn register(
        &mut self,
        sdata: SystemData,
        volatile: bool,
    ) -> Result<(), EcsError<SystemData>> {
        // Id and flags of the system must be valid here.
        debug_assert!(!sdata.id().is_dummy());
        debug_assert!(!sdata.flags().is_empty());

        self.sgroups
            .switch_to(sdata.id().group_index() as usize)
            .register(sdata, volatile)
    }

    /// Activates the system. If the system is already active, nothing takes place.
    pub(crate) fn activate(
        &mut self,
        target: &SystemId,
        at: InsertPos,
        live: Tick,
    ) -> Result<(), EcsError> {
        // For better error message.
        #[cfg(debug_assertions)]
        if let InsertPos::After(at) = &at {
            if target.group_index() != at.group_index() {
                let reason = debug_format!(
                    "tried to activate a system after a system belonging to different group"
                );
                return Err(EcsError::UnknownSystem(reason, ()));
            }
        }

        self.sgroups
            .switch_to(target.group_index() as usize)
            .activate(target, at, live)
    }

    pub(crate) fn unregister(&mut self, sid: &SystemId) -> Result<(), EcsError> {
        self.sgroups
            .switch_to(sid.group_index() as usize)
            .unregister(sid)
    }

    pub(crate) fn inactivate(&mut self, sid: &SystemId) -> Result<(), EcsError> {
        self.sgroups
            .switch_to(sid.group_index() as usize)
            .inactivate(sid)
    }

    pub(crate) fn drain_dead(&mut self) -> impl Iterator<Item = SystemData> + '_ {
        self.sgroups
            .iter_mut()
            .flat_map(|sgroup| sgroup.drain_dead())
    }

    pub(crate) fn drain_poisoned(&mut self) -> impl Iterator<Item = PoisonedSystem> + '_ {
        self.sgroups
            .iter_mut()
            .flat_map(|sgroup| sgroup.drain_poisoned())
    }
}
