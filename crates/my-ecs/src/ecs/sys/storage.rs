use super::system::{PoisonedSystem, SystemGroup, SystemId};
use crate::{
    MAX_GROUP,
    ds::Array,
    ecs::{
        EcsError,
        sys::system::{InsertPos, SystemData, Tick},
    },
};
use std::hash::BuildHasher;

#[derive(Debug)]
pub(crate) struct SystemStorage<S> {
    pub(crate) sgroups: Array<SystemGroup<S>, MAX_GROUP>,
}

impl<S> SystemStorage<S>
where
    S: BuildHasher + Default + 'static,
{
    pub(crate) fn new(num_groups: usize) -> Self {
        assert!(num_groups <= MAX_GROUP);

        let mut sgroups = Array::new();
        for gi in 0..num_groups {
            sgroups.push(SystemGroup::new(gi as u16));
        }

        Self { sgroups }
    }

    pub(crate) fn num_groups(&self) -> usize {
        self.sgroups.len()
    }

    pub(crate) fn get_group_mut(&mut self, gi: usize) -> &mut SystemGroup<S> {
        &mut self.sgroups[gi]
    }

    pub(crate) fn register(
        &mut self,
        sdata: SystemData,
        volatile: bool,
    ) -> Result<(), EcsError<SystemData>> {
        // Identifier and flags of the system must be valid here.
        debug_assert!(!sdata.id().is_dummy());
        debug_assert!(!sdata.flags().is_empty());

        let gi = sdata.id().group_index() as usize;
        self.sgroups[gi].register(sdata, volatile)
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
                let reason =
                    "tried to activate a system after a system belonging to different group";
                return Err(EcsError::UnknownSystem(reason.to_owned(), ()));
            }
        }

        let gi = target.group_index() as usize;
        self.sgroups[gi].activate(target, at, live)
    }

    pub(crate) fn unregister(&mut self, sid: &SystemId) -> Result<(), EcsError> {
        let gi = sid.group_index() as usize;
        self.sgroups[gi].unregister(sid)
    }

    pub(crate) fn inactivate(&mut self, sid: &SystemId) -> Result<(), EcsError> {
        let gi = sid.group_index() as usize;
        self.sgroups[gi].inactivate(sid)
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
