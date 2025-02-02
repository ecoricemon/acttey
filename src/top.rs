use crate::AppHasher;
use my_ecs::prelude::*;
use std::error::Error;

#[derive(Debug)]
pub struct Acttey {
    ecs: EcsApp<Worker, AppHasher>,
}

impl Acttey {
    pub fn new() -> Self {
        let pool = WorkerPool::with_all_cpus();
        let total = pool.len();
        Self::_new(pool.into(), &[total][..])
    }
}

impl Acttey {
    pub fn new_as<G>(groups: G) -> Self
    where
        G: AsRef<[usize]>,
    {
        let groups = groups.as_ref();
        let total = groups.iter().sum();
        let pool = WorkerPool::with_len(total);
        Self::_new(pool.into(), groups)
    }

    fn _new(workers: Vec<Worker>, groups: &[usize]) -> Self {
        debug_assert_eq!(workers.len(), groups.iter().sum::<usize>());

        let ecs = Ecs::create(workers, groups);
        Self { ecs }
    }

    pub fn step(&mut self) -> &mut Self {
        self.ecs.step();
        self
    }

    pub fn run(&mut self) -> &mut Self {
        self.ecs.run();
        self
    }
}

impl EcsEntry for Acttey {
    fn add_system<T, Sys>(
        &mut self,
        desc: T,
    ) -> WithResult<&mut Self, SystemId, EcsError<SystemDesc<Sys>>>
    where
        T: Into<SystemDesc<Sys>>,
        Sys: System,
    {
        let res = self.ecs.add_system(desc).take();
        WithResult::new(self, res)
    }

    fn add_once_system<T, Req, F>(
        &mut self,
        sys: T,
    ) -> WithResult<&mut Self, SystemId, EcsError<SystemDesc<FnOnceSystem<Req, F>>>>
    where
        T: Into<FnOnceSystem<Req, F>>,
        FnOnceSystem<Req, F>: System,
    {
        let res = self.ecs.add_once_system(sys).take();
        WithResult::new(self, res)
    }

    fn unregister_system(&mut self, sid: SystemId) -> WithResult<&mut Self, (), EcsError> {
        let res = self.ecs.unregister_system(sid).take();
        WithResult::new(self, res)
    }

    fn activate_system(
        &mut self,
        target: SystemId,
        at: InsertPos,
        live: Tick,
    ) -> WithResult<&mut Self, (), EcsError> {
        let res = self.ecs.activate_system(target, at, live).take();
        WithResult::new(self, res)
    }

    fn inactivate_system(&mut self, sid: SystemId) -> WithResult<&mut Self, (), EcsError> {
        let res = self.ecs.inactivate_system(sid).take();
        WithResult::new(self, res)
    }

    fn register_entity(&mut self, desc: EntityReg) -> WithResult<&mut Self, EntityIndex, EcsError> {
        let res = self.ecs.register_entity(desc).take();
        WithResult::new(self, res)
    }

    fn unregister_entity<C>(&mut self) -> WithResult<&mut Self, Box<dyn ContainEntity>, EcsError>
    where
        C: Components,
    {
        let res = self.ecs.unregister_entity::<C>().take();
        WithResult::new(self, res)
    }

    fn add_entity<E>(
        &mut self,
        ei: EntityIndex,
        value: E,
    ) -> WithResult<&mut Self, EntityId, EcsError<E>>
    where
        E: Entity,
    {
        let res = self.ecs.add_entity(ei, value).take();
        WithResult::new(self, res)
    }

    fn remove_entity(&mut self, eid: EntityId) -> WithResult<&mut Self, (), EcsError> {
        let res = self.ecs.remove_entity(eid).take();
        WithResult::new(self, res)
    }

    fn add_resource<T>(
        &mut self,
        desc: T,
    ) -> WithResult<&mut Self, ResourceIndex, EcsError<ResourceDesc>>
    where
        T: Into<ResourceDesc>,
    {
        let res = self.ecs.add_resource(desc).take();
        WithResult::new(self, res)
    }

    fn remove_resource<R>(&mut self) -> WithResult<&mut Self, Option<R>, EcsError>
    where
        R: Resource,
    {
        let res = self.ecs.remove_resource::<R>().take();
        WithResult::new(self, res)
    }

    fn get_resource<R>(&self) -> Option<&R>
    where
        R: Resource,
    {
        self.ecs.get_resource()
    }

    fn get_resource_mut<R>(&mut self) -> Option<&mut R>
    where
        R: Resource,
    {
        self.ecs.get_resource_mut()
    }

    fn get_resource_index<R>(&self) -> Option<ResourceIndex>
    where
        R: Resource,
    {
        self.ecs.get_resource_index::<R>()
    }

    fn execute_commands<T>(
        &mut self,
        cmds: T,
    ) -> WithResult<&mut Self, (), Box<dyn Error + Send + Sync + 'static>>
    where
        T: HelpExecuteManyCommands,
    {
        let res = self.ecs.execute_commands(cmds).take();
        WithResult::new(self, res)
    }

    fn execute_command<F, R>(
        &mut self,
        f: F,
    ) -> WithResult<&mut Self, (), Box<dyn Error + Send + Sync + 'static>>
    where
        F: FnOnce(Commander) -> R,
        R: Command,
    {
        let res = self.ecs.execute_command(f).take();
        WithResult::new(self, res)
    }
}

impl Default for Acttey {
    fn default() -> Self {
        Self::new()
    }
}
