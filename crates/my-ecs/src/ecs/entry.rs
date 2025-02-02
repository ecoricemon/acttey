use super::{
    cache::{CacheStorage, RefreshCacheStorage},
    cmd::{Command, Commander},
    ent::{
        component::Components,
        entity::{ContainEntity, Entity, EntityId, EntityIndex, EntityKeyRef},
        storage::{AsEntityReg, EntityContainer, EntityReg, EntityStorage},
    },
    resource::{Resource, ResourceDesc, ResourceIndex, ResourceKey, ResourceStorage},
    sched::{
        comm::{command_channel, CommandReceiver, CommandSender},
        ctrl::Scheduler,
    },
    share::Shared,
    sys::{
        storage::SystemStorage,
        system::{
            FnOnceSystem, InsertPos, PoisonedSystem, System, SystemData, SystemDesc, SystemFlags,
            SystemId, Tick,
        },
    },
    worker::Work,
    DynResult, EcsError,
};
use crate::util::{macros::debug_format, Or, WithResult};
use my_ecs_macros::repeat_macro;
use std::{
    any::Any,
    error::Error,
    fmt::Debug,
    hash::{BuildHasher, RandomState},
    marker::PhantomData,
    mem,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    rc::Rc,
    sync::Arc,
    thread,
};

pub mod prelude {
    pub use super::{Ecs, EcsApp, EcsEntry, EcsExt, HelpExecuteManyCommands, LeakedEcsApp};
}

/// Common interafaces that ECS instance should provide to clients.
///
/// ECS instance should provide some methods for adding/removing entities,
/// resources, and systems. Of cource compoenets are included in entities.
/// Moreover, ECS insance is required to be able to execute commands without
/// wrapping them in systems.
pub trait EcsEntry {
    // === System methods ===

    /// Adds the given systems.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// fn system0() { /* ... */ }
    /// fn system1() { /* ... */ }
    ///
    /// Ecs::default(WorkerPool::new(), [])
    ///     .add_systems((system0, system1))
    ///     .unwrap();
    /// ```
    fn add_systems<T, Systems>(&mut self, descs: T) -> WithResult<&mut Self, (), EcsError>
    where
        T: HelpAddManySystems<Systems>,
    {
        descs.add_systems(self)
    }

    /// Adds the given system.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// fn system() { /* ... */ }
    ///
    /// Ecs::default(WorkerPool::new(), [])
    ///     .add_system(system)
    ///     .unwrap();
    /// ```
    fn add_system<T, Sys>(
        &mut self,
        desc: T,
    ) -> WithResult<&mut Self, SystemId, EcsError<SystemDesc<Sys>>>
    where
        T: Into<SystemDesc<Sys>>,
        Sys: System;

    /// Adds the given [`FnOnce`] systems.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// let s = "string0".to_owned();
    /// let system0 = move || { drop(s); };
    /// let s = "string1".to_owned();
    /// let system1 = move || { drop(s); };
    ///
    /// Ecs::default(WorkerPool::new(), [])
    ///     .add_once_systems((system0, system1))
    ///     .unwrap();
    /// ```
    fn add_once_systems<T, Once>(&mut self, descs: T) -> WithResult<&mut Self, (), EcsError>
    where
        T: HelpAddManyOnce<Once>,
    {
        descs.add_once_systems(self)
    }

    /// Adds the given [`FnOnce`] system.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// let s = "string0".to_owned();
    /// let system = move || { drop(s); };
    ///
    /// Ecs::default(WorkerPool::new(), [])
    ///     .add_once_system(system)
    ///     .unwrap();
    /// ```
    fn add_once_system<T, Req, F>(
        &mut self,
        sys: T,
    ) -> WithResult<&mut Self, SystemId, EcsError<SystemDesc<FnOnceSystem<Req, F>>>>
    where
        T: Into<FnOnceSystem<Req, F>>,
        FnOnceSystem<Req, F>: System;

    /// Unregisters an inactive system for the given system id.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// let mut ecs = Ecs::default(WorkerPool::new(), []);
    ///
    /// // Adds an inactive empty system.
    /// let desc = SystemDesc::new().with_activation(0, InsertPos::Back);
    /// let sid = ecs.add_system(desc).unwrap();
    ///
    /// let res = ecs.unregister_system(sid);
    /// assert!(res.is_ok());
    /// ```
    fn unregister_system(&mut self, sid: SystemId) -> WithResult<&mut Self, (), EcsError>;

    /// Activates a system for the given system id if it's not active.
    ///
    /// If the system is already active, returns error.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// let mut ecs = Ecs::default(WorkerPool::new(), []);
    ///
    /// // Active system cannot be activated again.
    /// let sid = ecs.add_system(|| { /* ... */ }).unwrap();
    /// let res = ecs.activate_system(sid, InsertPos::Back, 2);
    /// assert!(res.is_err());
    ///
    /// // Adds an inactive empty system.
    /// let desc = SystemDesc::new().with_activation(0, InsertPos::Back);
    /// let sid = ecs.add_system(desc).unwrap();
    /// let res = ecs.activate_system(sid, InsertPos::Back, 2);
    /// assert!(res.is_ok());
    /// ```
    fn activate_system(
        &mut self,
        target: SystemId,
        at: InsertPos,
        live: Tick,
    ) -> WithResult<&mut Self, (), EcsError>;

    /// Inactivates a system for the given system id.
    ///
    /// If the system is already inactive, nothing takes place and returns Ok.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// let mut ecs = Ecs::default(WorkerPool::new(), []);
    ///
    /// // Inactivates an inactive system takes no effect.
    /// let desc = SystemDesc::new().with_activation(0, InsertPos::Back);
    /// let sid = ecs.add_system(desc).unwrap();
    /// let res = ecs.inactivate_system(sid);
    /// assert!(res.is_ok());
    ///
    /// // Inactivates an active system.
    /// let sid = ecs.add_system(|| { /* ... */ }).unwrap();
    /// let res = ecs.inactivate_system(sid);
    /// assert!(res.is_ok());
    /// ```
    fn inactivate_system(&mut self, sid: SystemId) -> WithResult<&mut Self, (), EcsError>;

    // === Entity methods ===

    /// Registers an entity type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Entity)] struct E { c: C }
    /// #[derive(Component)] struct C;
    ///
    /// Ecs::default(WorkerPool::new(), [])
    ///     .register_entity_of::<E>()
    ///     .unwrap();
    /// ```
    fn register_entity_of<T>(&mut self) -> WithResult<&mut Self, EntityIndex, EcsError>
    where
        T: AsEntityReg,
    {
        self.register_entity(T::entity_descriptor())
    }

    /// Registers an entity type from the given descriptor.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Component)] struct C;
    ///
    /// let mut desc = EntityReg::new(
    ///     Some(EntityName::new("my-entity".into())),
    ///     Box::new(SparseSet::<std::hash::RandomState>::new()),
    /// );
    /// desc.add_component_of::<C>();
    ///
    /// Ecs::default(WorkerPool::new(), [])
    ///     .register_entity(desc)
    ///     .unwrap();
    /// ```
    fn register_entity(&mut self, desc: EntityReg) -> WithResult<&mut Self, EntityIndex, EcsError>;

    /// Unregisters an entity type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Entity)] struct E { a: Ca, b: Cb }
    /// #[derive(Component)] struct Ca;
    /// #[derive(Component)] struct Cb;
    ///
    /// let mut ecs = Ecs::default(WorkerPool::new(), []);
    ///
    /// // You can unregister an entity type using entity type itself.
    /// let res = ecs
    ///     .register_entity_of::<E>()
    ///     .unregister_entity::<E>()
    ///     .take();
    /// assert!(res.is_ok());
    ///
    /// // Entity can be also identified by a combination of component types.
    /// let res = ecs
    ///     .register_entity_of::<E>()
    ///     .unregister_entity::<(Ca, Cb)>()
    ///     .take();
    /// assert!(res.is_ok());
    /// ```
    fn unregister_entity<C>(&mut self) -> WithResult<&mut Self, Box<dyn ContainEntity>, EcsError>
    where
        C: Components;

    /// Adds an entity.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Entity)] struct E { c: C }
    /// #[derive(Component)] struct C;
    ///
    /// let mut ecs = Ecs::default(WorkerPool::new(), []);
    /// let ei = ecs.register_entity_of::<E>().unwrap();
    /// ecs.add_entity(ei, E { c: C }).unwrap();
    /// ```
    fn add_entity<E>(
        &mut self,
        ei: EntityIndex,
        value: E,
    ) -> WithResult<&mut Self, EntityId, EcsError<E>>
    where
        E: Entity;

    /// Removes an entity.
    ///
    /// # Example
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Entity)] struct E { c: C }
    /// #[derive(Component)] struct C;
    ///
    /// let mut ecs = Ecs::default(WorkerPool::new(), []);
    /// let ei = ecs.register_entity_of::<E>().unwrap();
    /// let eid = ecs.add_entity(ei, E { c: C }).unwrap();
    /// let res = ecs.remove_entity(eid);
    /// assert!(res.is_ok());
    /// ```
    fn remove_entity(&mut self, eid: EntityId) -> WithResult<&mut Self, (), EcsError>;

    // === Resource methods ===

    /// Adds the given resources.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Resource)] struct Ra(i32);
    /// #[derive(Resource)] struct Rb(i32);
    ///
    /// Ecs::default(WorkerPool::new(), [])
    ///     .add_resources((Ra(0), Rb(1)))
    ///     .unwrap();
    /// ```
    fn add_resources<T>(&mut self, descs: T) -> WithResult<&mut Self, (), EcsError>
    where
        T: HelpAddManyResources,
    {
        descs.add_resources(self)
    }

    /// Adds the given resource.
    ///
    /// If the same resource type was already added, returns error.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Resource)] struct Ra;
    /// #[derive(Resource)] struct Rb(i32);
    ///
    /// let mut ecs = Ecs::default(WorkerPool::new(), []);
    ///
    /// // Adds an owned resource.
    /// let res = ecs.add_resource(Ra);
    /// assert!(res.is_ok());
    ///
    /// // You cannot register the same resource type.
    /// let res = ecs.add_resource(Ra);
    /// assert!(res.is_err());
    ///
    /// // Adds a not owned resource.
    /// let mut r = Rb(0);
    /// let ptr = &mut r as *mut Rb;
    /// let desc = unsafe { ResourceDesc::new().with_ptr(ptr) };
    /// let res = ecs.add_resource(desc);
    /// assert!(res.is_ok());
    /// ```
    fn add_resource<T>(
        &mut self,
        desc: T,
    ) -> WithResult<&mut Self, ResourceIndex, EcsError<ResourceDesc>>
    where
        T: Into<ResourceDesc>;

    /// Removes a resource from the given resource type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Resource, Debug, PartialEq)]
    /// struct R(i32);
    ///
    /// let mut ecs = Ecs::default(WorkerPool::new(), []);
    ///
    /// let res = ecs
    ///     .add_resource(R(42))
    ///     .remove_resource::<R>()
    ///     .unwrap();
    /// assert_eq!(res, Some(R(42)));
    /// ```
    fn remove_resource<R>(&mut self) -> WithResult<&mut Self, Option<R>, EcsError>
    where
        R: Resource;

    /// Retrieves shared reference to a resource for the given resource type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Resource)] struct R(i32);
    ///
    /// let mut ecs = Ecs::default(WorkerPool::new(), []);
    ///
    /// ecs.add_resource(R(42)).unwrap();
    /// let r = ecs.get_resource::<R>().unwrap();
    /// assert_eq!(r.0, 42);
    /// ```
    fn get_resource<R>(&self) -> Option<&R>
    where
        R: Resource;

    /// Retrieves mutable reference to a resource for the given resource type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Resource)] struct R(i32);
    ///
    /// let mut ecs = Ecs::default(WorkerPool::new(), []);
    ///
    /// ecs.add_resource(R(42)).unwrap();
    /// let r = ecs.get_resource_mut::<R>().unwrap();
    /// r.0 = 43;
    ///
    /// let r = ecs.get_resource_mut::<R>().unwrap();
    /// assert_eq!(r.0, 43);
    ///
    /// ```
    fn get_resource_mut<R>(&mut self) -> Option<&mut R>
    where
        R: Resource;

    /// Returns resource index for the given resource type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Resource)] struct R;
    ///
    /// let mut ecs = Ecs::default(WorkerPool::new(), []);
    ///
    /// let ei = ecs.add_resource(R).unwrap();
    /// let found = ecs.get_resource_index::<R>().unwrap();
    /// assert_eq!(found, ei);
    /// ```
    fn get_resource_index<R>(&self) -> Option<ResourceIndex>
    where
        R: Resource;

    // === Command methods ===

    /// Executes the given commands in order.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// let cmd0 = |ecs: Ecs| -> DynResult<()> { /* ... */ Ok(()) };
    /// let cmd1 = |ecs: Ecs| -> DynResult<()> { /* ... */ Ok(()) };
    ///
    /// Ecs::default(WorkerPool::new(), [])
    ///     .execute_commands((cmd0, cmd1))
    ///     .unwrap();
    /// ```
    fn execute_commands<T>(
        &mut self,
        cmds: T,
    ) -> WithResult<&mut Self, (), Box<dyn Error + Send + Sync + 'static>>
    where
        T: HelpExecuteManyCommands;

    /// Execute the given command.
    ///
    /// # Example
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Entity)] struct E { a: Ca }
    /// #[derive(Component)] struct Ca;
    /// #[derive(Component)] struct Cb;
    ///
    /// let mut ecs = Ecs::default(WorkerPool::new(), []);
    /// let ei = ecs.register_entity_of::<E>().unwrap();
    /// let eid = ecs.add_entity(ei, E { a: Ca }).unwrap();
    ///
    /// // Attaches `Cb` to `E` so that it's now (Ca, Cb);
    /// ecs.execute_command(move |cmdr| cmdr.entity(eid).attach(Cb).finish())
    ///     .unwrap();
    ///
    /// // We can unregister (Ca, Cb).
    /// let res = ecs.unregister_entity::<(Ca, Cb)>();
    /// assert!(res.is_ok());
    /// ```
    fn execute_command<F, R>(
        &mut self,
        f: F,
    ) -> WithResult<&mut Self, (), Box<dyn Error + Send + Sync + 'static>>
    where
        F: FnOnce(Commander) -> R,
        R: Command;
}

/// A helper trait for [`EcsEntry::add_systems`].
///
/// This helper trait is implemented for tuple of anonymous systems.
pub trait HelpAddManySystems<Systems> {
    fn add_systems<Ecs>(self, ecs: &mut Ecs) -> WithResult<&mut Ecs, (), EcsError>
    where
        Ecs: EcsEntry + ?Sized;
}

/// Implements [`HelpAddManySystems`] for tuple of systems.
macro_rules! impl_help_add_many_systems {
    ($n:expr, $($i:expr),*) => {
        paste::paste! {
            #[allow(unused_parens, non_snake_case)]
            impl<$([<A $i>], [<S $i>]),*> HelpAddManySystems<( $([<S $i>]),* )>
                for ( $([<A $i>]),* )
            where
            $(
                [<A $i>]: Into<SystemDesc<[<S $i>]>>,
                [<S $i>]: System,
            )*
            {
                fn add_systems<Ecs>(self, ecs: &mut Ecs) -> WithResult<&mut Ecs, (), EcsError>
                where
                    Ecs: EcsEntry + ?Sized,
                {
                    let ( $([<A $i>]),* ) = self;

                    $(
                        match ecs.add_system([<A $i>]).take() {
                            Ok(_) => {}
                            Err(e) => return WithResult::new(ecs, Err(e.without_data())),
                        }
                    )*

                    WithResult::new(ecs, Ok(()))
                }
            }
        }
    };
}
repeat_macro!(impl_help_add_many_systems, 1..=8);

/// A helper trait for [`EcsEntry::add_once_systems`].
///
/// This helper trait is implemented for tuple of anonymous once systems.
pub trait HelpAddManyOnce<Once> {
    fn add_once_systems<Ecs>(self, ecs: &mut Ecs) -> WithResult<&mut Ecs, (), EcsError>
    where
        Ecs: EcsEntry + ?Sized;
}

/// Implements [`HelpAddManyOnce`] for tuple of once systems.
macro_rules! impl_help_add_many_once {
    ($n:expr, $($i:expr),*) => {
        paste::paste! {
            #[allow(unused_parens, non_snake_case)]
            impl<$([<A $i>], [<R $i>], [<S $i>]),*> HelpAddManyOnce<( $( [<R $i>], [<S $i>] ),* )>
                for ( $([<A $i>]),* )
            where
            $(
                [<A $i>]: Into<FnOnceSystem<[<R $i>], [<S $i>]>>,
                FnOnceSystem<[<R $i>], [<S $i>]>: System,
            )*
            {
                fn add_once_systems<Ecs>(self, ecs: &mut Ecs) -> WithResult<&mut Ecs, (), EcsError>
                where
                    Ecs: EcsEntry + ?Sized,
                {
                    let ( $([<A $i>]),* ) = self;

                    $(
                        match ecs.add_once_system([<A $i>]).take() {
                            Ok(_) => {}
                            Err(e) => return WithResult::new(ecs, Err(e.without_data())),
                        }
                    )*

                    WithResult::new(ecs, Ok(()))
                }
            }
        }
    };
}
repeat_macro!(impl_help_add_many_once, 1..=8);

/// A helper trait for [`EcsEntry::add_resources`].
///
/// This helper trait is implemented for tuple of anonymous resources.
pub trait HelpAddManyResources {
    fn add_resources<Ecs>(self, ecs: &mut Ecs) -> WithResult<&mut Ecs, (), EcsError>
    where
        Ecs: EcsEntry + ?Sized;
}

/// Implements [`HelpAddManyResources`] for tuple of resources.
macro_rules! impl_help_add_many_resources {
    ($n:expr, $($i:expr),*) => {
        paste::paste! {
            #[allow(unused_parens, non_snake_case)]
            impl<$([<A $i>]),*> HelpAddManyResources for ( $([<A $i>]),* )
            where
            $(
                [<A $i>]: Into<ResourceDesc>,
            )*
            {
                fn add_resources<Ecs>(
                    self,
                    ecs: &mut Ecs
                ) -> WithResult<&mut Ecs, (), EcsError>
                where
                    Ecs: EcsEntry + ?Sized,
                {
                    let ( $([<A $i>]),* ) = self;

                    $(
                        match ecs.add_resource([<A $i>]).take() {
                            Ok(_) => {}
                            Err(e) => return WithResult::new(ecs, Err(e.without_data())),
                        }
                    )*

                    WithResult::new(ecs, Ok(()))
                }
            }
        }
    };
}
repeat_macro!(impl_help_add_many_resources, 1..=8);

/// A helper trait for [`EcsEntry::execute_commands`].
///
/// This helper trait is implemented for tuple of anonymous commands.
pub trait HelpExecuteManyCommands {
    fn command(&mut self, ecs: Ecs<'_>) -> DynResult<()>;
}

/// Implements [`HelpExecuteManyCommands`] for tuple of commands.
macro_rules! impl_help_execute_many_commands {
    (1, 0) => {
        impl<A0: Command> HelpExecuteManyCommands for A0 {
            fn command(&mut self, ecs: Ecs<'_>) -> DynResult<()> {
                self.command(ecs)
            }
        }
    };
    ($n:expr, $($i:expr),*) => {
        paste::paste! {
            #[allow(unused_parens)]
            impl<$([<A $i>]: Command),*> HelpExecuteManyCommands for ( $([<A $i>]),* ) {
                fn command(&mut self, ecs: Ecs<'_>) -> DynResult<()> {
                    $(
                        match self.$i.command(unsafe { ecs.copy() }) {
                            Ok(()) => {}
                            Err(e) => return Err(e),
                        }
                    )*
                    Ok(())
                }
            }
        }
    };
}
repeat_macro!(impl_help_execute_many_commands, 1..=8);

/// Internal function pointer table of [`EcsApp`].
/// 
/// # Why we need function pointer
/// 
/// To declare [`Command`], clients need to put `EcsApp` as a parameter in their
/// functions. But `EcsApp` is too verbose. So it is where [`Ecs`] comes in to
/// play. `Ecs` doesn't have complex generic parameters so clients can easily
/// remember and use it. This vtable helps `Ecs` to call functions in `EcsApp`.
#[rustfmt::skip]
#[allow(clippy::type_complexity)]
#[derive(Debug)]
struct EcsVTable {
    // === System methods ===

    register_system_inner:
        unsafe fn(NonNull<u8>, SystemData, u16, bool, bool)
        -> Result<SystemId, EcsError<SystemData>>,

    unregister_system_inner:
        unsafe fn(NonNull<u8>, &SystemId) -> Result<(), EcsError>,

    activate_system_inner:
        unsafe fn(NonNull<u8>, SystemId, InsertPos, Tick)
        -> Result<(), EcsError>,

    inactivate_system_inner:
        unsafe fn(NonNull<u8>, &SystemId) -> Result<(), EcsError>,

    // === Entity methods ===

    register_entity_inner:
        unsafe fn(NonNull<u8>, EntityReg) -> Result<EntityIndex, EcsError>,

    unregister_entity_inner:
        unsafe fn(NonNull<u8>, EntityKeyRef<'_>) -> Result<Box<dyn ContainEntity>, EcsError>,

    get_entity_container_mut:
        unsafe fn(NonNull<u8>, EntityKeyRef<'_>) -> Option<&mut EntityContainer>,

    // === Resource methods ===

    add_resource_inner:
        unsafe fn(NonNull<u8>, ResourceDesc)
        -> Result<ResourceIndex, EcsError<ResourceDesc>>,

    remove_resource_inner:
        unsafe fn(NonNull<u8>, &ResourceKey)
        -> Result<Option<Box<dyn Any>>, EcsError>,

    get_resource_inner:
        unsafe fn(NonNull<u8>, &ResourceKey) -> Option<NonNull<u8>>,

    get_resource_mut_inner:
        unsafe fn(NonNull<u8>, &ResourceKey) -> Option<NonNull<u8>>,

    get_resource_index_inner:
        unsafe fn(NonNull<u8>, &ResourceKey) -> Option<ResourceIndex>,

    // === Etc. ===

    get_shared:
        unsafe fn(NonNull<u8>) -> NonNull<Shared>,

    step:
        unsafe fn(NonNull<u8>),
}

impl EcsVTable {
    fn new<W, S>() -> Self
    where
        W: Work + 'static,
        S: BuildHasher + Default + 'static,
    {
        unsafe fn register_system_inner<W, S>(
            this: NonNull<u8>,
            sdata: SystemData,
            group_index: u16,
            volatile: bool,
            private: bool,
        ) -> Result<SystemId, EcsError<SystemData>>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S> = unsafe { this.cast().as_mut() };
            this.register_system_inner(sdata, group_index, volatile, private)
        }

        unsafe fn unregister_system_inner<W, S>(
            this: NonNull<u8>,
            sid: &SystemId,
        ) -> Result<(), EcsError>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S> = unsafe { this.cast().as_mut() };
            this.unregister_system_inner(sid)
        }

        unsafe fn activate_system_inner<W, S>(
            this: NonNull<u8>,
            target: SystemId,
            at: InsertPos,
            live: Tick,
        ) -> Result<(), EcsError>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S> = unsafe { this.cast().as_mut() };
            this.activate_system_inner(target, at, live)
        }

        unsafe fn inactivate_system_inner<W, S>(
            this: NonNull<u8>,
            sid: &SystemId,
        ) -> Result<(), EcsError>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S> = unsafe { this.cast().as_mut() };
            this.inactivate_system_inner(sid)
        }

        unsafe fn register_entity_inner<W, S>(
            this: NonNull<u8>,
            desc: EntityReg,
        ) -> Result<EntityIndex, EcsError>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S> = unsafe { this.cast().as_mut() };
            this.register_entity_inner(desc)
        }

        unsafe fn unregister_entity_inner<W, S>(
            this: NonNull<u8>,
            ekey: EntityKeyRef<'_>,
        ) -> Result<Box<dyn ContainEntity>, EcsError>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S> = unsafe { this.cast().as_mut() };
            this.unregister_entity_inner(ekey)
        }

        unsafe fn get_entity_container_mut<'o, W, S>(
            this: NonNull<u8>,
            ekey: EntityKeyRef<'_>,
        ) -> Option<&'o mut EntityContainer>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &'o mut EcsApp<W, S> = unsafe { this.cast().as_mut() };
            this.get_entity_container_mut(ekey)
        }

        unsafe fn add_resource_inner<W, S>(
            this: NonNull<u8>,
            desc: ResourceDesc,
        ) -> Result<ResourceIndex, EcsError<ResourceDesc>>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S> = unsafe { this.cast().as_mut() };
            this.add_resource_inner(desc)
        }

        unsafe fn remove_resource_inner<W, S>(
            this: NonNull<u8>,
            rkey: &ResourceKey,
        ) -> Result<Option<Box<dyn Any>>, EcsError>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S> = unsafe { this.cast().as_mut() };
            this.remove_resource_inner(rkey)
        }

        unsafe fn get_resource_inner<W, S>(
            this: NonNull<u8>,
            rkey: &ResourceKey,
        ) -> Option<NonNull<u8>>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S> = unsafe { this.cast().as_mut() };
            this.get_resource_inner(rkey)
        }

        unsafe fn get_resource_mut_inner<W, S>(
            this: NonNull<u8>,
            rkey: &ResourceKey,
        ) -> Option<NonNull<u8>>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S> = unsafe { this.cast().as_mut() };
            this.get_resource_mut_inner(rkey)
        }

        unsafe fn get_resource_index_inner<W, S>(
            this: NonNull<u8>,
            rkey: &ResourceKey,
        ) -> Option<ResourceIndex>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S> = unsafe { this.cast().as_mut() };
            this.get_resource_index_inner(rkey)
        }

        unsafe fn get_shared<W, S>(this: NonNull<u8>) -> NonNull<Shared>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S> = unsafe { this.cast().as_mut() };
            let shared = this.shared.as_ref();
            let ptr = (shared as *const Shared).cast_mut();
            unsafe { NonNull::new_unchecked(ptr) }
        }

        unsafe fn step<W, S>(this: NonNull<u8>)
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S> = unsafe { this.cast().as_mut() };
            this.step();
        }

        Self {
            register_system_inner: register_system_inner::<W, S>,
            unregister_system_inner: unregister_system_inner::<W, S>,
            activate_system_inner: activate_system_inner::<W, S>,
            inactivate_system_inner: inactivate_system_inner::<W, S>,
            register_entity_inner: register_entity_inner::<W, S>,
            unregister_entity_inner: unregister_entity_inner::<W, S>,
            get_entity_container_mut: get_entity_container_mut::<W, S>,
            add_resource_inner: add_resource_inner::<W, S>,
            remove_resource_inner: remove_resource_inner::<W, S>,
            get_resource_inner: get_resource_inner::<W, S>,
            get_resource_mut_inner: get_resource_mut_inner::<W, S>,
            get_resource_index_inner: get_resource_index_inner::<W, S>,
            get_shared: get_shared::<W, S>,
            step: step::<W, S>,
        }
    }
}

/// A handle to [`EcsApp`], which is real ecs instance.
///
/// This type is just for easy use in some cases such as writing a command. By
/// removing verbose generic parameters, clients can declare parameters as `Ecs`
/// instead of `EcsApp<W, S>` in their command functions for example.
//
// Do not implement Clone. This must be carefully cloned.
#[derive(Debug)]
pub struct Ecs<'ecs> {
    /// Type erased pointer to [`EcsApp`].
    this: NonNull<u8>,

    /// Function table of the [`EcsApp`].
    vtable: NonNull<EcsVTable>,

    /// Borrows [`EcsApp`] mutably.
    _marker: PhantomData<&'ecs mut ()>,
}

impl<'ecs> Ecs<'ecs> {
    /// Creates [`EcsApp`] with the given worker pool and group information.
    ///
    /// The returned instance uses [`RandomState`] as hasher builder for its
    /// internal data structures. If you want another, call [`Ecs::create`]
    /// instead.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// // Creates `EcsApp` with one group consisting of 4 workers.
    /// let pool = WorkerPool::with_len(4);
    /// let ecs = Ecs::default(pool, [4]);
    ///
    /// // Creates `EcsApp` with two groups consisting of 2 workers respectively.
    /// let pool = WorkerPool::with_len(4);
    /// let ecs = Ecs::default(pool, [2, 2]);
    /// ```
    pub fn default<Wp, W, G>(pool: Wp, groups: G) -> EcsApp<W, RandomState>
    where
        Wp: Into<Vec<W>>,
        W: Work + 'static,
        G: AsRef<[usize]>,
    {
        Self::create(pool.into(), groups.as_ref())
    }

    /// Creates [`EcsApp`] with the given workers, group information, and hasher
    /// builder type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::hash::{BuildHasher, DefaultHasher};
    ///
    /// // Something like `std::hash::RandomState`.
    /// #[derive(Default)] struct FixedState;
    /// impl BuildHasher for FixedState {
    ///     type Hasher = DefaultHasher;
    ///     fn build_hasher(&self) -> Self::Hasher {
    ///         DefaultHasher::new()
    ///     }
    /// }
    ///
    /// // Creates `EcsApp` with one group consisting of 4 workers.
    /// let pool = WorkerPool::with_len(4);
    /// let ecs: EcsApp<_, FixedState> = Ecs::create(pool, [4]);
    ///
    /// // Creates `EcsApp` with two groups consisting of 2 workers respectively.
    /// let pool = WorkerPool::with_len(4);
    /// let ecs: EcsApp<_, FixedState> = Ecs::create(pool, [2, 2]);
    /// ```
    pub fn create<Wp, W, G, S>(pool: Wp, groups: G) -> EcsApp<W, S>
    where
        Wp: Into<Vec<W>>,
        W: Work + 'static,
        G: AsRef<[usize]>,
        S: BuildHasher + Default + 'static,
    {
        EcsApp::new(pool.into(), groups.as_ref())
    }

    fn new<W, S>(ecs: &'ecs mut EcsApp<W, S>) -> Self
    where
        W: Work + 'static,
        S: BuildHasher + Default + 'static,
    {
        unsafe {
            let this = NonNull::new_unchecked(ecs as *mut _ as *mut u8);
            let vtable = NonNull::new_unchecked(&mut ecs.vtable as *mut _);
            Self {
                this,
                vtable,
                _marker: PhantomData,
            }
        }
    }

    /// # Safety
    ///
    /// Caller must guarantee that the returned replica will not violate pointer
    /// rules. In other words, callers must comply pointer aliasing and must not
    /// use it after free.
    pub(crate) unsafe fn copy(&self) -> Self {
        Self {
            this: self.this,
            vtable: self.vtable,
            _marker: self._marker,
        }
    }

    /// Returns pointer to a certain entity container for the given entity key.
    ///
    /// Note that you can acquire two or more entity container pointers at a
    /// time. But you must check pointer uniqueness when you're turning them
    /// into mutable references.
    pub(crate) fn entity_container_ptr(
        &self,
        ekey: EntityKeyRef<'_>,
    ) -> Option<NonNull<EntityContainer>> {
        unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.get_entity_container_mut)(self.this, ekey)
                .map(|cont| NonNull::new_unchecked(cont as *mut _))
        }
    }

    pub(crate) fn get_shared_ptr(&self) -> NonNull<Shared> {
        unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.get_shared)(self.this)
        }
    }
}

impl EcsEntry for Ecs<'_> {
    fn add_system<T, Sys>(
        &mut self,
        desc: T,
    ) -> WithResult<&mut Self, SystemId, EcsError<SystemDesc<Sys>>>
    where
        T: Into<SystemDesc<Sys>>,
        Sys: System,
    {
        let desc = desc.into();
        let SystemDesc {
            sys,
            private,
            group_index,
            volatile,
            activation: (live, insert_at),
        } = desc;
        let sdata = match sys {
            Or::A(sys) => sys.into_data(),
            Or::B(sdata) => sdata,
        };

        // Registers the system.
        let res = unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.register_system_inner)(self.this, sdata, group_index, volatile, private)
        };

        // @@@ TODO: If we failed to activate it, we have to unregister it.
        //
        // Activates the system.
        if let Ok(sid) = res.as_ref() {
            if live > 0 {
                let must_ok = self.activate_system(*sid, insert_at, live);
                assert!(must_ok.is_ok());
            }
        }

        let res = res.map_err(|err| {
            err.map_data(|sdata| {
                let any = sdata.try_into_any().unwrap();
                let sys = *any.downcast::<Sys>().unwrap();
                SystemDesc {
                    sys: Or::A(sys),
                    private,
                    group_index,
                    volatile,
                    activation: (live, insert_at),
                }
            })
        });
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
        let desc = SystemDesc::new().with_once(sys);
        self.add_system(desc)
    }

    fn unregister_system(&mut self, sid: SystemId) -> WithResult<&mut Self, (), EcsError> {
        let res = unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.unregister_system_inner)(self.this, &sid)
        };
        WithResult::new(self, res)
    }

    fn activate_system(
        &mut self,
        target: SystemId,
        at: InsertPos,
        live: Tick,
    ) -> WithResult<&mut Self, (), EcsError> {
        let res = unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.activate_system_inner)(self.this, target, at, live)
        };
        WithResult::new(self, res)
    }

    fn inactivate_system(&mut self, sid: SystemId) -> WithResult<&mut Self, (), EcsError> {
        let res = unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.inactivate_system_inner)(self.this, &sid)
        };
        WithResult::new(self, res)
    }

    fn register_entity(&mut self, desc: EntityReg) -> WithResult<&mut Self, EntityIndex, EcsError> {
        let res = unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.register_entity_inner)(self.this, desc)
        };
        WithResult::new(self, res)
    }

    fn unregister_entity<C>(&mut self) -> WithResult<&mut Self, Box<dyn ContainEntity>, EcsError>
    where
        C: Components,
    {
        let ckeys = C::sorted_keys();
        let ekey = EntityKeyRef::Ckeys(ckeys.as_ref());
        let res = unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.unregister_entity_inner)(self.this, ekey)
        };
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
        let ekey = EntityKeyRef::Index(&ei);

        let cont = unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.get_entity_container_mut)(self.this, ekey)
        };

        let res = if let Some(cont) = cont {
            let ri = value.move_to(&mut **cont);
            Ok(EntityId::new(ei, ri))
        } else {
            let reason = debug_format!("failed to find `{}`", std::any::type_name::<E>());
            Err(EcsError::UnknownEntity(reason, value))
        };
        WithResult::new(self, res)
    }

    fn remove_entity(&mut self, eid: EntityId) -> WithResult<&mut Self, (), EcsError> {
        let ei = eid.container_index();
        let ekey = EntityKeyRef::Index(&ei);

        let cont = unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.get_entity_container_mut)(self.this, ekey)
        };

        let res = if let Some(cont) = cont {
            let ri = eid.row_index();
            let is_removed = cont.remove_row(ri);
            if is_removed {
                Ok(())
            } else {
                let reason = debug_format!("failed to find an entity for {eid:?}");
                Err(EcsError::UnknownEntity(reason, ()))
            }
        } else {
            let reason = debug_format!("failed to find an entity for {eid:?}");
            Err(EcsError::UnknownEntity(reason, ()))
        };
        WithResult::new(self, res)
    }

    fn add_resource<T>(
        &mut self,
        desc: T,
    ) -> WithResult<&mut Self, ResourceIndex, EcsError<ResourceDesc>>
    where
        T: Into<ResourceDesc>,
    {
        let res = unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.add_resource_inner)(self.this, desc.into())
        };
        WithResult::new(self, res)
    }

    fn remove_resource<R>(&mut self) -> WithResult<&mut Self, Option<R>, EcsError>
    where
        R: Resource,
    {
        let res = unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.remove_resource_inner)(self.this, &R::key())
        };
        let res = res.map(|opt| opt.map(|any| *any.downcast::<R>().unwrap()));
        WithResult::new(self, res)
    }

    fn get_resource<R>(&self) -> Option<&R>
    where
        R: Resource,
    {
        unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.get_resource_inner)(self.this, &R::key()).map(|ptr| ptr.cast::<R>().as_ref())
        }
    }

    fn get_resource_mut<R>(&mut self) -> Option<&mut R>
    where
        R: Resource,
    {
        unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.get_resource_mut_inner)(self.this, &R::key())
                .map(|ptr| ptr.cast::<R>().as_mut())
        }
    }

    fn get_resource_index<R>(&self) -> Option<ResourceIndex>
    where
        R: Resource,
    {
        unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.get_resource_index_inner)(self.this, &R::key())
        }
    }

    fn execute_commands<T>(
        &mut self,
        mut cmds: T,
    ) -> WithResult<&mut Self, (), Box<dyn Error + Send + Sync + 'static>>
    where
        T: HelpExecuteManyCommands,
    {
        let res = cmds.command(unsafe { self.copy() });
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
        let shared = unsafe { self.get_shared_ptr().as_ref() };
        let cmdr = Commander::new(shared);
        let mut cmd = f(cmdr);
        let res = cmd.command(unsafe { self.copy() });
        WithResult::new(self, res)
    }
}

/// An ECS instance.
///
/// Clients can create the instance via [`Ecs::default`], [`Ecs::create`] or
/// [`EcsApp::new`]. It's possible to have multiple ECS instances as well if you
/// really need to do so, but it's recommended to have multiple groups instead
/// of multiple instances to reduce memory footprint and share data with ease.
///
/// * `W` - Worker type.
/// * `S` - Hasher builder type.
#[derive(Debug)]
pub struct EcsApp<W, S = RandomState>
where
    W: Work + 'static,
    S: BuildHasher + Default + 'static,
{
    /// System storage.
    sys_stor: SystemStorage<S>,

    /// Entity and component storage.
    /// The storage contains all kinds of entities and components.
    ent_stor: EntityStorage<S>,

    /// Resource storage.
    /// The storage contains pointers to resources.
    res_stor: ResourceStorage<S>,

    cache_stor: CacheStorage<S>,

    sched: Scheduler<W, S>,

    vtable: EcsVTable,

    shared: Arc<Shared>,
    tx_cmd: CommandSender,
    rx_cmd: Rc<CommandReceiver>,
}

impl<W, S> EcsApp<W, S>
where
    W: Work + 'static,
    S: BuildHasher + Default + 'static,
{
    /// Creates an ECS instance with the given workers, group information, and
    /// hasher builder type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::hash::{BuildHasher, DefaultHasher};
    ///
    /// // Something like `std::hash::RandomState`.
    /// #[derive(Default)]
    /// struct FixedState;
    /// impl BuildHasher for FixedState {
    ///     type Hasher = DefaultHasher;
    ///     fn build_hasher(&self) -> Self::Hasher {
    ///         DefaultHasher::new()
    ///     }
    /// }
    ///
    /// // Creates `EcsApp` with one group consisting of 4 workers.
    /// let pool = WorkerPool::with_len(4);
    /// let ecs: EcsApp<_, FixedState> = EcsApp::new(pool.into(), &[4]);
    ///
    /// // Creates `EcsApp` with two groups consisting of 2 workers respectively.
    /// let pool = WorkerPool::with_len(4);
    /// let ecs: EcsApp<_, FixedState> = EcsApp::new(pool.into(), &[2, 2]);
    /// ```
    pub fn new(workers: Vec<W>, groups: &[usize]) -> Self {
        // We need a group even if it's empty for now.
        let groups = if groups.is_empty() { &[0][..] } else { groups };

        let shared = Arc::new(Shared::new());
        let (tx_cmd, rx_cmd) = command_channel(thread::current());
        let rx_cmd = Rc::new(rx_cmd);
        let sched = Scheduler::new(workers, groups, &shared, &tx_cmd, Rc::clone(&rx_cmd));

        Self {
            sys_stor: SystemStorage::new(groups.len()),
            ent_stor: EntityStorage::new(),
            res_stor: ResourceStorage::new(),
            cache_stor: CacheStorage::new(),
            sched,
            shared,
            tx_cmd,
            rx_cmd,
            vtable: EcsVTable::new::<W, S>(),
        }
    }

    /// Destroys the ecs instance and returns workers.
    pub fn destroy(mut self) -> Vec<W> {
        // Remaining commands and systems must be cancelled.
        self.clear_command();
        self.clear_system();

        // Takes workers out from the scheduler.
        let old = mem::replace(
            &mut self.sched,
            Scheduler::new(
                Vec::new(),
                &[],
                &self.shared,
                &self.tx_cmd,
                Rc::clone(&self.rx_cmd),
            ),
        );
        old.take_workers()
    }

    /// Takes out poisoned systems so far.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// let v = Ecs::default(WorkerPool::with_len(1), [1])
    ///     .add_once_system(|| {
    ///         panic!("panics on purpose");
    ///     })
    ///     .step()
    ///     .collect_poisoned_systems();
    /// assert_eq!(v.len(), 1);
    /// ```
    pub fn collect_poisoned_systems(&mut self) -> Vec<PoisonedSystem> {
        self.sys_stor.drain_poisoned().collect()
    }

    /// Shrinks the capacity of internal data structures as much as possible.
    pub fn shrink_to_fit(&mut self) {
        self.shared.shrink_to_fit();
        // TODO: need more shrink methods.
    }

    /// Executes active systems of all groups once.
    ///
    /// Generated commands during the execution will be completely consumed at
    /// the end of system execution.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::sync::{Arc, Mutex};
    ///
    /// let cnt = Arc::new(Mutex::new(0));
    /// let c_cnt = Arc::clone(&cnt);
    ///
    /// Ecs::default(WorkerPool::new(), [])
    ///     .add_system(move || {
    ///         *c_cnt.lock().unwrap() += 1;
    ///     })
    ///     .step();
    /// assert_eq!(*cnt.lock().unwrap(), 1);
    /// ```
    pub fn step(&mut self) -> &mut Self {
        // Executes.
        if self.has_active_system() {
            let sgroups = &mut self.sys_stor.sgroups;
            let mut cache = RefreshCacheStorage::new(
                &mut self.cache_stor,
                &mut self.ent_stor,
                &mut self.res_stor,
            );
            self.sched.execute_all(sgroups, &mut cache);
        }

        // Consumes buffered commands.
        self.process_buffered_commands();

        // Clears dead systems caused by the execution above.
        self.clear_dead_system();

        self
    }

    /// Executes active systems of all groups until their lifetime goes to zero.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    /// use std::sync::{Arc, Mutex};
    ///
    /// let cnt = Arc::new(Mutex::new(0));
    /// let c_cnt = Arc::clone(&cnt);
    ///
    /// Ecs::default(WorkerPool::new(), [])
    ///     .add_system(
    ///         SystemDesc::new()
    ///             .with_activation(2, InsertPos::Back)
    ///             .with_system(move || {
    ///                 *c_cnt.lock().unwrap() += 1;
    ///             }),
    ///     )
    ///     .run();
    /// assert_eq!(*cnt.lock().unwrap(), 2);
    /// ```
    pub fn run(&mut self) -> &mut Self {
        debug_assert_eq!(self.sched.num_groups(), self.sys_stor.num_groups());

        while !self.step().wait_for_idle().is_completed() {}
        self
    }

    /// Waits for ecs to be idle.
    ///
    /// Ecs becomes `idle` when all group's workers get closed.
    pub fn wait_for_idle(&mut self) -> &mut Self {
        self.sched.wait_exhausted();
        self
    }

    /// Determines whether ecs has been executed completely, so that it cannot
    /// do anything.
    ///
    /// If all conditions below are met, then ecs is considered as completed.
    /// - No active systems
    /// - No uncompleted commands
    /// - No open sub workers
    pub fn is_completed(&self) -> bool {
        // For main worker, no active systems?
        let no_active = self
            .sys_stor
            .sgroups
            .iter()
            .all(|sgroup| sgroup.len_active() == 0);

        // For main worker, no uncompleted commands?
        let no_cmd = !self.sched.has_command();

        // For sub workers, they are closed?
        let is_sub_exhausted = self.sched.is_work_groups_exhausted();

        is_sub_exhausted && no_active && no_cmd
    }

    fn has_active_system(&self) -> bool {
        self.sys_stor
            .sgroups
            .iter()
            .any(|sgroup| sgroup.len_active() > 0)
    }

    // TODO: doc example to test downcast error
    // error: EcsError<SystemData>
    fn register_system_inner(
        &mut self,
        mut sdata: SystemData,
        group_index: u16,
        volatile: bool,
        private: bool,
    ) -> Result<SystemId, EcsError<SystemData>> {
        if let Err(e) = validate_request::<W, S>(self, &sdata) {
            return Err(e.with_data(sdata));
        }
        complete_data::<W, S>(self, &mut sdata, group_index, private);
        let sid = sdata.id();
        return self.sys_stor.register(sdata, volatile).map(|()| sid);

        // === Internal helper functions ===

        fn validate_request<W, S>(this: &EcsApp<W, S>, sdata: &SystemData) -> Result<(), EcsError>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            // Validation procedure is as follows.
            // 1. Validates `Read`, `Write`, `ResRead`, `ResWrite`, and `EntWrite`.
            // 2. Validates if queried resources exist.
            //    When it comes to resource queries, in contrast to component or
            //    entity queries, they must be known at the time of system
            //    registration. Assume that clients forgot to register required
            //    resources. Then, we can't give them to systems. But about
            //    components or entities, we can give empty iterator somehow.
            // 3. `EntWrite` must not overlap both `Read` and `Write`.

            // 1. Validates request's `Read`, `Write`, `ResRead`, `ResWrite`,
            // and `EntWrite` themselves.
            let rinfo = sdata.get_request_info();
            if let Err(reason) = rinfo.validate() {
                return Err(EcsError::InvalidRequest(reason, ()));
            }

            // 2. Tests if we can find queries resources now.
            for rkey in rinfo.resource_keys() {
                if !this.res_stor.contains(rkey) {
                    let reason = debug_format!("failed to find a resource `{:?}`", rkey);
                    return Err(EcsError::UnknownResource(reason, ()));
                }
            }

            // 3. `EntWrite` must not overlap both `Read` and `Write`.
            let r_sinfos = rinfo.read().1.select_infos();
            let w_sinfos = rinfo.write().1.select_infos();
            for (sinfo, finfo) in r_sinfos
                .chain(w_sinfos)
                .flat_map(|sinfo| rinfo.filters().iter().map(move |(_, finfo)| (sinfo, finfo)))
            {
                if !sinfo.is_disjoint2(finfo) {
                    let reason = debug_format!(
                        "{} contains conflicting queries: {}, {}",
                        rinfo.name(),
                        sinfo.name(),
                        finfo.name(),
                    );
                    return Err(EcsError::InvalidRequest(reason, ()));
                }
            }

            Ok(())
        }

        fn complete_data<W, S>(
            this: &mut EcsApp<W, S>,
            sdata: &mut SystemData,
            group_index: u16,
            private: bool,
        ) where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            // Completes system id.
            debug_assert!(sdata.id().is_dummy());
            {
                let sid = this
                    .sys_stor
                    .get_group_mut(group_index as usize)
                    .next_system_id();
                sdata.set_id(sid);
            }

            // Completes system flags.
            debug_assert!(sdata.flags().is_dedi_empty() && sdata.flags().is_private_empty());
            {
                let mut sflags = SystemFlags::empty();

                // Is dedicated system?
                let res_read = &sdata.get_request_info().res_read().1;
                let res_write = &sdata.get_request_info().res_write().1;
                if res_read
                    .resource_keys()
                    .iter()
                    .chain(res_write.resource_keys())
                    .any(|rkey| this.res_stor.is_dedicated2(rkey).unwrap())
                {
                    sflags |= SystemFlags::DEDI_SET;
                } else {
                    sflags |= SystemFlags::DEDI_RESET;
                }

                // Is private system?
                if private {
                    sflags |= SystemFlags::PRIVATE_SET;
                } else {
                    sflags |= SystemFlags::PRIVATE_RESET;
                }

                // If the app doesn't have sub workers, makes the system dedicated.
                debug_assert_eq!(this.sched.num_groups(), this.sys_stor.num_groups());
                if this.sched.num_workers() == 0 {
                    sflags |= SystemFlags::DEDI_SET;
                }

                sdata.union_flags(sflags);
            }
        }
    }

    fn unregister_system_inner(&mut self, sid: &SystemId) -> Result<(), EcsError> {
        self.sys_stor.unregister(sid)
    }

    fn activate_system_inner(
        &mut self,
        target: SystemId,
        at: InsertPos,
        live: Tick,
    ) -> Result<(), EcsError> {
        // Activates the system.
        self.sys_stor.activate(&target, at, live)?;

        // Refreshes cache item for the system.
        let sgroup = self.sys_stor.get_group_mut(target.group_index() as usize);
        // Safety: The system was successfully activated, so we definitely can
        // get the system data.
        let sdata = unsafe { sgroup.get_active(&target).unwrap_unchecked() };
        self.cache_stor.remove_item(target);
        self.cache_stor
            .create_item(sdata, &mut self.ent_stor, &mut self.res_stor);
        Ok(())
    }

    fn inactivate_system_inner(&mut self, sid: &SystemId) -> Result<(), EcsError> {
        self.sys_stor.inactivate(sid)
    }

    fn register_entity_inner(&mut self, desc: EntityReg) -> Result<EntityIndex, EcsError> {
        // Registers entity.
        let ei = self.ent_stor.register(desc)?;
        let ekey = EntityKeyRef::Index(&ei);
        self.cache_stor
            .update_by_entity_reg(ekey, &mut self.ent_stor, &mut self.res_stor);

        // Makes wait queue for the entity.
        let cont = unsafe { self.ent_stor.get_entity_container(ekey).unwrap_unchecked() };
        self.sched
            .get_wait_queues_mut()
            .initialize_entity_queue(ei.index(), cont.num_columns());

        Ok(ei)
    }

    fn unregister_entity_inner(
        &mut self,
        ekey: EntityKeyRef<'_>,
    ) -> Result<Box<dyn ContainEntity>, EcsError> {
        if self.ent_stor.get_entity_container(ekey).is_none() {
            let reason = debug_format!("failed to find an entity `{:?}`", ekey);
            return Err(EcsError::UnknownEntity(reason, ()));
        }

        // We must update cache before we unregister ent entity.
        self.cache_stor
            .update_by_entity_unreg(ekey, &mut self.ent_stor, &mut self.res_stor);

        let (_, cont) = self.ent_stor.unregister(ekey).unwrap();
        Ok(cont.into_inner())
    }

    fn add_resource_inner(
        &mut self,
        desc: ResourceDesc,
    ) -> Result<ResourceIndex, EcsError<ResourceDesc>> {
        let ri = self.res_stor.add(desc)?;
        self.sched
            .get_wait_queues_mut()
            .initialize_resource_queue(ri.index());
        Ok(ri)
    }

    fn remove_resource_inner(
        &mut self,
        rkey: &ResourceKey,
    ) -> Result<Option<Box<dyn Any>>, EcsError> {
        self.cache_stor
            .update_by_resource_unreg(rkey, |sid: &SystemId| {
                self.sys_stor.inactivate(sid).unwrap()
            });
        match self.res_stor.remove(rkey) {
            Some(Or::A(owned)) => Ok(Some(owned)),
            Some(Or::B(_ptr)) => Ok(None),
            None => {
                let reason = debug_format!("failed to find a resource `{:?}`", rkey);
                Err(EcsError::UnknownResource(reason, ()))
            }
        }
    }

    fn get_resource_inner(&self, rkey: &ResourceKey) -> Option<NonNull<u8>> {
        match self.res_stor.borrow2(rkey) {
            // If it is borrowed successfully, we can replace `Borrowed` with
            // shared reference.
            Ok(borrowed) => Some(borrowed.as_nonnull()),
            Err(..) => None,
        }
    }

    fn get_resource_mut_inner(&mut self, rkey: &ResourceKey) -> Option<NonNull<u8>> {
        match self.res_stor.borrow_mut2(rkey) {
            // If it is borrowed successfully, we can replace `Borrowed` with
            // mutable reference.
            Ok(borrowed) => Some(borrowed.as_nonnull()),
            Err(..) => None,
        }
    }

    fn get_resource_index_inner(&self, rkey: &ResourceKey) -> Option<ResourceIndex> {
        self.res_stor.index(rkey)
    }

    fn process_buffered_commands(&mut self) {
        while let Ok(cmd) = self.rx_cmd.try_recv() {
            let ecs = Ecs::new(self);
            let _ = cmd.command(ecs);
        }
    }

    /// Cancels out remaining commands.
    ///
    /// Commands are functions that can be executed or cancelled. They can be
    /// cancelled by getting called [`cancel`].
    ///
    /// [`cancel`]: Command::cancel
    fn clear_command(&mut self) {
        // Blocks more commands.
        self.rx_cmd.close();

        // Cancels out all buffered commands.
        while let Ok(cmd) = self.rx_cmd.try_recv() {
            cmd.cancel();
        }
    }

    /// Cancels out remaining systems.
    ///
    /// Systems are functions that can be executed or cancelled. They can be
    /// cancelled by becoming [`SystemState::Dead`] states.
    ///
    /// [`SystemState::Dead`]: crate::ecs::sys::system::SystemState::Dead
    fn clear_system(&mut self) {
        let num_groups = self.sys_stor.num_groups();
        for gi in 0..num_groups {
            self.sys_stor.get_group_mut(gi).clear();
        }
    }

    /// Clears dead systems with their cache items.
    fn clear_dead_system(&mut self) {
        for sdata in self.sys_stor.drain_dead() {
            self.cache_stor.remove_item(sdata.id());
        }
    }

    fn get_entity_container_mut<'r, K>(&mut self, ekey: K) -> Option<&mut EntityContainer>
    where
        K: Into<EntityKeyRef<'r>>,
    {
        self.ent_stor.get_entity_container_mut(ekey)
    }
}

impl<W, S> EcsEntry for EcsApp<W, S>
where
    W: Work + 'static,
    S: BuildHasher + Default + 'static,
{
    fn add_system<T, Sys>(
        &mut self,
        desc: T,
    ) -> WithResult<&mut Self, SystemId, EcsError<SystemDesc<Sys>>>
    where
        T: Into<SystemDesc<Sys>>,
        Sys: System,
    {
        let res = Ecs::new(self).add_system(desc).take();
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
        let res = Ecs::new(self).add_once_system(sys).take();
        WithResult::new(self, res)
    }

    fn unregister_system(&mut self, sid: SystemId) -> WithResult<&mut Self, (), EcsError> {
        let res = Ecs::new(self).unregister_system(sid).take();
        WithResult::new(self, res)
    }

    fn activate_system(
        &mut self,
        target: SystemId,
        at: InsertPos,
        live: Tick,
    ) -> WithResult<&mut Self, (), EcsError> {
        let res = Ecs::new(self).activate_system(target, at, live).take();
        WithResult::new(self, res)
    }

    fn inactivate_system(&mut self, sid: SystemId) -> WithResult<&mut Self, (), EcsError> {
        let res = Ecs::new(self).inactivate_system(sid).take();
        WithResult::new(self, res)
    }

    fn register_entity(&mut self, desc: EntityReg) -> WithResult<&mut Self, EntityIndex, EcsError> {
        let res = Ecs::new(self).register_entity(desc).take();
        WithResult::new(self, res)
    }

    fn unregister_entity<C>(&mut self) -> WithResult<&mut Self, Box<dyn ContainEntity>, EcsError>
    where
        C: Components,
    {
        let res = Ecs::new(self).unregister_entity::<C>().take();
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
        let res = Ecs::new(self).add_entity(ei, value).take();
        WithResult::new(self, res)
    }

    fn remove_entity(&mut self, eid: EntityId) -> WithResult<&mut Self, (), EcsError> {
        let res = Ecs::new(self).remove_entity(eid).take();
        WithResult::new(self, res)
    }

    fn add_resource<T>(
        &mut self,
        desc: T,
    ) -> WithResult<&mut Self, ResourceIndex, EcsError<ResourceDesc>>
    where
        T: Into<ResourceDesc>,
    {
        let res = Ecs::new(self).add_resource(desc).take();
        WithResult::new(self, res)
    }

    fn remove_resource<R>(&mut self) -> WithResult<&mut Self, Option<R>, EcsError>
    where
        R: Resource,
    {
        let res = Ecs::new(self).remove_resource::<R>().take();
        WithResult::new(self, res)
    }

    fn get_resource<R>(&self) -> Option<&R>
    where
        R: Resource,
    {
        self.get_resource_inner(&R::key())
            .map(|ptr| unsafe { ptr.cast::<R>().as_ref() })
    }

    fn get_resource_mut<R>(&mut self) -> Option<&mut R>
    where
        R: Resource,
    {
        self.get_resource_mut_inner(&R::key())
            .map(|ptr| unsafe { ptr.cast::<R>().as_mut() })
    }

    fn get_resource_index<R>(&self) -> Option<ResourceIndex>
    where
        R: Resource,
    {
        self.get_resource_index_inner(&R::key())
    }

    fn execute_commands<T>(
        &mut self,
        cmds: T,
    ) -> WithResult<&mut Self, (), Box<dyn Error + Send + Sync + 'static>>
    where
        T: HelpExecuteManyCommands,
    {
        let res = Ecs::new(self).execute_commands(cmds).take();
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
        let res = Ecs::new(self).execute_command(f).take();
        WithResult::new(self, res)
    }
}

impl<W, S> Drop for EcsApp<W, S>
where
    W: Work + 'static,
    S: BuildHasher + Default + 'static,
{
    fn drop(&mut self) {
        self.clear_command();
        self.clear_system();
    }
}

impl<W, S> Resource for EcsApp<W, S>
where
    EcsApp<W, S>: Send + 'static,
    W: Work + 'static,
    S: BuildHasher + Default + 'static,
{
}

impl<W, S> From<EcsApp<W, S>> for LeakedEcsApp
where
    W: Work + 'static,
    S: BuildHasher + Default + 'static,
{
    fn from(value: EcsApp<W, S>) -> Self {
        LeakedEcsApp::new(value)
    }
}

/// A handle to an [`EcsApp`].
///
/// This is useful when you need to move the ECS instance onto heap memory from
/// stack, then have ownership by its handle. Because this type deals with the
/// ownership, this is non-cloneable. When the handle is dropped, associated ECS
/// instance is also dropepd and deallocated from heap memory.
///
/// You can use [`From`] to convert `EcsApp` into this handle.
pub struct LeakedEcsApp {
    this: Ecs<'static>,
    drop: unsafe fn(Ecs<'static>),
}

impl LeakedEcsApp {
    fn new<W, S>(app: EcsApp<W, S>) -> Self
    where
        W: Work + 'static,
        S: BuildHasher + Default + 'static,
    {
        unsafe fn _drop<W, S>(ecs: Ecs<'static>)
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let ptr = ecs.this.cast::<EcsApp<W, S>>();
            let boxed_ecs = unsafe { Box::from_raw(ptr.as_ptr()) };
            drop(boxed_ecs);
        }

        Self {
            this: Ecs::new(Box::leak(Box::new(app))),
            drop: _drop::<W, S>,
        }
    }

    /// # Safety
    ///
    /// See [`Ecs::copy`].
    #[cfg(target_arch = "wasm32")]
    pub(crate) unsafe fn get(&self) -> EcsExt<'static> {
        EcsExt {
            ecs: self.this.copy(),
        }
    }
}

impl Drop for LeakedEcsApp {
    fn drop(&mut self) {
        // Safety:
        // - `self.drop` holds proper drop method for `self.this`
        // - It cannot be double free because we release `self.this` here only.
        // See `Self::new()` for more details.
        unsafe { (self.drop)(self.this.copy()) };
    }
}

impl Deref for LeakedEcsApp {
    type Target = Ecs<'static>;

    fn deref(&self) -> &Self::Target {
        &self.this
    }
}

impl DerefMut for LeakedEcsApp {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.this
    }
}

/// Extended [`Ecs`] with additional methods.
#[repr(transparent)]
pub struct EcsExt<'ecs> {
    ecs: Ecs<'ecs>,
}

impl EcsExt<'_> {
    /// Executes active systems of all groups once.
    ///
    /// Generated commands during the execution will be completely consumed at
    /// the end of system execution.
    pub fn step(&mut self) {
        unsafe {
            let vtable = self.ecs.vtable.as_ref();
            (vtable.step)(self.ecs.this);
        }
    }
}

impl EcsEntry for EcsExt<'_> {
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

#[cfg(test)]
mod tests {
    use crate as my_ecs;
    use crate::prelude::*;
    use std::sync::{Arc, Mutex};

    #[test]
    fn test_add_many_systems() {
        let mut ecs = Ecs::default(WorkerPool::with_len(1), [1]);

        let state = Arc::new(Mutex::new(vec![]));

        // Declares a system.
        struct StructSystem(Arc<Mutex<Vec<i32>>>);
        request!(Req);
        impl System for StructSystem {
            type Request = Req;
            fn run(&mut self, _resp: Response<'_, Self::Request>) {
                self.0.lock().unwrap().push(0);
            }
        }
        let sys0 = StructSystem(Arc::clone(&state));

        // Declares a system.
        let c_state = Arc::clone(&state);
        let sys1 = move || {
            c_state.lock().unwrap().push(1);
        };

        ecs.add_systems((sys0, sys1)).step();

        assert_eq!(*state.lock().unwrap(), vec![0, 1]);
    }

    #[test]
    fn test_execute_many_commands() {
        let mut ecs = Ecs::default(WorkerPool::with_len(1), [1]);

        let state = Arc::new(Mutex::new(vec![]));

        // Declares a command.
        struct StructCommand(Arc<Mutex<Vec<i32>>>);
        impl Command for StructCommand {
            fn command(&mut self, _ecs: Ecs<'_>) -> DynResult<()> {
                self.0.lock().unwrap().push(0);
                Ok(())
            }
        }
        let cmd0 = StructCommand(Arc::clone(&state));

        // Declares a command.
        let c_state = Arc::clone(&state);
        let cmd1 = move |_ecs: Ecs| {
            c_state.lock().unwrap().push(1);
            Ok(())
        };

        ecs.execute_commands((cmd0, Some(cmd1))).unwrap();

        assert_eq!(*state.lock().unwrap(), vec![0, 1]);
    }

    #[test]
    fn test_add_many_resources() {
        use crate as my_ecs;
        let mut ecs = Ecs::default(WorkerPool::with_len(1), [1]);

        // Declares a resource.
        #[derive(Resource)]
        struct Ra(i32);
        #[derive(Resource)]
        struct Rb(String);

        let ra = Ra(0);
        let rb = Rb("b".to_owned());
        ecs.add_resources((ra, rb))
            .add_once_system(|rr: ResRead<(Ra, Rb)>| {
                let (a, b) = rr.0;
                assert_eq!(a.0, 0);
                assert_eq!(&b.0, "b");
            })
            .step();

        assert!(ecs.collect_poisoned_systems().is_empty());
    }

    #[test]
    fn test_zero_workers() {
        let cnt = Arc::new(Mutex::new(0));
        let cnt0 = Arc::clone(&cnt);
        let cnt1 = Arc::clone(&cnt);

        Ecs::default(WorkerPool::new(), [])
            .add_once_systems((
                move || *cnt0.lock().unwrap() += 1,
                move || *cnt1.lock().unwrap() += 10,
            ))
            .step();

        assert_eq!(*cnt.lock().unwrap(), 11);
    }

    #[test]
    fn test_multiple_apps() {
        let mut a = Ecs::default(WorkerPool::with_len(1), [1]);
        let mut b = Ecs::default(WorkerPool::new(), []);

        let cnt = Arc::new(Mutex::new(0));
        let cnt_a = Arc::clone(&cnt);
        let cnt_b = Arc::clone(&cnt);

        a.add_once_system(move || *cnt_a.lock().unwrap() += 1)
            .unwrap();
        b.add_once_system(move || *cnt_b.lock().unwrap() += 10)
            .unwrap();

        a.step();
        assert_eq!(*cnt.lock().unwrap(), 1);
        drop(a);

        b.step();
        assert_eq!(*cnt.lock().unwrap(), 11);
        drop(b);
    }
}
