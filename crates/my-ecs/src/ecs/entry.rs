use super::{
    cache::{CacheStorage, RefreshCacheStorage},
    cmd::{Command, Commander, Commands},
    ent::{
        component::Components,
        entity::{Entity, EntityId, EntityIndex, EntityKeyRef},
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
    EcsError,
};
use crate::util::prelude::*;
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

pub trait EcsEntry {
    // === System methods ===

    fn add_systems<T, Systems>(&mut self, descs: T) -> WithResult<&mut Self, (), EcsError>
    where
        T: HelpAddManySystems<Systems>,
    {
        descs.add_systems(self)
    }

    fn add_system<T, Sys>(
        &mut self,
        desc: T,
    ) -> WithResult<&mut Self, SystemId, EcsError<SystemDesc<Sys>>>
    where
        T: Into<SystemDesc<Sys>>,
        Sys: System;

    fn add_once_systems<T, Once>(&mut self, descs: T) -> WithResult<&mut Self, (), EcsError>
    where
        T: HelpAddManyOnce<Once>,
    {
        descs.add_once_systems(self)
    }

    fn add_once_system<T, Req, F>(
        &mut self,
        sys: T,
    ) -> WithResult<&mut Self, SystemId, EcsError<SystemDesc<FnOnceSystem<Req, F>>>>
    where
        T: Into<FnOnceSystem<Req, F>>,
        FnOnceSystem<Req, F>: System;

    fn unregister_system(&mut self, sid: SystemId) -> WithResult<&mut Self, (), EcsError>;

    /// Activates the system. If the system is already active, nothing takes place.
    fn activate_system(
        &mut self,
        target: SystemId,
        at: InsertPos,
        live: Tick,
    ) -> WithResult<&mut Self, (), EcsError>;

    fn inactivate_system(&mut self, sid: SystemId) -> WithResult<&mut Self, (), EcsError>;

    // === Entity methods ===

    fn register_entity_of<T>(&mut self) -> WithResult<&mut Self, EntityIndex, EcsError>
    where
        T: AsEntityReg,
    {
        self.register_entity(T::as_entity_descriptor())
    }

    fn register_entity(&mut self, desc: EntityReg) -> WithResult<&mut Self, EntityIndex, EcsError>;

    fn unregister_entity<C>(&mut self) -> WithResult<&mut Self, EntityContainer, EcsError>
    where
        C: Components;

    fn add_entity<E>(
        &mut self,
        ei: EntityIndex,
        value: E,
    ) -> WithResult<&mut Self, EntityId, EcsError<E>>
    where
        E: Entity;

    fn remove_entity(&mut self, eid: EntityId) -> WithResult<&mut Self, (), EcsError>;

    // === Resource methods ===

    fn register_resources<T>(&mut self, descs: T) -> WithResult<&mut Self, (), EcsError>
    where
        T: HelpRegisterManyResources,
    {
        descs.register_resources(self)
    }

    /// Registers the resource.
    /// If the registration failed, nothing takes place and returns received value.
    /// In other words, the old resouce data won't be dropped.
    fn register_resource<T>(
        &mut self,
        desc: T,
    ) -> WithResult<&mut Self, ResourceIndex, EcsError<ResourceDesc>>
    where
        T: Into<ResourceDesc>;

    fn unregister_resource<R>(&mut self) -> WithResult<&mut Self, Option<Box<dyn Any>>, EcsError>
    where
        R: Resource;

    fn get_resource<R>(&self) -> Option<&R>
    where
        R: Resource;

    fn get_resource_mut<R>(&mut self) -> Option<&mut R>
    where
        R: Resource;

    fn get_resource_index<R>(&self) -> Option<ResourceIndex>
    where
        R: Resource;

    // === Command methods ===

    fn execute_commands(
        &mut self,
        cmds: impl Commands,
    ) -> WithResult<&mut Self, (), Box<dyn Error + Send + Sync + 'static>>;

    fn execute_command<F, R>(
        &mut self,
        f: F,
    ) -> WithResult<&mut Self, (), Box<dyn Error + Send + Sync + 'static>>
    where
        F: FnOnce(Commander) -> R,
        R: Command;
}

pub trait HelpAddManySystems<Systems> {
    fn add_systems<Ecs>(self, ecs: &mut Ecs) -> WithResult<&mut Ecs, (), EcsError>
    where
        Ecs: EcsEntry + ?Sized;
}

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

pub trait HelpAddManyOnce<Once> {
    fn add_once_systems<Ecs>(self, ecs: &mut Ecs) -> WithResult<&mut Ecs, (), EcsError>
    where
        Ecs: EcsEntry + ?Sized;
}

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

pub trait HelpRegisterManyResources {
    fn register_resources<Ecs>(self, ecs: &mut Ecs) -> WithResult<&mut Ecs, (), EcsError>
    where
        Ecs: EcsEntry + ?Sized;
}

macro_rules! impl_help_register_many_resources {
    ($n:expr, $($i:expr),*) => {
        paste::paste! {
            #[allow(unused_parens, non_snake_case)]
            impl<$([<A $i>]),*> HelpRegisterManyResources for ( $([<A $i>]),* )
            where
            $(
                [<A $i>]: Into<ResourceDesc>,
            )*
            {
                fn register_resources<Ecs>(
                    self,
                    ecs: &mut Ecs
                ) -> WithResult<&mut Ecs, (), EcsError>
                where
                    Ecs: EcsEntry + ?Sized,
                {
                    let ( $([<A $i>]),* ) = self;

                    $(
                        match ecs.register_resource([<A $i>]).take() {
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
repeat_macro!(impl_help_register_many_resources, 1..=8);

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
        unsafe fn(NonNull<u8>, EntityKeyRef<'_>) -> Result<EntityContainer, EcsError>,

    get_entity_container_mut:
        unsafe fn(NonNull<u8>, EntityKeyRef<'_>) -> Option<&mut EntityContainer>,

    // === Resource methods ===

    register_resource_inner:
        unsafe fn(NonNull<u8>, ResourceDesc)
        -> Result<ResourceIndex, EcsError<ResourceDesc>>,

    unregister_resource_inner:
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

    schedule_all:
        unsafe fn(NonNull<u8>),
}

impl EcsVTable {
    fn new<W, S, const N: usize>() -> Self
    where
        W: Work + 'static,
        S: BuildHasher + Default + 'static,
    {
        unsafe fn register_system_inner<W, S, const N: usize>(
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
            let this: &mut EcsApp<W, S, N> = this.cast().as_mut();
            this.register_system_inner(sdata, group_index, volatile, private)
        }

        unsafe fn unregister_system_inner<W, S, const N: usize>(
            this: NonNull<u8>,
            sid: &SystemId,
        ) -> Result<(), EcsError>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S, N> = this.cast().as_mut();
            this.unregister_system_inner(sid)
        }

        unsafe fn activate_system_inner<W, S, const N: usize>(
            this: NonNull<u8>,
            target: SystemId,
            at: InsertPos,
            live: Tick,
        ) -> Result<(), EcsError>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S, N> = this.cast().as_mut();
            this.activate_system_inner(target, at, live)
        }

        unsafe fn inactivate_system_inner<W, S, const N: usize>(
            this: NonNull<u8>,
            sid: &SystemId,
        ) -> Result<(), EcsError>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S, N> = this.cast().as_mut();
            this.inactivate_system_inner(sid)
        }

        unsafe fn register_entity_inner<W, S, const N: usize>(
            this: NonNull<u8>,
            desc: EntityReg,
        ) -> Result<EntityIndex, EcsError>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S, N> = this.cast().as_mut();
            this.register_entity_inner(desc)
        }

        unsafe fn unregister_entity_inner<W, S, const N: usize>(
            this: NonNull<u8>,
            ekey: EntityKeyRef<'_>,
        ) -> Result<EntityContainer, EcsError>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S, N> = this.cast().as_mut();
            this.unregister_entity_inner(ekey)
        }

        unsafe fn get_entity_container_mut<'o, W, S, const N: usize>(
            this: NonNull<u8>,
            ekey: EntityKeyRef<'_>,
        ) -> Option<&'o mut EntityContainer>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &'o mut EcsApp<W, S, N> = this.cast().as_mut();
            this.get_entity_container_mut(ekey)
        }

        unsafe fn register_resource_inner<W, S, const N: usize>(
            this: NonNull<u8>,
            desc: ResourceDesc,
        ) -> Result<ResourceIndex, EcsError<ResourceDesc>>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S, N> = this.cast().as_mut();
            this.register_resource_inner(desc)
        }

        unsafe fn unregister_resource_inner<W, S, const N: usize>(
            this: NonNull<u8>,
            rkey: &ResourceKey,
        ) -> Result<Option<Box<dyn Any>>, EcsError>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S, N> = this.cast().as_mut();
            this.unregister_resource_inner(rkey)
        }

        unsafe fn get_resource_inner<W, S, const N: usize>(
            this: NonNull<u8>,
            rkey: &ResourceKey,
        ) -> Option<NonNull<u8>>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S, N> = this.cast().as_mut();
            this.get_resource_inner(rkey)
        }

        unsafe fn get_resource_mut_inner<W, S, const N: usize>(
            this: NonNull<u8>,
            rkey: &ResourceKey,
        ) -> Option<NonNull<u8>>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S, N> = this.cast().as_mut();
            this.get_resource_mut_inner(rkey)
        }

        unsafe fn get_resource_index_inner<W, S, const N: usize>(
            this: NonNull<u8>,
            rkey: &ResourceKey,
        ) -> Option<ResourceIndex>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S, N> = this.cast().as_mut();
            this.get_resource_index_inner(rkey)
        }

        unsafe fn get_shared<W, S, const N: usize>(this: NonNull<u8>) -> NonNull<Shared>
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S, N> = this.cast().as_mut();
            let shared = this.shared.as_ref();
            let ptr = (shared as *const Shared).cast_mut();
            NonNull::new_unchecked(ptr)
        }

        unsafe fn schedule_all<W, S, const N: usize>(this: NonNull<u8>)
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let this: &mut EcsApp<W, S, N> = this.cast().as_mut();
            this.run().schedule_all();
        }

        Self {
            register_system_inner: register_system_inner::<W, S, N>,
            unregister_system_inner: unregister_system_inner::<W, S, N>,
            activate_system_inner: activate_system_inner::<W, S, N>,
            inactivate_system_inner: inactivate_system_inner::<W, S, N>,
            register_entity_inner: register_entity_inner::<W, S, N>,
            unregister_entity_inner: unregister_entity_inner::<W, S, N>,
            get_entity_container_mut: get_entity_container_mut::<W, S, N>,
            register_resource_inner: register_resource_inner::<W, S, N>,
            unregister_resource_inner: unregister_resource_inner::<W, S, N>,
            get_resource_inner: get_resource_inner::<W, S, N>,
            get_resource_mut_inner: get_resource_mut_inner::<W, S, N>,
            get_resource_index_inner: get_resource_index_inner::<W, S, N>,
            get_shared: get_shared::<W, S, N>,
            schedule_all: schedule_all::<W, S, N>,
        }
    }
}

// Do not implement Clone. This must be carefully copied.
#[derive(Debug)]
pub struct Ecs<'ecs> {
    this: NonNull<u8>,
    vtable: NonNull<EcsVTable>,
    _marker: PhantomData<&'ecs mut ()>,
}

impl<'ecs> Ecs<'ecs> {
    pub fn default<Wp, W, const N: usize>(pool: Wp, groups: [usize; N]) -> EcsApp<W, RandomState, N>
    where
        Wp: Into<Vec<W>>,
        W: Work + 'static,
    {
        Self::create(pool.into(), groups)
    }

    pub fn create<W, S, const N: usize>(workers: Vec<W>, groups: [usize; N]) -> EcsApp<W, S, N>
    where
        W: Work + 'static,
        S: BuildHasher + Default + 'static,
    {
        EcsApp::new(workers, groups)
    }

    fn new<W, S, const N: usize>(ecs: &'ecs mut EcsApp<W, S, N>) -> Self
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

    fn unregister_entity<C>(&mut self) -> WithResult<&mut Self, EntityContainer, EcsError>
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
            let reason = debug_format!("{}", std::any::type_name::<E>());
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

    fn register_resource<T>(
        &mut self,
        desc: T,
    ) -> WithResult<&mut Self, ResourceIndex, EcsError<ResourceDesc>>
    where
        T: Into<ResourceDesc>,
    {
        let res = unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.register_resource_inner)(self.this, desc.into())
        };
        WithResult::new(self, res)
    }

    fn unregister_resource<R>(&mut self) -> WithResult<&mut Self, Option<Box<dyn Any>>, EcsError>
    where
        R: Resource,
    {
        let res = unsafe {
            let vtable = self.vtable.as_ref();
            (vtable.unregister_resource_inner)(self.this, &R::key())
        };
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

    fn execute_commands(
        &mut self,
        mut cmds: impl Commands,
    ) -> WithResult<&mut Self, (), Box<dyn Error + Send + Sync + 'static>> {
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

/// * `W` - Worker type.
/// * `S` - Hasher.
/// * `N` - Number of groups.
//
// We know N > 0 due to the validation in `Multi`.
#[derive(Debug)]
pub struct EcsApp<W, S = std::hash::RandomState, const N: usize = 1>
where
    W: Work + 'static,
    S: BuildHasher + Default + 'static,
{
    /// System storage.
    sys_stor: SystemStorage<S, N>,

    /// Entity and component storage.
    /// The storage contains all kinds of entities and components.
    ent_stor: EntityStorage<S>,

    /// Resource storage.
    /// The storage contains pointers to resources.
    res_stor: ResourceStorage<S>,

    cache_stor: CacheStorage<S>,

    sched: Scheduler<W, S, N>,

    vtable: EcsVTable,

    shared: Arc<Shared>,
    tx_cmd: CommandSender,
    rx_cmd: Rc<CommandReceiver>,
}

impl<W, S, const N: usize> EcsApp<W, S, N>
where
    W: Work + 'static,
    S: BuildHasher + Default + 'static,
{
    pub fn new(workers: Vec<W>, groups: [usize; N]) -> Self {
        let shared = Arc::new(Shared::new());
        let (tx_cmd, rx_cmd) = command_channel(thread::current());
        let rx_cmd = Rc::new(rx_cmd);
        let sched = Scheduler::new(workers, groups, &shared, &tx_cmd, Rc::clone(&rx_cmd));

        Self {
            sys_stor: SystemStorage::new(),
            ent_stor: EntityStorage::new(),
            res_stor: ResourceStorage::new(),
            cache_stor: CacheStorage::new(),
            sched,
            shared,
            tx_cmd,
            rx_cmd,
            vtable: EcsVTable::new::<W, S, N>(),
        }
    }

    pub fn destroy(mut self) -> Vec<W> {
        // Remaining commands and systems must be cancelled.
        self.clear_command();
        self.clear_system();

        // Takes workers out from the scheduler.
        let old = mem::replace(
            &mut self.sched,
            Scheduler::new(
                Vec::new(),
                [0; N],
                &self.shared,
                &self.tx_cmd,
                Rc::clone(&self.rx_cmd),
            ),
        );
        old.take_workers()
    }

    pub fn into_raw(self) -> LeakedEcsApp {
        LeakedEcsApp::new(self)
    }

    pub fn collect_poisoned_systems(&mut self) -> Vec<PoisonedSystem> {
        self.sys_stor.drain_poisoned().collect()
    }

    pub fn shrink_to_fit(&mut self) {
        self.shared.shrink_to_fit();
        // TODO: need more shrink methods.
    }

    #[must_use]
    pub fn run(&mut self) -> RunningEcs<'_, W, S, N> {
        RunningEcs::new(self)
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
        if let Err(e) = validate_request::<W, S, N>(self, &sdata) {
            return Err(e.with_data(sdata));
        }
        complete_data::<W, S, N>(self, &mut sdata, group_index, private);
        let sid = sdata.id();
        return self.sys_stor.register(sdata, volatile).map(|()| sid);

        // === Internal helper functions ===

        fn validate_request<W, S, const N: usize>(
            this: &EcsApp<W, S, N>,
            sdata: &SystemData,
        ) -> Result<(), EcsError>
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

        fn complete_data<W, S, const N: usize>(
            this: &mut EcsApp<W, S, N>,
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
    ) -> Result<EntityContainer, EcsError> {
        self.cache_stor
            .update_by_entity_unreg(ekey, &mut self.ent_stor, &mut self.res_stor);
        if let Some((_, cont)) = self.ent_stor.unregister(ekey) {
            Ok(cont)
        } else {
            let reason = debug_format!("failed to find an entity `{:?}`", ekey);
            Err(EcsError::UnknownEntity(reason, ()))
        }
    }

    fn register_resource_inner(
        &mut self,
        desc: ResourceDesc,
    ) -> Result<ResourceIndex, EcsError<ResourceDesc>> {
        let ri = self.res_stor.register(desc)?;
        self.sched
            .get_wait_queues_mut()
            .initialize_resource_queue(ri.index());
        Ok(ri)
    }

    fn unregister_resource_inner(
        &mut self,
        rkey: &ResourceKey,
    ) -> Result<Option<Box<dyn Any>>, EcsError> {
        self.cache_stor
            .update_by_resource_unreg(rkey, |sid: &SystemId| {
                self.sys_stor.inactivate(sid).unwrap()
            });
        match self.res_stor.unregister(rkey) {
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
    /// [`cancel`]: crate::ecs::cmd::Command::cancel
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
        for gi in 0..N {
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

impl<W, S, const N: usize> EcsEntry for EcsApp<W, S, N>
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

    fn unregister_entity<C>(&mut self) -> WithResult<&mut Self, EntityContainer, EcsError>
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

    fn register_resource<T>(
        &mut self,
        desc: T,
    ) -> WithResult<&mut Self, ResourceIndex, EcsError<ResourceDesc>>
    where
        T: Into<ResourceDesc>,
    {
        let res = Ecs::new(self).register_resource(desc).take();
        WithResult::new(self, res)
    }

    fn unregister_resource<R>(&mut self) -> WithResult<&mut Self, Option<Box<dyn Any>>, EcsError>
    where
        R: Resource,
    {
        let res = Ecs::new(self).unregister_resource::<R>().take();
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

    fn execute_commands(
        &mut self,
        cmds: impl Commands,
    ) -> WithResult<&mut Self, (), Box<dyn Error + Send + Sync + 'static>> {
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

impl<W, S, const N: usize> Drop for EcsApp<W, S, N>
where
    W: Work + 'static,
    S: BuildHasher + Default + 'static,
{
    fn drop(&mut self) {
        self.clear_command();
        self.clear_system();
    }
}

impl<W, S, const N: usize> Resource for EcsApp<W, S, N>
where
    EcsApp<W, S, N>: Send + 'static,
    W: Work + 'static,
    S: BuildHasher + Default + 'static,
{
}

pub struct LeakedEcsApp {
    this: Ecs<'static>,
    drop: unsafe fn(Ecs<'static>),
}

impl LeakedEcsApp {
    fn new<W, S, const N: usize>(app: EcsApp<W, S, N>) -> Self
    where
        W: Work + 'static,
        S: BuildHasher + Default + 'static,
    {
        unsafe fn _drop<W, S, const N: usize>(ecs: Ecs<'static>)
        where
            W: Work + 'static,
            S: BuildHasher + Default + 'static,
        {
            let mut ptr = ecs.this.cast::<EcsApp<W, S, N>>();
            drop(Box::from_raw(ptr.as_mut()));
        }

        Self {
            this: Ecs::new(Box::leak(Box::new(app))),
            drop: _drop::<W, S, N>,
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

#[repr(transparent)]
pub struct EcsExt<'ecs> {
    ecs: Ecs<'ecs>,
}

impl EcsExt<'_> {
    pub fn schedule_all(&mut self) {
        unsafe {
            let vtable = self.ecs.vtable.as_ref();
            (vtable.schedule_all)(self.ecs.this);
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

    fn unregister_entity<C>(&mut self) -> WithResult<&mut Self, EntityContainer, EcsError>
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

    fn register_resource<T>(
        &mut self,
        desc: T,
    ) -> WithResult<&mut Self, ResourceIndex, EcsError<ResourceDesc>>
    where
        T: Into<ResourceDesc>,
    {
        let res = self.ecs.register_resource(desc).take();
        WithResult::new(self, res)
    }

    fn unregister_resource<R>(&mut self) -> WithResult<&mut Self, Option<Box<dyn Any>>, EcsError>
    where
        R: Resource,
    {
        let res = self.ecs.unregister_resource::<R>().take();
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

    fn execute_commands(
        &mut self,
        cmds: impl Commands,
    ) -> WithResult<&mut Self, (), Box<dyn Error + Send + Sync + 'static>> {
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

#[repr(transparent)]
pub struct RunningEcs<'ecs, W, S, const N: usize>
where
    W: Work + 'static,
    S: BuildHasher + Default + 'static,
{
    ecs: &'ecs mut EcsApp<W, S, N>,
}

impl<'ecs, W, S, const N: usize> RunningEcs<'ecs, W, S, N>
where
    W: Work + 'static,
    S: BuildHasher + Default + 'static,
{
    fn new(ecs: &'ecs mut EcsApp<W, S, N>) -> Self {
        Self { ecs }
    }

    pub fn schedule_all(&mut self) -> &mut Self {
        // Executes.
        if self.has_active_system() {
            let sgroups = self.ecs.sys_stor.sgroups.items_mut();
            let mut cache = RefreshCacheStorage::new(
                &mut self.ecs.cache_stor,
                &mut self.ecs.ent_stor,
                &mut self.ecs.res_stor,
            );
            self.ecs.sched.execute_all(sgroups, &mut cache);
        }

        // Consumes buffered commands.
        self.ecs.process_buffered_commands();

        // Clears dead systems caused by the execution above.
        self.ecs.clear_dead_system();

        self
    }

    // @@@ TODO : Easy API
    pub fn wait_for_idle(&mut self) -> &mut Self {
        self.ecs.sched.wait_exhausted();
        self
    }

    // @@@ TODO : Easy API
    /// Determines whether ECS has run completely, so that it cannot do anything.
    ///
    /// What conditions are considered?
    /// - For sub workers,
    ///   - Are all sub workers closed?
    ///   - Or, are all sub workers idle & Isn't there any running future
    ///     task?
    /// - For main worker,
    ///   - Isn't there any active system?
    ///   - Isn't there any uncompleted command?
    pub fn is_completed(&self) -> bool {
        // For sub workers, they are closed?
        let is_sub_exhausted = self.ecs.sched.is_work_groups_exhausted();

        // For main worker, no active system?
        let no_active = self
            .ecs
            .sys_stor
            .sgroups
            .iter()
            .all(|sgroup| sgroup.len_active() == 0);

        // For main worker, no uncompleted command?
        let no_cmd = !self.ecs.sched.has_command();

        is_sub_exhausted && no_active && no_cmd
    }

    fn has_active_system(&self) -> bool {
        self.ecs
            .sys_stor
            .sgroups
            .iter()
            .any(|sgroup| sgroup.len_active() > 0)
    }
}

#[cfg(test)]
mod tests {
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

        ecs.add_systems((sys0, sys1)).run().schedule_all();

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
    fn test_register_many_resources() {
        use crate as my_ecs;
        let mut ecs = Ecs::default(WorkerPool::with_len(1), [1]);

        // Declares a resource.
        #[derive(Resource)]
        struct Ra(i32);
        #[derive(Resource)]
        struct Rb(String);

        let ra = Ra(0);
        let rb = Rb("b".to_owned());
        ecs.register_resources((ra, rb))
            .add_once_system(|rr: ResRead<(Ra, Rb)>| {
                let (a, b) = rr.0;
                assert_eq!(a.0, 0);
                assert_eq!(&b.0, "b");
            })
            .run()
            .schedule_all();

        assert!(ecs.collect_poisoned_systems().is_empty());
    }
}
