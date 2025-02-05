#![doc = include_str!("../README.md")]

// Clients may use `ds` module directly. It's optional.
pub(crate) mod default;
pub mod ds;
pub(crate) mod ecs;
pub(crate) mod util;

pub(crate) type DefaultRandomState = std::hash::RandomState;
pub(crate) const MAX_GROUP: usize = 4;

/// Imports all you need at once.
pub mod prelude {
    pub use super::global;
    pub use super::my_ecs_macro_with_doc::*;
    pub use super::{default::prelude::*, ds::prelude::*, ecs::prelude::*, util::prelude::*};
    pub use super::{log, tinfo};
    pub use rayon::prelude::*;
}

pub mod test_util {
    pub use super::util::call_timeout;
}

/// Global functions.
pub mod global {
    pub use super::ecs::stat;
    #[cfg(target_arch = "wasm32")]
    pub use super::ecs::web::{set_panic_hook_once, web_panic_hook};
}

/// Imports `my_ecs_macro` with testable doc.
mod my_ecs_macro_with_doc {
    /// Implements [`Component`](crate::ecs::ent::component::Component) for the
    /// type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// // Zero-sized marker component.
    /// #[derive(Component)]
    /// struct Ca;
    ///
    /// #[derive(Component)]
    /// struct Cb(u8);
    ///
    /// #[derive(Component)]
    /// struct Cc {
    ///     vel: (f32, f32, f32),
    ///     acc: (f32, f32, f32),
    /// }
    /// ```
    pub use my_ecs_macros::Component;

    /// Impelments [`Entity`](crate::ecs::ent::entity::Entity) for the type.
    ///
    /// Actually, you don't have to define entity type explicitly, but by doing
    /// so, the crate becomes to be able to provide easy-to-use functions for
    /// you.
    ///
    /// You can designate which container type you use as well by attributes
    /// `container` and `random_state`.
    ///
    /// `container` means which container type you use for the entity. You can
    /// use your own types or choose one of built-in types shown below.
    /// * [`SparseSet`](crate::default::ent_cont::SparseSet) - Default
    /// * [`ChunkSparseSet`](crate::default::ent_cont::ChunkSparseSet)
    ///
    /// `random_state` means a state to make a hasher.
    /// [`std::hash::RandomState`] is default.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Component)]
    /// struct Ca;
    ///
    /// #[derive(Entity)]
    /// struct Ea {
    ///     a: Ca,
    /// }
    ///
    /// // Or, you can customize entity container.
    /// #[derive(Entity)]
    /// #[container(ChunkSparseSet)]
    /// #[random_state(std::hash::RandomState)]
    /// struct Eb {
    ///     a: Ca,
    /// }
    /// ```
    pub use my_ecs_macros::Entity;

    /// Implements [`Resource`](crate::ecs::resource::Resource) for the type.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Resource)]
    /// struct R(i32);
    /// ```
    pub use my_ecs_macros::Resource;

    /// Implements [`Filter`] for the type, and implements [`Select`] optionally
    /// if `Target` is defined.
    ///
    /// Types implementing `Filter` only can be used in [`EntWrite`] only. Types
    /// implementing both `Filter` and `Select`, on the other hand, also can be
    /// used in [`Read`] and [`Write`] as well. Because `Read` and `Write`
    /// mean requesting read or write access to a specific *target* component.
    ///
    /// See [`Filter`] and [`Select`] for more details.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Component)] struct Ca;
    /// #[derive(Component)] struct Cb;
    /// #[derive(Component)] struct Cc;
    /// #[derive(Component)] struct Cd;
    /// #[derive(Component)] struct Ce;
    ///
    /// // Declares `Fa` with an implemenation of `Filter`.
    /// filter!(Fa, All = Ca);
    ///
    /// // Declares `Fb` with an implemenation of `Filter`.
    /// filter!(Fb, All = Ca, Any = Cb, None = Cc);
    ///
    /// // Declares `Fc` with an implementation of `Filter` and `Select`.
    /// filter!(Fc, Target = Ca);
    ///
    /// // Declares `Fd` with an implementation of `Filter` and `Select`.
    /// filter!(Fd, Target = Ca, All = Cb, Any = Cc, None = (Cd, Ce));
    ///
    /// // All types implement `Filter` which means they can be used in
    /// // `EntWrite`.
    /// fn system_a(ew: EntWrite<(Fa, Fb, Fc, Fd)>) { /* ... */ }
    ///
    /// // On the other hand, `Fc` and `Fd` can be used in `Read` and `Write`
    /// // because they implement `Select` too.
    /// fn system_b(r: Read<Fc>, w: Write<Fd>) { /* ... */ }
    /// ```
    ///
    /// [`Filter`]: crate::ecs::sys::select::Filter
    /// [`Select`]: crate::ecs::sys::select::Select
    /// [`EntWrite`]: crate::ecs::sys::query::EntWrite
    /// [`Read`]: crate::ecs::sys::query::Read
    /// [`Write`]: crate::ecs::sys::query::Write
    pub use my_ecs_macros::filter;

    /// Implements [`Request`] for the type.
    ///
    /// Functions implement the `Request` by the crate internally, but others
    /// such as struct or enum don't. You must implement the `Request` yourself
    /// if you want it to act as a system. This macro helps you write just a
    /// little bit of code for that.
    ///
    /// # Examples
    ///
    /// ```
    /// use my_ecs::prelude::*;
    ///
    /// #[derive(Component)] struct Ca;
    /// #[derive(Component)] struct Cb;
    /// #[derive(Resource)] struct Ra(i32);
    /// #[derive(Resource)] struct Rb(i32);
    ///
    /// filter!(Fa, Target = Ca);
    /// filter!(Fb, Target = Cb);
    /// filter!(Fc, All = (Ca, Cb));
    ///
    /// // Declares `Req` with an implementation of `Request`.
    /// // You can omit Read, Write, ResRead, ResWrite, or EntWrite.
    /// request!(Req, Read = Fa, Write = Fb, ResRead = Ra, ResWrite = Rb, EntWrite = Fc);
    ///
    /// struct Sys {
    ///     data: String,
    /// }
    ///
    /// impl System for Sys {
    ///     type Request = Req;
    ///     fn run(&mut self, resp: Response<'_, Self::Request>) { /* ... */ }
    /// }
    /// ```
    ///
    /// [`Request`]: crate::ecs::sys::request::Request
    pub use my_ecs_macros::request;
}
