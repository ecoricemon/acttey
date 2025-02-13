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
    pub use super::{default::prelude::*, ds::prelude::*, ecs::prelude::*, util::prelude::*};
    pub use super::{log, tinfo};
    pub use my_ecs_macros::{filter, request, Component, Entity, Resource};
    #[doc(no_inline)]
    pub use rayon::prelude::*;
}

/// A module containing test helper functionalities.
pub mod test_util {
    pub use super::util::call_timeout;
}

/// Global functions.
pub mod global {
    pub use super::ecs::stat;
    #[cfg(target_arch = "wasm32")]
    pub use super::ecs::web::{set_panic_hook_once, web_panic_hook};
}

// Reason why we didn't put the doc on re-exports is rust-analyzer doesn't show
// us the comment in IDEs. rust-analyzer ignores comments on re-exports then
// only displays comments on source definitions.
//
// But the other problem occurs by putting doc comments in the proc macro. The
// proc macro doesn't know types or traits defined in this outer crate. So
// corresponding doc tests are here and dealt with as unit tests.
//
// This will be removed once rust-analyzer solves the problem above.
#[cfg(test)]
mod my_ecs_macros_doc_tests {
    #![allow(unused)]
    use crate as my_ecs;

    // SRC DOC: macros/src/lib.rs derive_component
    #[test]
    fn test_my_ecs_macros_doc_derive_component() {
        use my_ecs::prelude::*;

        // Zero-sized marker component.
        #[derive(Component)]
        struct Ca;

        #[derive(Component)]
        struct Cb(u8);

        #[derive(Component)]
        struct Cc {
            vel: (f32, f32, f32),
            acc: (f32, f32, f32),
        }
    }

    // SRC DOC: macros/src/lib.rs derive_entity
    #[test]
    fn test_my_ecs_macros_doc_derive_entity() {
        use my_ecs::prelude::*;

        #[derive(Component)]
        struct Ca;

        #[derive(Entity)]
        struct Ea {
            a: Ca,
        }

        // Or, you can customize entity container.
        #[derive(Entity)]
        #[container(ChunkSparseSet)]
        #[random_state(std::hash::RandomState)]
        struct Eb {
            a: Ca,
        }
    }

    // SRC DOC: macros/src/lib.rs derive_resource
    #[test]
    fn test_my_ecs_macros_doc_derive_resource() {
        use my_ecs::prelude::*;

        #[derive(Resource)]
        struct R(i32);
    }

    // SRC DOC: macros/src/lib.rs filter
    #[test]
    fn test_my_ecs_macros_doc_filter() {
        use my_ecs::prelude::*;

        #[derive(Component)]
        struct Ca;
        #[derive(Component)]
        struct Cb;
        #[derive(Component)]
        struct Cc;
        #[derive(Component)]
        struct Cd;
        #[derive(Component)]
        struct Ce;

        // Declares `Fa` with an implemenation of `Filter`.
        filter!(Fa, All = Ca);

        // Declares `Fb` with an implemenation of `Filter`.
        filter!(Fb, All = Ca, Any = Cb, None = Cc);

        // Declares `Fc` with an implementation of `Filter` and `Select`.
        filter!(Fc, Target = Ca);

        // Declares `Fd` with an implementation of `Filter` and `Select`.
        filter!(Fd, Target = Ca, All = Cb, Any = Cc, None = (Cd, Ce));

        // All types implement `Filter` which means they can be used in
        // `EntWrite`.
        fn system_a(ew: EntWrite<(Fa, Fb, Fc, Fd)>) { /* ... */
        }

        // On the other hand, `Fc` and `Fd` can be used in `Read` and `Write`
        // because they implement `Select` too.
        fn system_b(r: Read<Fc>, w: Write<Fd>) { /* ... */
        }
    }

    // SRC DOC: macros/src/lib.rs request
    #[test]
    fn test_my_ecs_macros_doc_request() {
        use my_ecs::prelude::*;

        #[derive(Component)]
        struct Ca;
        #[derive(Component)]
        struct Cb;
        #[derive(Resource)]
        struct Ra(i32);
        #[derive(Resource)]
        struct Rb(i32);

        filter!(Fa, Target = Ca);
        filter!(Fb, Target = Cb);
        filter!(Fc, All = (Ca, Cb));

        // Declares `Req` with an implementation of `Request`.
        // You can omit Read, Write, ResRead, ResWrite, or EntWrite.
        request!(
            Req,
            Read = Fa,
            Write = Fb,
            ResRead = Ra,
            ResWrite = Rb,
            EntWrite = Fc
        );

        struct Sys {
            data: String,
        }

        impl System for Sys {
            type Request = Req;
            fn run(&mut self, resp: Response<'_, Self::Request>) { /* ... */
            }
        }
    }
}
