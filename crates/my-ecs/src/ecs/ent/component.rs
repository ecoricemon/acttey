use my_ecs_macros::repeat_macro;

use crate::ds::prelude::*;

/// Ordinary rust types.
pub trait Component: Send + Sync + Sized + 'static {
    const IS_SEND: bool = true; // by bound
    const IS_SYNC: bool = true; // by bound

    // === Must be overwitten by something like [`my_ecs_macros::Component`] ===
    const IS_DEFAULT: bool = false; // depends on impl
    const FN_DEFAULT: FnDefaultRaw = unimpl_default; // depends on impl
    const IS_CLONE: bool = false; // depends on impl
    const FN_CLONE: FnCloneRaw = unimpl_clone; // depends on impl

    fn key() -> ComponentKey {
        ComponentKey::of::<Self>()
    }

    fn type_info() -> TypeInfo {
        let mut tinfo = Self::as_type_info();
        tinfo.set_additional(
            Self::IS_SEND,
            Self::IS_SYNC,
            Self::IS_DEFAULT.then_some(Self::FN_DEFAULT),
            Self::IS_CLONE.then_some(Self::FN_CLONE),
        );
        tinfo
    }
}

pub trait Components: 'static {
    type Keys: AsRef<[ComponentKey]>;
    type Infos: AsRef<[TypeInfo]>;

    const LEN: usize;

    fn keys() -> Self::Keys;
    fn infos() -> Self::Infos;
    fn sorted_keys() -> Self::Keys;
}

macro_rules! impl_components {
    ($n:expr, $($i:expr),*) => {const _: () = {
        #[allow(unused_imports)]
        use $crate::{
            ds::types::TypeInfo,
            ecs::ent::component::{Component, Components, ComponentKey},
        };
        use paste::paste;

        paste! {
            #[allow(unused_parens)]
            impl<$([<A $i>]: Component),*> Components for ( $([<A $i>]),* ) {
                type Keys = [ComponentKey; $n];
                type Infos = [TypeInfo; $n];

                const LEN: usize = $n;

                fn keys() -> Self::Keys {
                    [ $( [<A $i>]::key() ),* ]
                }

                fn infos() -> Self::Infos {
                    [ $( [<A $i>]::type_info() ),* ]
                }

                fn sorted_keys() -> Self::Keys {
                    let mut keys = [ $( [<A $i>]::key() ),* ];
                    keys.sort_unstable();
                    keys
                }
            }
        }
    };};
}
repeat_macro!(impl_components, ..=32);

/// Unique identifier for a type implementing [`Component`].
pub type ComponentKey = ATypeId<ComponentKey_>;
pub struct ComponentKey_;

#[cfg(test)]
mod tests {
    use super::*;
    use crate as my_ecs;
    use my_ecs_macros::Component;

    #[test]
    fn test_component_detect_impls() {
        #![allow(dead_code)]

        #[derive(Component, Clone)]
        struct CloneA([u8; 1]);
        #[derive(Component, Clone)]
        struct CloneB([u8; 2]);

        #[derive(Component)]
        struct NonCloneA([u8; 1]);
        #[derive(Component)]
        struct NonCloneB([u8; 2]);

        // Non-cloneable components have the same clone function which causes
        // panic.
        assert!(is_clone_fn_eq::<NonCloneA, NonCloneB>());

        // But cloneable components have different clone functions to each
        // other.
        assert!(is_clone_fn_ne::<CloneA, CloneB>());
        assert!(is_clone_fn_ne::<CloneA, NonCloneA>());
        assert!(is_clone_fn_ne::<CloneB, NonCloneA>());

        // === Internal helper functions ===

        fn is_clone_fn_eq<Ca: Component, Cb: Component>() -> bool {
            let clone_a = Ca::FN_CLONE as usize;
            let clone_b = Cb::FN_CLONE as usize;
            clone_a == clone_b
        }

        fn is_clone_fn_ne<Ca: Component, Cb: Component>() -> bool {
            let clone_a = Ca::FN_CLONE as usize;
            let clone_b = Cb::FN_CLONE as usize;
            clone_a != clone_b
        }
    }
}
