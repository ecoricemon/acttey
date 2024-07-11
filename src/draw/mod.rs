pub mod scene;

pub mod prelude {
    pub use super::scene::Scene;
}

// TODO: Delete
pub mod inner {
    use my_ecs::ecs::prelude::*;

    pub struct Scene {}

    pub struct SceneManager {}

    impl SceneManager {
        pub fn new() -> Self {
            Self {}
        }
    }

    impl Resource for SceneManager {}
}
