pub mod component;
pub mod entity;
pub mod storage;

pub mod prelude {
    use super::*;

    pub use component::{Component, ComponentKey, Components};
    pub use entity::{
        AddEntity, BorrowComponent, ContainEntity, Entity, EntityId, EntityIndex, EntityName,
        RegisterComponent,
    };
    pub use storage::{AsEntityReg, EntityReg};
}
