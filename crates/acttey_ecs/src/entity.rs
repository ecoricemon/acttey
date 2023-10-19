use acttey_ecs_trait::*;
use acttey_struct::traits::*;
use acttey_struct::SparseSet;
use std::any::TypeId;
use std::collections::HashMap;

#[derive(Default)]
pub struct EntityState {
    entities: HashMap<TypeId, SparseSet>,
}

impl EntityState {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_entity_type<T>(&mut self, capacity: usize)
    where
        T: 'static + Entity + NotifyType + Default,
    {
        self.entities.entry(TypeId::of::<T>()).or_insert_with(|| {
            let mut set = SparseSet::with_capacity(capacity);
            T::default().notify_types(&mut set);
            set
        });
    }

    pub fn insert_entity<T>(&mut self, id: usize, ent: T)
    where
        T: 'static + Entity + NotifyType + Default,
    {
        let set = self.entities.get_mut(&TypeId::of::<T>()).unwrap();
        ent.moves(set, id);
    }

    pub fn remove_entity<T>(&mut self, id: usize)
    where
        T: 'static + Entity + NotifyType + Default,
    {
        let set = self.entities.get_mut(&TypeId::of::<T>()).unwrap();
        set.remove(id);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use acttey_ecs_macro::*;
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    #[derive(Component, Default)]
    struct CompA {
        // x: i32,
    }

    #[derive(Component, Default)]
    struct CompB(i32);

    #[derive(Component, Default)]
    struct CompC;

    #[derive(Entity, Default)]
    struct EntAB {
        a: CompA,
        b: CompB,
    }

    #[wasm_bindgen_test]
    fn test_entity_state_add_entity_type() {
        let mut state = EntityState::new();
        state.add_entity_type::<EntAB>(0);
        let set = state.entities.get(&TypeId::of::<EntAB>()).unwrap();
        assert!(set.contains_type::<CompA>());
        assert!(set.contains_type::<CompB>());
        assert!(!set.contains_type::<CompC>());
    }
}
