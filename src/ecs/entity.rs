use super::traits::{Collect, Component, Entity, ErasedCollect};
use crate::{ds::sparse_set::SparseSet, ty};
use std::any::TypeId;
use std::collections::HashMap;
use std::mem::zeroed;

#[derive(Default)]
pub struct EntityState {
    // TODO: ErasedCollect causes performance loss but gives us flexibility. Users can make their own entity storage.
    entities: HashMap<TypeId, Box<dyn ErasedCollect>>,
}

impl EntityState {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_entity_type<E: Entity>(&mut self, capacity: usize) {
        self.entities.entry(ty!(<E>)).or_insert_with(|| {
            let mut collector = SparseSet::with_capacity(capacity);
            E::notify_types(&mut collector);
            Box::new(collector)
        });
    }

    pub fn insert_entity(&mut self, key: usize, ent: impl Entity) {
        let collector: &mut Box<dyn ErasedCollect> = self.entities.get_mut(&ty!(&ent)).unwrap();
        ent.moves(collector, key);
    }

    pub fn remove_entity(&mut self, ent_ty: &TypeId, key: usize) {
        self.entities.get_mut(ent_ty).unwrap().remove(key);
    }

    pub fn copy_component<C: Component>(&mut self, ent_ty: &TypeId, key: usize) -> Option<C> {
        let mut comp: C = unsafe { zeroed() };
        let collector: &mut Box<dyn ErasedCollect> = self.entities.get_mut(ent_ty).unwrap();
        collector.copy(key, &mut comp)?;
        Some(comp)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::acttey::*;
    use acttey_ecs_macros::{Component, Entity};
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    #[derive(Component, Default, Clone, PartialEq, Debug)]
    struct CompA {
        x: i32,
    }

    #[derive(Component, Default, Clone, PartialEq, Debug)]
    struct CompB(i32);

    #[derive(Component, Default, Clone, PartialEq, Debug)]
    struct CompC;

    #[derive(Entity, Default, Clone, PartialEq, Debug)]
    struct EntAB {
        a: CompA,
        b: CompB,
    }

    #[wasm_bindgen_test]
    fn test_entity_state_insert_remove() {
        let mut state = EntityState::new();

        // Adds entity type `EntAB` which has CompA and CompB.
        state.add_entity_type::<EntAB>(0);
        let collector = state.entities.get(&ty!(<EntAB>)).unwrap();
        assert!(collector.contains_type(&ty!(<CompA>)));
        assert!(collector.contains_type(&ty!(<CompB>)));
        assert!(!collector.contains_type(&ty!(<CompC>)));

        // Test entities
        let ents = [
            EntAB {
                a: CompA { x: 1 },
                b: CompB(2),
            },
            EntAB {
                a: CompA { x: 3 },
                b: CompB(4),
            },
        ];

        // Inserts the test entities.
        for (id, ent) in ents.iter().enumerate() {
            state.insert_entity(id, ent.clone());
        }

        // Inserted correctly?
        let collector = state.entities.get(&ty!(<EntAB>)).unwrap();
        assert_eq!(ents.len(), collector.len());
        for (id, ent) in ents.iter().enumerate() {
            assert_eq!(
                Some(ent.a.clone()),
                state.copy_component::<CompA>(&ty!(ent), id)
            );
            assert_eq!(
                Some(ent.b.clone()),
                state.copy_component::<CompB>(&ty!(ent), id)
            )
        }

        // Removes the first one.
        state.remove_entity(&ty!(<EntAB>), 0);

        // Removed correctly?
        let set = state.entities.get(&ty!(<EntAB>)).unwrap();
        assert_eq!(ents.len() - 1, set.len());
        for (id, ent) in ents.iter().enumerate().skip(1) {
            assert_eq!(
                Some(ent.a.clone()),
                state.copy_component::<CompA>(&ty!(ent), id)
            );
            assert_eq!(
                Some(ent.b.clone()),
                state.copy_component::<CompB>(&ty!(ent), id)
            )
        }
    }
}
