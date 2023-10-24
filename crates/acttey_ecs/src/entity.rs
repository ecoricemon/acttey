use acttey_struct::traits::*;
use acttey_struct::SparseSet;
use acttey_util::ty;
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
        T: 'static + Notify + Default,
    {
        self.entities.entry(ty!(<T>)).or_insert_with(|| {
            let mut set = SparseSet::with_capacity(capacity);
            T::default().notify_types(&mut set);
            set
        });
    }

    pub fn insert_entity(&mut self, id: usize, ent: impl 'static + Notify) {
        let set = self.entities.get_mut(&ty!(&ent)).unwrap();
        ent.moves(set, id);
    }

    pub fn remove_entity(&mut self, ent_ty: &TypeId, id: usize) {
        self.entities.get_mut(ent_ty).unwrap().remove(id);
    }

    pub fn get_component<T: 'static>(&self, ent_ty: &TypeId, id: usize) -> Option<&T> {
        self.entities.get(ent_ty)?.get(id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use acttey_ecs_macro::*;
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
        let set = state.entities.get(&ty!(<EntAB>)).unwrap();
        assert!(set.contains_type(&ty!(<CompA>)));
        assert!(set.contains_type(&ty!(<CompB>)));
        assert!(!set.contains_type(&ty!(<CompC>)));

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
        let set = state.entities.get(&ty!(<EntAB>)).unwrap();
        assert_eq!(ents.len(), set.len());
        for (id, ent) in ents.iter().enumerate() {
            assert_eq!(
                Some(ent.a.clone()),
                state.get_component::<CompA>(&ty!(ent), id).cloned()
            );
            assert_eq!(
                Some(ent.b.clone()),
                state.get_component::<CompB>(&ty!(ent), id).cloned()
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
                state.get_component::<CompA>(&ty!(ent), id).cloned()
            );
            assert_eq!(
                Some(ent.b.clone()),
                state.get_component::<CompB>(&ty!(ent), id).cloned()
            )
        }
    }
}
