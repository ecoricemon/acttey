use crate::{
    ds::sparse_set::SparseSet,
    ecs::{
        ckey, ekey, fkey,
        query::{Filter, FilterInfo, QueryIter, QueryIterMut},
        system::SystemInfo,
        traits::{Collect, CollectGeneric, Component, Downcast, Entity},
        ComponentKey, EntityKey, FilterKey, QueryKey, SystemKey,
    },
    ty,
    util::upcast_slice,
};
use ahash::AHashMap;
use std::{
    any::{type_name, Any, TypeId},
    iter::once,
    mem::{transmute, zeroed},
    ptr::NonNull,
};

#[derive(Debug)]
pub enum BorrowState {
    Available,
    Borrowed(usize),
    BorrowedMutably,
}

#[derive(Default)]
pub struct Storage {
    /// A map storing entities. Each entity type has its own storage.
    /// Key: `TypeId` of `Entity`.
    /// Value: `Entity` storage, e.g. `SparseSet`).
    pub collectors: AHashMap<EntityKey, Box<dyn Collect>>,

    //
    pub downcasters: AHashMap<ComponentKey, NonNull<()>>,

    //
    pub borrow_states: AHashMap<ComponentKey, BorrowState>,

    /// A map holding query results.
    /// Key: `TypeId` combination of `(Filter, (Query, System))`.
    /// Value: Vec<ptr to slice of a collector's piece>, e.g. `AnyVec` as a slice as a ptr.
    query_buffer: AHashMap<FilterKey, Vec<NonNull<[()]>>>,

    /// A map holding system info.
    /// Entries will never be removed from the map.
    sinfo: AHashMap<SystemKey, SystemInfo>,

    //
    filtered_ekeys: AHashMap<FilterKey, Vec<EntityKey>>,

    //
    filtered_ekeys_mut: AHashMap<FilterKey, Vec<EntityKey>>,
}

impl Storage {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert_default<E: Entity>(&mut self) {
        self.insert_entity_type::<E, _>(SparseSet::new());
    }

    pub fn insert_default_with<E: Entity>(&mut self, capacity: usize) {
        self.insert_entity_type::<E, _>(SparseSet::with_capacity(capacity));
    }

    pub fn insert_entity_type<E, T>(&mut self, mut collector: T)
    where
        E: Entity,
        T: Collect + CollectGeneric + Downcast,
    {
        E::notify_types(&mut collector, self);
        self.collectors.insert(ekey!(E), Box::new(collector));
    }

    pub fn insert_entity(&mut self, key: usize, ent: impl Entity) {
        let collector = self.collectors.get_mut(&ekey!(&&ent)).unwrap();
        ent.moves(collector, key);
    }

    pub fn remove_entity(&mut self, e_ty: TypeId, key: usize) {
        self.collectors
            .get_mut(&EntityKey::new(e_ty))
            .unwrap()
            .remove(key);
    }

    pub fn copy_component<C: Component>(&mut self, e_ty: TypeId, key: usize) -> Option<C> {
        let mut comp: C = unsafe { zeroed() };
        let collector = self.collectors.get_mut(&EntityKey::new(e_ty)).unwrap();
        collector.copy(key, &mut comp)?;
        Some(comp)
    }

    // Destructured parameters in order to call this function while borrowing self.
    #[inline]
    pub(crate) fn borrow_slice<'a, C: Component>(
        collectors: &'a mut AHashMap<EntityKey, Box<dyn Collect>>,
        borrow_states: &mut AHashMap<ComponentKey, BorrowState>,
        downcasters: &AHashMap<ComponentKey, NonNull<()>>,
        ekey: EntityKey,
        mutable: bool,
    ) -> &'a mut [C] {
        // Borrow check.
        let ckey = ckey!(C, ekey);
        if mutable {
            borrow_states
                .entry(ckey)
                .and_modify(|v| match v {
                    BorrowState::Available => *v = BorrowState::BorrowedMutably,
                    _ => panic!(
                        "You can't borrow {} mutably that is already borrowed",
                        type_name::<C>()
                    ),
                })
                .or_insert(BorrowState::BorrowedMutably);
        } else {
            borrow_states
                .entry(ckey)
                .and_modify(|v| match v {
                    BorrowState::Available => *v = BorrowState::Borrowed(1),
                    BorrowState::Borrowed(cnt) => *cnt += 1,
                    BorrowState::BorrowedMutably => panic!(
                        "You can't borrow {} that is already borrowed mutably.",
                        type_name::<C>()
                    ),
                })
                .or_insert(BorrowState::Borrowed(1));
        }

        let values = collectors.get_mut(&ekey).unwrap().values_as_any(&ty!(C));
        let downcaster = downcasters.get(&ckey).unwrap();

        unsafe {
            // Make sure that the signature must be same with Downcast::downcast.
            transmute::<_, fn(&mut dyn Any) -> &mut [C]>(downcaster.as_ptr())(values)
        }
    }

    #[inline]
    pub fn returns(&mut self, skey: &SystemKey) {
        let free = |bs: &mut BorrowState| match bs {
            BorrowState::Borrowed(cnt) if *cnt != 1 => *cnt -= 1,
            BorrowState::Available => {
                unreachable!("Tried to return something that was not borrowed.")
            }
            _ => *bs = BorrowState::Available,
        };

        let sinfo = self.sinfo.get(skey).unwrap();
        for ckey in sinfo.reads.iter().chain(sinfo.writes.iter()) {
            free(self.borrow_states.get_mut(ckey).unwrap());
        }
    }

    pub fn get_filtered<F: Filter>(
        &mut self,
        qkey: QueryKey,
        mutable: bool,
    ) -> &mut Vec<NonNull<[()]>> {
        let fkey = fkey!(F, qkey);
        let ekeys = if mutable {
            self.filtered_ekeys_mut.get(&fkey).unwrap()
        } else {
            self.filtered_ekeys.get(&fkey).unwrap()
        };

        self.query_buffer
            .entry(fkey)
            .and_modify(|prev| {
                prev.clear();
                for ekey in ekeys.iter().cloned() {
                    // Safety: `borrow_slice` returns a valid reference, therefore its upcasted pointer is also non-null.
                    prev.push(unsafe {
                        NonNull::new_unchecked(upcast_slice(Self::borrow_slice::<F::Target>(
                            &mut self.collectors,
                            &mut self.borrow_states,
                            &self.downcasters,
                            ekey,
                            mutable,
                        )))
                    });
                }
            })
            .or_insert_with(|| {
                // Safety: `borrow_slice` returns a valid reference, therefore its upcasted pointer is also non-null.
                ekeys
                    .iter()
                    .map(|&e_ty| unsafe {
                        NonNull::new_unchecked(upcast_slice(Self::borrow_slice::<F::Target>(
                            &mut self.collectors,
                            &mut self.borrow_states,
                            &self.downcasters,
                            e_ty,
                            mutable,
                        )))
                    })
                    .collect()
            })
    }

    pub(crate) fn insert_sinfo(&mut self, key: SystemKey, value: SystemInfo) {
        self.sinfo.insert(key, value);
    }

    pub(crate) fn invalidate_sinfo(&mut self) {
        self.filtered_ekeys.clear();
        self.filtered_ekeys_mut.clear();

        let gen_ekeys = |finfo: &FilterInfo| {
            self.collectors
                .iter()
                .filter_map(|(ekey, collector)| {
                    (finfo.all.iter().all(|ty| collector.contains_type(ty))
                        && once(&finfo.target)
                            .chain(finfo.any.iter())
                            .any(|ty| collector.contains_type(ty))
                        && !finfo.none.iter().any(|ty| collector.contains_type(ty)))
                    .then_some(*ekey)
                })
                .collect::<Vec<_>>()
        };

        for sinfo in self.sinfo.values_mut() {
            let mut reads = vec![];
            let mut writes = vec![];

            for finfo in sinfo.qinfo.finfo.iter() {
                let ekeys = gen_ekeys(finfo);
                for ekey in ekeys.iter() {
                    reads.push(ComponentKey::new(finfo.target, *ekey));
                }
                self.filtered_ekeys.insert(finfo.fkey, ekeys);
            }
            for finfo in sinfo.qinfo_mut.finfo.iter() {
                let ekeys = gen_ekeys(finfo);
                for ekey in ekeys.iter() {
                    writes.push(ComponentKey::new(finfo.target, *ekey));
                }
                self.filtered_ekeys_mut.insert(finfo.fkey, ekeys);
            }

            sinfo.set_rw(reads, writes);
        }
    }
}

pub trait Store {
    fn get<'a, F: Filter>(&mut self, qkey: QueryKey) -> QueryIter<'a, F::Target>;
    fn get_mut<'a, F: Filter>(&mut self, qkey: QueryKey) -> QueryIterMut<'a, F::Target>;
}

impl Store for Storage {
    #[inline]
    fn get<'a, F: Filter>(&mut self, qkey: QueryKey) -> QueryIter<'a, F::Target> {
        let query_value = self.get_filtered::<F>(qkey, false);

        // Safety: `k` is unique of all *System-Query-Filter* combinations.
        // As a result, we can guarantee that `v` is invariant during its usage because no one can generate the same `k` except itself.
        // Also, It means downcasting is valid.
        unsafe { QueryIter::new(query_value) }
    }

    #[inline]
    fn get_mut<'a, F: Filter>(&mut self, qkey: QueryKey) -> QueryIterMut<'a, F::Target> {
        let query_value = self.get_filtered::<F>(qkey, true);

        // Safety: `k` is unique of all *System-Query-Filter* combinations.
        // As a result, we can guarantee that `v` is invariant during its usage because no one can generate the same `k` except itself.
        // Also, It means downcasting is valid.
        unsafe { QueryIterMut::new(query_value) }
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
        let mut state = Storage::new();

        // Adds entity type `EntAB` which has CompA and CompB.
        state.insert_default::<EntAB>();
        let collector = state.collectors.get(&ekey!(EntAB)).unwrap();
        assert!(collector.contains_type(&ty!(CompA)));
        assert!(collector.contains_type(&ty!(CompB)));
        assert!(!collector.contains_type(&ty!(CompC)));

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
        let collector = state.collectors.get(&ekey!(EntAB)).unwrap();
        assert_eq!(ents.len(), collector.len());
        for (id, ent) in ents.iter().enumerate() {
            assert_eq!(
                Some(ent.a.clone()),
                state.copy_component::<CompA>(ty!(&ent), id)
            );
            assert_eq!(
                Some(ent.b.clone()),
                state.copy_component::<CompB>(ty!(&ent), id)
            )
        }

        // Removes the first one.
        state.remove_entity(ty!(EntAB), 0);

        // Removed correctly?
        let collector = state.collectors.get(&ekey!(EntAB)).unwrap();
        assert_eq!(ents.len() - 1, collector.len());
        for (id, ent) in ents.iter().enumerate().skip(1) {
            assert_eq!(
                Some(ent.a.clone()),
                state.copy_component::<CompA>(ty!(&ent), id)
            );
            assert_eq!(
                Some(ent.b.clone()),
                state.copy_component::<CompB>(ty!(&ent), id)
            )
        }
    }
}
