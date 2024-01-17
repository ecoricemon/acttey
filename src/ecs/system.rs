use crate::{
    ds::sparse_set::MonoSparseSet,
    ecs::{
        predefined::resource::ResourcePack,
        query::{Query, QueryInfo, QueryMut, ResQuery, ResQueryMut},
        skey,
        storage::Storage,
        ComponentKey, QueryKey, SystemKey,
    },
};

pub trait Invokable {
    // Depends on DataPool for object safety.
    fn invoke(&mut self, res_pack: &ResourcePack);
}

impl<S: System> Invokable for S {
    #[inline]
    fn invoke(&mut self, res_pack: &ResourcePack) {
        let storage = res_pack.get::<Storage>();
        let skey = skey!(S);
        self.run(
            <S::Ref as Query>::query(storage, skey),
            <S::Mut as QueryMut>::query_mut(storage, skey),
            <S::ResRef as ResQuery>::query(res_pack),
            <S::ResMut as ResQueryMut>::query_mut(res_pack),
        );
        storage.returns(&skey);
    }
}

pub trait System: 'static {
    type Ref: for<'a> Query<'a>;
    type Mut: for<'a> QueryMut<'a>;
    type ResRef: for<'a> ResQuery<'a>;
    type ResMut: for<'a> ResQueryMut<'a>;

    fn run(
        &mut self,
        r: <Self::Ref as Query>::Output,
        m: <Self::Mut as QueryMut>::Output,
        rr: <Self::ResRef as ResQuery>::Output,
        rm: <Self::ResMut as ResQueryMut>::Output,
    );

    fn info() -> SystemInfo {
        let skey = skey!(Self);
        SystemInfo {
            qkey: <Self::Ref as Query>::gen_key(skey),
            qkey_mut: <Self::Mut as QueryMut>::gen_key_mut(skey),
            qinfo: <Self::Ref as Query>::info(skey),
            qinfo_mut: <Self::Mut as QueryMut>::info_mut(skey),
            reads: vec![],
            writes: vec![],
        }
    }

    fn key() -> SystemKey {
        skey!(Self)
    }
}

pub struct SystemInfo {
    pub qkey: QueryKey,
    pub qkey_mut: QueryKey,
    pub qinfo: QueryInfo,
    pub qinfo_mut: QueryInfo,
    pub reads: Vec<ComponentKey>,
    pub writes: Vec<ComponentKey>,
}

impl SystemInfo {
    pub fn new<T: System>() -> Self {
        T::info()
    }

    pub fn set_rw(&mut self, reads: Vec<ComponentKey>, writes: Vec<ComponentKey>) {
        self.reads = reads;
        self.writes = writes;
    }
}

impl<T: System> From<T> for SystemInfo {
    fn from(_: T) -> Self {
        T::info()
    }
}

/// TODO: Systems should have priority or a sort of order to be runned.
/// The order must be kept when a supersystem inserts or removes.
pub type Systems = MonoSparseSet<SystemKey, Box<dyn Invokable>>;
