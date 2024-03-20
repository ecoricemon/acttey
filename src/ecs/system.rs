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

/// Object safe trait for various types of systems.
pub trait Invokable {
    fn invoke(&mut self, res_pack: &ResourcePack);
}

impl<S: System> Invokable for S {
    #[inline]
    fn invoke(&mut self, res_pack: &ResourcePack) {
        let storage = res_pack.get::<Storage>();
        let skey = skey!(S);
        let param = SysParam {
            read: <S::Read as Query>::query(storage, skey),
            write: <S::Write as QueryMut>::query_mut(storage, skey),
            res_read: <S::ResRead as ResQuery>::query(res_pack),
            res_write: <S::ResWrite as ResQueryMut>::query_mut(res_pack),
        };
        self.run(param);
    }
}

pub trait System: 'static + Sized {
    type Read: for<'a> Query<'a>;
    type Write: for<'a> QueryMut<'a>;
    type ResRead: for<'a> ResQuery<'a>;
    type ResWrite: for<'a> ResQueryMut<'a>;

    fn run(&mut self, param: SysParam<'_, Self>);

    fn info() -> SystemInfo {
        let skey = skey!(Self);
        SystemInfo {
            qkey: <Self::Read as Query>::gen_key(skey),
            qkey_mut: <Self::Write as QueryMut>::gen_key_mut(skey),
            qinfo: <Self::Read as Query>::info(skey),
            qinfo_mut: <Self::Write as QueryMut>::info_mut(skey),
            reads: vec![],
            writes: vec![],
        }
    }

    fn key() -> SystemKey {
        skey!(Self)
    }
}

/// A shallow warpper of query retrieval results.
pub struct SysParam<'a, S: System> {
    /// Retrieved data for the query [`System::Read`].
    pub read: <S::Read as Query<'a>>::Output,

    /// Retrieved data for the query [`System::Write`].
    pub write: <S::Write as QueryMut<'a>>::Output,

    /// Retrieved data for the query [`System::ResRead`].
    pub res_read: <S::ResRead as ResQuery<'a>>::Output,

    /// Retrieved data for the query [`System::ResWrite`].
    pub res_write: <S::ResWrite as ResQueryMut<'a>>::Output,
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
