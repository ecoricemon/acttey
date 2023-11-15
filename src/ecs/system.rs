use super::{
    query::{Query, QueryInfo, QueryMut},
    skey,
    storage::Storage,
    ComponentKey, QueryKey,
};

pub trait Invokable {
    fn invoke(&self, storage: &mut Storage); // Depends on DataPool for object safety.
}

impl<T: System> Invokable for T {
    #[inline]
    fn invoke(&self, storage: &mut Storage) {
        let skey = skey!(T);
        self.run(
            <T::Ref as Query>::query(storage, skey),
            <T::Mut as QueryMut>::query_mut(storage, skey),
        );
        storage.returns(&skey);
    }
}

pub trait System: 'static {
    type Ref: for<'a> Query<'a>;
    type Mut: for<'a> QueryMut<'a>;

    fn run(&self, r: <Self::Ref as Query>::Output, m: <Self::Mut as QueryMut>::Output);

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
