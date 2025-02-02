//! Implements [`Resource`] for some data types of the crate.

use crate::{
    ds::{
        AnyVec, ChunkAnyVec, GenQueue, GroupMap, IndexedMap, OptVec, SetList, SetValueList,
        SimpleVecPool,
    },
    ecs::resource::Resource,
};

impl Resource for AnyVec {}
impl Resource for ChunkAnyVec {}
impl<T, S> Resource for OptVec<T, S>
where
    T: Send + 'static,
    S: Send + 'static,
{
}
impl<T> Resource for SimpleVecPool<T> where T: Send + 'static {}
impl<K, V, S> Resource for SetList<K, V, S>
where
    K: Send + 'static,
    V: Send + 'static,
    S: Send + 'static,
{
}
impl<V, S> Resource for SetValueList<V, S>
where
    V: Send + 'static,
    S: Send + 'static,
{
}
impl<GK, GV, IK, IV, S> Resource for GroupMap<GK, GV, IK, IV, S>
where
    GK: Send + 'static,
    GV: Send + 'static,
    IK: Send + 'static,
    IV: Send + 'static,
    S: Send + 'static,
{
}
impl<K, V, S, const IMAP: bool> Resource for IndexedMap<K, V, S, IMAP>
where
    K: Send + 'static,
    V: Send + 'static,
    S: Send + 'static,
{
}
impl<T> Resource for GenQueue<T> where T: Send + 'static {}
