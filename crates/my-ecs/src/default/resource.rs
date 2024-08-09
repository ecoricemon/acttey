//! Data structures defined by this crate can be resources.
use crate::{ds::prelude::*, ecs::resource::Resource};

impl Resource for AnyVec {}
impl Resource for ChunkAnyVec {}
impl<T, S> Resource for OptVec<T, S>
where
    T: 'static,
    S: 'static,
{
}
impl<T> Resource for SimpleVecPool<T> where T: 'static {}
impl<K, V, S> Resource for SetList<K, V, S>
where
    K: 'static,
    V: 'static,
    S: 'static,
{
}
impl<V, S> Resource for SetValueList<V, S>
where
    V: 'static,
    S: 'static,
{
}
impl<GK, GV, IK, IV, S> Resource for GroupMap<GK, GV, IK, IV, S>
where
    GK: 'static,
    GV: 'static,
    IK: 'static,
    IV: 'static,
    S: 'static,
{
}
impl<K, V, S, const IMAP: bool> Resource for IndexedMap<K, V, S, IMAP>
where
    K: 'static,
    V: 'static,
    S: 'static,
{
}
impl<T> Resource for GenQueue<T> where T: 'static {}
impl<T> Resource for LimitedQueue<T> where T: 'static {}
