use super::{ent::entity::EntityIndex, resource::ResourceIndex};
use crate::ds::{AsDedupVec, DedupVec, GenQueue, OptVec};
use std::{fmt::Debug, hash::BuildHasher, ops::Deref};

/// A struct containing [`WaitQueue`]s for each component, resource, and entity.
/// Their indices are the same with the ones from the data containers.
#[derive(Debug)]
pub(crate) struct WaitQueues<S> {
    ent_queues: OptVec<OptVec<WaitQueue, S>, S>,
    res_queues: OptVec<WaitQueue, S>,
    generation: u64,
}

impl<S> WaitQueues<S>
where
    S: Default,
{
    pub(super) fn new() -> Self {
        Self {
            ent_queues: OptVec::new(),
            res_queues: OptVec::new(),
            generation: 0,
        }
    }
}

impl<S> WaitQueues<S>
where
    S: BuildHasher,
{
    /// Takes O(n) time.
    #[cfg(debug_assertions)]
    pub(super) fn is_all_queue_empty(&self) -> bool {
        self.ent_queues
            .pairs()
            .flat_map(|(_, cols)| cols.pairs())
            .all(|(_, col)| col.is_empty())
            && self.res_queues.pairs().all(|(_, col)| col.is_empty())
    }

    pub(super) fn initialize_resource_queue(&mut self, index: usize) {
        self.res_queues.extend_set(index, WaitQueue::new());
    }

    pub(super) fn enqueue(&mut self, wait: &WaitIndices, retry: &mut WaitRetryIndices) -> bool {
        retry.clean = if retry.clean {
            self._enqueue(wait, retry)
        } else {
            self._check_availability(retry)
        };
        retry.clean
    }

    pub(super) fn dequeue(&mut self, wait: &WaitIndices) {
        // Dequeues component read & write requests from their wait queues.
        let wait_read_iter = wait.read.iter().map(|(ei, ci)| (ei.index(), *ci));
        let wait_write_iter = wait.write.iter().map(|(ei, ci)| (ei.index(), *ci));
        dequeue_comp(&mut self.ent_queues, wait_read_iter);
        dequeue_comp(&mut self.ent_queues, wait_write_iter);

        // Dequeues resource read & write requests from their wait queues.
        dequeue_res(
            &mut self.res_queues,
            wait.res_read.iter().map(ResourceIndex::index),
        );
        dequeue_res(
            &mut self.res_queues,
            wait.res_write.iter().map(ResourceIndex::index),
        );

        // Dequeues entity write requests from their wait queues.
        dequeue_ent(
            &mut self.ent_queues,
            wait.ent_write.iter().map(EntityIndex::index),
        );

        // Increases generation.
        self.generation += 1;

        // === Internal helper functions ===

        /// Dequeues an item from component wait queues.
        /// The component queues are picked up by `wait` indices.
        fn dequeue_comp<S, I>(ent_queues: &mut OptVec<OptVec<WaitQueue, S>, S>, wait_iter: I)
        where
            S: BuildHasher,
            I: Iterator<Item = (usize, usize)>,
        {
            for (ei, ci) in wait_iter {
                let comp_queues = ent_queues.get_mut(ei).unwrap();
                let queue = comp_queues.get_mut(ci).unwrap();
                queue.pop();
            }
        }

        /// Dequeues an item from resource wait queues.
        /// The component queues are picked up by `wait` indices.
        fn dequeue_res<S, I>(res_queues: &mut OptVec<WaitQueue, S>, wait_iter: I)
        where
            S: BuildHasher,
            I: Iterator<Item = usize>,
        {
            for resi in wait_iter {
                let queue = res_queues.get_mut(resi).unwrap();
                queue.pop();
            }
        }

        /// Dequeues an item from component wait queues.
        /// The component queues are picked up by `wait` indices.
        fn dequeue_ent<S, I>(ent_queues: &mut OptVec<OptVec<WaitQueue, S>, S>, wait_iter: I)
        where
            S: BuildHasher,
            I: Iterator<Item = usize>,
        {
            for ei in wait_iter {
                let comp_queues = ent_queues.get_mut(ei).unwrap();
                for (_, queue) in comp_queues.pairs_mut() {
                    queue.pop();
                }
            }
        }
    }

    /// Enqueues requests for access authority of components or resources
    /// according to `wait` indices.
    /// If they're good to be accessed now, returns true.
    /// Otherwise, returns false, and then failed indices will be inserted into `retry`.
    fn _enqueue(&mut self, wait: &WaitIndices, retry: &mut WaitRetryIndices) -> bool {
        let mut res = true;

        // Enqueues component read & write requests into their wait queues.
        let wait_read_iter = wait.read.iter().map(|(ei, ci)| (ei.index(), *ci));
        let wait_write_iter = wait.write.iter().map(|(ei, ci)| (ei.index(), *ci));
        res &= enqueue_comp(
            &mut self.ent_queues,
            RW::Read,
            wait_read_iter,
            &mut retry.read,
        );
        res &= enqueue_comp(
            &mut self.ent_queues,
            RW::Write,
            wait_write_iter,
            &mut retry.write,
        );

        // Enqueues resource read & write requests into their wait queues.
        let wait_res_read_iter = wait.res_read.iter().map(ResourceIndex::index);
        let wait_res_write_iter = wait.res_write.iter().map(ResourceIndex::index);
        res &= enqueue_res(
            &mut self.res_queues,
            RW::Read,
            wait_res_read_iter,
            &mut retry.res_read,
        );
        res &= enqueue_res(
            &mut self.res_queues,
            RW::Write,
            wait_res_write_iter,
            &mut retry.res_write,
        );

        // Enqueues entity write requests into their wait queues.
        res &= enqueue_ent(
            &mut self.ent_queues,
            wait.ent_write.iter().map(EntityIndex::index),
            &mut retry.ent_write,
        );

        return res;

        // === Internal helper functions ===

        /// Enqueues waiting signal into component wait queues according to `wait` indices.
        /// If all of them are positioned to the front of corresponding queues,
        /// returns true, which means they're allowed to access corresponding components.
        /// Otherwise, returns false and then indices needed to retry later
        /// are pushed `retry`.
        ///
        /// # Panics
        ///
        /// Panics if any index in `wait` is out of bounds.
        fn enqueue_comp<S, I>(
            ent_queues: &mut OptVec<OptVec<WaitQueue, S>, S>,
            rw: RW,
            wait_iter: I,
            retry: &mut Vec<(u64, usize, usize)>,
        ) -> bool
        where
            S: BuildHasher,
            I: Iterator<Item = (usize, usize)>,
        {
            debug_assert!(retry.is_empty());

            let mut available = true;
            for (ei, ci) in wait_iter {
                let comp_queues = ent_queues.get_mut(ei).unwrap();
                let queue = comp_queues.get_mut(ci).unwrap();
                let target_gen = queue.push(rw);
                if target_gen != queue.generation() {
                    retry.push((target_gen, ei, ci));
                    available = false;
                }
            }
            available
        }

        /// Enqueues waiting signal into resource wait queues according to `wait` indices.
        /// If all of them are positioned to the front of corresponding queues,
        /// returns true, which means they're allowed to access corresponding resources.
        /// Otherwise, returns false and then indices needed to retry later
        /// are pushed `retry`.
        ///
        /// # Panics
        ///
        /// Panics if any index in `wait` is out of bounds.
        fn enqueue_res<S, I>(
            res_queues: &mut OptVec<WaitQueue, S>,
            rw: RW,
            wait_iter: I,
            retry: &mut Vec<(u64, usize)>,
        ) -> bool
        where
            S: BuildHasher,
            I: Iterator<Item = usize>,
        {
            debug_assert!(retry.is_empty());

            let mut available = true;
            for ri in wait_iter {
                let queue = res_queues.get_mut(ri).unwrap();
                let target_gen = queue.push(rw);
                if target_gen != queue.generation() {
                    retry.push((target_gen, ri));
                    available = false;
                }
            }
            available
        }

        /// Enqueues waiting signal into component wait queues according to `wait` indices.
        /// If all of them are positioned to the front of corresponding queues,
        /// returns true, which means they're allowed to access corresponding components.
        /// Otherwise, returns false and then indices needed to retry later
        /// are pushed `retry`.
        ///
        /// # Panics
        ///
        /// Panics if any index in `wait` is out of bounds.
        fn enqueue_ent<S, I>(
            ent_queues: &mut OptVec<OptVec<WaitQueue, S>, S>,
            wait_iter: I,
            retry: &mut Vec<(u64, usize, usize)>,
        ) -> bool
        where
            S: BuildHasher,
            I: Iterator<Item = usize>,
        {
            debug_assert!(retry.is_empty());

            let mut available = true;
            for ei in wait_iter {
                let comp_queues = ent_queues.get_mut(ei).unwrap();
                for (ci, queue) in comp_queues.pairs_mut() {
                    let target_gen = queue.push(RW::Write);
                    if target_gen != queue.generation() {
                        retry.push((target_gen, ei, ci));
                        available = false;
                    }
                }
            }
            available
        }
    }

    fn _check_availability(&mut self, retry: &mut WaitRetryIndices) -> bool {
        // Checks all requests once again.
        return check_comp(&self.ent_queues, &mut retry.read)
            && check_comp(&self.ent_queues, &mut retry.write)
            && check_res(&self.res_queues, &mut retry.res_read)
            && check_res(&self.res_queues, &mut retry.res_write)
            && check_comp(&self.ent_queues, &mut retry.ent_write);

        // === Internal helper functions ===

        /// Checks component availability according to `retry` indices.
        /// Passed items in `retry` will be removed, but the failed items
        /// will be left, so they'll be checked again later.
        /// If all items are ejected, returns true.
        fn check_comp<S>(
            ent_queues: &OptVec<OptVec<WaitQueue, S>, S>,
            retry: &mut Vec<(u64, usize, usize)>,
        ) -> bool
        where
            S: BuildHasher,
        {
            while let Some((target_gen, ei, ci)) = retry.pop() {
                let comp_queues = ent_queues.get(ei).unwrap();
                let queue = comp_queues.get(ci).unwrap();
                if target_gen != queue.generation() {
                    retry.push((target_gen, ei, ci));
                    return false;
                }
            }
            true
        }

        /// Checks resource availability according to `retry` indices.
        /// Passed items in `retry` will be removed, but the failed items
        /// will be left, so they'll be checked again later.
        /// If all items are ejected, returns true.
        fn check_res<S>(res_queues: &OptVec<WaitQueue, S>, retry: &mut Vec<(u64, usize)>) -> bool
        where
            S: BuildHasher,
        {
            while let Some((target_gen, resi)) = retry.pop() {
                let queue = res_queues.get(resi).unwrap();
                if target_gen != queue.generation() {
                    retry.push((target_gen, resi));
                    return false;
                }
            }
            true
        }
    }
}

impl<S> WaitQueues<S>
where
    S: BuildHasher + Default,
{
    /// Makes wait queues for an entity.
    /// This makes a queue at the position `ei`.
    /// Also, it makes inner queues for components of the entity as much as `ncol`.
    /// If there was a queue at the position, it will be dropped first.
    pub(super) fn initialize_entity_queue(&mut self, ei: usize, ncol: usize) {
        // Prepares wait queues for columns.
        let mut cols = OptVec::new();
        for _ in 0..ncol {
            cols.push(Some(WaitQueue::new()));
        }
        self.ent_queues.extend_set(ei, cols);
    }
}

impl<S> Default for WaitQueues<S>
where
    S: Default,
{
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
#[repr(transparent)]
struct WaitQueue(GenQueue<(RW, u32)>);

impl WaitQueue {
    const fn new() -> Self {
        Self(GenQueue::new())
    }

    fn push(&mut self, rw: RW) -> u64 {
        if rw == RW::Read {
            if let Some(back) = self.0.back_mut() {
                if back.0 == RW::Read {
                    back.1 += 1;
                    return self.generation() + (self.len() - 1) as u64;
                }
            }
        }
        self.0.push_back((rw, 1));
        self.generation() + (self.len() - 1) as u64
    }

    fn pop(&mut self) {
        let front = self.0.front_mut().unwrap();
        if front.1 > 1 {
            front.1 -= 1;
        } else {
            self.0.pop_front();
        }
    }
}

impl Deref for WaitQueue {
    type Target = GenQueue<(RW, u32)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Read or write
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub(super) enum RW {
    Read,
    Write,
}

#[derive(Debug)]
pub(super) struct WaitIndices {
    /// Wait queue index pairs to read-only components.
    /// Each pair is the same as entity container index and a specific column index.
    pub(super) read: DedupVec<(EntityIndex, usize), false>,

    /// Wait queue index pairs to writable components.
    /// Each pair is the same as entity container index and a specific column index.
    pub(super) write: DedupVec<(EntityIndex, usize), false>,

    /// Wait queue indices to read-only resources.
    pub(super) res_read: DedupVec<ResourceIndex, false>,

    /// Wait queue indices to writable resources.
    pub(super) res_write: DedupVec<ResourceIndex, false>,

    /// Wait queue indices to writable entity container.
    /// Each index is the same as entity container index.
    pub(super) ent_write: DedupVec<EntityIndex, false>,
}

#[derive(Debug)]
pub(super) struct WaitRetryIndices {
    /// Not ready [`WaitIndices::read`] and their target generations.
    pub(super) read: Vec<(u64, usize, usize)>,

    /// Not ready [`WaitIndices::write`] and their target generations.
    pub(super) write: Vec<(u64, usize, usize)>,

    /// Not ready [`WaitIndices::res_read`] and their target generations.
    pub(super) res_read: Vec<(u64, usize)>,

    /// Not ready [`WaitIndices::res_write`] and their target generations.
    pub(super) res_write: Vec<(u64, usize)>,

    /// Not ready [`WaitIndices::ent_write`] and their target generations.
    pub(super) ent_write: Vec<(u64, usize, usize)>,

    /// If it's not clean, that means it's in the middle of availability check.
    /// So we can continue the check rather than doing it from the beginning.
    pub(super) clean: bool,
}

impl WaitRetryIndices {
    pub(super) const fn new() -> Self {
        Self {
            read: Vec::new(),
            write: Vec::new(),
            res_read: Vec::new(),
            res_write: Vec::new(),
            ent_write: Vec::new(),
            clean: true,
        }
    }
}
